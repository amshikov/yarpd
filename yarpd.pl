#!/usr/local/bin/perl -w

no warnings 'experimental';
use strict;
use vars qw/$CF $CFG $CFGMON/;
use Sys::Syslog qw(:standard :macros);
use URI;
use SNMP;
use Data::Dumper;
$Data::Dumper::Deparse = 1;
use Hash::Merge qw(merge);
use Data::Rmap;
use Storable qw(dclone);
use RRDs;
use File::Monitor;
use List::Compare;
use DateTime;

$|=1;

$CF = './yarpd.conf';

#
# Configure File
#
Prepare_Config($CF);

#
# TODO: Daemonization
# At now it still runs only in foreground

#
# Create configuration file monitor
#
my $CFGMON = File::Monitor->new();
$CFGMON->watch($CF);
$CFGMON->scan();

#
# Main cycle
#
MAIN();

# ----------------------------------------------------------------------------
#
# Function reads configuration file and properly updates $CFG hashref
#
sub Prepare_Config {
	my $cf = shift;

	#
	# Check whether config is already loaded, or it's a brand new start
	#
	if (defined $CFG) {
		syslog(LOG_DEBUG, "! Configuration file change detected. Reloading...");
		# Close log file
		closelog();
		$CFG = undef;
	}

	#
	# Include config
	#
	do "$cf" or die $!;

	#
	# Open logging
	# TODO: Make facility configurable
	openlog('yarpd', 'ndelay', LOG_DAEMON);

	#
	# Process templates hierarchy
	#
	_CFG_Process_Templates();

	#
	# Update RRD files properly
	#
	_CFG_Process_Files();

	#
	# Process data gathering methods defined in 'source' field of each datasource
	#
	_CFG_Process_Sources();

	#
	# Process SNMP host keys, indexes etc.
	#
	_CFG_Process_SNMP();

}

sub _CFG_Process_Templates {
	syslog(LOG_NOTICE, "* Re-building templates...");

	while (my ($t, $template) = each $CFG->{templates}) {
		if (defined $template->{inherit}) {
			my @in_templates = @{$template->{inherit}};
			delete $template->{inherit};

			foreach my $ti (@in_templates) {
				if (defined $CFG->{templates}->{$ti}) {
					syslog(LOG_ERR, "  Inheriting template %s into %s...", $ti, $t);
					# Clone template
					my $tc = dclone($CFG->{templates}->{$ti});

					# Replace variables with values
					rmap { 
						my $line = $_; 
						while (/%%([\w\d\-_]+)%%/g) {
							my $v = $1;
							if (defined $template->{var}->{$v}) {
								$line =~ s/%%$v%%/$template->{var}->{$v}/e;
							} 
						}
						$_ = $line;
					} $tc;

					# Merge
					$template = $CFG->{templates}->{$t} = merge ($template, $tc);
				} else {
					syslog(LOG_ERR, "  Unable to inherit template %s into %s. %s is not configured!", $ti, $t, $ti);
					next;
				}
			}
			redo;
		} else {
			next;
		}
	}
}

#
# Function re-arranges RRD files
#
sub _CFG_Process_Files {
	syslog(LOG_NOTICE, "* Re-building RRD files...");
	while (my ($f, $file) = each $CFG->{files}) {
		#
		# Include template, if required
		#
		if (defined $file->{template}) {
			my @in_templates;
			if (ref $file->{template}) {
				if (ref $file->{template} eq 'ARRAY') {
					@in_templates = @{$file->{template}};
				} else {
					syslog (LOG_ERR, "  Template for file %s is not an array reference or scalar. Skip this file", $f);
					delete $CFG->{files}->{$f};
					next;
				}
			} else {
				push @in_templates, $file->{template};
			}
			delete $file->{template};

			foreach my $ti (@in_templates) {
				if (defined $CFG->{templates}->{$ti}) {
					syslog(LOG_DEBUG, "  Using template %s for file %s...", $ti, $f);
					# Clone template data
					my $tc = dclone($CFG->{templates}->{$ti});

                                        # Replace variables with values
                                        rmap {
                                                my $line = $_;
                                                while (/%%([\w\d\-_]+)%%/g) {
                                                        my $v = $1;
                                                        if (defined $file->{var}->{$v}) {
                                                                $line =~ s/%%$v%%/$file->{var}->{$v}/e;
                                                        }
                                                }
                                                $_ = $line;
                                        } $tc;

                                        # Merge
                                        $file = $CFG->{files}->{$f} = merge ($file, $tc);
				} else {
					syslog(LOG_ERR, "  Unable to use template %s for file %s. Template %s is not configured!", $file->{template}, $f, $file->{template});
                                        delete $CFG->{files}->{$f};
					next;
				}
			}
		} # End of template processing

		# Get the absolute path for the file ...
		my $fn = $f =~ /^\// ? $f : $CFG->{paths}->{rrd} . '/' . $f;
		$file->{apath} = $fn;	# ... and store it for the future use

		if (-f $fn) {
			syslog(LOG_DEBUG, "  Checking file %s...", $fn);
			my $ri = RRDs::info $fn;
			my @RRD;

			map { 
			   $ri->{ds}->{$1}->{$2} = $ri->{$_} if $_ =~ /^ds\[([\w\d]+)\]\.(type|min|max|.minimal_heartbeat)$/;
			} keys %$ri;

			# Compare datasources from current RRD file against configured 
			my $rc = List::Compare->new([keys $ri->{ds}], [keys $file->{rrd_datasources}]);

			# If there is new datasources in configuration then add them to a file
			foreach my $ds ($rc->get_Ronly) {
				syslog(LOG_DEBUG, "%s- Adding new datasource %s...", ' 'x4, $ds);
				my $ds = $file->{rrd_datasources}->{$ds};
				push @RRD, sprintf("DS:%s:%s:%s:%s:%s", $ds, $ds->{type}, 2*$file->{period}, $ds->{min}, $ds->{max} );
			}

			# DO NOT remove datasources from file if they are not configured.
			# Just remove them from future processing
                        foreach my $ds ($rc->get_Lonly) {
				delete $ri->{ds}->{$ds};
			}

			# Check MAX values for each datasource
			foreach my $ds (keys $ri->{ds}) {
				if (defined $ri->{ds}->{$ds}->{max} && $file->{rrd_datasources}->{$ds}->{max} ne 'U') {
					# Case 1: Both MAX values are defined. Compare them
					if ($ri->{ds}->{$ds}->{max} ne $file->{rrd_datasources}->{$ds}->{max}) {
						push @RRD, '--maximum', $ds . ':' . $file->{rrd_datasources}->{$ds}->{max};
						syslog(LOG_DEBUG, "%s- Changing datasource %s MAX value %s -> %s...", ' 'x4, $ds, $ri->{ds}->{$ds}->{max}, $file->{rrd_datasources}->{$ds}->{max});
					} 
				} elsif (defined $ri->{ds}->{$ds}->{max}) {
					# Case 2: MAX value is defined in file but not defined in configuration
					push @RRD, '--maximum', $ds . ':U';
					syslog(LOG_DEBUG, "%s- Changing datasource %s MAX value %s -> U...", ' 'x4, $ds, $ri->{ds}->{$ds}->{max});
				} elsif ($file->{rrd_datasources}->{$ds}->{max} ne 'U') {
					# Case 3: MAX value is not defined in file but defined in configuration
					push @RRD, '--maximum', $ds . ':' . $file->{rrd_datasources}->{$ds}->{max};
					syslog(LOG_DEBUG, "%s- Changing datasource %s MAX value U -> %s...", ' 'x4, $ds, $file->{rrd_datasources}->{$ds}->{max});
				} 
			}

			RRDs::tune($file->{apath}, @RRD);
			my $err = RRDs::error;
			if ($err) {
				syslog(LOG_ERR, "%s! ERROR: %s", ' 'x4, $err);
			}

		} else {
			syslog(LOG_DEBUG, "  Creating file %s...", $fn);
			my @DS;
			push @DS, '--step', $file->{period};

			while (my ($d, $ds) =  each $file->{rrd_datasources}) {
				push @DS, sprintf("DS:%s:%s:%s:%s:%s", $d, $ds->{type}, 2*$file->{period}, $ds->{min} || 'U', $ds->{max} || 'U' );
			}

			RRDs::create($fn, (@DS, @{$file->{rrd_archives}}));
			my $err = RRDs::error;
			if ($err) {
				syslog(LOG_ERR, "   * RRD ERROR: %s", $err);
			}
		}
	}
}

sub _CFG_Process_Sources {
	syslog(LOG_NOTICE, "* Re-building data gathering methods...");
        while (my ($f, $file) = each $CFG->{files}) {
                while (my ($d, $ds) = each $file->{rrd_datasources}) {
			syslog (LOG_DEBUG, "  file: %s, DS: %s", $f, $d);

			# Create new URI object
			my $url = URI->new($ds->{source});

                        # Get information about scheme
			# Only snmp2c is currently supported
                        if ($url->scheme eq 'snmp2c') {
				# 'snmp2c' is a network method. Parse URL and store community/host/port data
				if ($url->authority =~ /^([\w\-]+)\@([\w\-]+)(:(\d+))?/) {
					my $h = $2;
					my $host = $CFG->{hosts}->{$h};
					unless ( defined $host ) {
						$host = $CFG->{hosts}->{$h} = {};
						$CFG->{hosts}->{$h}->{session} = SNMP::Session->new(
                                                	DestHost        => $h . ':' . ($4 || '161'),
                                                	Version         => 2,
                                                	Community       => $1,
                                                	UseNumeric      => 0,
                                                	UseLongNames    => 0,
                                                	BestGuess       => 2            # This is needed to translation MIB Var -> OID
                              			);
					} 

					my ($key, $value) = ($url->query_form)[0,1];

		                        $host->{keys}->{$key} = {};

	        	                $ds->{_data} = {
						method  => 'snmp',
                        	       	        host    => $h,
                                        	key     => $key,
	                                        val     => $value,
						mib_var => substr($url->path, 1)
        		                };
				} else {
					syslog(LOG_ERR, "    Invalid URL %s. Ignoring.", $ds->{source});
					delete $file->{rrd_datasources}->{$d};
					next;
				}
                        } else {
				syslog(LOG_ERR, "    scheme %s is not supported. Ignoring.", $url->scheme);
				delete $file->{rrd_datasources}->{$d};
				next;
			}
		}
	}
}

#
# Function updates keys list for host as hashref: 
# $CFG->{hosts}->{$host}->{keys}->{ifName}->{ethernet1/1} = index;
sub _CFG_Process_SNMP {
	syslog(LOG_NOTICE, "* Updating SNMP indexes...");

	while ( my ($h, $host) = each $CFG->{hosts} ) {
		syslog(LOG_DEBUG, "  ... for host %s ...", $h);

		my $varlist = SNMP::VarList->new( [ keys $host->{keys} ] );

	        # Get the table of all keys-value
		my $resp = ($host->{session}->bulkwalk(0, 8, $varlist))[0];

		map {
#			syslog(LOG_DEBUG, "    Setting %s:%s to %s (Type: %s)", $_->tag, $_->val, $_->iid, $_->type);
			$host->{keys}->{$_->tag}->{$_->val} = $_->iid;
		} @$resp;
	}
}

sub MAIN {
	syslog(LOG_DEBUG, "* ENTERING MAIN LOOP");
	while(1) {
		#
		# Check configuration file changes
		#
		if ($CFGMON->scan()) {
			Prepare_Config($CF);
		}

		#
		# New approach. Run through the files but not through the hosts. Bigger num of SNMP requests but should be more smooth statistics
		# Always sort files in order to prevent update time slipping!

		my $ct = DateTime->now();

		foreach my $f (sort { $a cmp $b } keys $CFG->{files} ) {
			my $file = $CFG->{files}->{$f};

                        my $cur_time = DateTime->now();

			if (defined $file->{last_update}) {
				if ( DateTime->compare($file->{last_update}, $cur_time->clone()->subtract(seconds => $file->{period})) <= 0 ) {
					$file->{last_update}->add(seconds => $file->{period});
				} else {
					next;
 				}
			} else {
				$file->{last_update} = $cur_time->clone();
			}

			Update_File($file);
		}

		syslog(LOG_DEBUG, "TIME TAKEN to update all files: %s seconds", DateTime->now()->subtract_datetime_absolute($ct)->seconds());

		sleep 5;
	}
	closelog();
}

sub Update_File {
	my $file = shift;
	syslog(LOG_DEBUG, "  -- Updating file %s...", $file->{apath});

	my $data;

	#
	# Gather all SNMP data sources
	#
	while (my ($d, $ds) = each $file->{rrd_datasources}) {
		my $ds_data = $ds->{_data};
		if ($ds->{_data}->{method} eq 'snmp') {
			my $host = $CFG->{hosts}->{$ds_data->{host}};
			my $idx = $host->{keys}->{$ds_data->{key}}->{$ds_data->{val}};

			if (defined $idx) {
				my $oid = $ds_data->{mib_var} . '.' . $idx;
				$data->{$ds_data->{host}}->{$oid} = $d;
			} else {
				syslog(LOG_ERR, "    Unable to find index for key %s for datasource %s!", $ds_data->{key}, $d);
			}
		}
	}

	return unless defined $data;

	while (my ($hk, $hd) = each $data) {
		my $host = $CFG->{hosts}->{$hk};
		my @oids = (keys $hd);
		while (@oids) {
			# Send 64 OIDs per one request. It should be enough for most devices
			my $varlist = SNMP::VarList->new(map { [ $_ ] } splice @oids,0,64);
			syslog(LOG_DEBUG, "   - Sending SNMP request to %s...", $hk);
			my @res = $host->{session}->get($varlist);
			if ( $host->{session}->{ErrorNum} ) {
				syslog(LOG_ERR, "  ! SNMP ERROR: %s", $host->{session}->{ErrorStr});
				next;
			}

			foreach my $v (@$varlist) {
#				print $v->tag . '.' . $v->iid . '=' . $v->val, "\n"; 
				my $ds_name = $hd->{$v->tag . '.' . $v->iid};
				$file->{rrd_datasources}->{$ds_name}->{value} = $v->val;
			}
		}
	}
	RRD_update($file);
}

sub _async_snmp {
	my $host = shift;
	my $vl = shift;

	foreach my $v (@$vl) {
#		print $v->tag . '.' . $v->iid . '=' . $v->val, "\n";
		$host->{oids}->{$v->tag . '.' . $v->iid}->{value} = $v->val;
	
		my $file_name = $host->{oids}->{$v->tag . '.' . $v->iid}->{file};
		my $ds_name = $host->{oids}->{$v->tag . '.' . $v->iid}->{ds};
		$CFG->{files}->{$file_name}->{rrd_datasources}->{$ds_name}->{value} = $v->val;
	}

	SNMP::finish();
}

#
# Update RRD files
#
sub RRD_update {
	my $file = shift;
	syslog(LOG_DEBUG, "* Entering RRD Update for %s", $file->{apath});
	my @DS;
	my @VAL;
	while ( my ($d, $ds) = each $file->{rrd_datasources}) {
		if ( (defined $ds->{value}) && (defined $ds->{convert}) ) {
			syslog(LOG_DEBUG, "%s- Converting %s using %s...", ' 'x4, $ds->{value}, $ds->{convert});
			if ( ref($CFG->{converters}->{$ds->{convert}}) eq 'CODE' ) {
				$ds->{value} = &{$CFG->{converters}->{$ds->{convert}}}($ds->{value});
			} else {
				syslog(LOG_ERR, "%s! ERROR: Converter %s is not CODE reference!", ' 'x6, $ds->{convert});
			}
		}

		if (defined $ds->{value}) {
			push @DS, $d;
			push @VAL, $ds->{value};
		} 
		delete $ds->{value};
	}
	if ($#DS > -1) {
		my @args;

		my $template = join ':', @DS;
		syslog(LOG_DEBUG, "%s- Template: %s", ' 'x4, $template);
		push @args, '--template', $template;

		my $values = 'N:' . join ':', @VAL;
		syslog(LOG_DEBUG, "%s- Values: %s", ' 'x4, $values);
		push @args, $values;
		syslog(LOG_DEBUG, "%s- [%s] RRD code: rrdtool update %s %s", ' 'x4, DateTime->now(), $file->{apath}, join(' ', @args));

		RRDs::update($file->{apath}, @args);

		my $err = RRDs::error;
		if ($err) {
			syslog(LOG_ERR, "%s! ERROR: %s", ' 'x4, $err);
		}
	} else {
		syslog(LOG_DEBUG, "%s! No data for updating.", ' 'x4);
	}
}

#
# Function converts OID or MIB Variable to exact OID
#
sub _oid {
	my $o = shift;
        return $o =~ /^(.\d)+$/ ? $o : SNMP::translateObj($o);
}

