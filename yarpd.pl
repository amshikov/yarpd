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
	# Update SNMP session data and indexes
	#
	_CFG_Update_SNMP();
	#
	# Update RRD files properly
	#
	_CFG_Update_RRD();
}

#
# Function updates SNMP session data, indexes etc.
#
sub _CFG_Update_SNMP {
	while (my ($f, $file) = each $CFG->{files}) {
		#
		# Include template, if required
		#
		if (defined $file->{template}) {
			next unless defined $CFG->{templates}->{$file->{template}};
			# Clone template data
			my $tf = dclone($CFG->{templates}->{$file->{template}});
			# Replace variables with values
			rmap { s/%%([\w\d\-_]+)%%/$file->{var}->{$1}/g; $_; } $tf;
			$file = $CFG->{files}->{$f} = merge ($file, $tf);
		}

		#
		# Parse sources of data in files definition
		#
		foreach my $d (keys %{$file->{rrd_datasources}}) {
			# Datasource definition if file can be either scalar ot hash reference. The second option is intended for converting values returned by SNMP
			my $url_string;
			my $converter;
			if ( ref($file->{rrd_datasources}->{$d}) eq 'HASH' ) {
				$url_string = $file->{rrd_datasources}->{$d}->{url};
				$converter  = $file->{rrd_datasources}->{$d}->{convert};
			} else {
				$url_string = $file->{rrd_datasources}->{$d};
			}

			# Create new URI object
			my $url = URI->new($url_string);

			# Get information about scheme
			next unless $url->scheme eq 'snmp2c';

			# Create sessions for all hosts found in URIs
			# Skip if it was already created
			my $host;
			if ($url->authority =~ /^([\w\-]+)\@([\w\-]+)(:(\d+))?/) {
			   $host = $2;
			   unless (defined $CFG->{hosts}->{$host}) {
			      my $h = $CFG->{hosts}->{$host} = {};
			      # Store host data (not used now, may be removed in future)
			      $h->{snmp}->{hostname}	= $host;
			      $h->{snmp}->{port}	= $4 ? $4 : 161;
			      $h->{snmp}->{community}	= $1;
			      $h->{snmp}->{version} 	= 2;
			      # Store session
			      $CFG->{hosts}->{$host}->{session} = SNMP::Session->new(
						DestHost	=> $2 . ':' . $h->{snmp}->{port},
						Version		=> 2,
						Community	=> $1,
						UseNumeric      => 0,
						UseLongNames	=> 0,
						BestGuess	=> 2		# This is needed to translation MIB Var -> OID
			      );
			   }
			} else {
			   next;
			}
			my $h = $CFG->{hosts}->{$host};

			# Save information about all keys
			my ($key, $value) = ($url->query_form)[0,1];
			my $mib_var = substr($url->path, 1);
			if (defined $key) {
				# Update keys if they wasn't updated yet
				SNMP_update_keys($h, $key) unless defined $h->{keys}->{$key};

				# Convert information about datasource uri to host and exact oid

				die "key: $key value: $value" unless defined $h->{keys}->{$key}->{$value};

			

				$mib_var .= '.' . $h->{keys}->{$key}->{$value};
			}

			# --- replace DS definition in URL form with hash reference
			my $rrd_ds_def = $CFG->{rrd_datasources}->{$d};
			delete $file->{rrd_datasources}->{$d};
			unless (defined $rrd_ds_def) {
				syslog(LOG_ERR, "  ! ERROR: %s is not defined in rrd_data_sources!", $rrd_ds_def);
				next;
			}

			my $ds = $file->{rrd_datasources};

			if ($rrd_ds_def =~ /^(\w+):(COUNTER|GAUGE):(\-?\d+):(\-?(\d+|U))$/) {
				$ds->{$1}->{host} = $h->{snmp}->{hostname};
				$ds->{$1}->{oid}  = $mib_var;
				$ds->{$1}->{type} = $2;
				$ds->{$1}->{min}  = $3;
				$ds->{$1}->{max}  = $4;
				$ds->{$1}->{convert} = $converter;
			} else {
				syslog(LOG_ERR, "  ! ERROR: RRD Data Source definition %s is not supported!", $rrd_ds_def);
				next;
			}
		}
	}
}

#
# Function checks all RRD files and updates them if needed
#
sub _CFG_Update_RRD {
	syslog(LOG_DEBUG, "* Entering %s...\n", (caller 0)[3] );
	
	my $hosts = $CFG->{hosts};
	foreach my $f (keys %{$CFG->{files}}) {
		my $file = $CFG->{files}->{$f};

		#
		# Check whether file exists
		#
		my $fn = $f =~ /^\// ? $f : $CFG->{paths}->{rrd} . '/' . $f;

		$file->{apath} = $fn;	# Store absolute path for the future

		if (-f $fn) {
			my $ri = RRDs::info $fn;
			map { 
			   $ri->{ds}->{$1}->{$2} = $ri->{$_} if $_ =~ /^ds\[([\w\d]+)\]\.(type|min|max|.minimal_heartbeat)$/;
			} keys %$ri;

			# Compare datasources from configuration with ones from file
			my $rc = List::Compare->new([keys $ri->{ds}], [keys $file->{rrd_datasources}]);

			# Add new datasources to file if they appear in configuration
			_rrd_add_ds($file, $rc->get_Ronly_ref);
		} else {
			my @DS;
			push @DS, '--step', $file->{period};

			while (my ($d, $ds) =  each $file->{rrd_datasources}) {
				push @DS, sprintf("DS:%s:%s:%s:%s:%s", $d, $ds->{type}, 2*$file->{period}, $ds->{min}, $ds->{max} );
			}

			RRDs::create($fn, (@DS, @{$file->{rrd_archives}}));
			my $err = RRDs::error;
			if ($err) {
				syslog(LOG_ERR, "   * RRD ERROR: %s", $err);
			}
			my $RRD = merge \@DS, $file->{rrd_archives};
		}

		#
		# Form a hash for SNMP requests:
		# host->{oids}->{oid}->{file} = file to save in
		# host->{oids}->{oid}->{ds} = data source to use
		# host->{oids}->{oid}->{period} = period
                while (my ($d, $ds) = each $file->{rrd_datasources}) {
			$hosts->{$ds->{host}}->{oids}->{$ds->{oid}}->{file} = $f;
			$hosts->{$ds->{host}}->{oids}->{$ds->{oid}}->{ds}   = $d;
			$hosts->{$ds->{host}}->{oids}->{$ds->{oid}}->{period} = $file->{period};
                }
	}
}

#
# Function takes host and key (i.e. lsr-1gdr, ifName) as arguments
# and updates keys list for host as hashref: 
# $CFG->{hosts}->{$host}->{keys}->{ifName}->{ethernet1/1} = index;
sub SNMP_update_keys {
	my $h 	= shift;		# reference to $CFG->{hosts}->{$host}

	syslog(LOG_DEBUG, "* Entering %s for %s...", (caller 0)[3], $h->{snmp}->{hostname});
	my $key = shift;
	my $s 	= $h->{session}; 

	# Convert $key from MIB variable to OID
	my $oid	= $key;
	my $varlist = SNMP::VarList->new([ $oid ]);
	# Get the table of all keys-value
	my $resp = ($s->bulkwalk(0, 8, $varlist))[0];
	map { 
		syslog(LOG_DEBUG, "%sSetting %s:%s to %s (Type: %s)", ' 'x2, $key, $_->val, $_->iid, $_->type);
		$h->{keys}->{$key}->{$_->val} = $_->iid;
	} @$resp;
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
	syslog(LOG_DEBUG, "* Entering Update_file for %s...", $file->{apath});

	my $data;

	while (my ($d, $ds) = each $file->{rrd_datasources}) {
		$data->{$ds->{host}}->{$ds->{oid}} = $d;
	}

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

sub _rrd_add_ds {
	my $file   = shift;	# reference to file hash from configuration
	my $new_ds = shift;	# reference to list of datasource names to be added

	return unless scalar @$new_ds;

	syslog(LOG_NOTICE, "%s- Adding new datasources (%s) to file %s...", ' 'x2, join (', ', @$new_ds), $file->{apath});

	my @DS;
	foreach my $ds_name (@$new_ds) {
		my $ds = $file->{rrd_datasources}->{$ds_name};
		push @DS, sprintf("DS:%s:%s:%s:%s:%s", $ds_name, $ds->{type}, 2*$file->{period}, $ds->{min}, $ds->{max} );
        }
	RRDs::tune($file->{apath}, @DS);
	my $err = RRDs::error;
	if ($err) {
		syslog(LOG_ERR, "%s! ERROR: %s", ' 'x4, $err);
	}
}

#
# Function converts OID or MIB Variable to exact OID
#
sub _oid {
	my $o = shift;
        return $o =~ /^(.\d)+$/ ? $o : SNMP::translateObj($o);
}

