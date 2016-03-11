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
	printf "* Entering %s...\n", (caller 0)[3];
	#
	# Include config
	#
	$CFG = undef if defined $CFG;
	do "$cf" or die $!;
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
				$mib_var .= '.' . $h->{keys}->{$key}->{$value};
			}

			# --- replace DS definition in URL form with hash reference
			my $rrd_ds_def = $CFG->{rrd_datasources}->{$d};
			delete $file->{rrd_datasources}->{$d};
			unless (defined $rrd_ds_def) {
				printf "  ! ERROR: %s is not defined in rrd_data_sources!\n";
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
				printf "  ! ERROR: RRD Data Source definition %s is not supported!\n", $rrd_ds_def;
				next;
			}
		}
	}
}

#
# Function checks all RRD files and updates them if needed
#
sub _CFG_Update_RRD {
	printf "* Entering %s...\n", (caller 0)[3];
	
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
				printf "   * ERROR: %s\n", $err;
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
	printf "* Entering %s for %s...\n", (caller 0)[3], $h->{snmp}->{hostname};
	my $key = shift;
	my $s 	= $h->{session}; 

	# Convert $key from MIB variable to OID
	my $oid	= $key;
	my $varlist = SNMP::VarList->new([ $oid ]);
	# Get the table of all keys-value
	my $resp = ($s->bulkwalk(0, 8, $varlist))[0];
	map { 
		printf "%sSetting %s:%s to %s (Type: %s)\n", ' 'x2, $key, $_->val, $_->iid, $_->type;
		$h->{keys}->{$key}->{$_->val} = $_->iid;
	} @$resp;
}

sub MAIN {
	printf "* ENTERING MAIN LOOP\n";
	while(1) {
		#
		# Check configuration file changes
		#
		if ($CFGMON->scan()) {
			print "! Configuration file change detected. Reloading...\n";
			Prepare_Config($CF);
		}
		foreach my $h (keys %{$CFG->{hosts}}) {
			my $host = $CFG->{hosts}->{$h};
			#
			# Select only those OIDs which should be updated
			#
			my @oids;
			foreach my $o (keys $host->{oids}) {
				my $oid = $host->{oids}->{$o};
				if (defined $oid->{next_update}) {
					if ( DateTime->compare($oid->{next_update}, DateTime->now()) <= 0 ) {
	                                        $host->{oids}->{$o}->{next_update}->add(seconds => $oid->{period});
	                                        push @oids, $o;	
					}
				} else {
					$oid->{next_update} = DateTime->now()->add(seconds => $oid->{period});
					push @oids, $o;
				}
			}

			while (@oids) {
				# Send 64 OIDs per one request. It should be enough for most devices
				my $varlist = SNMP::VarList->new(map { [ $_ ] } splice @oids,0,64);
				printf "* Sending SNMP request to %s...\n", $h;
				my @res = $host->{session}->get($varlist);
				if ( $host->{session}->{ErrorNum} ) {
					printf "  ! ERROR: %s\n", $host->{session}->{ErrorStr};
					next;
				}
				foreach my $v (@$varlist) {
					print $v->tag . '.' . $v->iid . '=' . $v->val, "\n"; 
					$host->{oids}->{$v->tag . '.' . $v->iid}->{value} = $v->val;

					my $file_name = $host->{oids}->{$v->tag . '.' . $v->iid}->{file};
					my $ds_name = $host->{oids}->{$v->tag . '.' . $v->iid}->{ds};
					$CFG->{files}->{$file_name}->{rrd_datasources}->{$ds_name}->{value} = $v->val;
				}
			}
		}
		RRD_update();
		sleep 5;
	}
}

#
# Update RRD files
#
sub RRD_update {
	print "* Entering RRD Update...\n";
	while (my ($f, $file) = each $CFG->{files} ) {
		printf "%s- Updating %s...\n", ' 'x2, $file->{apath};
		my @DS;
		my @VAL;
		while ( my ($d, $ds) = each $file->{rrd_datasources}) {
			if (defined $ds->{value}) {
				if (defined $ds->{convert}) {
					printf "%s- Converting %s using %s...\n", ' 'x4, $ds->{value}, $ds->{convert};
					if ( ref($CFG->{converters}->{$ds->{convert}}) eq 'CODE' ) {
						$ds->{value} = &{$CFG->{converters}->{$ds->{convert}}}($ds->{value});
					} else {
						printf "%s! ERROR: Converter %s is not CODE reference!\n", ' 'x6, $ds->{convert}
					}
				}
				push @DS, $d;
				push @VAL, $ds->{value};
			} 
			delete $ds->{value};
		}
		if ($#DS > -1) {
			my @args;

			my $template = join ':', @DS;
			printf "%s- Template: %s\n", ' 'x4, $template;
			push @args, '--template', $template;

			my $values = 'N:' . join ':', @VAL;
			printf "%s- Values: %s\n", ' 'x4, $values;
			push @args, $values;
			printf "%s- RRD code: rrdtool update %s %s\n", ' 'x4, $file->{apath}, join(' ', @args);

			RRDs::update($file->{apath}, @args);

			my $err = RRDs::error;
			if ($err) {
				printf "%s! ERROR: %s\n", ' 'x4, $err;
			}
		} else {
			printf "%s! No data for updating.\n", ' 'x4;
		}
	}
}

sub _rrd_add_ds {
	my $file   = shift;	# reference to file hash from configuration
	my $new_ds = shift;	# reference to list of datasource names to be added

	return unless scalar @$new_ds;

	printf "%s- Adding new datasources (%s) to file %s...\n", ' 'x2, join (', ', @$new_ds), $file->{apath};

	my @DS;
	foreach my $ds_name (@$new_ds) {
		my $ds = $file->{rrd_datasources}->{$ds_name};
		push @DS, sprintf("DS:%s:%s:%s:%s:%s", $ds_name, $ds->{type}, 2*$file->{period}, $ds->{min}, $ds->{max} );
        }
	RRDs::tune($file->{apath}, @DS);
	my $err = RRDs::error;
	if ($err) {
		printf "%s! ERROR: %s\n", ' 'x4, $err;
	}
}

#
# Function converts OID or MIB Variable to exact OID
#
sub _oid {
	my $o = shift;
        return $o =~ /^(.\d)+$/ ? $o : SNMP::translateObj($o);
}

