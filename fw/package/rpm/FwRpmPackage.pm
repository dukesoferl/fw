#! /usr/bin/perl

package FwRpmPackage;

use strict;
use warnings;

BEGIN {
   use Exporter   ();
   our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

   # if using RCS/CVS, this may be preferred
   $VERSION = sprintf "%d.%03d", q$Revision: 1.7 $ =~ /(\d+)/g;

   @ISA         = qw (Exporter);
   @EXPORT      = qw (&proctalk &get_state &get_dependencies &closure
                      &get_dependencies_closure 
                      &reverse_provides &parse_depends &rpmvercmp &rpmevrcmp);
   %EXPORT_TAGS = ( );     # eg: TAG => [ qw!name1 name2! ],

   # your exported package globals go here,
   # as well as any optionally exported functions
   @EXPORT_OK   = qw ();
}
our @EXPORT_OK;

use IO::Pipe;
use POSIX ":sys_wait_h";

#---------------------------------------------------------------------
#                              proctalk                               
# 
# Fork a process and talk to it over a pipe pair.
#---------------------------------------------------------------------

sub proctalk ($$)
  {
    my ($parent_code, $child_code) = @_;

    my $par_to_child = new IO::Pipe;
    my $child_to_par = new IO::Pipe;

    my $pid;

    if ($pid = fork ())
      {
        # parent

        $par_to_child->writer ();
        $child_to_par->reader ();

        $parent_code-> ($child_to_par, $par_to_child);

        undef $par_to_child;
        undef $child_to_par;

        waitpid ($pid, 0);

        die "subprocess failed" if $?;
      }
    else
      {
        # child

        $par_to_child->reader ();
        $child_to_par->writer ();

        $child_code-> ($par_to_child, $child_to_par);

        die "child_code failed to exit";
      }
  }

#---------------------------------------------------------------------
#                              get_state                              
# 
# Get all of the installed packages and versions.
#---------------------------------------------------------------------

sub get_state ()
  {
    my %state;

    proctalk (
      sub
        {
          my ($readfh, $writefh) = @_;

          while (defined ($_ = <$readfh>))
            {
              chomp;

              my ($package, $version) = split /\s+/, $_, 2;
              if ($version =~ m/^\(none\):(.*)$/)
                {
                  $version = "$1";
                }
              $state{$package} = $version;
            }
        },
      sub
        {
          my ($readfh, $writefh) = @_;

          close STDIN;
          open STDIN, "<&", $readfh or die "can't dup STDIN: $!";

          close STDOUT;
          open STDOUT, ">&", $writefh or die "can't dup STDOUT: $!";

          close STDERR unless $ENV{"FW_TRACE"};

          exec "rpm",
               "-qa",
               '--queryformat',
               '%-{name}\t%{epoch}:%{version}-%{release}\n';
        }
    );

    return (\%state, {});
  }

#---------------------------------------------------------------------
#                           get_dependencies                            
# 
# For a set of packages, identify the set of packages which are a 
# direct dependency of a member of the set.
#---------------------------------------------------------------------

sub get_dependencies ($$$$@)
  {
    my ($state, undef, $arch, $release, @packages) = @_;

    my %dependencies;
    my %deps_by_package;

    return () unless scalar @packages;

    proctalk (
      sub 
        {
          my ($readfh, $writefh) = @_;

          foreach my $package (@packages)
            {
              print $writefh "$package\n";
            }

          $writefh->close ();

          my $in_depends;
          my $package;

          while (defined ($_ = <$readfh>))
            {
              chomp;

              my ($package, undef) = split /\s+/, $_;

              # TODO: rpm -qR lists all sorts of wierd stuff ...
              next unless $state->{$package};

              my $alldeps = parse_depends ($state, $arch, $_, $release);
              my @pkgs = @{$alldeps->{"packages"}};
              scalar map { $deps_by_package{$package}->{$_} = 1;
                           $dependencies{$_} = 1 }
                         @pkgs;
            }
        },
      sub
        {
          my ($readfh, $writefh) = @_;

          close STDIN;
          open STDIN, "<&", $readfh or die "can't dup STDIN: $!";

          close STDOUT;
          open STDOUT, ">&", $writefh or die "can't dup STDOUT: $!";

          close STDERR unless $ENV{"FW_TRACE"};

          exec "xargs", "rpm", "-qR", "--" or die "exec failed: $!";
        }
    );

    return (wantarray) ? keys %dependencies : \%deps_by_package;
    return undef;
  }

#---------------------------------------------------------------------
#                               closure                               
# 
# Form the closure of an operation on a set.
#---------------------------------------------------------------------

sub closure ($@)
  {
    my ($func, @packages) = @_;

    my %pkghash = map { $_ => 1 } @packages;

    my $finished;

    do
      {
        my @deps = $func-> (@packages);

        $finished = 1;

        @packages = map { $finished = 0; $pkghash{$_} = 1; $_ }
                    grep { ! exists $pkghash{$_} } @deps;
      }
    while (! $finished);

    return keys %pkghash;
  }

#---------------------------------------------------------------------
#                       get_dependencies_closure                        
# 
# For a set of packages, identify all installed packages which a 
# member of the set depends upon, either directly or indirectly.  
#---------------------------------------------------------------------

sub get_dependencies_closure ($$$$@)
  {
    my ($state, $virtual, $arch, $release, @packages) = @_;

    return 
      closure (sub { get_dependencies ($state, $virtual, $arch, $release, @_) },
               @packages);
  }

#---------------------------------------------------------------------
#                          reverse_provides                           
# 
# (Attempt to) find an installed package which provides a given
# package.
#---------------------------------------------------------------------

sub reverse_provides ($$)
  {
    my ($state, $package) = @_;
    my $reverse_provider;

    my $cmd = "rpm -q --whatprovides "
             ."--queryformat '%-{name} %{version}-%{release}\n' "
             .$package;
    my $provides = `$cmd`;
    $provides =~ m/^(\S+) / or die "unexpected rpm output: $provides";
    $reverse_provider = $1 if $state->{$1};

    return $reverse_provider;
  }

#---------------------------------------------------------------------
#                              rpmvercmp
#
# Compare two version strings as RPM does, returning 1 if the first is
# new than the second, 0 if they are the same, and -1 if the second is
# newer than the first.
# ---------------------------------------------------------------------

sub rpmvercmp ($$)
{
  my ($a, $b) = @_;

  # This function attempts to follow the C code as closely as possible.
  # http://www.rpm.org/api/4.4.2.2/rpmvercmp_8c-source.html

  if (! defined($a) || ! defined($b)) {
    if (defined($a)) { return 1; }  # $a is defined and $b isn't.
    if (defined($b)) { return -1; } # $b is defined and $a isn't.
    return 0;			    # Neither $a nor $b is defined.
  }

  if ($a eq $b) { return 0; }

  my $one = $a;
  my $two = $b;
  while (length($one) > 0 && length($two) > 0)
    {
      $one =~ s/^[^[:alnum:]]+//;
      $two =~ s/^[^[:alnum:]]+//;

      if (length($one) == 0 || length($two) == 0) { last; }

      my $isnum;
      my $str1 = $one;
      my $str2 = $two;
      if ($str1 =~ m/^[[:digit:]]/)
	{
	  $str1 =~ m/^([[:digit:]]*)(.*)/ and do { $one = $1; $str1 = $2; };
	  $str2 =~ m/^([[:digit:]]*)(.*)/ and do { $two = $1; $str2 = $2; };
	  $isnum = 1;
	}
      else
	{
	  $str1 =~ m/^([[:alpha:]]*)(.*)/ and do { $one = $1; $str1 = $2; };
	  $str2 =~ m/^([[:alpha:]]*)(.*)/ and do { $two = $1; $str2 = $2; };
	  $isnum = 0;
	}

      if (length($one) == 0) { return -1; } # Shouldn't happen.
      if (length($two) == 0) { return $isnum ? 1 : -1; }

      if ($isnum) {
	$one =~ s/^0+//;
	$two =~ s/^0+//;

	if (length($one) > length($two)) { return 1; }
	if (length($two) > length($one)) { return -1; }
      }

      my $rc = $one cmp $two;
      if ($rc != 0) { return $rc; }

      $one = $str1;
      $two = $str2;
    }

  if (length($one) == 0 && length($two) == 0) { return 0; }

  if (length($one) == 0) { return -1; } else { return 1; }
}

#---------------------------------------------------------------------
#                              rpmevrcmp
#
# Compare two epoch:version-revision strings as RPM does, returning 1
# if the first is newer than the second, 0 if they are the same, and -1
# if the second is newer than the first.
# ---------------------------------------------------------------------

sub rpmevrcmp
  {
    my ($a, $b) = @_;

    my ($ae, $av, $ar) = ($a =~ m{^(?:([^:]*):)?([^-]*)(?:-(.*))?$});
    my ($be, $bv, $br) = ($b =~ m{^(?:([^:]*):)?([^-]*)(?:-(.*))?$});

    my $cmp = rpmvercmp($ae, $be);
    if ($cmp != 0) { return $cmp; }

    $cmp = rpmvercmp($av, $bv);
    if ($cmp != 0) { return $cmp; }

    return rpmvercmp($ar, $br);
}

#---------------------------------------------------------------------
#                             enforce_op                              
# 
# Check whether $installed $op $version is true.
#---------------------------------------------------------------------

sub enforce_op ($$$)
  {
    my ($operation, $installed, $version) = @_;

    if (! defined ($operation) || $operation eq "")
      {
        return 1;
      }
    else
      {
        my $cmp = rpmevrcmp ($installed, $version);

        if ($operation eq "<<" || $operation eq "<")
          {
            return ($cmp < 0);
          }
        elsif ($operation eq "<=")
          {
            return ($cmp <= 0);
          }
        elsif ($operation eq "=")
          {
            return ($cmp == 0);
          }
        elsif ($operation eq ">=")
          {
            return ($cmp >= 0);
          }
        elsif ($operation eq ">>" || $operation eq ">")
          {
            return ($cmp > 0);
          }
        else
          {
            return 0;
          }
      }
  }

#---------------------------------------------------------------------
#                            parse_depends                            
# 
# Parse FW_PACKAGE_BUILD_DEPENDENCIES, which is in debian build-time
# dependency format.  Returns the set of installed packages which 
# satisfy the dependencies.
#---------------------------------------------------------------------

sub parse_depends ($$$$)
  {
    my ($state, $arch, $depends, $release) = @_;

    my @missing;
    my @missing_specs;
    my %packages;
    my $first = 1;

# libc6 (>= 2.2.1), exim | mail-transport-agent
# kernel-headers-2.2.10 [!hurd-i386], hurd-dev [hurd-i386]
# erlang-nox (= INSTALLED)

  SPEC:  foreach my $spec (split(/\s*,\s*/, $depends))
      {
        my $p = undef;
        my $possible_missing = "";
  OPTION: foreach my $option (split(/\s*\|\s*/, $spec))
          {
            $option =~ 
              m/^(\S+)	 # package name
		\s*
		(?:      # optional version specification: "( OP VERSION )"
		  \( \s* (<<|<=|>=|>>|<(?!=)|=|>(?!=)) \s* ([^\s\)]+) \s* \)
		)?
		\s*
		(?:      # optional architecture specification: "[ ARCH ]" or "[ ! ARCH ]"
		  \[ \s* (!)? \s* ([^\s\!\]]+) \s* \]
		)?
		\s*$/x or
              die "can't parse dependencies '$depends' (option '$option')";

            $p = $1;
            my $op = $2;
            my $rawv = defined ($3) ? $3 : "";
            my $version = ($rawv eq "INSTALLED") ? $state->{$p} : $rawv;
	    my $not = $4;
	    my $restrict = $5;

	    if (defined $not) {
                if ($not && $restrict eq $arch) {
                  next SPEC;
                }
                if (! $not && $restrict ne $arch) {
                  next SPEC;
                }
              }

            if ($state->{$p})
              {
                if (enforce_op ($op, $state->{$p}, $version))
                  {
                    $packages{$p} = 
                      (defined ($op) && $op ne "") ? "$op $version"
                                                   : ">= $state->{$p}";
                    next SPEC;
                  }
              }
            else
              {
                my $rev_p = reverse_provides ($state, $p);

                if ($rev_p && enforce_op ($op, $state->{$rev_p}, $version))
                  {
                    $packages{$rev_p} =
                      (defined ($op) && $op ne "") ? "$op $version"
                                                   : ">= $state->{$rev_p}";
                    next SPEC;
                  }
              }

            # I want to make sure that if this is an "=" dependency that
            # the exact version is installed, so I check here and keep
            # track of all specified = versions (so things like
            # pkg (= 1.0) | pkg (= 2.0) still sort of work).
            if (defined $op and $op eq "=")
              {
                my $yum_version_with_epoch = undef;

                # special processing needed for packages with epochs
                if ($version =~ m#^([^:]+):(.*)$#)
                  {
                    $yum_version_with_epoch = $1.":".$p."-".$2.".".$arch;
                  }
                else
                  {
                    $yum_version_with_epoch = "$p-$version";
                  }

                # just concatenate all the versions and hope that yum will
                # sort it out :)
                $possible_missing .= $yum_version_with_epoch." ";
              }
          }

        # keep track of mising packages
        if ($possible_missing ne "")
          {
            push @missing, $possible_missing;
          }
        else
          {
            push @missing, $p;
          }
        push @missing_specs, $spec;

        die "package/rpm/dependency-closure: fatal: '$spec' not installed\n" 
          if $release eq "yes";

        if ($first) {
          warn "\n";
          $first = 0;
        }
        warn "package/rpm/dependency-closure: warning: '$spec' not installed\n" 
      }

    my @pk = keys %packages;
    return { "packages" => \@pk,
             "missing" => \@missing,
             "missing_specs" => \@missing_specs,
             "manifest" => \%packages
           };
  }

END { }       # module clean-up code here (global destructor)

1;  # don't forget to return a true value from the file

