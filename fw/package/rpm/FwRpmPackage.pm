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
    my %virtual; # sometimes package provide another version, this is for those

    proctalk (
      sub
        {
          my ($readfh, $writefh) = @_;

          while (defined ($_ = <$readfh>))
            {
              chomp;

              my ($package, $version, $other) = split /\s+/, $_, 3;
              if ($version =~ m/^\(none\):(.*)$/)
                {
                  $version = "$1";
                  # providesversion is often blank, or the same version, which
                  # can be ignored.  Otherwise it's often the version with the
                  # hyphen release, so if removing that makes it the same
                  # also ignore.  Otherwise it's a different version so keep
                  # track of it in the virtual table
                  if ($other ne ""
                      and $other ne $version
                      and $version =~ m/^([^\-]+)\-/
                      and $1 ne $other) {
                    $virtual{$package} = $other;
                  }
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
               '%-{name}\t%{epoch}:%{version}-%{release}\t%{provideversion}\n';
        }
    );

    return (\%state, \%virtual);
  }

#---------------------------------------------------------------------
#                           get_dependencies                            
# 
# For a set of packages, identify the set of packages which are a 
# direct dependency of a member of the set.
#---------------------------------------------------------------------

sub get_dependencies ($$$$@)
  {
    my ($state, $virtual, $arch, $release, @packages) = @_;

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

              my $depends = $_;
              my $alldeps = parse_depends ($state, $virtual, $arch, $depends, $release);
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
    $provides =~ m/^(\S+) / or die "unexpected rpm output for \"$cmd\": $provides";
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
# Compare two epoch:version-release strings as RPM does, the first of
# which is the installed EVR and the second of which is the dependency
# EVR.  rpmevrcmp returns 1 if the installed EVR is greater than the
# dependency EVR, 0 if they are the equal, and -1 if the installed EVR
# is less than the dependency EVR.
# ---------------------------------------------------------------------

sub rpmevrcmp
  {
    my ($a, $b) = @_;

    # The parts of an RPM version number are EPOCH:VERSION-RELEASE.
    my ($ae, $av, $ar) = ($a =~ m{^(?:([^:]*):)?([^-]*)(?:-(.*))?$});
    my ($be, $bv, $br) = ($b =~ m{^(?:([^:]*):)?([^-]*)(?:-(.*))?$});

    my $cmp = rpmvercmp($ae, $be);
    if ($cmp != 0) { return $cmp; }

    $cmp = rpmvercmp($av, $bv);
    if ($cmp != 0) { return $cmp; }

    # If the dependency release is not specified, consider any
    # installed release to be equal.
    if (! defined ($br)) { return 0; }

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

sub parse_depends ($$$$$)
  {
    my ($state, $virtual, $arch, $depends, $release) = @_;

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
        my $op = "";
        my $version = "";
  OPTION: foreach my $option (split(/\s*\|\s*/, $spec))
          {
            my ($not, $restrict);
          PARSE_OPTION: {
              $option =~ # DEB FORMAT
                m/^(\S+)   # package name
                  \s*
                  (?:      # optional version specification: "( OP VERSION )"
                    \( \s* (<<|<=|>=|>>|<(?!=)|=|>(?!=)) \s* ([^\s\)]+) \s* \)
                  )?
                  \s*
                  (?:      # optional architecture specification: "[ ARCH ]" or "[ ! ARCH ]"
                    \[ \s* (!)? \s* ([^\s\!\]]+) \s* \]
                  )?
                  \s*$/x and do {
                    $p = $1;
                    $op = $2;
                    $version = (not defined $3) ? '' : $3 eq 'INSTALLED' ? $state->{$p} : $3;
                    $not = $4;
                    $restrict = $5;
                    # no warnings 'uninitialized';
                    # print STDERR "deb $option -> $p $op $version $not $restrict\n";
                    last PARSE_OPTION;
                  };
              $option =~ # RPM FORMAT
                m/^(\S+)   # package name
                  \s*
                  (?:      # optional version specification: "OP VERSION"
                    (<<|<=|>=|>>|<(?!=)|=|>(?!=)) \s* ([^\s\)]+)
                  )?
                  \s*$/x and do {
                    $p = $1;
                    $op = $2;
                    $version = (not defined $3) ? '' : $3 eq 'INSTALLED' ? $state->{$p} : $3;
                    # no warnings 'uninitialized';
                    # print STDERR "rpm $option -> $p $op $version\n";
                    last PARSE_OPTION;
                  };
              die "can't parse dependencies '$depends' (option '$option')";
            }

            if (defined $not) {
                if ($not && $restrict eq $arch) {
                  next SPEC;
                }
                if (! $not && $restrict ne $arch) {
                  next SPEC;
                }
              }

            foreach my $s ($state, $virtual)
              {
                if ($s->{$p})
                  {
                    if (enforce_op ($op, $s->{$p}, $version))
                      {
                        $packages{$p} =
                          (defined ($op) && $op ne "") ? "$op $version"
                                                       : ">= $s->{$p}";
                        next SPEC;
                      }
                  }
                else
                  {
                    my $rev_p = reverse_provides ($s, $p);

                    if ($rev_p && enforce_op ($op, $s->{$rev_p}, $version))
                      {
                        $packages{$rev_p} =
                          (defined ($op) && $op ne "") ? "$op $version"
                                                       : ">= $s->{$rev_p}";
                        next SPEC;
                      }
                  }
              }
          }

        push @missing, "$p $op $version";
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

