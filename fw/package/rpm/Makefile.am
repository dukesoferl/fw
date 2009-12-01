fwrpmdir = $(prefix)/share/fw/package/rpm

dist_fwrpm_SCRIPTS = 		\
  FwRpmPackage.pm		\
  canonicalize-package		\
  canonicalize-version		\
  check-build-environment	\
  check-for-packages		\
  dependency-closure		\
  dependency-range		\
  detect			\
  get-arch			\
  list-files			\
  make-build-package		\
  make-hook			\
  make-package			\
  release-ok			\
  release-package		\
  source-install		\
  tag-name

install-data-hook:
	find "$(DESTDIR)$(fwrpmdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwrpm_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
