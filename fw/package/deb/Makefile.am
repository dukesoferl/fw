fwdebdir = $(prefix)/share/fw/package/deb

dist_fwdeb_SCRIPTS = 		\
  FwDebPackage.pm		\
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
	find "$(DESTDIR)$(fwdebdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwdeb_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
