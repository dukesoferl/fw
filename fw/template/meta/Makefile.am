fwmetadir = $(prefix)/share/fw/template/meta

dist_fwmeta_SCRIPTS =			\
  bootstrap				\
  init					\
  load-config				\
  release-ok				\
  source-install

dist_fwmeta_DATA = 			\
  Makefile_dot_am			\
  configure.ac

install-data-hook:
	find "$(DESTDIR)$(fwmetadir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwmeta_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
