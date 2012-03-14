fwnonedir = $(prefix)/share/fw/template/none

dist_fwnone_SCRIPTS =			\
  bootstrap				\
  init					\
  load-config				\
  release-ok				\
  source-install

dist_fwnone_DATA = 			\
  Makefile_dot_am			\
  configure.ac

install-data-hook:
	find "$(DESTDIR)$(fwnonedir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwnone_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
