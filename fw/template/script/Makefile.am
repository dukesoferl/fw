SUBDIRS = bin tests

fwscriptdir = $(prefix)/share/fw/template/script

dist_fwscript_SCRIPTS =			\
  bootstrap				\
  init					\
  load-config				\
  release-ok				\
  source-install

dist_fwscript_DATA = 			\
  Makefile_dot_am			\
  configure.ac

install-data-hook:
	find "$(DESTDIR)$(fwscriptdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwscript_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
