SUBDIRS = fw.local tests

fwfwtemplatedir = $(prefix)/share/fw/template/fw-template

dist_fwfwtemplate_SCRIPTS =		\
  bootstrap				\
  init					\
  load-config				\
  release-ok				\
  source-install

dist_fwfwtemplate_DATA = 		\
  Makefile_dot_am			\
  configure.ac

install-data-hook:
	find "$(DESTDIR)$(fwfwtemplatedir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwfwtemplate_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
