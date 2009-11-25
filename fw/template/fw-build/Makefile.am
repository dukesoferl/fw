SUBDIRS = fw.local

fwfwbuilddir = $(prefix)/share/fw/template/fw-build

dist_fwfwbuild_SCRIPTS =		\
  bootstrap				\
  init					\
  load-config				\
  release-ok				\
  source-install

dist_fwfwbuild_DATA = 			\
  Makefile_dot_am			\
  configure.ac

install-data-hook:
	find "$(DESTDIR)$(fwfwbuilddir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwfwbuild_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
