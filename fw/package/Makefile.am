SUBDIRS = deb rpm

fwpackagedir = $(prefix)/share/fw/package/

dist_fwpackage_SCRIPTS =			\
  check-for-pkgconfig-packages

install-data-hook:
	find "$(DESTDIR)$(fwpackagedir)" -type f | xargs chmod -w

noinst_DATA =					\
  $(dist_fwpackage_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
