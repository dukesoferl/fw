SUBDIRS = NAME

fwfwbuildfwlocalbuilddir = $(prefix)/share/fw/template/fw-build/fw.local/build

dist_fwfwbuildfwlocalbuild_DATA = 		\
  Makefile_dot_am				\
  Makefile.am.local

install-data-hook:
	find "$(DESTDIR)$(fwfwbuildfwlocalbuilddir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
