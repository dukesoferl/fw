SUBDIRS = build

fwfwbuildfwlocaldir = $(prefix)/share/fw/template/fw-build/fw.local

dist_fwfwbuildfwlocal_DATA = 		\
  Makefile_dot_am			\
  Makefile.am.local

install-data-hook:
	find "$(DESTDIR)$(fwfwbuildfwlocaldir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
