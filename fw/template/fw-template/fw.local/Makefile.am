SUBDIRS = template

fwfwtemplatefwlocaldir = $(prefix)/share/fw/template/fw-template/fw.local

dist_fwfwtemplatefwlocal_DATA = 	\
  Makefile_dot_am			\
  Makefile.am.local

install-data-hook:
	find "$(DESTDIR)$(fwfwtemplatefwlocaldir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
