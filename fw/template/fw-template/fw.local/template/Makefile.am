SUBDIRS = NAME

fwfwtemplatefwlocaltemplatedir = $(prefix)/share/fw/template/fw-template/fw.local/template

dist_fwfwtemplatefwlocaltemplate_DATA = 	\
  Makefile_dot_am				\
  Makefile.am.local

install-data-hook:
	find "$(DESTDIR)$(fwfwtemplatefwlocaltemplatedir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
