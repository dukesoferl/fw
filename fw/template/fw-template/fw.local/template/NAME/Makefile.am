fwfwtemplatefwlocalnamedir = $(prefix)/share/fw/template/fw-template/fw.local/template/NAME

dist_fwfwtemplatefwlocalname_DATA = 		\
  Makefile_dot_am				\
  Makefile_dot_am_dot_am			\
  Makefile.am.local				\
  bootstrap					\
  configure.ac					\
  init						\
  load-config					\
  make-dist					\
  release-ok					\
  source-install

install-data-hook:
	find "$(DESTDIR)$(fwfwtemplatefwlocalnamedir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
