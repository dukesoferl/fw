fwfwbuildfwlocalnamedir = $(prefix)/share/fw/template/fw-build/fw.local/build/NAME

dist_fwfwbuildfwlocalname_DATA = 		\
  Makefile_dot_am				\
  Makefile.am.local				\
  bootstrap					\
  build						\
  init						\
  make-dist					\
  release-ok					\
  source-install

install-data-hook:
	find "$(DESTDIR)$(fwfwbuildfwlocalnamedir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
