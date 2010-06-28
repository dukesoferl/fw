fwscriptbindir = $(prefix)/share/fw/template/script/bin

dist_fwscriptbin_DATA = 		\
  Makefile.am.local			\
  Makefile_dot_am			\
  myscript

install-data-hook:
	find "$(DESTDIR)$(fwscriptbindir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
