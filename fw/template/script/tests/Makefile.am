fwscripttestsdir = $(prefix)/share/fw/template/script/tests

dist_fwscripttests_DATA = 		\
  Makefile.am.local			\
  Makefile_dot_am			\
  testmyscript

install-data-hook:
	find "$(DESTDIR)$(fwscripttestsdir)" -type f | xargs chmod -w

MAINTAINERCLEANFILES = Makefile.in

include $(top_srcdir)/fw/build/automake/gnu-make-check.am
