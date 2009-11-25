fwfwtemplatetestsdir = $(prefix)/share/fw/template/fw-template/tests

dist_fwfwtemplatetests_SCRIPTS =	\
  test-template.in

dist_fwfwtemplatetests_DATA = 		\
  Makefile.am.local			\
  Makefile_dot_am

install-data-hook:
	find "$(DESTDIR)$(fwfwtemplatetestsdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwfwtemplatetests_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES = 			\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
