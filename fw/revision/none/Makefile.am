fwnonedir = $(prefix)/share/fw/revision/none

dist_fwnone_SCRIPTS = 	\
  add-dir		\
  add-files		\
  init			\
  check-for-tag		\
  check-up-to-date	\
  ignore-files		\
  remove-files		\
  tag

install-data-hook:
	find "$(DESTDIR)$(fwnonedir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwnone_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
