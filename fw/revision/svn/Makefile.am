fwsvndir = $(prefix)/share/fw/revision/svn

dist_fwsvn_SCRIPTS = 		\
  add-dir			\
  add-files			\
  check-for-tag			\
  check-up-to-date		\
  detect			\
  ignore-files			\
  init				\
  init-fw-pkgin-config		\
  remove-files			\
  tag

install-data-hook:
	find "$(DESTDIR)$(fwsvndir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwsvn_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
