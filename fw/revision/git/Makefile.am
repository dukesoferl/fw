fwgitdir = $(prefix)/share/fw/revision/git

dist_fwgit_SCRIPTS = 		\
  add-dir			\
  add-files			\
  check-for-tag			\
  check-up-to-date		\
  detect			\
  ignore-files			\
  init				\
  remove-files			\
  tag

install-data-hook:
	find "$(DESTDIR)$(fwgitdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwgit_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
