fwseeveeessdir = $(prefix)/share/fw/revision/see-vee-ess

dist_fwseeveeess_SCRIPTS = 	\
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
	find "$(DESTDIR)$(fwseeveeessdir)" -type f | xargs chmod -w
	mv "$(DESTDIR)$(fwseeveeessdir)" 				\
	   "$(DESTDIR)$(prefix)/share/fw/revision/cvs"

noinst_DATA =				\
  $(dist_fwseeveeess_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
