fwshdir = $(prefix)/share/fw/sh

dist_fwsh_SCRIPTS = 		\
  bootstrap-util.sh		\
  detect-revision.sh		\
  fatal.sh			\
  fw-exec.sh			\
  fw-find.sh			\
  parse-args.sh			\
  validate-args.sh

install-data-hook:
	find "$(DESTDIR)$(fwshdir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwsh_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
