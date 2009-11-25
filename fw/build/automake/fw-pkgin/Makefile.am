fwautomakefwpkgindir=$(prefix)/share/fw/build/automake/fw-pkgin

fwautomakefwpkgin_SCRIPTS =			\
  post-install					\
  post-remove					\
  pre-install					\
  pre-remove					\
  start						\
  stop

fwautomakefwpkgin_DATA =			\
  Makefile.am.local				\
  Makefile_dot_am

install-data-hook:
	find "$(DESTDIR)$(fwautomakefwpkgindir)" -type f | xargs chmod -w

noinst_DATA =					\
  $(fwautomakefwpkgin_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =				\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
