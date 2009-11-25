SUBDIRS = fw-pkgin

fwautomakedir = $(prefix)/share/fw/build/automake

dist_fwautomake_SCRIPTS = 	\
  bootstrap			\
  build				\
  build-ac-init			\
  init				\
  load-config			\
  make-dist			\
  release-ok			\
  source-install

dist_fwautomake_DATA =		\
  AUTHORS 			\
  ChangeLog			\
  Makefile.am.local		\
  Makefile_dot_am		\
  NEWS				\
  README			\
  check-shell.am		\
  configure.ac			\
  configure.ac.local		\
  gnu-make-check.am

install-data-hook:
	find "$(DESTDIR)$(fwautomakedir)" -type f | xargs chmod -w

noinst_DATA =				\
  $(dist_fwautomake_SCRIPTS:%=.%.script_ok)

CLEANFILES =

MAINTAINERCLEANFILES =		\
  Makefile.in

include $(top_srcdir)/fw/build/automake/check-shell.am
include $(top_srcdir)/fw/build/automake/gnu-make-check.am
