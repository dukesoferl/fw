AC_DEFUN([FW_CHECK_BUILD_ENVIRONMENT],
[
  AC_ARG_ENABLE(getdeps,
                [--enable-getdeps        fetch build dependencies - rpm systems only for the moment],
                [case "${enableval}" in
                   yes) FW_ENABLE_GETDEPS="yes" ;;
                   no) FW_ENABLE_GETDEPS="no" ;;
                   *) AC_MSG_ERROR([bad value ${enableval} for --enable-getdeps]) ;;
                 esac],
                [FW_ENABLE_GETDEPS="no"])
  AC_SUBST([FW_ENABLE_GETDEPS])

  AC_REQUIRE([FW_DETECT_NATIVE_PACKAGE])

  if test "$FW_NATIVE_PACKAGE_TYPE" != none
    then
      AC_MSG_CHECKING([for build dependencies])

      path="package/$FW_NATIVE_PACKAGE_TYPE/check-build-environment"
      fw-exec "$path" --template "$FW_TEMPLATE" --getdeps "$FW_ENABLE_GETDEPS" || exit 1

      AC_MSG_RESULT([ok])
    fi
])
