AC_INIT([diseq],[0.1.2])

# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_PROG_CPP

AC_CHECK_HEADERS(execution)
AS_IF([test "X$ac_cv_header_execution" = "Xyes"], [
  AC_MSG_CHECKING([whether the libtbb is available])
  OLD_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="-std=c++17"
  OLD_LIBS="$LIBS"
  LIBS="$LIBS -ltbb"
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[#include<algorithm>
                       #include<execution> ]],
    [[int v[2] = {1, 1};
      std::for_each(std::execution::par_unseq, v, v + 2, [](int &n){ n++; });]])],
    [AC_MSG_RESULT([yes])
     PKG_CPPFLAGS="$PKG_CPPFLAGS -D_DISEQ_HAS_EXECUTION_POLICIES_ -std=c++17"
     PKG_LIBS="$PKG_LIBS -ltbb"],
    [AC_MSG_RESULT([no])])
  CPPFLAGS="$OLDCPPFLAGS"
  LIBS="$OLDLIBS"
])
AC_CHECK_LIB([m],[cos])
AC_CHECK_LIB([gslcblas],[cblas_dgemm])
AC_CHECK_LIB([gsl],[gsl_blas_dgemm])

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS} ${PKG_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
