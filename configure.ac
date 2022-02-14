AC_INIT([diseq],[0.4.2])

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

GSL_LIBS=`gsl-config --libs`
AS_IF([test "X$GSL_LIBS" != "X"], [
  CPPFLAGS="$CPPFLAGS `gsl-config --cflags`"
  LIBS="$LIBS `gsl-config --libs`"
  AC_MSG_CHECKING([whether libgsl is available])
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[#include "gsl/gsl_multimin.h"]],
		     [[gsl_vector v;]])],
    [AC_MSG_RESULT([yes])
     PKG_CPPFLAGS="$PKG_CPPFLAGS -D_DISEQ_HAS_GSL_ `gsl-config --cflags`"
     PKG_LIBS="$PKG_LIBS `gsl-config --libs`"],
    [AC_MSG_RESULT([no])])
])

AC_CHECK_HEADERS(execution)
AS_IF([test "X$ac_cv_header_execution" = "Xyes"], [
  AC_MSG_CHECKING([whether the libtbb is available])
  CPPFLAGS="$CPPFLAGS -std=c++17"
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[#include<execution>]],
    [[const auto& ex = std::execution::par_unseq;]])],
    [AC_MSG_RESULT([yes])
     PKG_CPPFLAGS="$PKG_CPPFLAGS -D_DISEQ_HAS_EXECUTION_POLICIES_  -std=c++17"
     PKG_LIBS="$PKG_LIBS `pkg-config tbb --libs`"],
    [LIBS="$LIBS -ltbb"
     AC_LINK_IFELSE(
     [AC_LANG_PROGRAM([[#include<execution>]],
     [[const auto& ex = std::execution::par_unseq;]])],
     [AC_MSG_RESULT([yes])
      PKG_CPPFLAGS="$PKG_CPPFLAGS -D_DISEQ_HAS_EXECUTION_POLICIES_  -std=c++17"
      PKG_LIBS="$PKG_LIBS -ltbb"],
     [AC_MSG_RESULT([no])])
    ])
])

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${PKG_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${PKG_CPPFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
