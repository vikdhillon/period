# XANADU programming stuff (for csh/tcsh users)
# You must run qinitu before calling this routine.

# For each system, define FCOMPL, FCOPTS, CCOPTS, PGLIBS, and SYSLIB as needed.

if      ( "$EXT" == "cry" ) then
   setenv FCOMPL cf77
   setenv FCOPTS " "
   setenv FLOPTS " "
   setenv CCOMPL cc
   setenv CCOPTS " "
   setenv RANLIB touch
   setenv PGLIBS "${XANSYS}/lib/libpgplot.a -lX11"

else if ( "$EXT" == "lnx" ) then
    setenv FCOMPL gfortran
    setenv FCOPTS "-O -fno-second-underscore -fno-backslash"
    setenv CCOMPL gcc
    setenv RANLIB ranlib
    setenv PGLIBS "-L/usr/local/lib -lpgplot -L/opt/X11/lib -lX11"
    setenv SYSLIB " "

else if ( "$EXT" == "mac" ) then
   setenv FCOMPL g77
   setenv FCOPTS "-O -Wimplicit -fno-second-underscore -fno-backslash"
   setenv FLOPTS " "
   setenv CCOMPL gcc
   setenv CCOPTS "-I${XANTOP}/xanlib/cfortran -Df2cFortran"
   setenv RANLIB ranlib
   if ( -d /usr/X11R6 ) then
      setenv PGLIBS "-L${XANSYS}/lib -lpgplot -L/usr/X11R6/lib -lX11"
   else
      setenv PGLIBS "-L${XANSYS}/lib -lpgplot -lpng -lz"
   endif
   setenv SYSLIB "-lcc_dynamic"

else if ( "$EXT" == "nfc" ) then
   setenv FCOMPL nfc
   setenv FCOPTS "-u -O"
   setenv FLOPTS " "
   setenv CCOMPL cc
   setenv CCOPTS "-I${XANTOP}/xanlib/cfortran -Df2cFortran"
   setenv RANLIB ranlib
   setenv PGLIBS "-L/usr/X11R6/lib ${XANSYS}/lib/libpgplot.a -lX11"
   setenv SYSLIB " "

else if ( "$EXT" == "sgi" ) then
   setenv MAKE "/usr/bin/make"
   setenv FCOMPL "f77"
   setenv FCOPTS "-O -backslash"
   setenv FLOPTS " "
   setenv CCOMPL "cc"
   setenv CCOPTS "-O -DHAVE_DIRENT_H -DHAVE_STRING_H -DHAVE_POSIX_SIGNALS -DEXT=sgi -I$XANTOP/xanlib/cfortran -I$XANSYS/include"
   setenv RANLIB ":"
   setenv PGLIBS "$XANSYS/lib/libpgplot.a -lX11"

else if ( "$EXT" == "sol" ) then
   setenv MAKE "/usr/ccs/bin/make"
   setenv FCOMPL "g77"
   setenv FCOPTS "-O -Wimplicit -fno-second-underscore -fno-backslash"
   setenv FLOPTS " "
   setenv CCOMPL "gcc"
   setenv CCOPTS "-g -O2 -DHAVE_DIRENT_H -DHAVE_STRING_H -INONE -LNONE -Dsolaris -Wl,-R/opt/SUNWspro/lib:/opt/SUNWspro/SC4.0/lib:/usr/ccs/lib:/usr/lib:${XANSYS}/lib -DEXT=sol -I$XANTOP/xanlib/cfortran -I$XANSYS/include"
   setenv RANLIB "ranlib"
   setenv PGLIBS "$XANSYS/lib/libpgplot.a -lX11"
   setenv SYSLIB "-lsocket -lnsl"

else if ( "$EXT" == "sun" ) then
   setenv MAKE "/usr/bin/make"
   setenv FCOMPL "f77"
   setenv FCOPTS "-O -f"
   setenv FLOPTS " "
   setenv CCOMPL "gcc"
   setenv CCOPTS "-g -O2 -DHAVE_DIRENT_H -DHAVE_STRING_H -INONE -LNONE -DEXT=sun -I$XANTOP/xanlib/cfortran -I$XANSYS/include"
   setenv RANLIB "ranlib"
   setenv PGLIBS "$XANSYS/lib/libpgplot.a -LNONE -LNONE -lX11"
   setenv SYSLIB "-L/usr/lang/SC1.0/cg87 -L/usr/lang/SC1.0 -ltermcap -ldl -lF77 -lm -lc -LNONE -lX11"

else
   echo Unknown system.  Using generic settings.
   setenv FCOMPL f77
   setenv CCOMPL cc
   setenv RANLIB ranlib

endif


# Add $XANTOP/tools to PATH
if ( "`echo $PATH | grep $XANTOP/tools`" == "" ) then
   setenv PATH $XANTOP/tools:$PATH
endif

# If there is a local initp file, then source it.
if ( -f $XANTOP/local/tools/initp.csh ) then
   source $XANTOP/local/tools/initp.csh
endif
