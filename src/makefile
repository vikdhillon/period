#
# Simple makefile for period.
# Written by Vik Dhillon, Sheffield, 07-May-2003
# Modified for MAC-OSX by Vik Dhillon, Sheffield, 24-Apr-2006
# Now handles different compilers, Vik Dhillon, Sheffield, 27-Aug-2009
# Now handles SOFA in a different place, Vik Dhillon, Sheffield, 28-Dec-2010
# Removed png and X11 link dependency for mac, Vik Dhillon, Sheffield, 7-Mar-2015
# Reintroduced png and X11 link dependency for mac (!), Vik Dhillon, Sheffield, 1-May-2015
#
# You should only need to change the following variables, depending
# on which platform you are using:
# COMPILER is the fortran compiler you wish to use - only f77 and gfortran allowed
# OPTIONS are the fortran compiler options you wish to use
# PERIOD_DIR is the path to the directory containing $VERS/
# PGPLOT is the rule for linking to pgplot
# SLALIB is the rule for linking to slalib
# QDP is the rule for linking to QDP/PLT
# SOFA is the rule for linking to SOFA

# Settings for macbook pro, 64-bit, Mavericks
COMPILER = gfortran
OPTIONS = -Wall -Wno-character-truncation -Wtabs
PERIOD_DIR = /Users/vsd/period
PGPLOT = -L/usr/local/lib -lpgplot -L/usr/local/Cellar/libpng -lpng -L/opt/X11/lib -lX11
SLALIB = -L/Users/vsd/period/qdp/lnx/lib -lsla
QDP = -L/Users/vsd/period/qdp/lnx/lib -lxanlib
SOFA = -L/Users/vsd/period/sofa/20170420/f77/src -lsofa

# Settings for pulsar
#COMPILER = gfortran
#OPTIONS = -Wall -Wno-character-truncation -Wno-tabs 
#PERIOD_DIR = /local/vsd/backed_up_on_astro3/period
#PGPLOT = -L/usr/local/pgplot-5.2.2 -lpgplot -L/usr/lib -lX11 -lpng
#SLALIB = -L/local/vsd/backed_up_on_astro3/period/qdp/lnx/lib -lsla
#QDP = -L/local/vsd/backed_up_on_astro3/period/qdp/lnx/lib -lxanlib
#SOFA = -L/local/vsd/backed_up_on_astro3/period/sofa/20180130/f77/src -lsofa

# Settings for ULTRACAM data reduction PC
#COMPILER = gfortran
#OPTIONS = -Wall -Wno-character-truncation -Wtabs 
#PERIOD_DIR = /home/vsd/progs/period
#PGPLOT = -L/usr/local/pgplot-5.2.2 -lpgplot -L/usr/lib -lX11
#SLALIB = -L/home/vsd/progs/period/qdp/lnx/lib -lsla
#QDP = -L/home/vsd/progs/period/qdp/lnx/lib -lxanlib

# Version number
VERS = period_v10.0

OBJ =   period_airmass.o \
	period_append.o \
	period_arith.o \
	period_autolim.o \
	period_bin.o \
	period_boot.o \
	period_case.o \
	period_clean.o \
	period_clip.o \
	period_copy.o \
	period_dcf.o \
	period_del.o \
	period_deriv.o \
	period_detrend.o \
	period_errors.o \
	period_fake.o \
	period_fit.o \
	period_fold.o \
	period_four1.o \
	period_ft.o \
	period_gasdev.o \
	period_help.o \
	period_hinput.o \
	period_hist.o \
	period_hslots.o \
	period_input.o \
	period_integ.o \
	period_intro.o \
	period_jiggle.o \
	period_log.o \
	period_lsquar.o \
	period_main.o \
	period_medfilt.o \
	period_median.o \
	period_menu.o \
	period_moment.o \
	period_noise.o \
	period_output.o \
	period_parse.o \
	period_pdm.o \
	period_period.o \
	period_phase.o \
	period_plt.o \
	period_poly.o \
	period_polyfit.o \
	period_quit.o \
	period_ran1.o \
	period_readfree.o \
	period_realft.o \
	period_resamp.o \
	period_scargle.o \
	period_select.o \
	period_shellsort.o \
	period_show.o \
	period_sine.o \
	period_sinfit.o \
	period_smooth.o \
	period_sort.o \
	period_string.o \
	period_time.o \
	period_ucal.o \
	period_uinput.o \
	period_unblue.o \
	period_uslots.o \
	period_window.o

EXE = $(PERIOD_DIR)/$(VERS)/bin

XCURSES = -L/usr/lib -lcurses

period: $(OBJ)
	$(COMPILER) -O2 $(OBJ) $(SLALIB) $(QDP) $(SOFA) $(XCURSES) $(PGPLOT) -o $(EXE)/$(VERS)

.f.o:
	$(COMPILER) -c $(OPTIONS) $*.f

clean:
	rm -f *.o
	rm -f $(EXE)/$(VERS)
