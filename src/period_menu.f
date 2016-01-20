
	SUBROUTINE PERIOD_MENU (BASE, LOG, OVERPLOT, QDP, OFFSET)

C=============================================================================
C Routine to show PERIOD menu.
C
C Written by Vikram Singh Dhillon @Paranal 2005-May-11.
C=============================================================================

	IMPLICIT NONE
C-----------------------------------------------------------------------------
C PERIOD_MENU declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION OFFSET
	LOGICAL BASE, LOG, OVERPLOT, QDP

	WRITE(*,*)' '
	WRITE(*,*)'Options.'
	WRITE(*,*)'--------'
	WRITE(*,*)' '
	WRITE(*,*)'   AIRMASS  --  Calculate airmass.'
	WRITE(*,*)'   APPEND   --  Append slot.'
	WRITE(*,*)'   ARITH    --  Slot arithmetic.'
	IF (BASE) THEN
	  WRITE(*,*)'   BASE     --  Subtracting integer part in PLT.'
	ELSE
	  WRITE(*,*)'   BASE     --  Not subtracting integer part in PLT.'
	END IF
	WRITE(*,*)'   BIN      --  Bin data.'
	WRITE(*,*)'   BOOT     --  Bootstrap data.'
	WRITE(*,*)'   CLIP     --  Clip data.'
	IF (LOG) THEN
	   WRITE(*,*)'   CLOSE    --  Close log file.'
	END IF
	WRITE(*,*)'   COPY     --  Copy slots.'
	WRITE(*,*)'   DEL      --  Delete slots.'
	WRITE(*,*)'   DERIV    --  Derivative of data.'
	WRITE(*,*)'   DETREND  --  Detrend data.'
	WRITE(*,*)'   ERRORS   --  Calculate error bars.'
	WRITE(*,*)'   FAKE     --  Create fake data.'
	WRITE(*,*)'   FIT      --  Fit sine curve to folded data.'
	WRITE(*,*)'   FOLD     --  Fold data on given period.'
	WRITE(*,*)'   HELP     --  On-line help.'
	WRITE(*,*)'   HIST     --  Histogram or CDF of data.'
	WRITE(*,*)'   INPUT    --  Input ASCII data.'
	WRITE(*,*)'   INTEG    --  Integrate data.'
	WRITE(*,*)'   JIGGLE   --  Jiggle data.'
	WRITE(*,*)'   NOISE    --  Randomise data.'
	IF (OVERPLOT) THEN
	   WRITE(*,*)'   OFF      --  Add y-offset to plots = ',OFFSET
	END IF
	IF (.NOT. LOG) THEN
	   WRITE(*,*)'   OPEN     --  Open log file.'
	END IF
	WRITE(*,*)'   OUTPUT   --  Output data.'
	IF (OVERPLOT) THEN
	   WRITE(*,*)'   OVERPLOT --  Overplotting ON.'
	ELSE
	   WRITE(*,*)'   OVERPLOT --  Overplotting OFF.'
	END IF
	WRITE(*,*)'   PERIOD   --  Find periodicities.'
	WRITE(*,*)'   PLT      --  Call PLT.'
	WRITE(*,*)'   POLY     --  Polynomial fit to data.'
	IF (QDP) THEN
	   WRITE(*,*)'   QDP      --  QDP functionality ON.'
	ELSE
	   WRITE(*,*)'   QDP      --  QDP functionality OFF.'
	END IF
	WRITE(*,*)'   QUIT     --  Quit PERIOD.'
	WRITE(*,*)'   SHOW     --  Slot information.'
	WRITE(*,*)'   SINE     --  +, -, /, * sine curves.'
	WRITE(*,*)'   SMOOTH   --  Smooth data.'
	WRITE(*,*)'   TIME     --  Barycentric/heliocentric correction.'
	WRITE(*,*)'   UCAL     --  Flux calibrate ULTRACAM data.'
	WRITE(*,*)'   UINPUT   --  Input ULTRACAM data.'
	WRITE(*,*)'   UNBLUE   --  Correct ULTRACAM data for NBLUE>1.'
	WRITE(*,*)'   USLOTS   --  Loads slots with ULTRACAM data.'
	WRITE(*,*)'   WINDOW   --  Set y-data to unity.'
	WRITE(*,*)'   XCOR     --  Cross-correlate slots.'
	WRITE(*,*)'  '

	RETURN
	END
