
	SUBROUTINE PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +				   FINTERVAL, FMIN, FMAX, FINT, IFAIL)

C=============================================================================
C Routine to set the frequency search limits. If any one of the frequency 
C limits are less than or equal to zero, PERIOD_AUTOLIM returns default values.
C These are calculated as follows: MINFREQ = 0 (ie. infinite period); 
C MAXFREQ = NYQUIST critical frequency (ie. 1/(2*SMALLEST TIME INTERVAL)); 
C FINTERVAL = (FMAX-FMIN)/(MXROW*0.5). PERIOD_AUTOLIM also checks whether the 
C limits are valid, and if not, returns IFAIL = 1.
C
C Written by Vikram Singh Dhillon @Sussex 16-June-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C Modified FINTERVAL settings, Vik Dhillon @WHT 24-August-2004.
C=============================================================================

	IMPLICIT NONE	

	INTEGER NDATA, MXROW, IFAIL, I
	DOUBLE PRECISION FDATA(NDATA)
	DOUBLE PRECISION MINFREQ, MAXFREQ, FINTERVAL
	DOUBLE PRECISION FMIN, FMAX, FINT
	DOUBLE PRECISION STI

C-----------------------------------------------------------------------------
C Calculate the Smallest Time Interval, STI.
C-----------------------------------------------------------------------------

	IFAIL = 0
	STI = 1.0D+32
	DO I = 2, NDATA
		IF ( (FDATA(I)-FDATA(I-1)) .LT. STI) THEN
			STI = FDATA(I) - FDATA(I-1)
		END IF
	END DO

C-----------------------------------------------------------------------------
C If any frequency limit is less than or equal to zero, set to default value.
C Replaced the default FINTERVAL=1/(POINTS PER BEAM*OVERALL TIME INTERVAL) by
C (FMAX-FMIN)/(MXROW*0.5). If a negative FINTERVAL is entered, this routine now
C interprets it as the total number of intervals wanted. 
C-----------------------------------------------------------------------------

	IF (MINFREQ .LE. 0.) THEN
		FMIN = 0.0D0
	ELSE
		FMIN = MINFREQ
	END IF
	IF (MAXFREQ .LE. 0.) THEN
		FMAX = 1.0D0 / (2.0D0 * STI)
	ELSE
 		FMAX = MAXFREQ
	END IF
	IF (FINTERVAL .EQ. 0.0D0) THEN
		FINT = (FMAX-FMIN)/(MXROW*0.5D0)
	ELSE IF (FINTERVAL .LT. 0.) THEN
		FINT = (FMAX-FMIN)/((FINTERVAL+1.0D0)*(-1.0D0))
	ELSE
		FINT = FINTERVAL
	END IF

C-----------------------------------------------------------------------------
C Output final frequency limits.
C-----------------------------------------------------------------------------

	WRITE(*,*)'** OK: Minimum frequency  = ',FMIN
	WRITE(*,*)'** OK: Maximum frequency  = ',FMAX
	WRITE(*,*)'** OK: Frequency interval = ',FINT

C-----------------------------------------------------------------------------
C Check frequency limits.
C-----------------------------------------------------------------------------

	IF (FMAX .LE. FMIN) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum frequency not greater than'
		WRITE(*,*)'** ERROR: minimum frequency in PERIOD_AUTOLIM.'
		IFAIL = 1
		GOTO 99
	ELSE IF ( DSQRT((AINT((FMAX-FMIN)/FINT))**2.0D0) .GE. MXROW) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Frequency interval too small'
		WRITE(*,*)'** ERROR: in PERIOD_AUTOLIM.'
		IFAIL = 1
		GOTO 99
	END IF

99	RETURN
	END
