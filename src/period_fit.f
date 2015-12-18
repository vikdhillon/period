        
	SUBROUTINE PERIOD_FIT  (Y, MXROW, MXSLOT,
     +			        NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY,
     +				LOG, LOGUNIT)

C===========================================================================
C Folds data on a given PERIOD with a zero point defined by ZEROPT and
C then fits a sine curve to the data.
C
C Written by Vikram Singh Dhillon @Sussex 20-March-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Number of x-axis points in output slot option added VSD@Sheffield 14/12/09 
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_FIT declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA, NUMPTS
	DOUBLE PRECISION GAMMA, KVEL, PHASE, VAR(6), F, PI, HJD0
	DOUBLE PRECISION PERIOD, ZEROPT
        DOUBLE PRECISION XDATA(MXROW), YDATA(MXROW), YERR(MXROW)
	DOUBLE PRECISION PERROR, HJD0ERR, ERRPHPE, ZERROR
	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I, FIRSTOUT, IFAIL, SLOTOUT
	INTEGER NP, LOGUNIT, COUNTER
	LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT), LOG
	CHARACTER*1 REPLY
	CHARACTER*72 INFILEARRAY(MXSLOT)
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for PERIOD and ZEROPT parameters.
C-----------------------------------------------------------------------------
	
	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Enter period to fold data on : '
	READ(*,*,ERR=10)PERIOD
	IF (PERIOD .LE. 0.) GOTO 10
20	WRITE(*,'(A,$)')'Enter error in period : '
	READ(*,*,ERR=20)PERROR
	IF (PERROR .LT. 0.) GOTO 20
30	WRITE(*,'(A,$)')'Enter zero point (0 for first
     + data point) : '
	READ(*,*,ERR=30)ZEROPT
40	WRITE(*,'(A,$)')'Enter error in zero point : '
	READ(*,*,ERR=40)ZERROR
	IF (ZERROR .LT. 0.) GOTO 40
50	WRITE(*,'(A,$)')'Number of x-axis points in output slot ? (0 for
     + same as input slot) : '
	READ(*,*,ERR=50)NUMPTS
        IF (NUMPTS .LT. 0.) GOTO 50
        IF (NUMPTS .GT. MXROW) THEN
           WRITE(*,*)'** ERROR: Maximum number of data points = ',MXROW
           GOTO 50
        END IF
	COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
	SLOTOUT = FIRSTOUT + COUNTER
	COUNTER = COUNTER + 1
	IF (ZEROPT .EQ. 0.) THEN
		ZEROPT = Y(1,1,SLOT)		
	END IF

C-----------------------------------------------------------------------------
C Fold and sort data.
C-----------------------------------------------------------------------------
        
	NDATA = NPTSARRAY(SLOT)
	DO I = 1, NDATA
		XDATA(I) = Y(I,1,SLOT)
		YDATA(I) = Y(I,2,SLOT)
		IF (YERRORARRAY(SLOT)) THEN
		YERR(I) = Y(I,3,SLOT)
		ELSE 
		YERR(I) = 1.0D0
		END IF
	END DO
	CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, ZEROPT, PERIOD)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Fit data.
C-----------------------------------------------------------------------------

	CALL PERIOD_SINFIT (XDATA, YDATA, YERR, NDATA, 1.0D0,
     +		     GAMMA, KVEL, PHASE, VAR, NP, F, IFAIL)	
	IF (IFAIL .EQ. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Sine fit unsuccessful.'
		GOTO 99
	END IF 	
	HJD0 = ZEROPT + (PHASE * PERIOD)
	ERRPHPE = DSQRT (((PHASE * PERIOD)**2.0D0) * 
     +		       ((VAR(6) / (PHASE**2.0D0)) + 
     +		       ((PERROR**2.0D0) / (PERIOD**2.0D0))))
	HJD0ERR = DSQRT ( (ZERROR**2.0D0) + (ERRPHPE**2.0D0) )
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Period = ',PERIOD
	WRITE(*,*)'** OK: Sigma period = ',PERROR
	WRITE(*,*)'** OK: Reduced chi-squared = ',F/(DBLE(NP)-3.0D0)
	WRITE(*,*)'** OK: Number of points = ',NP
	WRITE(*,*)'** OK: Gamma = ',GAMMA
	WRITE(*,*)'** OK: Sigma gamma = ',DSQRT(VAR(1))
	WRITE(*,*)'** OK: Semi-amplitude = ',KVEL
	WRITE(*,*)'** OK: Sigma semi-amplitude = ',DSQRT(VAR(4))
	WRITE(*,*)'** OK: Phi0 = ',PHASE
	WRITE(*,*)'** OK: Sigma phi0 = ',DSQRT(VAR(6))
	WRITE(*,*)'** OK: Original zero point = ',ZEROPT
	WRITE(*,*)'** OK: Revised zero point = ',HJD0
	WRITE(*,*)'** OK: Error in revised zero point = ',HJD0ERR
	WRITE(*,*)' '
45	WRITE(*,'(A,$)')'Do you want to log this fit ? [N] : '
	READ(*,'(A)',ERR=45)REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.)
	IF (REPLY .EQ. 'Y') THEN
	IF (.NOT. LOG) THEN
	WRITE(*,*)ACHAR(7)
	WRITE(*,*)'** ERROR: No log file has been opened.'
	GOTO 60
	END IF
	WRITE(LOGUNIT,*)' '
	WRITE(LOGUNIT,*)
     + '** OK: Sinusoidal fit to '//INFILEARRAY(SLOT)
	WRITE(LOGUNIT,*)' '
	WRITE(LOGUNIT,*)'** OK: Period = ',PERIOD
	WRITE(LOGUNIT,*)'** OK: Sigma period = ',PERROR
	WRITE(LOGUNIT,*)'** OK: Reduced chi-squared = ',F/(DBLE(NP)-3.0D0)
	WRITE(LOGUNIT,*)'** OK: Number of points = ',NP
	WRITE(LOGUNIT,*)'** OK: Gamma = ',GAMMA
	WRITE(LOGUNIT,*)'** OK: Sigma gamma = ',DSQRT(VAR(1))
	WRITE(LOGUNIT,*)'** OK: Semi-amplitude = ',KVEL
	WRITE(LOGUNIT,*)'** OK: Sigma semi-amplitude = ',DSQRT(VAR(4))
	WRITE(LOGUNIT,*)'** OK: Phi0 = ',PHASE
	WRITE(LOGUNIT,*)'** OK: Sigma phi0 = ',DSQRT(VAR(6))
	WRITE(LOGUNIT,*)'** OK: Original zero point = ',ZEROPT
	WRITE(LOGUNIT,*)'** OK: Revised zero point = ',HJD0
	WRITE(LOGUNIT,*)'** OK: Error in revised zero point = ',HJD0ERR
	WRITE(LOGUNIT,'(A)')'.'
	END IF

C-----------------------------------------------------------------------------
C Load output slot.
C-----------------------------------------------------------------------------

60	CONTINUE
        IF (NUMPTS .GT. 0) THEN
           NDATA = NUMPTS
        ELSE
           NDATA = NPTSARRAY(SLOT)
        END IF
	DO I = 0, NDATA
           IF (NUMPTS .GT. 0) THEN
              Y(I,1,SLOTOUT) = DBLE(I) / DBLE(NDATA)
           ELSE
              Y(I,1,SLOTOUT) = XDATA(I)
           END IF
	Y(I,2,SLOTOUT) = GAMMA+
     + (KVEL*DSIN(2.0D0*PI*(Y(I,1,SLOTOUT)-PHASE)))
	Y(I,3,SLOTOUT) = 0.0D0
	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	NPTSARRAY(SLOTOUT) = NDATA
	INFILEARRAY(SLOTOUT) = 'sine-fit, '//INFILEARRAY(SLOT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
