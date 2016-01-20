        
	SUBROUTINE PERIOD_DETREND (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY, 
     +				  INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Detrends data by either subtracting a polynomial fit or 
C subtracting the mean and dividing by the standard deviation.
C
C Written by Vikram Singh Dhillon @LPO 25-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Subtract integer part of x-axis to enable the routine to work with
C MJD's, Vik Dhillon @Paranal 09-May-2005.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_DETREND declarations.
C-----------------------------------------------------------------------------

	INTEGER MAXPOLY
	PARAMETER (MAXPOLY = 20)
	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR, RMS
	DOUBLE PRECISION PFT(MAXPOLY), CHISQ, PERIOD_POLY
	DOUBLE PRECISION XM(MAXPOLY,2*MAXPOLY+3), X(3,MXROW)
	INTEGER N, NDATA, NPOLY, SLOT, FIRSTSLOT, LASTSLOT, I
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER, NORM
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*1 OPTION, REPLY
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF
	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Detrend using the [M]ean or a'//
     + ' [P]olynomial fit ? [M] : '
	READ(*,'(A)',ERR=10)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)
	IF (OPTION .EQ. 'P' .OR. OPTION .EQ. 'M' 
     + .OR. OPTION .EQ. ' ') THEN
	IF (OPTION .EQ. 'P') THEN
15	WRITE(*,'(A,$)')'Enter order of polynomial : '
	READ(*,*,ERR=15)NPOLY
	IF (NPOLY .GT. MAXPOLY) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum polynomial
     + order = ',MAXPOLY
		GOTO 99
	ELSE IF (NPOLY .LE. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Minimum polynomial
     + order = 1'
		GOTO 99
	END IF
	END IF
	ELSE
		GOTO 99
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
	IF (DETRENDARRAY(SLOT)) THEN
	WRITE(*,*)ACHAR(7)
	WRITE(*,*)'** WARNING: Slot already detrended = ',SLOT
20	WRITE(*,'(A,$)')'** WARNING: Are you sure you want
     + to continue ? [N] : '
	READ(*,'(A)',ERR=20)REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.)
	IF (REPLY .EQ. 'Y') THEN
		GOTO 30
	ELSE
		GOTO 99
	END IF
	END IF
30	CONTINUE
	IF (OPTION .EQ. 'P') THEN
		IF (NPOLY .GE. NPTSARRAY(SLOT)) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Number of polynomial
     + terms greater than or equal to'
	                WRITE(*,*)'** ERROR: number of data points. Fit
     + aborted.'                  
			GOTO 99					
		END IF
		DO I = 1, NPTSARRAY(SLOT)
                   X(1,I) = Y(I,1,SLOT)-AINT(Y(1,1,SLOT))
                   X(2,I) = Y(I,2,SLOT)
		IF (YERRORARRAY(SLOT)) THEN
			X(3,I) = Y(I,3,SLOT)
		ELSE
			X(3,I) = 1.0D0
		END IF
		END DO
		NDATA = NPTSARRAY(SLOT)
		NORM = 1
		CALL PERIOD_LSQUAR (X, NDATA, NPOLY, PFT, CHISQ, XM, NORM)
		IF (CHISQ .EQ. -1.) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Singular matrix in
     + PERIOD_LSQUAR.'
			GOTO 99
		ELSE IF (CHISQ .EQ. -2.) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Overflow or divide check
     + occurred in PERIOD_LSQUAR.'
			GOTO 99
		ELSE IF (CHISQ .EQ. -3.) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Invalid parameters input
     + to PERIOD_LSQUAR.'
			GOTO 99
		END IF
		RMS = 0.0D0
		DO I = 1, NPTSARRAY(SLOT)
			Y(I,1,SLOTOUT) = Y(I,1,SLOT)
			Y(I,2,SLOTOUT) = Y(I,2,SLOT) - 
     +                  PERIOD_POLY(PFT, NPOLY, 
     +                  Y(I,1,SLOTOUT)-AINT(Y(1,1,SLOT)))
			Y(I,3,SLOTOUT) = Y(I,3,SLOT)
			RMS = RMS + (Y(I,2,SLOTOUT)**2.0D0)
		END DO		
		RMS = RMS / DBLE(NPTSARRAY(SLOT))
		RMS = DSQRT(RMS)
		WRITE(*,*)' '
		WRITE(*,*)'** OK: RMS of polynomial fit = ',RMS
		DETRENDARRAY(SLOTOUT) = .TRUE.
		YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
		INFILEARRAY(SLOTOUT) = 'detrended, '//INFILEARRAY(SLOT)
		NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	ELSE
		DO I = 1, NPTSARRAY(SLOT)
			DATA(I) = Y(I,2,SLOT)
		END DO
		N = NPTSARRAY(SLOT)
		CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
		WRITE(*,*)' '
		WRITE(*,*)'** OK: Y data mean = ',AVE
		WRITE(*,*)'** OK: Y data standard deviation = ',SDEV
		IF (SDEV .EQ. 0.) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Standard deviation is zero.'
			GOTO 99
		END IF
		DO I = 1, NPTSARRAY(SLOT)
			Y(I,1,SLOTOUT) = Y(I,1,SLOT)
			Y(I,2,SLOTOUT) = (Y(I,2,SLOT) - AVE) / SDEV
			IF (YERRORARRAY(SLOT)) THEN
			Y(I,3,SLOTOUT) = DSQRT( (Y(I,3,SLOT)**2.0D0) / 
     +				            (SDEV**2.0D0) )
			ELSE
			Y(I,3,SLOTOUT) = 0.0D0
			END IF
		END DO		
		DETRENDARRAY(SLOTOUT) = .TRUE.
		YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
		NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
		INFILEARRAY(SLOTOUT) = 'detrended, '//INFILEARRAY(SLOT)
	END IF
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
