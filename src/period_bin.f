        
	SUBROUTINE PERIOD_BIN (Y, MXROW, MXSLOT,
     +			       NPTSARRAY, YERRORARRAY, 
     +			       INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Bins data in the x-axis (i.e. time, in most cases) in order to reduce the 
C number of data points and/or increase the signal-to-noise of the resulting 
C points. Note that only a straight-forward average of the x- and y-values 
C is performed (i.e. no account is taken of the errors on the points during 
C the averaging, although any error-bars, if present, are correctly 
C propagated. Note that, unless the total number of points is an integer 
C multiple of the number of points per bin, the last bin will contain fewer 
C points than specified - a warning to this effect is issued to the user.
C
C Written by Vikram Singh Dhillon @Exeter 14-September-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_BIN declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION XSUM, YSUM, ESUM
	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER
        INTEGER NPOINTS, NPOINTS_IN_BIN, NBIN
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for number of points per bin.
C-----------------------------------------------------------------------------

        WRITE(*,*)' '
10      WRITE(*,'(A,$)')'Enter number of points per bin : '
        READ(*,*,ERR=10)NPOINTS
        IF (NPOINTS .LT. 1) GOTO 10

C-----------------------------------------------------------------------------
C Bin data in x.
C-----------------------------------------------------------------------------

        COUNTER = 0
        DO SLOT = FIRSTSLOT, LASTSLOT
        IF (NPTSARRAY(SLOT) .EQ. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Slot empty =',SLOT
           GOTO 99
        END IF
        IF (NPOINTS .GT. NPTSARRAY(SLOT)) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Number of points per bin too large.'
           GOTO 99
        END IF
        SLOTOUT = FIRSTOUT + COUNTER
        COUNTER = COUNTER + 1
        NPOINTS_IN_BIN = 0
        NBIN = 0
        XSUM = 0.0D0
        YSUM = 0.0D0
        ESUM = 0.0D0
        DO I = 1, NPTSARRAY(SLOT)
              XSUM = XSUM + Y(I,1,SLOT)
              YSUM = YSUM + Y(I,2,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 ESUM = ESUM + (Y(I,3,SLOT)**2.0D0)
              END IF
              NPOINTS_IN_BIN = NPOINTS_IN_BIN + 1
              IF (NPOINTS_IN_BIN .EQ. NPOINTS) THEN
                 NBIN = NBIN + 1
                 Y(NBIN,1,SLOTOUT) = XSUM / DBLE(NPOINTS)
                 Y(NBIN,2,SLOTOUT) = YSUM / DBLE(NPOINTS)
                 IF (YERRORARRAY(SLOT)) THEN
                    Y(NBIN,3,SLOTOUT) = DSQRT(ESUM) / DBLE(NPOINTS)
                 END IF
                 NPOINTS_IN_BIN = 0
                 XSUM = 0.0D0
                 YSUM = 0.0D0
                 ESUM = 0.0D0
              END IF
        END DO
        WRITE(*,*)' '
        IF (NPOINTS_IN_BIN .NE. 0) THEN
           NBIN = NBIN + 1
           Y(NBIN,1,SLOTOUT) = XSUM / DBLE(NPOINTS_IN_BIN)
           Y(NBIN,2,SLOTOUT) = YSUM / DBLE(NPOINTS_IN_BIN)
           IF (YERRORARRAY(SLOT)) THEN
              Y(NBIN,3,SLOTOUT) = DSQRT(ESUM) / DBLE(NPOINTS_IN_BIN)
           END IF
           WRITE(*,*)'** WARNING: Number of points in last bin = ',
     + NPOINTS_IN_BIN
        END IF
        YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
        DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
        NPTSARRAY(SLOTOUT) = NBIN
        INFILEARRAY(SLOTOUT) = 'binned, '//INFILEARRAY(SLOT)
        WRITE(*,*)'** OK: Number of bins = ',NBIN
        WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END DO

99	RETURN
	END
