        
	SUBROUTINE PERIOD_PHASE (Y, MXROW, MXSLOT,
     +			         NPTSARRAY, YERRORARRAY, 
     +			         INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Folds and bins data on a given PERIOD with a zero point defined by ZEROPT. 
C
C Written by Vikram Dhillon @Sussex 20-March-1992.
C Added option to store more than one cycle, Vik Dhillon @ING 21-May-2003.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Fixed error calc mistake spotted by Erik Kuulkers, VSD@Madrid, 07-May-2005
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_PHASE declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA
	DOUBLE PRECISION PERIOD, ZEROPT
        DOUBLE PRECISION XDATA(MXROW), YDATA(MXROW), YERR(MXROW)
	DOUBLE PRECISION BINWID, CENBIN, MINBIN, MAXBIN
	DOUBLE PRECISION XBIN(MXROW), YBIN(MXROW), EBIN(MXROW)
	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I, FIRSTOUT, IFAIL, SLOTOUT
	INTEGER NBIN, J, NCYCLES
	INTEGER COUNTER, COUNTER2, BINCOUNT
	LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT)
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
C Prompt for PERIOD and ZEROPT parameters.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Enter period to fold data on : '
	READ(*,*,ERR=10)PERIOD
	IF (PERIOD .LE. 0.) GOTO 10
20	WRITE(*,'(A,$)')'Enter zero point (0 for first
     + data point) : '
	READ(*,*,ERR=20)ZEROPT
30	WRITE(*,'(A,$)')'Enter number of phase bins (0 for simple
     + folding) : '
	READ(*,*,ERR=30)NBIN
40	WRITE(*,'(A,$)')'Enter number of cycles : '
	READ(*,*,ERR=40)NCYCLES
	IF (NCYCLES .LE. 0) GOTO 40
	COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
        IF (NBIN .EQ. 0) THEN
           IF (NPTSARRAY(SLOT)*NCYCLES .GT. MXROW) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Number of cycles too large.'
		GOTO 99
           END IF
        ELSE
           IF (NBIN*NCYCLES .GT. MXROW) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Number of cycles too large.'
		GOTO 99
           END IF           
        END IF
	WRITE(*,*)' '
	SLOTOUT = FIRSTOUT + COUNTER
	COUNTER = COUNTER + 1
	IF (ZEROPT .EQ. 0.) THEN           
		ZEROPT = Y(1,1,SLOT)
                WRITE(*,*)'** OK: Zero point = ',ZEROPT
	END IF

C-----------------------------------------------------------------------------
C Fold and sort data.
C-----------------------------------------------------------------------------
        
	NDATA = NPTSARRAY(SLOT)
	DO I = 1, NDATA
		XDATA(I) = Y(I,1,SLOT)
		YDATA(I) = Y(I,2,SLOT)
		YERR(I) = Y(I,3,SLOT)
	END DO
	CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, ZEROPT, PERIOD)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Bin data in phase bins (if NBIN = 0, leave data unchanged). 
C-----------------------------------------------------------------------------

	IF (NBIN .GT. 1) THEN
	BINWID = 1.0D0 / DBLE(NBIN)
	DO I = 1, NBIN
		CENBIN = (DBLE(I)-1.0D0) * BINWID
		MINBIN = CENBIN - (BINWID / 2.0D0)
		MAXBIN = CENBIN + (BINWID / 2.0D0)
		XBIN(I) = CENBIN
		YBIN(I) = 0.0D0
		EBIN(I) = 0.0D0
		BINCOUNT = 0
		DO J = 1, NDATA
			IF (I .EQ. 1) THEN
			IF (XDATA(J) .LT. MAXBIN) THEN
				YBIN(I) = YBIN(I) + YDATA(J)
				EBIN(I) = EBIN(I) + (YERR(J)**2.0D0)
				BINCOUNT = BINCOUNT + 1
			END IF
			IF (XDATA(J) .GE. MINBIN+1.) THEN
				YBIN(I) = YBIN(I) + YDATA(J)
				EBIN(I) = EBIN(I) + (YERR(J)**2.0D0)
				BINCOUNT = BINCOUNT + 1
			END IF
			ELSE
			IF (XDATA(J) .GE. MINBIN) THEN
			IF (XDATA(J) .LT. MAXBIN) THEN
				YBIN(I) = YBIN(I) + YDATA(J)
				EBIN(I) = EBIN(I) + (YERR(J)**2.0D0)
				BINCOUNT = BINCOUNT + 1
			END IF
			END IF	
			END IF
		END DO
		IF (BINCOUNT .EQ. 0) THEN
			YBIN(I) = 0.0D0
			EBIN(I) = 0.0D0
		ELSE
		YBIN(I) = YBIN(I) / DBLE(BINCOUNT)
		EBIN(I) = DSQRT(EBIN(I)) / DBLE(BINCOUNT)
		END IF
	END DO
	DO I = 1, NBIN
		Y(I,1,SLOTOUT) = XBIN(I)
		Y(I,2,SLOTOUT) = YBIN(I)
		Y(I,3,SLOTOUT) = EBIN(I)
	END DO
	YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 'folded, binned, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NBIN
	ELSE 
	DO I = 1, NDATA
		Y(I,1,SLOTOUT) = XDATA(I)
		Y(I,2,SLOTOUT) = YDATA(I)
		Y(I,3,SLOTOUT) = YERR(I)
	END DO
	YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 'folded, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NDATA
	END IF

C-----------------------------------------------------------------------------
C Store more than one cycle if requested.
C-----------------------------------------------------------------------------

	IF (NCYCLES .GT. 1) THEN
        DO J = 1, NCYCLES-1
           COUNTER2 = 0
           DO I = (NPTSARRAY(SLOTOUT)*J)+1, NPTSARRAY(SLOTOUT)*(J+1)
              COUNTER2 = COUNTER2 + 1
              IF (NBIN .GT. 1) THEN
                 Y(I,1,SLOTOUT) = XBIN(COUNTER2)+DBLE(J)
                 Y(I,2,SLOTOUT) = YBIN(COUNTER2)
                 Y(I,3,SLOTOUT) = EBIN(COUNTER2)
              ELSE 
                 Y(I,1,SLOTOUT) = XDATA(COUNTER2)+DBLE(J)
                 Y(I,2,SLOTOUT) = YDATA(COUNTER2)
                 Y(I,3,SLOTOUT) = YERR(COUNTER2)
              END IF
           END DO
        END DO
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOTOUT)*NCYCLES
        END IF
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END DO

99	RETURN

	END
