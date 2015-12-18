        
	SUBROUTINE PERIOD_INTEG (Y, MXROW, MXSLOT,
     +				 NPTSARRAY, YERRORARRAY,
     +				 INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Routine to integrate data, i.e. to run through a light curve and add up all 
C of the flux values and output the running total as a function of time.
C
C Written by Vikram Singh Dhillon @TNT 28-Mar-2014.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_INTEG declarations.
C-----------------------------------------------------------------------------

	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
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
	COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
	SLOTOUT = FIRSTOUT + COUNTER
	COUNTER = COUNTER + 1
	DO I = 1, NPTSARRAY(SLOT)
		Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                IF (I .EQ. 1) THEN
                   Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                   Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                ELSE
                   Y(I,2,SLOTOUT) = Y(I-1,2,SLOTOUT)+Y(I,2,SLOT)
                   Y(I,3,SLOTOUT) = DSQRT( (Y(I-1,3,SLOT)**2.0D0) + 
     +                  (Y(I,3,SLOT)**2.0D0) )
                END IF

	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	INFILEARRAY(SLOTOUT) = 'accumulated, '//INFILEARRAY(SLOT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
