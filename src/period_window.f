        
	SUBROUTINE PERIOD_WINDOW (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY,
     +				  INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Sets all data points to unity so that the window function can be generated.
C
C Written by Vikram Singh Dhillon @IAC 31-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_WINDOW declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION WINDOW, EWINDOW
	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT)
	DATA WINDOW, EWINDOW /1.0D0, 0.0D0/

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
		Y(I,2,SLOTOUT) = WINDOW
		Y(I,3,SLOTOUT) = EWINDOW
	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	INFILEARRAY(SLOTOUT) = 'windowed, '//INFILEARRAY(SLOT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
