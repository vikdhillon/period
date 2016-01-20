
	SUBROUTINE PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, 
     +			 	  MXSLOT, IFAIL)	

C=============================================================================
C Simple routine to select input and output slots.
C
C Written by Vikram Singh Dhillon @Sussex 29-March-1992.
C=============================================================================

	IMPLICIT NONE	

	INTEGER FIRSTSLOT, LASTSLOT, MXSLOT, IFAIL
	INTEGER FIRSTOUT, LASTOUT

C-----------------------------------------------------------------------------
C Select input and output data 
C-----------------------------------------------------------------------------

10	WRITE(*,'(A,$)')
     + 'Enter first and last slots for input (0,0 to quit) : '
	READ(*,*,ERR=10)FIRSTSLOT, LASTSLOT
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) THEN
		GOTO 99
	ELSE IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
		GOTO 99
	END IF
20	WRITE(*,'(A,$)')
     + 'Enter first and last slots for output (0,0 to quit) : '
	READ(*,*,ERR=20)FIRSTOUT, LASTOUT
	IF (FIRSTOUT .EQ. 0 .OR. LASTOUT .EQ. 0) THEN
		GOTO 99
	ELSE IF (FIRSTOUT .GT. MXSLOT .OR. LASTOUT .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
		GOTO 99
	ELSE IF ((LASTOUT-FIRSTOUT) .NE. (LASTSLOT-FIRSTSLOT)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Unequal number of input and
     + output slots.'
		GOTO 99
	ELSE
		IFAIL = 0
		RETURN
	END IF

99	IFAIL = 1

	RETURN
	END
