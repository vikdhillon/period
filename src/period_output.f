        
	SUBROUTINE PERIOD_OUTPUT (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY)

C===========================================================================
C Output data to a disk file.
C
C Written by Vikram Singh Dhillon @LPO 27-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Removed format in write line, which prevented exponentials
C    being properly written, Vik Dhillon @Sheffield 7-March-2015.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_OUTPUT declarations.
C-----------------------------------------------------------------------------

	INTEGER FIRSTSLOT, LASTSLOT, SLOT, IUNIT, I
	LOGICAL YERRORARRAY(MXSLOT)
	CHARACTER*1 OUTFILE*32
	DATA IUNIT /12/

C-----------------------------------------------------------------------------
C Select slots to output.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
10      WRITE(*,'(A,$)')
     + 'Enter first and last slots for output (0,0 to quit) : '
	READ(*,*,ERR=10)FIRSTSLOT, LASTSLOT
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
	IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
		GOTO 99
	END IF
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
        IF (SLOT .NE. FIRSTSLOT) WRITE(*,*)' '
20	WRITE(*,'(A,$)')'Enter output filename (q to quit) : '
	READ(*,'(A)',ERR=20)OUTFILE
	IF (OUTFILE .EQ. 'Q' .OR. OUTFILE .EQ. 'q') GOTO 99
	OPEN (UNIT=IUNIT, FILE=OUTFILE, STATUS='NEW', ERR=20)
	DO I = 1, NPTSARRAY(SLOT)
		IF (YERRORARRAY(SLOT)) THEN
c		WRITE(IUNIT,'(F22.11,2X,F22.11,2X,F22.11)')
c     + Y(I,1,SLOT), Y(I,2,SLOT), Y(I,3,SLOT)
		WRITE(IUNIT,*)Y(I,1,SLOT),Y(I,2,SLOT),Y(I,3,SLOT)
		ELSE
c		WRITE(IUNIT,'(F22.11,2X,F22.11)')
c     + Y(I,1,SLOT), Y(I,2,SLOT)
		WRITE(IUNIT,*)Y(I,1,SLOT),Y(I,2,SLOT)
		END IF
	END DO
	CLOSE (UNIT=IUNIT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Saved slot = ',SLOT
	END DO

99	RETURN
	END
