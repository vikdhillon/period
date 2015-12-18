        
	SUBROUTINE PERIOD_DEL (MXSLOT, NPTSARRAY)

C==============================================================================
C Delete slots. 
C
C Written by Vik Dhillon @WHT 24-Aug-2004.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXSLOT
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_DEL declarations.
C-----------------------------------------------------------------------------

	INTEGER FIRSTSLOT, LASTSLOT, SLOT

C-----------------------------------------------------------------------------
C Delete slots.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')
     +       'Enter first and last slots for deletion (0,0 to quit) : '
        READ(*,*,ERR=10)FIRSTSLOT, LASTSLOT
        IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
        IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
           GOTO 99
        END IF
        WRITE(*,*)' '
        DO 20 SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)'** OK: Slot empty =',SLOT
              GOTO 20
           ELSE
              NPTSARRAY(SLOT) = 0
              WRITE(*,*)'** OK: Deleted slot number = ',SLOT
           END IF
 20     CONTINUE
        
 99     RETURN
        END
