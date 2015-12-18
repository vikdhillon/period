        
	SUBROUTINE PERIOD_APPEND (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY,
     +				  INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Append slots. 
C
C Written by Vik Dhillon @WHT 24-Aug-2004.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_APPEND declarations.
C-----------------------------------------------------------------------------

	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT
	INTEGER COUNTER, SLOTOUT
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT), STRING

C-----------------------------------------------------------------------------
C Append slots.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')
     +       'Enter first and last slots for input (0,0 to quit) : '
        READ(*,*,ERR=10)FIRSTSLOT, LASTSLOT
        IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) THEN
           GOTO 99
        ELSE IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. 
     +          MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 99
        END IF
 20     WRITE(*,'(A,$)')'Enter slot for output (0 to quit) : '
        READ(*,*,ERR=20)SLOTOUT
        IF (SLOTOUT .EQ. 0) THEN
           GOTO 99
        ELSE IF (SLOTOUT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 99
        END IF
        
        YERRORARRAY(SLOTOUT) = .TRUE.
        COUNTER = 0
        WRITE(*,*)' '
        DO 30 SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)'** ERROR: Slot empty =',SLOT
              GOTO 30
           END IF
           DO I = 1, NPTSARRAY(SLOT)
              COUNTER = COUNTER + 1
              IF (COUNTER .GT. MXROW) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)
     +             '** ERROR: Maximum number of rows reached = ',MXROW
                 GOTO 99
              END IF
              Y(COUNTER,1,SLOTOUT) = Y(I,1,SLOT)
              Y(COUNTER,2,SLOTOUT) = Y(I,2,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 Y(COUNTER,3,SLOTOUT) = Y(I,3,SLOT)
              ELSE
                 YERRORARRAY(SLOTOUT) = .FALSE.
              END IF
           END DO
           WRITE(STRING,'(a,i3,a,i3)')
     +          'OK: Appended slot ',SLOT,' to slot ',SLOTOUT
           WRITE(*,*)STRING
 30     CONTINUE
        NPTSARRAY(SLOTOUT) = COUNTER
        DETRENDARRAY(SLOTOUT) = .FALSE.
        WRITE(STRING,'(a,i3,a,i3,a)')
     +       'slots ',FIRSTSLOT,' to ',LASTSLOT,' appended'
        INFILEARRAY(SLOTOUT) = STRING
        
 99     RETURN
        END
