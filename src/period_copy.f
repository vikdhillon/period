        
	SUBROUTINE PERIOD_COPY (Y, MXROW, MXSLOT,
     +				NPTSARRAY, YERRORARRAY,
     +				INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Copy slots. 
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
C PERIOD_COPY declarations.
C-----------------------------------------------------------------------------

	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT), STRING
           
C-----------------------------------------------------------------------------
C Copy slots.
C-----------------------------------------------------------------------------

        CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, 
     +       MXSLOT,IFAIL)
        IF (IFAIL .EQ. 1) THEN
           GOTO 99
        END IF
        COUNTER = 0
        WRITE(*,*)' '
        DO 10 SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)'** ERROR: Slot empty =',SLOT
              GOTO 10
           END IF
           SLOTOUT = FIRSTOUT + COUNTER
           COUNTER = COUNTER + 1
           DO I = 1, NPTSARRAY(SLOT)
              Y(I,1,SLOTOUT) = Y(I,1,SLOT)
              Y(I,2,SLOTOUT) = Y(I,2,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                 YERRORARRAY(SLOTOUT) = .TRUE.                       
              ELSE
                 YERRORARRAY(SLOTOUT) = .FALSE. 
              END IF
           END DO
           DETRENDARRAY(SLOTOUT) = .FALSE.
           NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           INFILEARRAY(SLOTOUT) = INFILEARRAY(SLOT)
           WRITE(STRING,'(a,i3,a,i3)')
     +          'OK: Copied slot ',SLOT,' to slot ',SLOTOUT
           WRITE(*,*)STRING
 10     CONTINUE
        
 99     RETURN
        END
