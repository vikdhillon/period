        
	SUBROUTINE PERIOD_UNBLUE (Y, MXROW, MXSLOT,
     +			          NPTSARRAY, YERRORARRAY, 
     +			          INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Routine to correct for NBLUE>1, which means that only 1 in NBLUE u-band
C frames has valid data. This is indicated by error code 12 in the
C log file, and by the fact that the flux and error are recorded as
C 0, -1 respectively.
C 
C Written by Vikram Singh Dhillon @NTT 20-April-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_UNBLUE declarations.
C-----------------------------------------------------------------------------

	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I, NBLUE, FINAL_NBLUE
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER, COUNTER2
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

        WRITE(*,*)' '
        CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, 
     +       IFAIL)
        IF (IFAIL .EQ. 1) THEN
           GOTO 99
        END IF

C-----------------------------------------------------------------------------
C Strip points from slot with (fluxes,errors) of (0,-1). This (usually)
C corresponds to when NBLUE>1 and hence when only one in every NBLUE
C frames is valid. 
C-----------------------------------------------------------------------------

        COUNTER = 0
        WRITE(*,*)' '
        DO SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Slot empty =',SLOT
              GOTO 99
           END IF
           SLOTOUT = FIRSTOUT + COUNTER
           COUNTER = COUNTER + 1
           COUNTER2 = 0
           NBLUE = 1
           FINAL_NBLUE = 0
           DO I = 1, NPTSARRAY(SLOT)
              IF (Y(I,2,SLOT) .NE. 0. .AND.
     +             Y(I,3,SLOT) .NE. -1.) THEN

C-----------------------------------------------------------------------------
C Now write the new data arrays.
C-----------------------------------------------------------------------------

                    COUNTER2 = COUNTER2 + 1
                    Y(COUNTER2,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(COUNTER2,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(COUNTER2,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                    NBLUE = 1
              ELSE
                 NBLUE = NBLUE + 1
                 IF (NBLUE .GT. FINAL_NBLUE) THEN
                    FINAL_NBLUE = NBLUE
                 END IF
              END IF
           END DO
           IF (FINAL_NBLUE .EQ. 0) THEN
              WRITE(*,*)'** ERROR: NBLUE = 1'
              GOTO 99
           END IF
           YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
           DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
           NPTSARRAY(SLOTOUT) = COUNTER2
           INFILEARRAY(SLOTOUT) = 'NBLUE-corrected, '//INFILEARRAY(SLOT)
           WRITE(*,*)'** OK: NBLUE = ',FINAL_NBLUE
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           WRITE(*,*)' '
        END DO
        
 99     RETURN
        END
