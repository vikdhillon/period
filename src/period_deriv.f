        
	SUBROUTINE PERIOD_DERIV (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY,
     +				  INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Routine to calculate the derivative of a dataset. The derivative is
C centred on each x-axis point, i.e. the derivative of a point I is
C given by (Y[I+1]-Y[I-1])/(X[I+1]-X[I-1]). For the start and end points
C of the dataset, the derivative is one-sided, i.e. for the start:
C (Y(I+1)-Y(I))/(X(I+1)-X(I)).
C
C Written by Vikram Singh Dhillon @Santiago 19-June-2007.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_DERIV declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION DERIVATIVE, ERR_DERIVATIVE
        DOUBLE PRECISION DELTA_X, DELTA_Y, ERR_DELTA_Y
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
           IF (I .EQ. 1) THEN
              DELTA_Y = Y(I+1,2,SLOT) - Y(I,2,SLOT)
              DELTA_X = Y(I+1,1,SLOT) - Y(I,1,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 ERR_DELTA_Y = 
     +           DSQRT( (Y(I+1,3,SLOT)**2.0D0) + (Y(I,3,SLOT)**2.0D0) )
              END IF
           ELSE IF (I .EQ. NPTSARRAY(SLOT)) THEN
              DELTA_Y = Y(I,2,SLOT) - Y(I-1,2,SLOT)
              DELTA_X = Y(I,1,SLOT) - Y(I-1,1,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 ERR_DELTA_Y = 
     +           DSQRT( (Y(I,3,SLOT)**2.0D0) + (Y(I-1,3,SLOT)**2.0D0) )
              END IF
           ELSE
              DELTA_Y = Y(I+1,2,SLOT) - Y(I-1,2,SLOT)
              DELTA_X = Y(I+1,1,SLOT) - Y(I-1,1,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 ERR_DELTA_Y = 
     +           DSQRT( (Y(I+1,3,SLOT)**2.0D0) + (Y(I-1,3,SLOT)**2.0D0))
              END IF
           END IF
           Y(I,1,SLOTOUT) = Y(I,1,SLOT)
           DERIVATIVE = DELTA_Y / DELTA_X
           Y(I,2,SLOTOUT) = DERIVATIVE
           IF (YERRORARRAY(SLOT)) THEN
              ERR_DERIVATIVE = ERR_DELTA_Y / DELTA_X
              ERR_DERIVATIVE = DSQRT(ERR_DERIVATIVE**2.0D0)
              Y(I,3,SLOTOUT) = ERR_DERIVATIVE
           END IF
	END DO
        YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
	DETRENDARRAY(SLOTOUT) = .FALSE.
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	INFILEARRAY(SLOTOUT) = 'derivative, '//INFILEARRAY(SLOT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
