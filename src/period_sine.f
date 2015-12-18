        
	SUBROUTINE PERIOD_SINE (Y, MXROW, MXSLOT,
     +			        NPTSARRAY, YERRORARRAY,
     +				INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Adds, subtracts, multiplies or divides sine curves from data.
C
C Written by Vikram Singh Dhillon @Sussex 10-February-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_SINE declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION PERIOD, AMPLITUDE, ZEROPT, GAMMA, PI
	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I
	INTEGER FIRSTOUT, IFAIL, COUNTER, SLOTOUT
	LOGICAL DETRENDARRAY(MXSLOT), YERRORARRAY(MXSLOT)
	CHARACTER*1 OPTION
	CHARACTER*72 INFILEARRAY(MXSLOT)
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for SINE option.
C-----------------------------------------------------------------------------
	
	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Enter period, semi-amplitude, zero point
     + and gamma : '	    
	READ(*,*,ERR=10)PERIOD, AMPLITUDE, ZEROPT, GAMMA
	IF (PERIOD .LE. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Invalid value for PERIOD.'
		GOTO 10
	END IF
20	WRITE(*,'(A,$)')'[A]dd, [S]ubtract, [M]ultiply
     + or [D]ivide sine curve ? [S] : '
	READ(*,'(A)',ERR=20)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)
	COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
	WRITE(*,*)' '
	SLOTOUT = FIRSTOUT + COUNTER
	COUNTER = COUNTER + 1

C-----------------------------------------------------------------------------
C Add, subtract, multiply or divide sine curve.
C-----------------------------------------------------------------------------

	DO I = 1, NPTSARRAY(SLOT)
		Y(I,1,SLOTOUT) = Y(I,1,SLOT) 
		Y(I,3,SLOTOUT) = Y(I,3,SLOT) 
		IF (OPTION .EQ. 'A') THEN
			Y(I,2,SLOTOUT) = Y(I,2,SLOT) + (GAMMA + 
     +                    (AMPLITUDE * DSIN(((2.0D0*PI)/PERIOD) * 
     +                    (Y(I,1,SLOT)-ZEROPT)))) 
		ELSE IF (OPTION .EQ. 'M') THEN
			Y(I,2,SLOTOUT) = Y(I,2,SLOT) * (GAMMA + 
     +                    (AMPLITUDE * DSIN(((2.0D0*PI)/PERIOD) * 
     +                    (Y(I,1,SLOT)-ZEROPT)))) 
		ELSE IF (OPTION .EQ. 'D') THEN
			Y(I,2,SLOTOUT) = Y(I,2,SLOT) / (GAMMA + 
     +                    (AMPLITUDE * DSIN(((2.0D0*PI)/PERIOD) * 
     +                    (Y(I,1,SLOT)-ZEROPT)))) 
		ELSE IF (OPTION .EQ. 'S' .OR. OPTION .EQ. ' ') THEN
			Y(I,2,SLOTOUT) = Y(I,2,SLOT) - (GAMMA + 
     +                    (AMPLITUDE * DSIN(((2.0D0*PI)/PERIOD) * 
     +                    (Y(I,1,SLOT)-ZEROPT)))) 
		END IF
	END DO
	DETRENDARRAY(SLOTOUT) = .FALSE.
	YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	IF (OPTION .EQ. 'A') THEN
	INFILEARRAY(SLOTOUT) = 'sine+, '//INFILEARRAY(SLOT)
	ELSE IF (OPTION .EQ. 'M') THEN
	INFILEARRAY(SLOTOUT) = 'sine*, '//INFILEARRAY(SLOT)
	ELSE IF (OPTION .EQ. 'D') THEN
	INFILEARRAY(SLOTOUT) = 'sine/, '//INFILEARRAY(SLOT)
	ELSE IF (OPTION .EQ. 'S' .OR. OPTION .EQ. ' ') THEN
	INFILEARRAY(SLOTOUT) = 'sine-, '//INFILEARRAY(SLOT)
	END IF
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	END DO

99	RETURN
	END
