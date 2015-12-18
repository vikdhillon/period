        
	SUBROUTINE PERIOD_NOISE (Y, MXROW, MXSLOT,
     +			 	 NPTSARRAY, YERRORARRAY, 
     +				 INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Form a random dataset with the same mean and standard deviation as the 
C original, and the same x-axis sampling.
C
C Written by Vikram Singh Dhillon @IAC 31-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C SEED now set to the time in milliseconds, VSD@Nether Edge 18-April-2004.
C Added data/value option for setting sigma, VSD@Sheffield 14-Dec-2009. 
C Removed error bar options and added bootstrap, VSD@Durham 21-May-2010. 
C Removed bootstrap and jiggle options (!), VSD@Paris 7-June-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_NOISE declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR
        DOUBLE PRECISION DMEAN, EMEAN
	DOUBLE PRECISION DSDEV, ESDEV
	REAL RANDOM, IR(97), GSET
	INTEGER IDUM, ISET, SEED, IY
	INTEGER I, N, SLOT, FIRSTSLOT, LASTSLOT
	INTEGER IFAIL, FIRSTOUT, SLOTOUT, COUNTER
        INTEGER VALUES(8)
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        CHARACTER*12 DATE, TIME, ZONE
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output slots to process.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Initialise random number generator
C-----------------------------------------------------------------------------

        CALL DATE_AND_TIME (DATE, TIME, ZONE, VALUES)
        SEED = 1 + (VALUES(7) * VALUES(8))
	IDUM =  - ABS(SEED)
	ISET = 0
        CALL PERIOD_GASDEV (ISET, IDUM, RANDOM, IR, IY, GSET)

C-----------------------------------------------------------------------------
C Create a new dataset with the same mean and standard deviation as the old.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
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
              DATA(I) = Y(I,2,SLOT)
           END DO
           N = NPTSARRAY(SLOT)
           CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
           DMEAN = AVE
           DSDEV = SDEV
           IF (YERRORARRAY(SLOT)) THEN
              DO I = 1, NPTSARRAY(SLOT)
                 DATA(I) = Y(I,3,SLOT)
              END DO
              N = NPTSARRAY(SLOT)
              CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
              EMEAN = AVE
              ESDEV = SDEV
           END IF
           DO I = 1, NPTSARRAY(SLOT)
              Y(I,1,SLOTOUT) = Y(I,1,SLOT)
              IF (MOD(ISET,2) .NE. 0) THEN
                 ISET = 2
              ELSE 
                 ISET = 1
              END IF
              CALL PERIOD_GASDEV(ISET, IDUM, RANDOM, IR, IY, GSET)
              Y(I,2,SLOTOUT) = (DBLE(RANDOM) * DSDEV) + DMEAN
              IF (YERRORARRAY(SLOT)) THEN
                 IF (MOD(ISET,2) .NE. 0) THEN
                    ISET = 2
                 ELSE 
                    ISET = 1
                 END IF
                 CALL PERIOD_GASDEV(ISET, IDUM, RANDOM, IR, IY, GSET)
                 Y(I,3,SLOTOUT) = (DBLE(RANDOM) * ESDEV) + EMEAN
              END IF
           END DO
           NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
           DETRENDARRAY(SLOTOUT) = .FALSE.
           INFILEARRAY(SLOTOUT) = 'randomised, '//INFILEARRAY(SLOT)
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END DO

C-----------------------------------------------------------------------------
C and exit
C-----------------------------------------------------------------------------

 99     RETURN
        END
