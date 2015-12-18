        
	SUBROUTINE PERIOD_JIGGLE (Y, MXROW, MXSLOT,
     +			 	  NPTSARRAY, YERRORARRAY, 
     +				  INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Jiggles data by adding Gaussian noise to x- and y-axis data. The former
C effectively adds irregularities to the x-data sampling intervals. Note
C that the latter increases the total noise in the dataset.
C
C Written by Vikram Singh Dhillon @Paris 7-June-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_JIGGLE declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR
	DOUBLE PRECISION NSIG, SIGVAL
	DOUBLE PRECISION DSDEV
	REAL RANDOM, IR(97), GSET
	INTEGER IDUM, ISET, SEED, IY
	INTEGER I, N, SLOT, FIRSTSLOT, LASTSLOT
	INTEGER IFAIL, FIRSTOUT, SLOTOUT, COUNTER
        INTEGER VALUES(8)
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*1 NOISE, OPTION
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
C Create a jiggled dataset from the original.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')
     +       'Add noise to the X or Y data ? [Y] : '
        READ(*,'(A)',ERR=10)NOISE
        IF (NOISE .EQ. ' ') NOISE = 'Y'
        CALL PERIOD_CASE (NOISE, .TRUE.)
        IF (NOISE .NE. 'Y' .AND. NOISE .NE. 'X') GOTO 10
 11     WRITE(*,'(A,$)')
     +  'Calculate sigma from [D]ata or enter a sigma [V]alue ? [D] : '
        READ(*,'(A)',ERR=11)OPTION
        IF (OPTION .EQ. ' ') OPTION = 'D'
        CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .NE. 'D' .AND. OPTION .NE. 'V') GOTO 11
        IF (OPTION .EQ. 'V') THEN
 12        WRITE(*,'(A,$)')'Enter sigma value : '
           READ(*,*,ERR=12)SIGVAL
        END IF
 13     WRITE(*,'(A,$)')'Enter number of sigma : '
        READ(*,*,ERR=13)NSIG
        
C-----------------------------------------------------------------------------
C First jiggle option - add noise to the Y data
C-----------------------------------------------------------------------------

        IF (NOISE .EQ. 'Y') THEN
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
              IF (OPTION .EQ. 'D') THEN
                 DSDEV = SDEV
              ELSE
                 DSDEV = SIGVAL
              END IF
              DO I = 1, NPTSARRAY(SLOT)
                 Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                 Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                 Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                 IF (MOD(ISET,2) .NE. 0) THEN
                    ISET = 2
                 ELSE 
                    ISET = 1
                 END IF
                 CALL PERIOD_GASDEV(ISET, IDUM, RANDOM, 
     +                IR, IY, GSET)
                 Y(I,2,SLOTOUT) = Y(I,2,SLOTOUT) + 
     +                ( DSDEV * NSIG * DBLE(RANDOM) )
              END DO
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
              YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
              DETRENDARRAY(SLOTOUT) = .FALSE.
              INFILEARRAY(SLOTOUT) = 'noisy, '//INFILEARRAY(SLOT)
              WRITE(*,*)
     +             '** OK: Filled slot = ',SLOTOUT
           END DO   
        
C-----------------------------------------------------------------------------
C Second jiggle option - add noise to the X data, i.e. irregularities to the 
C X data sampling
C-----------------------------------------------------------------------------

        ELSE IF (NOISE .EQ. 'X') THEN
           COUNTER = 0
           DO SLOT = FIRSTSLOT, LASTSLOT
              IF (NPTSARRAY(SLOT) .EQ. 0) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)'** ERROR: Slot empty =',SLOT
                 GOTO 99
              END IF
              SLOTOUT = FIRSTOUT + COUNTER
              COUNTER = COUNTER + 1
              DO I = 1, NPTSARRAY(SLOT) - 1
                 DATA(I) = DSQRT((Y(I+1,1,SLOT)-Y(I,1,SLOT))**2.0D0)
              END DO
              N = NPTSARRAY(SLOT) - 1
              CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
              IF (OPTION .EQ. 'D') THEN
                 DSDEV = SDEV
              ELSE
                 DSDEV = SIGVAL
              END IF
              DO I = 1, NPTSARRAY(SLOT)
                 Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                 Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                 Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                 IF (MOD(ISET,2) .NE. 0) THEN
                    ISET = 2
                 ELSE 
                    ISET = 1
                 END IF
                 CALL PERIOD_GASDEV(ISET, IDUM, RANDOM, 
     +                IR, IY, GSET)
                 Y(I,1,SLOTOUT) = Y(I,1,SLOTOUT) + 
     +                ( DSDEV * NSIG * DBLE(RANDOM) ) 
              END DO
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
              YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
              DETRENDARRAY(SLOTOUT) = .FALSE.
              INFILEARRAY(SLOTOUT) = 'noisy, '//INFILEARRAY(SLOT)
              WRITE(*,*)
     +             '** OK: Filled slot = ',SLOTOUT
           END DO       
        END IF
        
C-----------------------------------------------------------------------------
C and exit
C-----------------------------------------------------------------------------

 99   RETURN
      END
