        
	SUBROUTINE PERIOD_BOOT (Y, MXROW, MXSLOT,
     +				NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Routine to bootstrap data. N samples are randomly taken from a dataset 
C containing N points. The same point can be selected more than once, and
C some points might not be selected at all. If the same point is selected
C more than once, two options are given: 
C
C 1. To divide the error bar, if present, by the square root of the number
C    of the number of times the point has been selected. If a point is not
C    selected at all, it is removed from the dataset. 
C 2. To have multiple points with the same x and y values. Any error bars
C    are left unchanged. 
C 
C Since most of the period-finding routines do not use error bars, it is 
C likely that option 2 will be of most use. 
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
C PERIOD_BOOT declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION XWORK(MXROW), YWORK(MXROW), EWORK(MXROW)
	REAL RANDOM, IR(97)
        INTEGER KEY(MXROW)
	INTEGER SEED, IY, EL
	INTEGER I, N, SLOT, FIRSTSLOT, LASTSLOT
	INTEGER IFAIL, FIRSTOUT, SLOTOUT, COUNTER
        INTEGER VALUES(8)
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*1 OPTION
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
        SEED = -ABS(1 + (VALUES(7) * VALUES(8)))
        CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)

C-----------------------------------------------------------------------------
C Prompt for bootstrap option.
C-----------------------------------------------------------------------------
	
	WRITE(*,*)' '
10	WRITE(*,'(A,$)')
     + '[R]epeat points or manipulate [E]rror bars ? [R] : '
	READ(*,'(A)',ERR=10)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .EQ. ' ') OPTION = 'R'
        IF (OPTION .NE. 'E' .AND. OPTION .NE. 'R') GOTO 10
        WRITE(*,*)' '

C-----------------------------------------------------------------------------
C First option - repeat points.
C-----------------------------------------------------------------------------

        IF (OPTION .EQ. 'R') THEN
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
                 CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
                 EL = 1+NINT(DBLE(NPTSARRAY(SLOT)-1)*RANDOM)
                 Y(I,1,SLOTOUT) = Y(EL,1,SLOT)
                 Y(I,2,SLOTOUT) = Y(EL,2,SLOT)
                 IF (YERRORARRAY(SLOT)) THEN
                    Y(I,3,SLOTOUT) = Y(EL,3,SLOT)
                 END IF
              END DO
              DO I = 1, NPTSARRAY(SLOT)
                 XWORK(I) = Y(I,1,SLOTOUT)
                 YWORK(I) = Y(I,2,SLOTOUT)
                 IF (YERRORARRAY(SLOT)) THEN
                    EWORK(I) = Y(I,3,SLOTOUT)
                 END IF
              END DO
              N = NPTSARRAY(SLOT)
              CALL PERIOD_SHELLSORT (N, XWORK, KEY)
              DO I = 1, NPTSARRAY(SLOT)
                 Y(I,1,SLOTOUT) = XWORK(KEY(I))
                 Y(I,2,SLOTOUT) = YWORK(KEY(I))
                 IF (YERRORARRAY(SLOT)) THEN
                    Y(I,3,SLOTOUT) = EWORK(KEY(I))
                 END IF
              END DO
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
              YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
              DETRENDARRAY(SLOTOUT) = .FALSE.
              INFILEARRAY(SLOTOUT) = 'bootstrapped, '//INFILEARRAY(SLOT)
              WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           END DO

C-----------------------------------------------------------------------------
C Second option - manipulate error bars.
C-----------------------------------------------------------------------------

        ELSE IF (OPTION .EQ. 'E') THEN
           COUNTER = 0
           DO SLOT = FIRSTSLOT, LASTSLOT
              IF (NPTSARRAY(SLOT) .EQ. 0) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)'** ERROR: Slot empty =',SLOT
                 GOTO 99
              END IF
              IF (.NOT. YERRORARRAY(SLOT)) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)
     +           '** ERROR: No error bars present in slot =',SLOT
                 GOTO 99
              END IF
              SLOTOUT = FIRSTOUT + COUNTER
              COUNTER = COUNTER + 1
              DO I = 1, NPTSARRAY(SLOT)
                 EWORK(I) = 0
              END DO
              DO I = 1, NPTSARRAY(SLOT)
                 CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
                 EL = 1+NINT(DBLE(NPTSARRAY(SLOT)-1)*RANDOM)
                 EWORK(EL) = EWORK(EL) + 1
              END DO
              DO I = 1, NPTSARRAY(SLOT)
                 Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                 Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                 IF (EWORK(I) .EQ. 0) THEN
                    Y(I,3,SLOTOUT) = 1.D32
                 ELSE
                    Y(I,3,SLOTOUT) = Y(I,3,SLOT) / SQRT(EWORK(I))
                 END IF
              END DO
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
              YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
              DETRENDARRAY(SLOTOUT) = .FALSE.
              INFILEARRAY(SLOTOUT) = 'bootstrapped, '//INFILEARRAY(SLOT)
              WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           END DO
        END IF
           
C-----------------------------------------------------------------------------
C and exit
C-----------------------------------------------------------------------------

 99     RETURN
        END
