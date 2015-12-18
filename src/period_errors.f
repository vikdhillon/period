        
	SUBROUTINE PERIOD_ERRORS (Y, MXROW, MXSLOT,
     +			 	  NPTSARRAY, YERRORARRAY, 
     +				  INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Routine to calculate error bars from the standard deviation of the y-axis 
C data, the square root of the y-axis data, or from a user-entered value. If 
C error bars are already present, they are overwritten.
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
C PERIOD_ERRORS declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR
	DOUBLE PRECISION NSIG, SIGVAL
	DOUBLE PRECISION DSDEV
	INTEGER I, N, SLOT, FIRSTSLOT, LASTSLOT
	INTEGER IFAIL, FIRSTOUT, SLOTOUT, COUNTER
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*1 OPTION, REPLY
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
C Prompt for error bar calculation option.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')'Calculate sigma from [S]tandard deviation, '//
     +       'square [R]oot of data, or enter a [V]alue ? [S] : '
        READ(*,'(A)',ERR=10)OPTION
        IF (OPTION .EQ. ' ') OPTION = 'S'
        CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .NE. 'S' .AND. OPTION .NE. 'R' .AND.
     +       OPTION .NE. 'V') GOTO 10
        IF (OPTION .EQ. 'V') THEN
 11        WRITE(*,'(A,$)')'Enter sigma value : '
           READ(*,*,ERR=11)SIGVAL
        END IF
 12     WRITE(*,'(A,$)')'Enter number of sigma : '
        READ(*,*,ERR=12)NSIG
        
C-----------------------------------------------------------------------------
C Calculate error bars.
C-----------------------------------------------------------------------------

        COUNTER = 0
        DO SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Slot empty =',SLOT
              GOTO 99
           END IF
           IF (YERRORARRAY(SLOT)) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)
     + '** WARNING: Data already has errors. This routine will'//
     + ' overwrite them.'
 13           WRITE(*,'(A,$)')' ** WARNING: Are you sure you want to'//
     + ' continue ? [N] : '
              READ(*,'(A)',ERR=13)REPLY
              WRITE(*,*)' '
              CALL PERIOD_CASE (REPLY, .TRUE.)
              IF (REPLY .EQ. 'Y') THEN 
                 GOTO 14
              ELSE
                 GOTO 99
              END IF
           ELSE
              WRITE(*,*)' '
           END IF
 14        CONTINUE
           SLOTOUT = FIRSTOUT + COUNTER
           COUNTER = COUNTER + 1
           DO I = 1, NPTSARRAY(SLOT)
              DATA(I) = Y(I,2,SLOT)
           END DO
           N = NPTSARRAY(SLOT)
           CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
           IF (OPTION .EQ. 'S') THEN
              DSDEV = SDEV
           ELSE IF (OPTION .EQ. 'V') THEN
              DSDEV = SIGVAL
           END IF
           DO I = 1, NPTSARRAY(SLOT)
              Y(I,1,SLOTOUT) = Y(I,1,SLOT)
              Y(I,2,SLOTOUT) = Y(I,2,SLOT)
              IF (OPTION .EQ. 'R') THEN
                 IF (Y(I,2,SLOTOUT) .LT. 0.) THEN
                    WRITE(*,*)ACHAR(7)
                    WRITE(*,*)'** ERROR: Square root of'//
     +                   ' negative number in PERIOD_ERRORS.'
                    GOTO 99
                 END IF
                 Y(I,3,SLOTOUT) = NSIG * DSQRT(Y(I,2,SLOTOUT))
              ELSE
                 Y(I,3,SLOTOUT) = DSDEV * NSIG
              END IF
           END DO
           NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           YERRORARRAY(SLOTOUT) = .TRUE.
           DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
           INFILEARRAY(SLOTOUT) = 
     +          'error bars added, '//INFILEARRAY(SLOT)
           WRITE(*,*)
     +          '** OK: Filled slot = ',SLOTOUT
        END DO
        
C-----------------------------------------------------------------------------
C and exit
C-----------------------------------------------------------------------------

 99     RETURN
        END
