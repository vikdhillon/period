        
	SUBROUTINE PERIOD_SMOOTH (Y, MXROW, MXSLOT,
     +				  NPTSARRAY, YERRORARRAY, 
     +				  INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Smooths data. Only median filtering is currently implemented.
C
C Written by Vikram Singh Dhillon @Sheffield 7-October-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_SMOOTH declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION YDATA(MXROW), EDATA(MXROW), WORK(MXROW)
	DOUBLE PRECISION YFILTERED(MXROW), EFILTERED(MXROW)
        INTEGER IWORK(MXROW)
	INTEGER NDATA, SLOT, FIRSTSLOT, LASTSLOT, I, WIDTH
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER
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

C-----------------------------------------------------------------------------
C Prompt for width of median filter (must be odd).
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
 10     WRITE(*,'(A,$)')'Enter width of median filter (odd) : '
	READ(*,*,ERR=10)WIDTH
	IF (WIDTH .LT. 1) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Minimum filter width = 1'
           WRITE(*,*)' '
           GOTO 10
	ELSE IF (MOD(WIDTH,2) .EQ. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Filter width must be odd.'
           WRITE(*,*)' '
           GOTO 10
	ELSE IF (WIDTH .GT. MXROW) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum filter width = ',MXROW
           WRITE(*,*)' '
           GOTO 10
	END IF
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
           DO I = 1, NPTSARRAY(SLOT)
              YDATA(I) = Y(I,2,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 EDATA(I) = Y(I,3,SLOT)
              END IF
           END DO
           NDATA = NPTSARRAY(SLOT)
           CALL PERIOD_MEDFILT (YDATA, YFILTERED, NDATA, WIDTH, IWORK, 
     +          WORK, IFAIL)
           IF (IFAIL .EQ. 1) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: In PERIOD_MEDFILT.'
              GOTO 99
           END IF
           IF (YERRORARRAY(SLOT)) THEN
              CALL PERIOD_MEDFILT (EDATA, EFILTERED, NDATA, WIDTH, 
     +             IWORK, WORK, IFAIL)
              IF (IFAIL .EQ. 1) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)'** ERROR: In PERIOD_MEDFILT.'
                 GOTO 99
              END IF
           END IF
        DO I = 1, NPTSARRAY(SLOT)
           Y(I,1,SLOTOUT) = Y(I,1,SLOT)
           Y(I,2,SLOTOUT) = YFILTERED(I)            
           IF (YERRORARRAY(SLOT)) THEN
              Y(I,3,SLOTOUT) = EFILTERED(I) 
           END IF
        END DO		
        DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
        YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
        INFILEARRAY(SLOTOUT) = 'filtered, '//INFILEARRAY(SLOT)
        NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
        WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
      END DO
      
 99   RETURN
      END
