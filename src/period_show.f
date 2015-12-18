
	SUBROUTINE PERIOD_SHOW (Y, MXROW, MXSLOT, 
     +                          NPTSARRAY, YERRORARRAY, 
     +			        INFILEARRAY, COMMAND, DETRENDARRAY, 
     +			        LOGFILE, LOG, LOGUNIT)

C=============================================================================
C Routine to return information about the data currently stored by PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 4-June-1991.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_SHOW declarations.
C-----------------------------------------------------------------------------

	INTEGER FIRSTSLOT, LASTSLOT, I, N, SLOT, LOGUNIT
	INTEGER COUNTER, J, IENTRY
	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR, MEDIAN
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT), LOG
	LOGICAL LSHOW
	CHARACTER*72 INFILEARRAY(MXSLOT), LOGFILE, STRING(50), DUMMY
 	CHARACTER*1 OPTION
        CHARACTER*12 COMMAND
	DATA LSHOW /.TRUE./

C-----------------------------------------------------------------------------
C Determine output required from command line, if present.
C-----------------------------------------------------------------------------

	IF (COMMAND(3:3) .EQ. '\' .OR. COMMAND(4:4) 
     +     .EQ. '\' .OR. COMMAND(5:5) .EQ. '\' 
     +     .OR. COMMAND(6:6) .EQ. '\') THEN
	   FIRSTSLOT = 1
	   LASTSLOT = MXSLOT
	   LSHOW = .FALSE.
	ELSE IF (COMMAND(3:4) .EQ. '  ' .OR. COMMAND(4:5) 
     +     .EQ. '  ' .OR. COMMAND(5:6) .EQ. '  ') THEN
	   LSHOW = .TRUE.
	ELSE
	   READ(COMMAND(4:12),*,ERR=99)FIRSTSLOT, LASTSLOT
	   LSHOW = .FALSE.
	END IF

C-----------------------------------------------------------------------------
C If full output required, prompt for display mode.
C-----------------------------------------------------------------------------

	IF (LSHOW) THEN
	WRITE(*,*)' '
5	WRITE(*,'(A,$)')'Display information on
     + [L]og file or [D]ata ? [D] : '
	READ(*,'(A)',ERR=5)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)

C-----------------------------------------------------------------------------
C Return information on log file.
C-----------------------------------------------------------------------------

	IF (OPTION .EQ. 'L') THEN
		IF (.NOT. LOG) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: No log file has been opened.'
			GOTO 99
		END IF
		REWIND (LOGUNIT)
		COUNTER = 1
		IENTRY = 0
		DO I = 1, 1000000000
			READ(LOGUNIT,'(A)',END=23)DUMMY
			IF (DUMMY(1:1) .NE. '.') THEN
			STRING(COUNTER) = DUMMY
			COUNTER = COUNTER + 1
			ELSE
			IENTRY = IENTRY + 1
			WRITE(*,*)' '
			WRITE(*,*)'LOG FILE  = ',LOGFILE(1:60)
			WRITE(*,*)'LOG ENTRY =',IENTRY
			DO J = 1, COUNTER - 1
				WRITE(*,*)STRING(J)
			END DO
			COUNTER = 1
			GOTO 20
			END IF
20			CONTINUE
		END DO
23		IF (I .EQ. 1) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** ERROR: Log file is empty.'
		END IF
		GOTO 99
		
C-----------------------------------------------------------------------------
C Return information on stored data.
C-----------------------------------------------------------------------------

	ELSE IF (OPTION .EQ. 'D' .OR. OPTION .EQ. ' ') THEN
	WRITE(*,*)' '
25	WRITE(*,'(A,$)')'Enter first and last slots for input
     + (0,0 to quit) : '
	READ(*,*,ERR=25)FIRSTSLOT, LASTSLOT
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
	IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
		GOTO 99
	END IF
	DO 10 SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)' '
		WRITE(*,*)'** OK: No data present in slot number',SLOT
		GOTO 10
	ELSE
	        WRITE(*,*)' '
		WRITE(*,*)'** OK: Slot number = ',SLOT
		WRITE(*,*)'** OK: File = ',INFILEARRAY(SLOT)
		WRITE(*,*)'** OK: Number of X,Y data pairs in file = ',
     + NPTSARRAY(SLOT)
		WRITE(*,*)'** OK: Sample Y data = ',
     + Y(1,2,SLOT),Y(NPTSARRAY(SLOT)/2,2,SLOT),Y(NPTSARRAY(SLOT),2,SLOT)
		DO I = 1, NPTSARRAY(SLOT)
			DATA(I) = Y(I,2,SLOT)
		END DO
		N = NPTSARRAY(SLOT)
		CALL PERIOD_MEDIAN (DATA, N, MEDIAN)
		CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
		WRITE(*,*)'** OK: Y data median = ',MEDIAN
		WRITE(*,*)'** OK: Y data mean = ',AVE
		WRITE(*,*)'** OK: Y data standard deviation = ',SDEV
		WRITE(*,*)'** OK: Sample X data = ',
     + Y(1,1,SLOT),Y(NPTSARRAY(SLOT)/2,1,SLOT),Y(NPTSARRAY(SLOT),1,SLOT)
		WRITE(*,*)'** OK: Sample X intervals = ',
     + Y(2,1,SLOT) - Y(1,1,SLOT), Y((NPTSARRAY(SLOT)/2)+1,1,SLOT) -
     + Y(NPTSARRAY(SLOT)/2,1,SLOT), Y(NPTSARRAY(SLOT),1,SLOT) -
     + Y(NPTSARRAY(SLOT)-1,1,SLOT)
		DO I = 1, NPTSARRAY(SLOT) - 1
			DATA(I) = Y(I,1,SLOT)
		END DO
		N = NPTSARRAY(SLOT) - 1
		CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
		WRITE(*,*)'** OK: X data mean = ',AVE
		DO I = 1, NPTSARRAY(SLOT) - 1
			DATA(I) = DSQRT((Y(I+1,1,SLOT)-Y(I,1,SLOT))**2.0D0)
		END DO
		N = NPTSARRAY(SLOT) - 1
		CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
		WRITE(*,*)'** OK: X data interval mean = ',AVE
		WRITE(*,*)'** OK: X data interval standard deviation
     + = ',SDEV
		IF (YERRORARRAY(SLOT)) THEN
		WRITE(*,*)'** OK: Errors on Y data points = .TRUE.'	
		WRITE(*,*)'** OK: Sample Y errors = ',
     + Y(1,3,SLOT),Y(NPTSARRAY(SLOT)/2,3,SLOT),Y(NPTSARRAY(SLOT),3,SLOT)
		DO I = 1, NPTSARRAY(SLOT)
			DATA(I) = Y(I,3,SLOT)
		END DO
		N = NPTSARRAY(SLOT)
		CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
		WRITE(*,*)'** OK: Y data error mean = ',AVE
		WRITE(*,*)'** OK: Y data error standard deviation =
     + ',SDEV
		ELSE
		WRITE(*,*)'** OK: Errors on Y data points = .FALSE.'	
		END IF
		IF (DETRENDARRAY(SLOT)) THEN
		WRITE(*,*)'** OK: Detrended Y data = .TRUE.'	
		ELSE
		WRITE(*,*)'** OK: Detrended Y data = .FALSE.'	
		END IF
	END IF
10	CONTINUE
	ELSE
		GOTO 5
	END IF

C-----------------------------------------------------------------------------
C If summary output required, output to screen.
C-----------------------------------------------------------------------------

	ELSE
	   WRITE(*,*)' '
	   DO 30 SLOT = FIRSTSLOT, LASTSLOT
	      IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		 WRITE(*,*)'No data present in slot number',SLOT
		 GOTO 30
	      ELSE
		 WRITE(*,'(a7,i3,a9,i7,a9,a37)')
     + 'Slot = ',SLOT,', Npts = ',NPTSARRAY(SLOT),
     + ', File = ',INFILEARRAY(SLOT)
	      END IF
30	   CONTINUE
	END IF

99 	RETURN
	END
