        
	SUBROUTINE PERIOD_RESAMP (Y, MXROW, MXSLOT,
     +			          NPTSARRAY, YERRORARRAY, 
     +			          INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Routine to resample data onto a finer grid. Written for analysis of the
C PSR J1023+0038 ULTRASPEC data, which was taken in clear mode. The light
C curve is resampled onto a finer time-resolution grid, with no data
C interpolation. The output is written to a binary file.
C
C Written by Vikram Singh Dhillon @Manchester 06-Dec-2017.        
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_RESAMP declarations.
C-----------------------------------------------------------------------------

	INTEGER SLOT, SLOTOUT, I, J, N, IUNIT
	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR
        DOUBLE PRECISION DT, TEXP, BINSTART, BINEND, BINMID
        DOUBLE PRECISION XVAL, YVAL, EVAL
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT)
	CHARACTER*1 OUTFILE*32
        DATA IUNIT /12/
        
C-----------------------------------------------------------------------------
C Select input data slot.
C-----------------------------------------------------------------------------

10      WRITE(*,'(A,$)')
     +       'Enter slot for input (0 to quit) : '
        READ(*,*,ERR=10)SLOT
	IF (SLOT .EQ. 0) THEN
           GOTO 99
        ELSE IF (SLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 10
        END IF

C-----------------------------------------------------------------------------
C Select output data slot.
C-----------------------------------------------------------------------------
        
c15      WRITE(*,'(A,$)')'Enter slot for output (0 to quit) : '
c        READ(*,*,ERR=15)SLOTOUT
c        IF (SLOTOUT .EQ. 0) THEN
c           GOTO 99
c        ELSE IF (SLOTOUT .GT. MXSLOT) THEN
c           WRITE(*,*)ACHAR(7)
c           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
c           GOTO 15
c        END IF
        
C-----------------------------------------------------------------------------
C Give user information on time sampling in the input slot.
C-----------------------------------------------------------------------------
        
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
           WRITE(*,*)' '
           WRITE(*,*)'** OK: No data present in slot number',SLOT
           GOTO 99
	ELSE
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Slot number = ',SLOT
           WRITE(*,*)'** OK: File = ',INFILEARRAY(SLOT)
           WRITE(*,*)'** OK: Number of X,Y data pairs in file = ',
     + NPTSARRAY(SLOT)
           WRITE(*,*)'** OK: Sample X data = ',
     + Y(1,1,SLOT),Y(NPTSARRAY(SLOT)/2,1,SLOT),Y(NPTSARRAY(SLOT),1,SLOT)
           WRITE(*,*)'** OK: Sample X intervals = ',
     + Y(2,1,SLOT) - Y(1,1,SLOT), Y((NPTSARRAY(SLOT)/2)+1,1,SLOT) -
     + Y(NPTSARRAY(SLOT)/2,1,SLOT), Y(NPTSARRAY(SLOT),1,SLOT) -
     + Y(NPTSARRAY(SLOT)-1,1,SLOT)
           DO I = 1, NPTSARRAY(SLOT) - 1
              DATA(I) = Y(I,1,SLOT)
           END DO
           DO I = 1, NPTSARRAY(SLOT) - 1
              DATA(I) = DSQRT((Y(I+1,1,SLOT)-Y(I,1,SLOT))**2.0D0)
           END DO
           N = NPTSARRAY(SLOT) - 1
           CALL PERIOD_MOMENT (DATA, N, AVE, ADEV, SDEV, VAR)
           WRITE(*,*)'** OK: X data interval mean = ',AVE
           WRITE(*,*)'** OK: X data interval standard deviation
     + = ',SDEV
	END IF

C-----------------------------------------------------------------------------
C Prompt for desired time sampling.
C-----------------------------------------------------------------------------
        
        WRITE(*,*)' '
20      WRITE(*,'(A,$)')'Enter sampling time required (secs) : '
        READ(*,*,ERR=20)DT
        DT = DT / 86400.D0
        IF (DT .GE. AVE) THEN
           WRITE(*,*)ACHAR(7)              
           WRITE(*,*)
     + '** ERROR: New sampling time must be less than old.'   
           GOTO 99
        END IF
        N = INT((Y(NPTSARRAY(SLOT),1,SLOT)-Y(2,1,SLOT))/DT)+1
c        IF (N .GT. MXROW) THEN
c           WRITE(*,*)ACHAR(7)
c           WRITE(*,*)'** ERROR: Number of points per bin too large.'
c           GOTO 99
c        END IF
30      WRITE(*,'(A,$)')'Enter exposure time (secs): '
        READ(*,*,ERR=30)TEXP
        TEXP = TEXP / 86400.D0
        IF (TEXP .GE. AVE) THEN
           WRITE(*,*)ACHAR(7)              
           WRITE(*,*)
     + '** ERROR: Exposure time must be less than old sampling time.'   
           GOTO 99
        END IF
40	WRITE(*,'(A,$)')'Enter output binary filename (q to quit) : '
	READ(*,'(A)',ERR=40)OUTFILE
	IF (OUTFILE .EQ. 'Q' .OR. OUTFILE .EQ. 'q') GOTO 99
	OPEN (UNIT=IUNIT, FILE=OUTFILE, STATUS='NEW',
     +       FORM='UNFORMATTED', ERR=40)

C-----------------------------------------------------------------------------
C Resample data onto finer grid, without interpolation.
C-----------------------------------------------------------------------------
        
        DO I = 1, N
           BINSTART = Y(2,1,SLOT)+(DBLE(I-1)*DT)
           BINEND = Y(2,1,SLOT)+(DBLE(I)*DT)
           BINMID = (BINEND+BINSTART)/2.D0
c           Y(I,1,SLOTOUT) = BINMID
           XVAL = BINMID
c           Y(I,2,SLOTOUT) = 0.D0
           YVAL = 0.D0
           IF (YERRORARRAY(SLOT)) THEN
c              Y(I,3,SLOTOUT) = 0.D0
              EVAL = 0.D0
           END IF                    
           DO J = 1, NPTSARRAY(SLOT)
              IF (Y(J,1,SLOT)-(TEXP/2.D0) .GE. BINSTART .AND.
     +             Y(J,1,SLOT)+(TEXP/2.D0) .LT. BINEND) THEN
c                 Y(I,2,SLOTOUT) = Y(J,2,SLOT)
                 YVAL = Y(J,2,SLOT)
                 IF (YERRORARRAY(SLOT)) THEN
c                    Y(I,3,SLOTOUT) = Y(J,3,SLOT)
                    EVAL = Y(J,3,SLOT)
                 END IF
              ELSE IF (Y(J,1,SLOT)-(TEXP/2.D0) .LE. BINSTART .AND.
     +                Y(J,1,SLOT)+(TEXP/2.D0) .GT. BINEND) THEN
c                 Y(I,2,SLOTOUT) = Y(J,2,SLOT)
                 YVAL = Y(J,2,SLOT)
                 IF (YERRORARRAY(SLOT)) THEN
c                    Y(I,3,SLOTOUT) = Y(J,3,SLOT)
                    EVAL = Y(J,3,SLOT)
                 END IF
              ELSE IF (Y(J,1,SLOT)+(TEXP/2.D0) .GE. BINSTART .AND.
     +                Y(J,1,SLOT)+(TEXP/2.D0) .LT. BINEND) THEN
c                 Y(I,2,SLOTOUT) = Y(J,2,SLOT)
                 YVAL = Y(J,2,SLOT)
                 IF (YERRORARRAY(SLOT)) THEN
c                    Y(I,3,SLOTOUT) = Y(J,3,SLOT)
                    EVAL = Y(J,3,SLOT)
                 END IF
              END IF
           END DO
           WRITE(IUNIT)YVAL
        END DO
        CLOSE(UNIT=IUNIT)
        WRITE(*,*)' '
	WRITE(*,*)
     + '** OK: Number of points written to binary file = ',N
c        YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
c        DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
c        NPTSARRAY(SLOTOUT) = N
c        INFILEARRAY(SLOTOUT) = 'resampled, '//INFILEARRAY(SLOT)
c        WRITE(*,*)'** OK: Number of points in slot = ',N
c        WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           
99      RETURN
	END
