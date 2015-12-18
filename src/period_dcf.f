
        
	SUBROUTINE PERIOD_DCF (Y, MXROW, MXSLOT,
     +                         NPTSARRAY, YERRORARRAY, 
     +                         INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Cross correlate two datasets using the Discrete Correlation Function
C of Edelson & Krolik (1988, ApJ, 333, 646). The output slot contains
C the correlation coefficient between the first and second slot. A
C correlation peak at positive lag implies that the second data set lags
C the first. 
C 
C Adapted for PERIOD from a program provided by Kieran O'Brien, and draws
C on some parts of the PONTO implementation by Tom Marsh.
C Vik Dhillon @Paranal 17-June-2007.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_DCF declarations.
C-----------------------------------------------------------------------------

	INTEGER FIRSTSLOT, LASTSLOT, SLOTOUT
        INTEGER I, J, N1, N2, N_X, NBIN, BIN_NUM
        INTEGER NUM_PTS(MXROW)
        DOUBLE PRECISION DATA(MXROW)
        DOUBLE PRECISION AVE1, ADEV1, SDEV1, VAR1
        DOUBLE PRECISION AVE2, ADEV2, SDEV2, VAR2
        DOUBLE PRECISION AVE_X, ADEV_X, SDEV_X, VAR_X
        DOUBLE PRECISION BIN_SIZE, OVERLAP
        DOUBLE PRECISION MAX_SHIFT, MAX_SHIFT1, MAX_SHIFT2
        DOUBLE PRECISION SHIFT(MXROW)
        DOUBLE PRECISION LOW, HIGH, T_DIFF, CORCOM
        DOUBLE PRECISION X_SUM(MXROW), Y_SUM(MXROW), XY_SUM(MXROW)
        DOUBLE PRECISION CORR(MXROW), CORVAR(MXROW), BIN_SIG(MXROW)
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LZEROLAG
	CHARACTER*1 OPTION
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')'Enter first slot for input (0 to quit) : '
        READ(*,*,ERR=10)FIRSTSLOT
        IF (FIRSTSLOT .EQ. 0) THEN
           GOTO 99
        ELSE IF (FIRSTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 99
        END IF
 20     WRITE(*,'(A,$)')
     +       'Enter second slot for input (0 to quit) : '
        READ(*,*,ERR=20)LASTSLOT
        IF (LASTSLOT .EQ. 0) THEN
           GOTO 99
        ELSE IF (LASTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 99
        END IF
 30     WRITE(*,'(A,$)')'Enter slot for output (0 to quit) : '
        READ(*,*,ERR=30)SLOTOUT
        IF (SLOTOUT .EQ. 0) THEN
           GOTO 99
        ELSE IF (SLOTOUT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 99
        END IF
        
        IF (NPTSARRAY(FIRSTSLOT) .EQ. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Slot empty =',FIRSTSLOT
           GOTO 99
        ELSE IF (NPTSARRAY(LASTSLOT) .EQ. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Slot empty =',LASTSLOT
           GOTO 99
        END IF
        
C-----------------------------------------------------------------------------
C Determine mean and variance of the y-axes of the two input slots.
C-----------------------------------------------------------------------------

        DO I = 1, NPTSARRAY(FIRSTSLOT)
           DATA(I) = Y(I,2,FIRSTSLOT)
        END DO
        N1 = NPTSARRAY(FIRSTSLOT)
        CALL PERIOD_MOMENT (DATA, N1, AVE1, ADEV1, SDEV1, VAR1)
        
        DO I = 1, NPTSARRAY(LASTSLOT)
           DATA(I) = Y(I,2,LASTSLOT)
        END DO
        N2 = NPTSARRAY(LASTSLOT)
        CALL PERIOD_MOMENT (DATA, N2, AVE2, ADEV2, SDEV2, VAR2)
        
C-----------------------------------------------------------------------------
C Prompt for bin size (in X units). The bin size should ideally be set
C so that it includes at least one data point, on average. Below, the
C mean interval of the first input slot is given for guidance.
C-----------------------------------------------------------------------------

        DO I = 1, NPTSARRAY(FIRSTSLOT) - 1
           DATA(I) = DSQRT((Y(I+1,1,FIRSTSLOT)-Y(I,1,FIRSTSLOT))**2.0D0)
        END DO
        N_X = NPTSARRAY(FIRSTSLOT) - 1
        CALL PERIOD_MOMENT (DATA, N_X, AVE_X, ADEV_X, SDEV_X, VAR_X) 
        WRITE(*,*)' '
        WRITE(*,*)'** OK: X data interval mean = ',AVE_X
        WRITE(*,*)'** OK: X data interval standard deviation
     + = ',SDEV_X
        WRITE(*,*)' '
 40     WRITE(*,'(A,$)')
     +       'Enter bin size (0 for mean x-axis interval) : '
        READ(*,*,ERR=40)BIN_SIZE
        IF (BIN_SIZE .LE. 0.0D0) BIN_SIZE = AVE_X

C-----------------------------------------------------------------------------
C Prompt for the maximum shift. This is the largest time-shift to
C calculate the DCF at. This should obviously depend on the amount by
C which the datasets overlap, as calculating the DCF at delays where no
C dataset overlap occurs is not going to give sensible answers. Below,
C the overlap between the two datasets is given for guidance.
C-----------------------------------------------------------------------------

        MAX_SHIFT1 = Y(NPTSARRAY(LASTSLOT),1,LASTSLOT)-Y(1,1,FIRSTSLOT) 
        MAX_SHIFT2 = Y(NPTSARRAY(FIRSTSLOT),1,FIRSTSLOT)-
     + Y(1,1,LASTSLOT) 
        OVERLAP = MIN(ABS(MAX_SHIFT1),ABS(MAX_SHIFT2))
        WRITE(*,*)' '
        WRITE(*,*)'** OK: Overlap between slots = ',OVERLAP
        WRITE(*,*)' '
 50     WRITE(*,'(A,$)')
     +       'Enter maximum shift (0 for slot overlap) : '
        READ(*,*,ERR=50)MAX_SHIFT
        IF (MAX_SHIFT .LE. 0.0D0) MAX_SHIFT = OVERLAP
         
C-----------------------------------------------------------------------------
C Prompt whether or not to eliminate zero-lag pairs. Typically, the DCF will
C show a lot of power at a delay of zero if you don't reject points with
C a zero time lag. The recommended value is therefore "Y". 
C-----------------------------------------------------------------------------

 60     WRITE(*,'(A,$)')'Eliminate zero-lag pairs ? [Y] : '
	READ(*,'(A)',ERR=60)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)
	IF (OPTION .EQ. ' ' .OR. OPTION .EQ. 'Y') THEN
           LZEROLAG = .TRUE.
        ELSE IF (OPTION .EQ. 'N') THEN
           LZEROLAG = .FALSE.
        ELSE
           GOTO 60
        END IF

C-----------------------------------------------------------------------------
C Calculate number of bins.
C-----------------------------------------------------------------------------
        
        NBIN = (2 * INT((MAX_SHIFT/BIN_SIZE))) + 1
        WRITE(*,*)' '
        WRITE(*,*)'** OK: Number of bins = ',NBIN

C-----------------------------------------------------------------------------
C Define shifts.
C-----------------------------------------------------------------------------

        DO I = 1, NBIN
           SHIFT(I) = (-1.D0*MAX_SHIFT) + ((DBLE(I)-1.D0)*BIN_SIZE)
        END DO

C-----------------------------------------------------------------------------
C Initialise arrays.
C-----------------------------------------------------------------------------

        DO I = 1, MXROW
           X_SUM(I) = 0.D0
           Y_SUM(I) = 0.D0
           XY_SUM(I) = 0.D0
           NUM_PTS(I) = 0
           CORVAR(I) = 0.D0
        END DO

C-----------------------------------------------------------------------------
C Cross-correlation loop.
C Loop over the second input slot.
C-----------------------------------------------------------------------------

        DO J = 1, NPTSARRAY(LASTSLOT)
           LOW = Y(J,1,LASTSLOT) - (MAX_SHIFT + (0.5D0*BIN_SIZE))
           HIGH = Y(J,1,LASTSLOT) + (MAX_SHIFT + (0.5D0*BIN_SIZE))

C-----------------------------------------------------------------------------
C Loop over the first input slot, ignoring the zero lags.
C-----------------------------------------------------------------------------

           DO 70 I = 1, NPTSARRAY(FIRSTSLOT)
              IF (LZEROLAG) THEN
                 IF (Y(I,1,FIRSTSLOT) .EQ. Y(J,1,LASTSLOT)) GOTO 70
              END IF
              IF (Y(I,1,FIRSTSLOT) .GE. LOW .AND. 
     +             Y(I,1,FIRSTSLOT) .LE. HIGH) THEN
                 GOTO 80
              END IF
              GOTO 70
 80           T_DIFF = Y(J,1,LASTSLOT) - Y(I,1,FIRSTSLOT)
              BIN_NUM = 1 + NINT((T_DIFF + MAX_SHIFT) / BIN_SIZE)
              X_SUM(BIN_NUM) = X_SUM(BIN_NUM) + 
     +             ((Y(I,2,FIRSTSLOT) - AVE1)**2.0D0)
              Y_SUM(BIN_NUM) = Y_SUM(BIN_NUM) + 
     +             ((Y(J,2,LASTSLOT) - AVE2)**2.0D0)
              XY_SUM(BIN_NUM) = XY_SUM(BIN_NUM) +
     +            ((Y(I,2,FIRSTSLOT) - AVE1) * (Y(J,2,LASTSLOT) - AVE2))
              NUM_PTS(BIN_NUM) = NUM_PTS(BIN_NUM) + 1

C-----------------------------------------------------------------------------
C End of first loops. Now calculate mean and load delay array.
C-----------------------------------------------------------------------------

 70        CONTINUE
        END DO

        DO I = 1, NBIN
           XY_SUM(I) = XY_SUM(I) / DSQRT(X_SUM(I)*Y_SUM(I))
           CORR(I) = 0.0D0
           IF (NUM_PTS(I) .GT. 0) THEN
              CORR(I) = XY_SUM(I)
           END IF
        END DO
        
C-----------------------------------------------------------------------------
C Now loop again.
C-----------------------------------------------------------------------------

        DO J = 1, NPTSARRAY(LASTSLOT)
           LOW = Y(J,1,LASTSLOT) - (MAX_SHIFT + (0.5D0*BIN_SIZE))
           HIGH = Y(J,1,LASTSLOT) + (MAX_SHIFT + (0.5D0*BIN_SIZE))
           DO 90 I = 1, NPTSARRAY(FIRSTSLOT)
              IF (LZEROLAG) THEN
                 IF (Y(I,1,FIRSTSLOT) .EQ. Y(J,1,LASTSLOT)) GOTO 90
              END IF
              IF (Y(I,1,FIRSTSLOT) .GE. LOW .AND. 
     +             Y(I,1,FIRSTSLOT) .LE. HIGH) THEN
                 GOTO 100
              END IF
              GOTO 90
 100          T_DIFF = Y(J,1,LASTSLOT) - Y(I,1,FIRSTSLOT)
              BIN_NUM = 1 + NINT((T_DIFF + MAX_SHIFT) / BIN_SIZE)
              CORCOM = ((Y(I,2,FIRSTSLOT) - AVE1) *
     +             (Y(J,2,LASTSLOT) - AVE2)) / DSQRT (VAR1 * VAR2)
              CORVAR(BIN_NUM) = CORVAR(BIN_NUM) + 
     +             ((CORCOM - CORR(BIN_NUM))**2.0D0)
 90        CONTINUE
        END DO
        
        DO I = 1, NBIN
           IF (NUM_PTS(I).GE. 2) THEN 
              BIN_SIG(I) = DSQRT( CORVAR(I) /
     +             (DBLE(NUM_PTS(I))*DBLE((NUM_PTS(I))-1.0D0)) )
           ELSE
              BIN_SIG(I) = DSQRT(CORVAR(I))
           END IF
        END DO

C-----------------------------------------------------------------------------
C Load output slot.
C-----------------------------------------------------------------------------

	DO I = 1, NBIN
           Y(I,1,SLOTOUT) = SHIFT(I)
           Y(I,2,SLOTOUT) = CORR(I)
           Y(I,3,SLOTOUT) = BIN_SIG(I)
        END DO
        YERRORARRAY(SLOTOUT) = .TRUE.
        DETRENDARRAY(SLOTOUT) = .FALSE.
        NPTSARRAY(SLOTOUT) = NBIN
        INFILEARRAY(SLOTOUT) = 'dcf, '//INFILEARRAY(FIRSTSLOT)
        WRITE(*,*)' '
        WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        WRITE(*,*)' '
99	RETURN
	END
