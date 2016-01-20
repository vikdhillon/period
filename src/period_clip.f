        
	SUBROUTINE PERIOD_CLIP (Y, MXROW, MXSLOT,
     +                          NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Clips y-axis data by removing data with y-values greater than a 
C user-defined range, or number of standard deviations from the mean.
C Written by Vikram Singh Dhillon @Nether Edge 26-February-2004.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Added option to replace points by mean of dataset/nearest neighbours,
C or zero. Also added upper and lower sigma limits for clipping 
C Vik Dhillon @Paranal 10-May-2005.
C
C Routine now also clips x-axis data by simply removing points
C before and after user-defined limits. This routine assumes all
C slots have the same x-axis range as the first input slot, and
C hence will all be x-clipped by the same amount.
C Vik Dhillon @Paranal 16-June-2007.
C
C Added v-clipping option, VSD @La Silla, 09-Nov-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_CLIP declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION XMIN, XMAX, NEW_XMIN, NEW_XMAX
	DOUBLE PRECISION DATA(MXROW), AVE, ADEV, SDEV, VAR
        DOUBLE PRECISION PSIGMA, NSIGMA, PERCENT
        DOUBLE PRECISION UPPER, LOWER, VALUE
	INTEGER N, SLOT, FIRSTSLOT, LASTSLOT, I
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER
        INTEGER COUNTER2, NCLIP
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LSIGMA, LXCLIP, LYCLIP
	CHARACTER*1 OPTION1, OPTION2, OPTION3
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------
        
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
           GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for x- or y-clipping.
C-----------------------------------------------------------------------------

 10     WRITE(*,'(A,$)')'[X]-clip or [Y]-clip data? [Y] : '
        READ(*,'(A)',ERR=10)OPTION1
        IF (OPTION1 .EQ. ' ') OPTION1 = 'Y'
        CALL PERIOD_CASE (OPTION1, .TRUE.)
        IF (OPTION1 .NE. 'X' .AND. OPTION1 .NE. 'Y') GOTO 10
        IF (OPTION1 .EQ. 'Y') THEN
           LYCLIP = .TRUE.
           LXCLIP = .FALSE.
        ELSE 
           LXCLIP = .TRUE.
           LYCLIP = .FALSE.
        END IF

C-----------------------------------------------------------------------------
C y-clipping section.
C-----------------------------------------------------------------------------

        IF (LYCLIP) THEN

C-----------------------------------------------------------------------------
C Prompt for clip technique.
C-----------------------------------------------------------------------------

 20     WRITE(*,'(A,$)')'[R]emove points or replace by '// 
     +       '[M]ean, [Z]ero, [V]alue or [N]earest neighbour ? [R] : '
        READ(*,'(A)',ERR=20)OPTION2
        IF (OPTION2 .EQ. ' ') OPTION2 = 'R'
        CALL PERIOD_CASE (OPTION2, .TRUE.)
        IF (OPTION2 .NE. 'R' .AND. OPTION2 .NE. 'M' .AND.
     +      OPTION2 .NE. 'N' .AND. OPTION2 .NE. 'Z' .AND.
     +      OPTION2 .NE. 'V') GOTO 20

C-----------------------------------------------------------------------------
C Prompt for clip range.
C-----------------------------------------------------------------------------

 30     WRITE(*,'(A,$)')'Sigma clipping [Y/N] ? [N] : '
        READ(*,'(A)',ERR=30)OPTION3
        IF (OPTION3 .EQ. ' ') OPTION3 = 'N'
        CALL PERIOD_CASE (OPTION3, .TRUE.)
        IF (OPTION3 .NE. 'Y' .AND. OPTION3 .NE. 'N') GOTO 30
        IF (OPTION3 .EQ. 'Y') THEN
           LSIGMA = .TRUE.
        ELSE 
           LSIGMA = .FALSE.
        END IF
        
        IF (LSIGMA) THEN
40      WRITE(*,'(A,$)')'Enter upper and lower sigma for '//
     +                    'clipping (e.g. 3,2.5): '
        READ(*,*,ERR=40)PSIGMA, NSIGMA
        IF (NSIGMA .LE. 0. .OR. PSIGMA .LE. 0.) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Number of sigma must be greater than 0.'
           WRITE(*,*)' '
           GOTO 40
        END IF
        ELSE
50      WRITE(*,'(A,$)')'Enter upper and lower limits for '//
     +                    'clipping (e.g. 1000,0): '
        READ(*,*,ERR=50)UPPER, LOWER
        IF (LOWER .GE. UPPER) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)
     +     '** ERROR: Upper limit must be greater than lower limit.'
           WRITE(*,*)' '
           GOTO 50
        END IF
        END IF

C-----------------------------------------------------------------------------
C Determine mean and standard deviation of data.
C-----------------------------------------------------------------------------

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
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Y data mean = ',AVE
           WRITE(*,*)'** OK: Y data standard deviation = ',SDEV
           IF (SDEV .EQ. 0.) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Standard deviation is zero.'
              GOTO 99
           END IF

           COUNTER2 = 0
           NCLIP = 0
           DO I = 1, NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C R-clipping.
C-----------------------------------------------------------------------------
              
              IF (OPTION2 .EQ. 'R') THEN
                 IF (LSIGMA) THEN

                 IF ( Y(I,2,SLOT)-AVE .GT. SDEV*PSIGMA .OR.
     +                Y(I,2,SLOT)-AVE .LT. -1.D0*SDEV*NSIGMA) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: R-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                 ELSE
                    COUNTER2 = COUNTER2 + 1
                    Y(COUNTER2,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(COUNTER2,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(COUNTER2,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 ELSE

                 IF ( Y(I,2,SLOT) .GT. UPPER .OR.
     +                Y(I,2,SLOT) .LT. LOWER) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: R-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                 ELSE
                    COUNTER2 = COUNTER2 + 1
                    Y(COUNTER2,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(COUNTER2,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(COUNTER2,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF
                    
                 END IF

C-----------------------------------------------------------------------------
C M-clipping.
C-----------------------------------------------------------------------------

              ELSE IF (OPTION2 .EQ. 'M') THEN
                 IF (LSIGMA) THEN

                 IF ( Y(I,2,SLOT)-AVE .GT. SDEV*PSIGMA .OR.
     +                Y(I,2,SLOT)-AVE .LT. -1.D0*SDEV*NSIGMA) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: M-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = AVE
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 ELSE

                 IF ( Y(I,2,SLOT) .GT. UPPER .OR.
     +                Y(I,2,SLOT) .LT. LOWER) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: M-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = AVE
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 END IF

C-----------------------------------------------------------------------------
C Z-clipping.
C-----------------------------------------------------------------------------

              ELSE IF (OPTION2 .EQ. 'Z') THEN
                 IF (LSIGMA) THEN

                 IF ( Y(I,2,SLOT)-AVE .GT. SDEV*PSIGMA .OR.
     +                Y(I,2,SLOT)-AVE .LT. -1.D0*SDEV*NSIGMA) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: Z-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = 0.D0
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 ELSE

                 IF ( Y(I,2,SLOT) .GT. UPPER .OR.
     +                Y(I,2,SLOT) .LT. LOWER) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: Z-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = 0.D0
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 END IF

C-----------------------------------------------------------------------------
C V-clipping.
C-----------------------------------------------------------------------------

              ELSE IF (OPTION2 .EQ. 'V') THEN
                 IF (I .EQ. 1) THEN
 55                 WRITE(*,'(A,$)')'Enter replacement value : '
                    READ(*,*,ERR=55)VALUE
                 END IF

                 IF (LSIGMA) THEN

                 IF ( Y(I,2,SLOT)-AVE .GT. SDEV*PSIGMA .OR.
     +                Y(I,2,SLOT)-AVE .LT. -1.D0*SDEV*NSIGMA) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: V-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = VALUE
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 ELSE

                 IF ( Y(I,2,SLOT) .GT. UPPER .OR.
     +                Y(I,2,SLOT) .LT. LOWER) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: Z-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = VALUE
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 END IF

C-----------------------------------------------------------------------------
C N-clipping.
C-----------------------------------------------------------------------------

              ELSE IF (OPTION2 .EQ. 'N') THEN
                 IF (LSIGMA) THEN

                 IF ( Y(I,2,SLOT)-AVE .GT. SDEV*PSIGMA .OR.
     +                Y(I,2,SLOT)-AVE .LT. -1.D0*SDEV*NSIGMA) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: N-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    IF (I .EQ. 1) THEN
                       Y(I,2,SLOTOUT) = Y(I+1,2,SLOT)
                    ELSE IF (I .EQ. NPTSARRAY(SLOT)) THEN
                       Y(I,2,SLOTOUT) = Y(I-1,2,SLOT) 
                    ELSE
                       Y(I,2,SLOTOUT)=(Y(I+1,2,SLOT)+Y(I-1,2,SLOT))/2.D0
                    END IF
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 ELSE

                 IF ( Y(I,2,SLOT) .GT. UPPER .OR.
     +                Y(I,2,SLOT) .LT. LOWER) THEN
                    IF (NCLIP .EQ. 0) WRITE(*,*)' '
                    WRITE(*,*)
     +              '** OK: N-clipped x,y pair ',Y(I,1,SLOT),Y(I,2,SLOT)
                    NCLIP = NCLIP + 1
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    IF (I .EQ. 1) THEN
                       Y(I,2,SLOTOUT) = Y(I+1,2,SLOT)
                    ELSE IF (I .EQ. NPTSARRAY(SLOT)) THEN
                       Y(I,2,SLOTOUT) = Y(I-1,2,SLOT) 
                    ELSE
                       Y(I,2,SLOTOUT)=(Y(I+1,2,SLOT)+Y(I-1,2,SLOT))/2.D0
                    END IF
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                    END IF
                 END IF

                 END IF

              END IF
           END DO
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Number of points clipped = ',NCLIP
           PERCENT = (DBLE(NCLIP)/DBLE(NPTSARRAY(SLOT)))*100.0D0
           WRITE(*,*)'** OK: Percentage of points clipped = ',PERCENT
           DETRENDARRAY(SLOTOUT) = .FALSE.
           YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
           IF (OPTION2 .EQ. 'R') THEN           
              NPTSARRAY(SLOTOUT) = COUNTER2
           ELSE IF (OPTION2 .EQ. 'M') THEN           
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           ELSE IF (OPTION2 .EQ. 'N') THEN           
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           ELSE IF (OPTION2 .EQ. 'Z') THEN           
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           ELSE IF (OPTION2 .EQ. 'V') THEN           
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           END IF
           INFILEARRAY(SLOTOUT) = 'y-clipped, '//INFILEARRAY(SLOT)
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
      END DO
      
      END IF

C-----------------------------------------------------------------------------
C x-clipping section.
C-----------------------------------------------------------------------------

      IF (LXCLIP) THEN

C-----------------------------------------------------------------------------
C Tell user the current x-axis range.
C-----------------------------------------------------------------------------

        IF (NPTSARRAY(FIRSTSLOT) .EQ. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Slot empty =',SLOT
           GOTO 99
        END IF
        XMIN = Y(1,1,FIRSTSLOT)
        XMAX = Y(NPTSARRAY(FIRSTSLOT),1,FIRSTSLOT)
        WRITE(*,*)'** OK: x-axis range of first slot = ', XMIN, XMAX

C-----------------------------------------------------------------------------
C Prompt for new x-axis range.
C-----------------------------------------------------------------------------

 60     WRITE(*,'(A,$)')
     + 'Enter new x-axis range (0,0 to quit, -1 for old value) : '
        READ(*,*,ERR=60)NEW_XMIN, NEW_XMAX
        IF (NEW_XMIN .EQ. 0. .AND. NEW_XMAX .EQ. 0.) GOTO 99
        IF (NEW_XMIN .EQ. -1) NEW_XMIN = XMIN
        IF (NEW_XMAX .EQ. -1) NEW_XMAX = XMAX
        IF (NEW_XMIN .LT. XMIN .OR. NEW_XMAX .GT. XMAX) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: New range must lie inside old one.'
           WRITE(*,*)' '
           GOTO 60
        END IF

C-----------------------------------------------------------------------------
C Now xclip the data.
C-----------------------------------------------------------------------------

        COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
           IF (NPTSARRAY(SLOT) .EQ. 0) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Slot empty =',SLOT
              WRITE(*,*)' '
              GOTO 99
           END IF
           SLOTOUT = FIRSTOUT + COUNTER
           COUNTER = COUNTER + 1
           COUNTER2 = 0
           DO I = 1, NPTSARRAY(SLOT)
              IF (Y(I,1,SLOT) .GE. NEW_XMIN) THEN
              IF (Y(I,1,SLOT) .LE. NEW_XMAX) THEN
                 COUNTER2 = COUNTER2 + 1
                 Y(COUNTER2,1,SLOTOUT) = Y(I,1,SLOT)
                 Y(COUNTER2,2,SLOTOUT) = Y(I,2,SLOT)
                 IF (YERRORARRAY(SLOT)) THEN
                    Y(COUNTER2,3,SLOTOUT) = Y(I,3,SLOT)
                 END IF
              END IF
              END IF
           END DO
           DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
           YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
           INFILEARRAY(SLOTOUT) = 'x-clipped, '//INFILEARRAY(SLOT)
           NPTSARRAY(SLOTOUT) = COUNTER2
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           WRITE(*,*)' '
      END DO
      
      END IF

99    RETURN
      END
