        
	SUBROUTINE PERIOD_ARITH (Y, MXROW, MXSLOT,
     +				 NPTSARRAY, YERRORARRAY,
     +				 INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Perform slot arithmetic. Current options are to: +, -, *, or / constants 
C from slots; +, -, *, or /  one slot from/to/by another; take logs,
C square root and 10** of y-values (useful to convert counts into magnitudes).
C
C Written by Vik Dhillon @Hotel Taburiente(!) 17-May-2003.
C Modified by Vik Dhillon @Nether Edge 1-Feb-2004 to prevent -ve errorbars.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_ARITH declarations.
C-----------------------------------------------------------------------------

	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
        DOUBLE PRECISION CONSTANT
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LCONSTANT, LXAXIS
	CHARACTER*1 REPLY, REPLY2
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Determine whether constant-slot or slot-slot operations are to be performed.
C-----------------------------------------------------------------------------

        WRITE(*,'(A,$)')
     + 'Constant-slot [c] or slot-slot'//
     + ' [s] operations? [s] : '
        READ(*,'(A)')REPLY
        IF (REPLY .EQ. ' ') REPLY = 's'
        CALL PERIOD_CASE (REPLY, .TRUE.)
        IF (REPLY .EQ. 'S') THEN 
           LCONSTANT = .FALSE.
        ELSE 
           LCONSTANT = .TRUE.
        END IF

C-----------------------------------------------------------------------------
C Constant-slot operation: Determine type of arithmetic operation to perform.
C-----------------------------------------------------------------------------

        IF (LCONSTANT) THEN
10         WRITE(*,'(A,$)')'[a]dd, [s]ub, [d]iv,'//
     + ' [m]ul by constant, or take [l]og10,'//
     + ' [t]en** or s[q]uare root? [d] : '
           READ(*,'(A)')REPLY
           IF (REPLY .EQ. ' ') REPLY = 'd'
           CALL PERIOD_CASE (REPLY, .TRUE.)
           IF (REPLY .NE. 'A' .AND. REPLY .NE. 'S' .AND. 
     +          REPLY .NE. 'D' .AND. REPLY .NE. 'M' .AND.
     +          REPLY .NE. 'T' .AND.
     +          REPLY .NE. 'L' .AND. REPLY .NE. 'Q') THEN 
              GOTO 10
           END IF
           IF (REPLY .EQ. 'A' .OR. REPLY .EQ. 'S' .OR.
     +          REPLY .EQ. 'D' .OR. REPLY .EQ. 'M') THEN
20            WRITE(*,'(A,$)')'Enter constant : '
              READ(*,*,ERR=20)CONSTANT
              IF (REPLY .EQ. 'D') THEN
                 IF (CONSTANT .EQ. 0.) GOTO 20
              END IF
           END IF
           IF (REPLY .EQ. 'A' .OR. REPLY .EQ. 'S' .OR.
     +          REPLY .EQ. 'D' .OR. REPLY .EQ. 'M' .OR.
     +          REPLY .EQ. 'T' .OR.
     +          REPLY .EQ. 'L' .OR. REPLY .EQ. 'Q') THEN
              WRITE(*,'(A,$)')'Operate on x-axis? [n] : '
              READ(*,'(A)')REPLY2
              IF (REPLY2 .EQ. ' ') REPLY2 = 'n'
              CALL PERIOD_CASE (REPLY2, .TRUE.)
              IF (REPLY2 .EQ. 'Y') THEN 
                 LXAXIS = .TRUE.
              ELSE 
                 LXAXIS = .FALSE.
              END IF
           END IF

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

           CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, 
     +                         MXSLOT, IFAIL)
           IF (IFAIL .EQ. 1) THEN
              GOTO 99
           END IF
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
                 IF (LXAXIS) THEN
                    Y(I,2,SLOTOUT) = Y(I,2,SLOT)
                    IF (YERRORARRAY(SLOT)) THEN
                       Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                       YERRORARRAY(SLOTOUT) = .TRUE.
                    ELSE
                       YERRORARRAY(SLOTOUT) = .FALSE. 
                    END IF
                    IF (REPLY .EQ. 'A') THEN
                       Y(I,1,SLOTOUT) = Y(I,1,SLOT)+CONSTANT
                    ELSE IF (REPLY .EQ. 'S') THEN
                       Y(I,1,SLOTOUT) = Y(I,1,SLOT)-CONSTANT
                    ELSE IF (REPLY .EQ. 'M') THEN
                       Y(I,1,SLOTOUT) = Y(I,1,SLOT)*CONSTANT
                    ELSE IF (REPLY .EQ. 'D') THEN
                       Y(I,1,SLOTOUT) = Y(I,1,SLOT)/CONSTANT
                    ELSE IF (REPLY .EQ. 'L') THEN
                       Y(I,1,SLOTOUT) = DLOG10(Y(I,1,SLOT))
                    ELSE IF (REPLY .EQ. 'T') THEN
                       Y(I,1,SLOTOUT) = 10.**(Y(I,1,SLOT))
                    ELSE IF (REPLY .EQ. 'Q') THEN
                       Y(I,1,SLOTOUT) = DSQRT(Y(I,1,SLOT))
                    END IF
                 ELSE
                    Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                    IF (REPLY .EQ. 'A') THEN
                       Y(I,2,SLOTOUT) = Y(I,2,SLOT)+CONSTANT
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'S') THEN
                       Y(I,2,SLOTOUT) = Y(I,2,SLOT)-CONSTANT
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = Y(I,3,SLOT)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'M') THEN
                       Y(I,2,SLOTOUT) = Y(I,2,SLOT)*CONSTANT
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = Y(I,3,SLOT)*CONSTANT
                          Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'D') THEN
                       Y(I,2,SLOTOUT) = Y(I,2,SLOT)/CONSTANT
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = Y(I,3,SLOT)/CONSTANT
                          Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'L') THEN
                       Y(I,2,SLOTOUT) = DLOG10(Y(I,2,SLOT))
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = 
     +                    Y(I,3,SLOT)/(DLOG(10.0D0)*Y(I,2,SLOT))
                          Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'T') THEN
                       Y(I,2,SLOTOUT) = 10.**(Y(I,2,SLOT))
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = Y(I,2,SLOTOUT) *
     +                    (DLOG(10.0D0)) * Y(I,3,SLOT) 
                          Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    ELSE IF (REPLY .EQ. 'Q') THEN
                       Y(I,2,SLOTOUT) = DSQRT(Y(I,2,SLOT))
                       IF (YERRORARRAY(SLOT)) THEN
                          Y(I,3,SLOTOUT) = 
     +                   Y(I,2,SLOTOUT)*0.5D0*Y(I,3,SLOT)/Y(I,3,SLOTOUT)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       ELSE
                          YERRORARRAY(SLOTOUT) = .FALSE. 
                       END IF
                    END IF
                 END IF
              END DO
              DETRENDARRAY(SLOTOUT) = .FALSE.
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
              INFILEARRAY(SLOTOUT) = INFILEARRAY(SLOT)
              IF (COUNTER .EQ. 1) WRITE(*,*)' '
              WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           END DO

C-----------------------------------------------------------------------------
C Slot-slot operation: Determine type of arithmetic operation to perform.
C-----------------------------------------------------------------------------

        ELSE
30         WRITE(*,'(A,$)')'[a]dd, [s]ub, [d]iv,'//
     + ' or [m]ul slots? [d]: '
           READ(*,'(A)')REPLY
           IF (REPLY .EQ. ' ') REPLY = 'd'
           CALL PERIOD_CASE (REPLY, .TRUE.)
           IF (REPLY .NE. 'A' .AND. REPLY .NE. 'S' .AND. 
     +          REPLY .NE. 'D' .AND. REPLY .NE. 'M') THEN
              GOTO 30
           END IF

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

 40        WRITE(*,'(A,$)')'Enter first slot for input (0 to quit) : '
           READ(*,*,ERR=40)FIRSTSLOT
           IF (FIRSTSLOT .EQ. 0) THEN
              GOTO 99
           ELSE IF (FIRSTSLOT .GT. MXSLOT) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
              GOTO 99
           END IF
 50        WRITE(*,'(A,$)')
     +     'Enter second slot for input (0 to quit) : '
           READ(*,*,ERR=50)LASTSLOT
           IF (LASTSLOT .EQ. 0) THEN
              GOTO 99
           ELSE IF (LASTSLOT .GT. MXSLOT) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
              GOTO 99
           END IF
 60        WRITE(*,'(A,$)')'Enter slot for output (0 to quit) : '
           READ(*,*,ERR=60)SLOTOUT
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

           IF (NPTSARRAY(FIRSTSLOT) .NE. NPTSARRAY(LASTSLOT)) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)
     + '** ERROR: Input slots have different number of points.'
              GOTO 99
           END IF

           DO I = 1, NPTSARRAY(FIRSTSLOT)
              Y(I,1,SLOTOUT) = Y(I,1,FIRSTSLOT)
              IF (REPLY .EQ. 'A') THEN
                 Y(I,2,SLOTOUT) = Y(I,2,FIRSTSLOT)+Y(I,2,LASTSLOT)
                 WRITE(INFILEARRAY(SLOTOUT),'(A,I2,A,I2)')
     +                 'slot ',FIRSTSLOT,' + slot ',LASTSLOT
                 IF (YERRORARRAY(FIRSTSLOT)) THEN                    
c                    IF (YERRORARRAY(LASTSLOT)) THEN                    
                       Y(I,3,SLOTOUT) = 
     + DSQRT( (Y(I,3,FIRSTSLOT)**2.0D0) + (Y(I,3,LASTSLOT)**2.0D0) )
                       YERRORARRAY(SLOTOUT) = .TRUE.
c                    ELSE
c                       YERRORARRAY(SLOTOUT) = .FALSE. 
c                    END IF
                 ELSE
                    YERRORARRAY(SLOTOUT) = .FALSE. 
                 END IF
              ELSE IF (REPLY .EQ. 'S') THEN
                 Y(I,2,SLOTOUT) = Y(I,2,FIRSTSLOT)-Y(I,2,LASTSLOT)
                 WRITE(INFILEARRAY(SLOTOUT),'(A,I2,A,I2)')
     +                 'slot ',FIRSTSLOT,' - slot ',LASTSLOT
                 IF (YERRORARRAY(FIRSTSLOT)) THEN
c                    IF (YERRORARRAY(LASTSLOT)) THEN                    
                       Y(I,3,SLOTOUT) = 
     + DSQRT( (Y(I,3,FIRSTSLOT)**2.0D0) + (Y(I,3,LASTSLOT)**2.0D0) )
                       YERRORARRAY(SLOTOUT) = .TRUE.               
c                    ELSE
c                       YERRORARRAY(SLOTOUT) = .FALSE. 
c                    END IF
                 ELSE
                    YERRORARRAY(SLOTOUT) = .FALSE. 
                 END IF
              ELSE IF (REPLY .EQ. 'M') THEN
                 Y(I,2,SLOTOUT) = Y(I,2,FIRSTSLOT)*Y(I,2,LASTSLOT)
                 WRITE(INFILEARRAY(SLOTOUT),'(A,I2,A,I2)')
     +                 'slot ',FIRSTSLOT,' * slot ',LASTSLOT
                 IF (YERRORARRAY(FIRSTSLOT)) THEN
c                    IF (YERRORARRAY(LASTSLOT)) THEN                    
                       Y(I,3,SLOTOUT) = Y(I,2,SLOTOUT) *
     + DSQRT( ((Y(I,3,FIRSTSLOT)/Y(I,2,FIRSTSLOT))**2.0D0) + 
     +       ((Y(I,3,LASTSLOT)/Y(I,2,LASTSLOT))**2.0D0) )
                       Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                       YERRORARRAY(SLOTOUT) = .TRUE.
c                    ELSE
c                       YERRORARRAY(SLOTOUT) = .FALSE. 
c                    END IF
                 ELSE
                    YERRORARRAY(SLOTOUT) = .FALSE. 
                 END IF
              ELSE IF (REPLY .EQ. 'D') THEN
                 IF (Y(I,2,LASTSLOT) .EQ. 0.0d0) THEN
                    WRITE(*,*)ACHAR(7)
                    WRITE(*,*)'** WARNING: Divide by zero at MJD = ',
     + Y(I,1,LASTSLOT)
                    WRITE(*,*)
     + '** WARNING: Setting y-value to zero in output slot'
                    Y(I,2,SLOTOUT) = 0.0d0
                 ELSE
                    Y(I,2,SLOTOUT) = Y(I,2,FIRSTSLOT)/Y(I,2,LASTSLOT)
                 END IF
                 WRITE(INFILEARRAY(SLOTOUT),'(A,I2,A,I2)')
     +                 'slot ',FIRSTSLOT,' / slot ',LASTSLOT
                 IF (YERRORARRAY(FIRSTSLOT)) THEN
c                    IF (YERRORARRAY(LASTSLOT)) THEN                    
                       IF (Y(I,2,LASTSLOT) .EQ. 0.0d0) THEN
                          Y(I,3,SLOTOUT) = 0.0d0
                       ELSE
                          Y(I,3,SLOTOUT) = Y(I,2,SLOTOUT) *
     + DSQRT( ((Y(I,3,FIRSTSLOT)/Y(I,2,FIRSTSLOT))**2.0D0) + 
     +       ((Y(I,3,LASTSLOT)/Y(I,2,LASTSLOT))**2.0D0) )
                          Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                          YERRORARRAY(SLOTOUT) = .TRUE.
                       END IF
c                    ELSE
c                       YERRORARRAY(SLOTOUT) = .FALSE. 
c                    END IF
                 ELSE
                    YERRORARRAY(SLOTOUT) = .FALSE. 
                 END IF
              END IF
           END DO
           DETRENDARRAY(SLOTOUT) = .FALSE.
           NPTSARRAY(SLOTOUT) = NPTSARRAY(FIRSTSLOT)
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END IF
        
 99   RETURN
      END
