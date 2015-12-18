       
	SUBROUTINE PERIOD_POLYFIT (Y, MXROW, MXSLOT,
     +				   NPTSARRAY, YERRORARRAY, 
     +				   INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Fit polynomial to data.
C
C Written by Vikram Singh Dhillon @La Silla 12-June-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_POLYFIT declarations.
C-----------------------------------------------------------------------------

	INTEGER MAXPOLY, MAXMASK
	PARAMETER (MAXPOLY = 20, MAXMASK = 10)
	DOUBLE PRECISION PFT(MAXPOLY), CHISQ, PERIOD_POLY, RMS
	DOUBLE PRECISION XM(MAXPOLY,2*MAXPOLY+3), X(3,MXROW)
        DOUBLE PRECISION INTERVAL
        DOUBLE PRECISION LMASK(MAXMASK), RMASK(MAXMASK)
	INTEGER NDATA, NPOLY, SLOT, FIRSTSLOT, LASTSLOT, I
        INTEGER J, NMASK
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER, NORM, NPTS
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
C Prompt for fit parameters.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
 10     WRITE(*,'(A,$)')'Enter order of polynomial : '
	READ(*,*,ERR=10)NPOLY
	IF (NPOLY .GT. MAXPOLY) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum polynomial
     +order = ',MAXPOLY
           GOTO 99
	ELSE IF (NPOLY .LE. 0) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Minimum polynomial
     +order = 1'
           GOTO 99
	END IF
 20     WRITE(*,'(A,$)')
     +       'Enter number of points (0 for input slot values) : '
	READ(*,*,ERR=20)NPTS
        IF (NPTS .GT. MXROW) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum number of '//
     +          'points = ',MXROW
           GOTO 99
        END IF
        NMASK = 0
        DO I = 1, MAXMASK
 30        WRITE(*,'(A,$)')
     +          'Enter x range to mask from fit (0,0 to continue) : '
           READ(*,*,ERR=30)LMASK(I),RMASK(I)
           IF (LMASK(I) .EQ. 0.D0 .AND. RMASK(I) .EQ. 0.D0) GOTO 40
           NMASK = NMASK + 1
           IF (NMASK .GT. MAXMASK) GOTO 40
        END DO
 40     CONTINUE

C-----------------------------------------------------------------------------
C Perform polynomial fit
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
           IF (NPOLY .GE. NPTSARRAY(SLOT)) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Number of polynomial
     +terms greater than or equal to'
              WRITE(*,*)'** ERROR: number of data points. Fit
     +aborted.'                  
              GOTO 99					
           END IF
           DO I = 1, NPTSARRAY(SLOT)
              X(1,I) = Y(I,1,SLOT)-AINT(Y(1,1,SLOT))
              X(2,I) = Y(I,2,SLOT)
              IF (YERRORARRAY(SLOT)) THEN
                 X(3,I) = Y(I,3,SLOT)
              ELSE
                 X(3,I) = 1.0D0
              END IF
           END DO
           IF (NMASK .NE. 0) THEN
              DO I = 1, NPTSARRAY(SLOT)
                 DO J = 1, NMASK
                    IF (X(1,I) .GE. LMASK(J)-AINT(Y(1,1,SLOT)) .AND. 
     +                   X(1,I) .LE. RMASK(J)-AINT(Y(1,1,SLOT))) THEN
                       X(3,I) = 1.0D32                 
                    END IF
                 END DO
              END DO
           END IF
           NDATA = NPTSARRAY(SLOT)
           NORM = 1
           CALL PERIOD_LSQUAR (X, NDATA, NPOLY, PFT, CHISQ, XM, NORM)
           IF (CHISQ .EQ. -1.) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Singular matrix in
     +PERIOD_LSQUAR.'
              GOTO 99
           ELSE IF (CHISQ .EQ. -2.) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Overflow or divide check
     +occurred in PERIOD_LSQUAR.'
              GOTO 99
           ELSE IF (CHISQ .EQ. -3.) THEN
              WRITE(*,*)ACHAR(7)
              WRITE(*,*)'** ERROR: Invalid parameters input
     +to PERIOD_LSQUAR.'
              GOTO 99
           END IF
           
C-----------------------------------------------------------------------------
C Evaluate fit.
C-----------------------------------------------------------------------------

           IF (NPTS .EQ. 0) THEN
              DO I = 1, NPTSARRAY(SLOT)
                 Y(I,1,SLOTOUT) = Y(I,1,SLOT)
                 Y(I,2,SLOTOUT) = PERIOD_POLY(PFT, NPOLY, 
     +                Y(I,1,SLOT)-AINT(Y(1,1,SLOT)))
              END DO
           ELSE
              INTERVAL = (Y(NPTSARRAY(SLOT),1,SLOT) - 
     +             Y(1,1,SLOT))/DBLE(NPTS-1)
              DO I = 1, NPTS
                 Y(I,1,SLOTOUT) = Y(1,1,SLOT) + (INTERVAL*DBLE(I-1))
                 Y(I,2,SLOTOUT) = PERIOD_POLY(PFT, NPOLY, 
     +                Y(I,1,SLOTOUT)-AINT(Y(1,1,SLOT)))
              END DO
           END IF
           
C-----------------------------------------------------------------------------
C Calculate rms.
C-----------------------------------------------------------------------------

           RMS = 0.0D0
           DO I = 1, NPTSARRAY(SLOT)
              RMS = RMS + ((Y(I,2,SLOT)-PERIOD_POLY(PFT, NPOLY, 
     +             Y(I,1,SLOT)-AINT(Y(1,1,SLOT))))**2.0D0)
           END DO
           RMS = RMS / DBLE(NPTSARRAY(SLOT))
           RMS = DSQRT(RMS)
           WRITE(*,*)' '
           WRITE(*,*)'** OK: RMS of polynomial fit = ',RMS
           DETRENDARRAY(SLOTOUT) = .FALSE.
           YERRORARRAY(SLOTOUT) = .FALSE.
           INFILEARRAY(SLOTOUT) = 
     +          'polynomial fit, '//INFILEARRAY(SLOT)
           IF (NPTS .EQ. 0) THEN
              NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           ELSE
              NPTSARRAY(SLOTOUT) = NPTS
           END IF
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END DO
        
 99     RETURN
        END
