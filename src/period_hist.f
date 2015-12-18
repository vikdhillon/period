        
	SUBROUTINE PERIOD_HIST (Y, MXROW, MXSLOT,
     +				NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Plot histogram or cumulative density function (CDF) of y-axis data. 
C
C Written by Vikram Singh Dhillon @Sheffield 13-May-2010.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_HIST declarations.
C-----------------------------------------------------------------------------

	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I, J, NBIN
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER
        DOUBLE PRECISION DATMIN, DATMAX
        DOUBLE PRECISION BINWID, MINBIN, MAXBIN
        DOUBLE PRECISION CENBIN(MXROW), N(MXROW)
        DOUBLE PRECISION CDF_NORM(MXROW), CDF, SUM
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LCDF
	CHARACTER*72 INFILEARRAY(MXSLOT)
	CHARACTER*1 REPLY

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for histogram or CDF.
C-----------------------------------------------------------------------------

        WRITE(*,'(A,$)')
     + '[h]istogram or [c]umulative density function? [h] : '
        READ(*,'(A)')REPLY
        IF (REPLY .EQ. ' ') REPLY = 'h'
        CALL PERIOD_CASE (REPLY, .TRUE.)
        IF (REPLY .EQ. 'C') THEN 
           LCDF = .TRUE.
        ELSE
           LCDF = .FALSE.
        END IF

C-----------------------------------------------------------------------------
C Prompt for number of bins. The number of y-axis points in the first slot is 
C given for guidance.
C-----------------------------------------------------------------------------

        WRITE(*,*)' '
        WRITE(*,*)'** OK: Number of y-axis points in first slot = ',
     +       NPTSARRAY(FIRSTSLOT)
        WRITE(*,*)' '
 10     WRITE(*,'(A,$)')
     + 'Enter number of bins (0 for number of y-axis points'//
     + ' in first slot) : '
        READ(*,*,ERR=10)NBIN
        IF (NBIN .GT. MXROW) NBIN = MXROW
        IF (NBIN .EQ. 0.D0) NBIN = NPTSARRAY(FIRSTSLOT)

C-----------------------------------------------------------------------------
C Loop through slots.
C-----------------------------------------------------------------------------

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

C-----------------------------------------------------------------------------
C Find the maximum and minimum y-axis points.
C-----------------------------------------------------------------------------

           DATMAX = -1.0D+32
           DATMIN =  1.0D+32
           DO I = 1, NPTSARRAY(SLOT)
              IF (Y(I,2,SLOT).GT. DATMAX) THEN
                 DATMAX = Y(I,2,SLOT)
              END IF
              IF (Y(I,2,SLOT) .LT. DATMIN) THEN
                 DATMIN = Y(I,2,SLOT)
              END IF
           END DO

C-----------------------------------------------------------------------------
C Calculate histogram (this part is also needed for CDF calculation).
C-----------------------------------------------------------------------------
           
           DO I = 1, NBIN
              N(I) = 0.D0
           END DO
           BINWID = (DATMAX-DATMIN)/DBLE(NBIN-1)
           DO I = 1, NBIN
              MINBIN = DATMIN-(BINWID/2.D0)+((DBLE(I)-1.D0)*BINWID)
              MAXBIN = DATMIN-(BINWID/2.D0)+(DBLE(I)*BINWID)
              CENBIN(I) = MINBIN+((MAXBIN-MINBIN)/2.D0)
              DO J = 1, NPTSARRAY(SLOT)
                 IF (I .EQ. NBIN) THEN
                    IF (Y(J,2,SLOT) .GE. MINBIN .AND.
     +                   Y(J,2,SLOT) .LE. MAXBIN) THEN
                       N(I)=N(I)+1.D0
                    END IF
                 ELSE
                    IF (Y(J,2,SLOT) .GE. MINBIN .AND.
     +                   Y(J,2,SLOT) .LT. MAXBIN) THEN
                       N(I)=N(I)+1.D0
                    END IF
                 END IF
              END DO
           END DO
           
C-----------------------------------------------------------------------------
C or calculate the CDF.
C-----------------------------------------------------------------------------

           IF (LCDF) THEN
              DO I = 1, NBIN
                 SUM = 0.D0
                 CDF = 0.D0
                 CDF_NORM(I) = 0.D0
              END DO
              DO I = 1, NBIN
                 SUM = SUM + N(I)
              END DO
              DO I = 1, NBIN
                 CDF = (CDF + N(I))
                 CDF_NORM(I) = CDF / SUM
              END DO
           END IF

C-----------------------------------------------------------------------------
C Write output slot.
C-----------------------------------------------------------------------------
              
           DO I = 1, NBIN
              Y(I,1,SLOTOUT) = CENBIN(I)
              IF (LCDF) THEN
                 Y(I,2,SLOTOUT) = CDF_NORM(I)
              ELSE 
                 Y(I,2,SLOTOUT) = N(I)
              END IF
           END DO
           DETRENDARRAY(SLOTOUT) = .FALSE.
           YERRORARRAY(SLOTOUT) = .FALSE.
           IF (LCDF) THEN
              INFILEARRAY(SLOTOUT) = 'CDF, '//INFILEARRAY(SLOT)
           ELSE 
              INFILEARRAY(SLOTOUT) = 'histogram, '//INFILEARRAY(SLOT)
           END IF
           NPTSARRAY(SLOTOUT) = NBIN
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
        END DO

C-----------------------------------------------------------------------------
C and exit.
C-----------------------------------------------------------------------------

 99     RETURN
        END
