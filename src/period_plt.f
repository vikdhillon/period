
	SUBROUTINE PERIOD_PLT (Y, MXROW, MXSLOT, 
     + 			       NPTSARRAY, YERRORARRAY,
     +			       INFILEARRAY, COMMAND, 
     +                         MXVEC, OVERPLOT, QDP, OFFSET,
     +                         BASE, CMD, NCMD, MXCMD)

C=============================================================================
C Routine to call PLT from within PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 3-June-1991.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Now accepts command-line slot selection, VSD @Sheffield 21-April-2004.
C Modified to allow overplotting (with y offsets), VSD @Madrid 27-April-2004.
C Implemented extra commands in QDP mode, VSD @WHT 11-August-2005.
C Found a bug/feature in qdp/plt - when you try and plot a slot in which the 
C start and end of the xaxis differ only in the fifth decimal place (e.g. 
C x(first)=0.254945, x(last)=0.254951), the numbers on the x-axis go crazy.
C Obviously something to do with the fact that QDP/PLT (and/or PGPLOT) only 
C handle numbers with REAL*4 precision, VSD @Sheffield 27-April-2006.
C
C At long last, I've modified PERIOD_PLT to overplot data with differing 
C numbers of x-axis points (such as when NBLUE>1). 
C Vik Dhillon @ING 18-October-2007. 
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXSLOT, MXROW, MXVEC, MXCMD
	CHARACTER CMD(MXCMD)*72
	REAL      PDATA(MXROW, MXVEC)
	INTEGER   IERY(MXVEC), NVEC, NPTS, NCMD, IER

C-----------------------------------------------------------------------------
C PLT common block for fit parameters.
C-----------------------------------------------------------------------------

	INTEGER   MXPAR
	PARAMETER (MXPAR=50)
	INTEGER   MXMOD
	PARAMETER (MXMOD=4)
	REAL      PVAL, PLIM
	INTEGER   ICOMP, NTERM
	COMMON    /PARAM/ PVAL(MXPAR,MXMOD),PLIM(3,MXPAR,MXMOD),
     +		   	  ICOMP(2*MXPAR,MXMOD),NTERM(MXMOD)

C-----------------------------------------------------------------------------
C PERIOD_PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER FIRSTSLOT, LASTSLOT, J, K
        INTEGER COUNTER, NSLOT, SUM
	INTEGER NPTSARRAY(MXSLOT)
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT), OFFSET, DBASE
	LOGICAL YERROR, YERRORARRAY(MXSLOT), LCOMMAND
	LOGICAL OVERPLOT, QDP, BASE
        CHARACTER*12 COMMAND
	CHARACTER*72 INFILEARRAY(MXSLOT)
	DATA LCOMMAND /.FALSE./

C-----------------------------------------------------------------------------
C Determine slots from command line, if present.
C-----------------------------------------------------------------------------

	IF (COMMAND(3:4) .EQ. '  ' .OR. COMMAND(4:5) .EQ. '  '
     +      .OR. COMMAND(5:6) .EQ. '  ' .OR. COMMAND(6:7) .EQ. ' ') THEN
	   LCOMMAND = .FALSE.
	ELSE
	   READ(COMMAND(4:12),*,ERR=99)FIRSTSLOT, LASTSLOT
	   LCOMMAND = .TRUE.
	END IF

C-----------------------------------------------------------------------------
C Select slots to process.
C-----------------------------------------------------------------------------

	IF (.NOT. LCOMMAND) THEN
1	   WRITE(*,'(A,$)')
     + 'Enter first and last slots for input (0,0 to quit) : '
	   READ(*,*,ERR=1)FIRSTSLOT, LASTSLOT
	END IF
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
	IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
		GOTO 99
	END IF
	IF (OVERPLOT) THEN
	   SUM = 0
	   DO J = FIRSTSLOT, LASTSLOT
	      SUM = SUM + NPTSARRAY(J)
	   END DO
	   IF (SUM .GT. MXROW) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)
     + '** ERROR: Total number of points to plot > ',MXROW
	      WRITE(*,*)'** ERROR: Reduce number of slots to overplot.'
	      GOTO 99
	   END IF
	END IF

	COUNTER = 0
	NSLOT = 0
	DO J = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(J) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',J
		GOTO 99
	END IF
	IF (OVERPLOT) THEN
	IF (J .NE. FIRSTSLOT) THEN
c       IF (NPTSARRAY(J) .NE. NPTS) THEN
c	      WRITE(*,*)'** WARNING: Different x-axis in slot =',J
c	   END IF
c	   IF (Y(1,1,J) .NE. Y(1,1,FIRSTSLOT)) THEN
c	      WRITE(*,*)'** WARNING: Different x-axis in slot =',J
c	   END IF
	   IF (YERRORARRAY(J) .NEQV. YERRORARRAY(FIRSTSLOT)) THEN
	      IF (YERRORARRAY(J)) THEN
	      WRITE(*,*)'** WARNING: No error bars in slot = ',FIRSTSLOT
	      WRITE(*,*)'** WARNING: Error bars in slot = ',J
	      ELSE
	      WRITE(*,*)'** WARNING: Error bars in slot = ',FIRSTSLOT
	      WRITE(*,*)'** WARNING: No error bars in slot = ',J
	      END IF
	   END IF
	END IF
	END IF
	CMD(1) = 'DEVICE /XS'
	CMD(2) = 'PROMPT period-plt>'
	CMD(3) = 'LI S'
	CMD(4) = 'FO NO'
	CMD(5) = 'LAB T '//INFILEARRAY(J)
	IF (QDP) THEN
	   NCMD = 5
	END IF

	IF (OVERPLOT) THEN
	   CMD(6) = 'CO 2 ON 1'
	   CMD(7) = 'CO 3 ON 2'
	   CMD(8) = 'CO 4 ON 3'
	   CMD(9) = 'SKIP SINGLE'
	   IF (QDP) THEN
	      NCMD = 9
	   END IF
	ELSE
	   NPTS = NPTSARRAY(J)
	END IF

	YERROR = YERRORARRAY(J)

C-----------------------------------------------------------------------------
C Plot data (without overplotting).
C-----------------------------------------------------------------------------

	IF (.NOT. OVERPLOT) THEN
	   NVEC = 2
	   IF (YERROR) THEN
	      IERY(1) = 0
	      IERY(2) = 1
	   ELSE
	      IERY(1) = 0
	      IERY(2) = 0
	   END IF
	   DO K = 1, NPTSARRAY(J)
	      IF (BASE) THEN
		 DBASE = AINT(Y(1,1,J))
		 PDATA(K,1) = REAL(Y(K,1,J) - DBASE)
	      ELSE
		 PDATA(K,1) = REAL(Y(K,1,J))
	      END IF
	      PDATA(K,2) = REAL(Y(K,2,J))
	      IF (YERROR) THEN
		 PDATA(K,3) = REAL(Y(K,3,J))
	      END IF
	   END DO
	   WRITE(*,*)' '
	   WRITE(*,*)'** OK: Plotting slot number =',J
	   IF (BASE) THEN
	      WRITE(*,*)'** OK: x-axis is now MJD - ',DBASE
	   END IF
	   WRITE(*,*)' '
	   CALL PLT(PDATA, IERY, MXROW, NPTS, NVEC, CMD, NCMD, IER)

C-----------------------------------------------------------------------------
C Plot data (with overplotting).
C-----------------------------------------------------------------------------

	ELSE
	      DO K = 1, NPTSARRAY(J)
		 COUNTER = COUNTER + 1
		 IF (BASE) THEN
		    DBASE = AINT(Y(1,1,J)) 
		    PDATA(COUNTER,1) = REAL(Y(K,1,J) - DBASE)
		 ELSE
		    PDATA(COUNTER,1) = REAL(Y(K,1,J))
		 END IF
		 PDATA(COUNTER,2) = REAL(Y(K,2,J) + OFFSET*(DBLE(NSLOT)))
		 IF (YERROR) THEN
		    PDATA(COUNTER,3) = REAL(Y(K,3,J))
		 END IF
	      END DO
	      IERY(1) = 0 
	      IF (YERROR) THEN
		 IERY(2) = 1 
	      ELSE
		 IERY(2) = 0
	      END IF
	      COUNTER = COUNTER + 1
	      PDATA(COUNTER,1) = -1.2E-34
	      PDATA(COUNTER,2) = -1.2E-34
	      IF (YERROR) THEN
		 PDATA(COUNTER,3) = -1.2E-34
	      END IF
	END IF
	NSLOT = NSLOT + 1
	END DO
	NPTS = COUNTER
	NVEC = 2
	IF (OVERPLOT) THEN
	   WRITE(*,*)' '
           WRITE(*,*)'** OK: Plotting slot numbers =',
     + FIRSTSLOT, LASTSLOT
	   IF (BASE) THEN
	      WRITE(*,*)'** OK: x-axis is now MJD - ',DBASE
	   END IF
	   WRITE(*,*)' '
	   CALL PLT(PDATA, IERY, MXROW, NPTS, NVEC, CMD, NCMD, IER)
	END IF

99 	RETURN
	END
