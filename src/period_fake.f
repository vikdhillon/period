
 	SUBROUTINE PERIOD_FAKE (Y, MXROW, MXSLOT, 
     +                          NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY)

C=============================================================================
C Generates fake data with which to test the period finding programs. There
C are three options: (1) A strictly periodic dataset, (2) a chaotic 
C and (3) a random (Gaussian noise) dataset.
C
C Written by Vikram Singh Dhillon @LPO 25-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Added random option, Vik Dhillon @Durham 21-May-2010.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_FAKE declarations.
C-----------------------------------------------------------------------------

	INTEGER MAXSIN
	PARAMETER (MAXSIN = 20)
	DOUBLE PRECISION PERIOD(MAXSIN), AMPLITUDE(MAXSIN)
	DOUBLE PRECISION ZEROPT(MAXSIN), GAMMA(MAXSIN)
	DOUBLE PRECISION COEFF, INITIAL, INITVAL
	DOUBLE PRECISION PI, STARTPT, ENDPT, INTERVAL
	DOUBLE PRECISION AVE, SDEV
        REAL RANDOM, IR(97), GSET
        INTEGER IDUM, ISET, SEED, IY
	INTEGER NUMSIN, NUMPTS, COUNTER
	INTEGER I, J, K, L, SLOT, FIRSTSLOT, LASTSLOT
        INTEGER VALUES(8)
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	CHARACTER*72 INFILEARRAY(MXSLOT), OPTION*1
        CHARACTER*12 DATE, TIME, ZONE
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/

C-----------------------------------------------------------------------------
C Select slots to process.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
1	WRITE(*,'(A,$)')'Enter first and last slots for output
     + (0,0 to quit) : '
	READ(*,*,ERR=1)FIRSTSLOT, LASTSLOT
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
	IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
	    WRITE(*,*)ACHAR(7)
	    WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT	
	    GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Select periodic or chaotic data.
C-----------------------------------------------------------------------------

 	WRITE(*,*)' '
5	WRITE(*,'(A,$)')
     + '[P]eriodic, [C]haotic or [R]andom data ? [P] : '	
	READ(*,'(A)',ERR=5)OPTION
	CALL PERIOD_CASE (OPTION, .TRUE.)

C-----------------------------------------------------------------------------
C Calculate periodic fake data.
C-----------------------------------------------------------------------------

	IF (OPTION .EQ. 'P' .OR. OPTION .EQ. ' ') THEN
10	WRITE(*,'(A,$)')'Enter number of sine curves : '
	READ(*,*,ERR=10)NUMSIN
	IF (NUMSIN .LE. 0) GOTO 10
	IF (NUMSIN .GT. MAXSIN) THEN
	    WRITE(*,*)ACHAR(7)
	    WRITE(*,*)'** ERROR: Maximum number of sine curves = ',MAXSIN	
	    GOTO 99
	END IF
20	WRITE(*,'(A,$)')'Enter number of data points : '
	READ(*,*,ERR=20)NUMPTS
	IF (NUMPTS .LE. 0) GOTO 20
	IF (NUMPTS .GT. MXROW) THEN
	    WRITE(*,*)ACHAR(7)
	    WRITE(*,*)'** ERROR: Maximum number of data points = ',MXROW
	    GOTO 99
	END IF
30	WRITE(*,'(A,$)')'Enter range of data points : '
	READ(*,*,ERR=30)STARTPT, ENDPT
	DO J = 1, NUMSIN
	    WRITE(*,*)' '
	    WRITE(*,*)'SINE CURVE NUMBER = ',J
	    WRITE(*,*)' '
40	    WRITE(*,'(A,$)')'Enter period, semi-amplitude, zero point
     + and gamma : '	    
	    READ(*,*,ERR=40)PERIOD(J), AMPLITUDE(J), ZEROPT(J), GAMMA(J)
	    IF (PERIOD(J) .LE. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Invalid value for the period.'
		WRITE(*,*)' '
		GOTO 40
	    END IF
	END DO	
	WRITE(*,*)' '
	DO SLOT = FIRSTSLOT, LASTSLOT
	DO I = 1, MXROW
		Y(I,1,SLOT) = 0.0D0
		Y(I,2,SLOT) = 0.0D0		
		Y(I,3,SLOT) = 0.0D0
	END DO
	INTERVAL = (ENDPT - STARTPT) / (DBLE(NUMPTS) - 1.0D0)
	COUNTER = 0
	DO K = 0, NUMPTS-1
		COUNTER = COUNTER + 1
		Y(COUNTER,1,SLOT) = STARTPT + (INTERVAL * DBLE(K))
		DO L = 1, NUMSIN
		Y(COUNTER,2,SLOT) = Y(COUNTER,2,SLOT) + (GAMMA(L) + 
     +                    (AMPLITUDE(L) * DSIN(((2.0D0*PI)/PERIOD(L)) * 
     +                    (Y(COUNTER,1,SLOT)-ZEROPT(L)))))
	        END DO
	END DO
	INFILEARRAY(SLOT) = 'fake periodic data'
	YERRORARRAY(SLOT) = .FALSE.	
	DETRENDARRAY(SLOT) = .FALSE.	
	WRITE(*,*)'** OK: Filled slot = ',SLOT
	NPTSARRAY(SLOT) = COUNTER
	END DO

C-----------------------------------------------------------------------------
C Calculate chaotic fake data.
C-----------------------------------------------------------------------------

	ELSE IF (OPTION .EQ. 'C') THEN
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Logistic Equation Xn+1 = LAMBDA * Xn * (1-Xn)'
	WRITE(*,*)' '
50	WRITE(*,'(A,$)')'Enter LAMBDA : '
	READ(*,*,ERR=50)COEFF
	IF (COEFF .LT. 0. .OR. COEFF .GT. 4.) THEN
	    WRITE(*,*)ACHAR(7)
	    WRITE(*,*)'** ERROR: LAMBDA must lie between 0 and 4.'
	    GOTO 99
	END IF
60	WRITE(*,'(A,$)')'Enter initial value : '
	READ(*,*,ERR=60)INITVAL
70	WRITE(*,'(A,$)')'Enter number of data points : '
	READ(*,*,ERR=70)NUMPTS
	IF (NUMPTS .GT. MXROW) THEN
	    WRITE(*,*)ACHAR(7)
	    WRITE(*,*)'** ERROR: Maximum number of data points = ',MXROW
	    GOTO 99
	END IF
80	WRITE(*,'(A,$)')'Enter range of data points : '
	READ(*,*,ERR=80)STARTPT, ENDPT
	INTERVAL = (ENDPT - STARTPT) / (DBLE(NUMPTS) - 1.0D0)
	WRITE(*,*)' '
	DO SLOT = FIRSTSLOT, LASTSLOT
	DO I = 1, MXROW
		Y(I,1,SLOT) = 0.0D0
		Y(I,2,SLOT) = 0.0D0	
		Y(I,3,SLOT) = 0.0D0
	END DO
	COUNTER = 0
	INITIAL = INITVAL
	DO K = 0, NUMPTS-1
		COUNTER = COUNTER + 1
		Y(COUNTER,1,SLOT) = STARTPT + (INTERVAL * DBLE(K))
		Y(COUNTER,2,SLOT) = COEFF * INITIAL * (1.D0 - INITIAL)
		INITIAL = Y(COUNTER,2,SLOT)
	END DO
	INFILEARRAY(SLOT) = 'fake chaotic data'
	YERRORARRAY(SLOT) = .FALSE.	
	DETRENDARRAY(SLOT) = .FALSE.	
	WRITE(*,*)'** OK: Filled slot = ',SLOT
	NPTSARRAY(SLOT) = COUNTER
	END DO

C-----------------------------------------------------------------------------
C Calculate random data.
C-----------------------------------------------------------------------------

	ELSE IF (OPTION .EQ. 'R') THEN
 81	   WRITE(*,'(A,$)')'Enter mean : '
	   READ(*,*,ERR=81)AVE
 82	   WRITE(*,'(A,$)')'Enter standard deviation : '
	   READ(*,*,ERR=82)SDEV
 83	   WRITE(*,'(A,$)')'Enter range of data points : '
	   READ(*,*,ERR=83)STARTPT, ENDPT
 84	   WRITE(*,'(A,$)')'Enter number of data points : '
	   READ(*,*,ERR=84)NUMPTS
	   IF (NUMPTS .GT. MXROW) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)'** ERROR: Maximum number of data points = ',MXROW
	      GOTO 99
	   END IF
	   INTERVAL = (ENDPT - STARTPT) / (DBLE(NUMPTS) - 1.0D0)
	   
	   CALL DATE_AND_TIME (DATE, TIME, ZONE, VALUES)
	   SEED = 1 + (VALUES(7) * VALUES(8))
	   IDUM =  - ABS(SEED)
	   ISET = 0
	   CALL PERIOD_GASDEV (ISET, IDUM, RANDOM, IR, IY, GSET)

	   WRITE(*,*)' '
	   DO SLOT = FIRSTSLOT, LASTSLOT
	      DO I = 1, MXROW
		 Y(I,1,SLOT) = 0.0D0
		 Y(I,2,SLOT) = 0.0D0	
		 Y(I,3,SLOT) = 0.0D0
	      END DO
	      COUNTER = 0
	      INITIAL = INITVAL
	      DO K = 0, NUMPTS-1
		 COUNTER = COUNTER + 1
		 IF (MOD(ISET,2) .NE. 0) THEN
		    ISET = 2
		 ELSE 
		    ISET = 1
		 END IF
		 CALL PERIOD_GASDEV(ISET, IDUM, RANDOM, IR, IY, GSET)
		 Y(COUNTER,1,SLOT) = STARTPT + (INTERVAL * DBLE(K))
		 Y(COUNTER,2,SLOT) = AVE+(SDEV*DBLE(RANDOM))
	      END DO
	      INFILEARRAY(SLOT) = 'fake random data'
	      YERRORARRAY(SLOT) = .FALSE.	
	      DETRENDARRAY(SLOT) = .FALSE.	
	      WRITE(*,*)'** OK: Filled slot = ',SLOT
	      NPTSARRAY(SLOT) = COUNTER
	   END DO

	ELSE
	   GOTO 5
	END IF
	
 99	RETURN
	END
	
