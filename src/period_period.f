
	SUBROUTINE PERIOD_PERIOD (Y, MXROW, MXSLOT,
     +                            NPTSARRAY, YERRORARRAY, 
     +                            INFILEARRAY, DETRENDARRAY,
     +				  LOG, LOGUNIT, NWK, WORK1, WORK2,
     +                            ONES, WFREQ, DFREQ, D, W, R, B, C, S,
     +                            MXVEC, OVERPLOT, QDP, OFFSET, 
     +                            CMD, NCMD, MXCMD)

C==============================================================================
C The work-horse routine. Finds periodicities in data using a number of 
C different techniques.
C
C Written by Vikram Singh Dhillon @Sussex 22-February-1992.
C All LSELECT restrictions removed by Vik Dhillon @ING 20-May-2002.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 15-April-2004.
C SEED now set to the time in milliseconds, VSD@Nether Edge 18-April-2004.
C Added the SLIDE option, Vik Dhillon @WHT 10-August-2005 (Izzy's birthday!)
C Added the AMPLITUDE option, Vik Dhillon @NTT 13-June-2009
C Removed loops over real variables, VSD @Sheffield 27-August-2009
C Added new CDF significance option, VSD @Sheffield 14-May-2010
C==============================================================================

	IMPLICIT NONE

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
     +                    ICOMP(2*MXPAR,MXMOD),NTERM(MXMOD)

C-----------------------------------------------------------------------------
C PERIOD_PERIOD declarations.
C-----------------------------------------------------------------------------

        INTEGER MXROW, MXSLOT, MXVEC, MXCMD
	INTEGER NPTSARRAY(MXSLOT)
	INTEGER FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, SLOTOUT
	INTEGER SLOTFIRST, SLOTLAST
	INTEGER I, J, NFREQ, COUNTER, NOPTION, LOGUNIT, IY
	INTEGER LOOP, ISTEP, NDATA, IFAIL
	INTEGER NP, NWK, NOUT, NCL, NBIN
	INTEGER SAMPLE, NPERMS, SEED, EL, MAXPERMS, ELMAX, ELMIN
	INTEGER CFREQ_LEN, CSTAT_LEN
	INTEGER NUMPTS, STEP, NUMSTEPS, IDEV, PGBEG
        INTEGER VALUES(8), NCMD
	INTEGER SHADE, SHADE_MIN, SHADE_MAX
	PARAMETER (MAXPERMS = 10000)
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	DOUBLE PRECISION STAT(MAXPERMS+1), SIG(2,MXSLOT)
	DOUBLE PRECISION WORK1(NWK), WORK2(NWK)
	DOUBLE PRECISION YRAN(MXROW), ERAN(MXROW)
	DOUBLE PRECISION FDATA(MXROW), WK1(MXROW), WK2(MXROW)
	DOUBLE PRECISION XDATA(MXROW), YDATA(MXROW), YERR(MXROW)
	DOUBLE PRECISION ZEROPT, FREQ, GAMMA, KVEL, PHASE, VAR(6), F
	DOUBLE PRECISION MINFREQ, MAXFREQ, FINTERVAL
	DOUBLE PRECISION FMIN, FMAX, FINT
	DOUBLE PRECISION MINF, MAXF
	DOUBLE PRECISION AVE, ADEV, SDEV, VARI
	DOUBLE PRECISION GAIN, STRING, PDM, WBIN
	DOUBLE PRECISION PEAK, TROUGH, PERIOD, PERROR
	DOUBLE PRECISION OFFSET, DBASE
	DOUBLE PRECISION SHUFFLE1, SHUFFLE2
	DOUBLE PRECISION SIGNOISE, SIGPERIOD, ERRNOISE, ERRPERIOD
	DOUBLE PRECISION PROCFRAC, STEPFRAC
	DOUBLE PRECISION WHITE, BLACK, MAXWHITE, MAXBLACK
	DOUBLE PRECISION ONES(0:MXROW-1), WFREQ(0:(2*MXROW)-1)
        DOUBLE PRECISION DFREQ(0:MXROW-1)
        DOUBLE COMPLEX D(0:MXROW-1), W(0:(2*MXROW)-1)
	DOUBLE COMPLEX R(0:MXROW-1)
	DOUBLE COMPLEX B(0:MXROW-1), C(0:MXROW-1), S(0:MXROW-1)
	REAL RANDOM, IR(97)
	REAL XPTS(4), YPTS(4), XMIN, XMAX, YMIN, YMAX, P, PMIN, PMAX
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	LOGICAL PERIOD_PARSE, LSELECT, LOG
	LOGICAL LPEAK, LOGWRITE, LSIG, LFAP1, LCDF
	LOGICAL OVERPLOT, QDP, BASE, LREPLOT
	CHARACTER*1 COMMAND*12, REPLY
        CHARACTER*12 DATE, TIME, ZONE
	CHARACTER*32 CFREQ, CSTAT
	CHARACTER*72 INFILEARRAY(MXSLOT), CMD(MXCMD)
	DATA ISTEP /50/
	DATA LSIG, LCDF, NPERMS /.FALSE., .FALSE., 0/
	DATA LREPLOT /.FALSE./
	DATA MINFREQ, MAXFREQ, FINTERVAL /0.0D0, 0.0D0, 0.0D0/

C-----------------------------------------------------------------------------
C Initialise slot selection. 
C-----------------------------------------------------------------------------

	LSELECT = .FALSE.

C-----------------------------------------------------------------------------
C User prompt.
C-----------------------------------------------------------------------------

10     WRITE(*,'(A,$)')'period_period> '
       READ(*,'(A)',ERR=10) COMMAND
       CALL PERIOD_CASE (COMMAND, .TRUE.)
       IF( PERIOD_PARSE( COMMAND, 'SELECT')   )GOTO 100
       IF( PERIOD_PARSE( COMMAND, 'FREQ' )    )GOTO 200
       IF( PERIOD_PARSE( COMMAND, 'CHISQ')    )GOTO 300
       IF( PERIOD_PARSE( COMMAND, 'FT')       )GOTO 400
       IF( PERIOD_PARSE( COMMAND, 'SCARGLE')  )GOTO 500
       IF( PERIOD_PARSE( COMMAND, 'CLEAN')    )GOTO 600
       IF( PERIOD_PARSE( COMMAND, 'STRING')   )GOTO 700
       IF( PERIOD_PARSE( COMMAND, 'PDM')      )GOTO 800
       IF( PERIOD_PARSE( COMMAND(1:4), 'HELP'))GOTO 900
       IF( PERIOD_PARSE( COMMAND, 'PEAKS')    )GOTO 1000
       IF( PERIOD_PARSE( COMMAND, 'SIG')      )GOTO 1100
       IF( PERIOD_PARSE( COMMAND(1:3), 'PLT') )GOTO 1200
       IF( PERIOD_PARSE( COMMAND, 'SLIDE')    )GOTO 1300
       IF( PERIOD_PARSE( COMMAND, 'AMPLITUDE'))GOTO 1400
       IF( PERIOD_PARSE( COMMAND, '?')        )GOTO 20
       IF( PERIOD_PARSE( COMMAND, 'QUIT')     )RETURN
       IF( PERIOD_PARSE( COMMAND, 'EXIT')     )RETURN
       GOTO 10

C-----------------------------------------------------------------------------
C Menu.
C-----------------------------------------------------------------------------

20     WRITE(*,*)' '
       WRITE(*,*)'Options.'
       WRITE(*,*)'--------'
       WRITE(*,*)' '
       WRITE(*,*)'   SELECT    --  Select data slots.'
       WRITE(*,*)'   FREQ      --  Set/show frequency search limits.'
       WRITE(*,*)'   AMPLITUDE --  Amplitude spectrum.'
       WRITE(*,*)'   CHISQ     --  Chi-squared of sine fit vs freq.'
       WRITE(*,*)'   CLEAN     --  CLEANed power spectrum.'
       WRITE(*,*)'   FT        --  Discrete Fourier power spectrum.'
       WRITE(*,*)'   INTERM    --  Period of intermittent signal.'
       WRITE(*,*)'   PDM       --  Phase dispersion minimization.'
       WRITE(*,*)'   SCARGLE   --  Lomb-Scargle normalized periodogram.'
       WRITE(*,*)'   SLIDE     --  Sliding Scargle periodogram.'
       WRITE(*,*)'   STRING    --  String-length vs frequency.'
       WRITE(*,*)'   PEAKS     --  Calculate period from periodogram.'
       WRITE(*,*)'   SIG       --  Enable/disable significance calc.'
       WRITE(*,*)'   PLT       --  Call PLT.'
       WRITE(*,*)'   HELP      --  On-line help.'
       WRITE(*,*)'   QUIT      --  Quit PERIOD_PERIOD.'
       WRITE(*,*)'  '
       GOTO 10

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

100	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		LSELECT = .FALSE.		
		GOTO 10
	ELSE
		WRITE(*,*)' '
		WRITE(*,*)'** OK: Input and output slots selected.'
		LSELECT = .TRUE.
	END IF
	GOTO 10

C-----------------------------------------------------------------------------
C Set or show frequency search limits. 
C-----------------------------------------------------------------------------

200	WRITE(*,*)' '
	WRITE(*,*)'FREQUENCY SEARCH LIMITS: '
	WRITE(*,*)' '
	IF (MINFREQ .LE. 0.) THEN
	WRITE(*,*)'1. Minimum frequency  = 0.0'
	ELSE 
	WRITE(*,*)'1. Minimum frequency  = ',MINFREQ
	END IF
	IF (MAXFREQ .LE. 0.) THEN
	WRITE(*,*)'2. Maximum frequency  = NYQUIST'
	ELSE 
	WRITE(*,*)'2. Maximum frequency  = ',MAXFREQ
	END IF
	IF (FINTERVAL .EQ. 0.) THEN
	WRITE(*,*)'3. Frequency interval = (FMAX-FMIN)/MXROW*0.5'
	ELSE
	WRITE(*,*)'3. Frequency interval = ',FINTERVAL
	END IF
	WRITE(*,*)' '
201	WRITE(*,'(A,$)')'Enter number you wish to change (0 to quit) : '
	READ(*,*,ERR=201)NOPTION
	IF (NOPTION .EQ. 1) THEN
202	WRITE(*,'(A,$)')'Enter minimum frequency  (0 for default) : '
		READ(*,*,ERR=202)MINFREQ
	ELSE IF (NOPTION .EQ. 2) THEN
203	WRITE(*,'(A,$)')'Enter maximum frequency  (0 for default) : '
		READ(*,*,ERR=203)MAXFREQ
	ELSE IF (NOPTION .EQ. 3) THEN
204	WRITE(*,'(A,$)')'Enter frequency interval (0 for default,'//
     + ' -ve for number of points) : '
		READ(*,*,ERR=204)FINTERVAL
	ELSE IF (NOPTION .EQ. 0) THEN
		GOTO 10
	END IF
	GOTO 200

C-----------------------------------------------------------------------------
C Least-squares sine-curve fitting.
C-----------------------------------------------------------------------------

300	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	IF (FMIN .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No sine fit for zero frequency.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	ZEROPT = AVE
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	COUNTER = 0
	LOOP = 1
	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
	   DO I = 1, NDATA
	      XDATA(I) = Y(I,1,SLOT)
	      IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
	      YDATA(I) = YRAN(I)
	      IF (.NOT. YERRORARRAY(SLOT)) THEN
		 YERR(I) = 1.0D0
	      ELSE 
		 IF (SAMPLE .EQ. 1) ERAN(I) = Y(I,3,SLOT)
		 YERR(I) = ERAN(I)
	      END IF
	   END DO
	   PERIOD = 1.0D0 / FREQ
	   CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA,
     + ZEROPT, PERIOD)
	   IF (IFAIL .EQ. 1) THEN
	      GOTO 10			
	   END IF 	
	   CALL PERIOD_SINFIT (XDATA, YDATA, YERR, NDATA, 1.0D0,
     + GAMMA, KVEL, PHASE, VAR, NP, F, IFAIL)	
	   IF (IFAIL .EQ. 1) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)'** ERROR: Sine fit unsuccessful.'
	      GOTO 10			
	   END IF
	   COUNTER = COUNTER + 1
	   IF (SAMPLE .EQ. 1) THEN
	      IF (COUNTER .EQ. (LOOP*ISTEP)+1) THEN
		 WRITE(CFREQ,*)FREQ
		 CFREQ_LEN = LEN_TRIM(CFREQ)
		 WRITE(CSTAT,*)F/(DBLE(NP)-3.0D0)
		 CSTAT_LEN = LEN_TRIM(CSTAT)
		 WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', reduced chi-squared = '//CSTAT(1:CSTAT_LEN)
		 LOOP = LOOP + 1
	      END IF
	   END IF

C-----------------------------------------------------------------------------
C If the first permutation, load the output slot.
C-----------------------------------------------------------------------------
	   
	   IF (SAMPLE .EQ. 1) THEN
	      Y(COUNTER,1,SLOTOUT) = FREQ
	      Y(COUNTER,2,SLOTOUT) = F/(DBLE(NP)-3.0D0)
	      Y(COUNTER,3,SLOTOUT) = 0.0D0
	   END IF
	   WK2(COUNTER) = F/(DBLE(NP)-3.0D0)
	END DO

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     + 'reduced chi**2 vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = COUNTER
	SIG(1,SLOTOUT) = -1.0D0
	SIG(2,SLOTOUT) = -1.0D0
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = 1.0D+32
		DO I = 1, COUNTER
		IF (WK2(I) .LT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .LE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation. Note that EL can never get to
C NDATA+1 as RANDOM can never get above 0.999999 (I checked!)
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		      SHUFFLE1 = ERAN(I) 
		      SHUFFLE2 = ERAN(EL)
		      ERAN(I) = SHUFFLE2
		      ERAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .LE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C Discrete Fourier power spectrum.
C-----------------------------------------------------------------------------

400	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF 	

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		XDATA(I) = Y(I,1,SLOT)
		IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
		YDATA(I) = YRAN(I)
	END DO
	CALL PERIOD_FT (XDATA, YDATA, NDATA, MXROW, FMIN, FMAX, 
     +			FINT, WK1, WK2, NOUT, SAMPLE)

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	DO I = 1, NOUT
		Y(I,1,SLOTOUT) = WK1(I)
		Y(I,2,SLOTOUT) = WK2(I)
		Y(I,3,SLOTOUT) = 0.0D0
	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     + 'normalized power vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NOUT
	SIG(1,SLOTOUT) = -1.0D0
	SIG(2,SLOTOUT) = -1.0D0
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = -1.0D+32
		DO I = 1, NOUT
		IF (WK2(I) .GT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .GE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .GE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C Fast evaluation of the Lomb-Scargle statistic for each frequency.
C-----------------------------------------------------------------------------

500	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF 	

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		XDATA(I) = Y(I,1,SLOT)
		IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
		YDATA(I) = YRAN(I)
	END DO
	CALL PERIOD_SCARGLE (XDATA, YDATA, NDATA, WORK1, WORK2, NWK, 
     +			     NOUT, IFAIL, FMIN, FMAX, FINT, SAMPLE)
	IF (IFAIL .EQ. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Lomb-Scargle statistic calculation
     + unsuccessful.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	DO I = 1, NOUT
		Y(I,1,SLOTOUT) = WORK1(I)
		Y(I,2,SLOTOUT) = WORK2(I)
		Y(I,3,SLOTOUT) = 0.0D0
	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     + 'Lomb-Scargle statistic vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NOUT
	SIG(1,SLOTOUT) = -1.0D0
	SIG(2,SLOTOUT) = -1.0D0
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = -1.0D+32
		DO I = 1, NOUT
		IF (WORK2(I) .GT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WORK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WORK2(I) .GE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .GE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C CLEANed power spectrum.
C-----------------------------------------------------------------------------

600	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF 	
	WRITE(*,*)' '
601     WRITE(*,'(A,$)')'Enter number of CLEAN iterations : '
        READ(*,*,ERR=601)NCL
602     WRITE(*,'(A,$)')'Enter loop gain : '
        READ(*,*,ERR=602)GAIN
	IF ((GAIN .LE. 0.) .OR. (GAIN .GE. 2.)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Gain must lie between 0 and 2.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	FMIN = 0.0D0
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		XDATA(I) = Y(I,1,SLOT)
		IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
		YDATA(I) = YRAN(I)
	END DO
	CALL PERIOD_CLEAN (XDATA, YDATA, NDATA, MXROW, FMIN, FMAX, 
     +			   FINT, NCL, GAIN, WK1, WK2, NOUT, SAMPLE,
     +                     ONES, WFREQ, DFREQ, D, W, R, B, C, S)
	IF (NOUT .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: CLEAN calculation unsuccessful.'
		NPTSARRAY(SLOTOUT) = NOUT
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	DO I = 1, NOUT
		Y(I,1,SLOTOUT) = WK1(I)
		Y(I,2,SLOTOUT) = WK2(I)
		Y(I,3,SLOTOUT) = 0.0D0
	END DO
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     +  'CLEANed power vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NOUT
	SIG(1,SLOTOUT) = -1.0D0
	SIG(2,SLOTOUT) = -1.0D0
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = -1.0D+32
		DO I = 1, NOUT
		IF (WK2(I) .GT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .GE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .GE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C String-length method.
C-----------------------------------------------------------------------------

700	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF 	

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	IF (FMIN .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No string-length for zero frequency.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	ZEROPT = AVE
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Fold on trial frequencies and calculate string-lengths.
C-----------------------------------------------------------------------------

	COUNTER = 0
	LOOP = 1
	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
	   DO I = 1, NDATA
	      XDATA(I) = Y(I,1,SLOT)
	      IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
	      YDATA(I) = YRAN(I)
	   END DO
	   PERIOD = 1.0D0 / FREQ
	   CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, 
     + ZEROPT, PERIOD)
	   CALL PERIOD_STRING (XDATA, YDATA, NDATA,
     + STRING, IFAIL)
	   IF (IFAIL .EQ. 1) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)
     + '** ERROR: String-length calculation unsuccessful.'
	      GOTO 10			
	   END IF 	
	   COUNTER = COUNTER + 1
	   IF (SAMPLE .EQ. 1) THEN
	      IF (COUNTER .EQ. (LOOP*ISTEP)+1) THEN
		 WRITE(CFREQ,*)FREQ
		 CFREQ_LEN = LEN_TRIM(CFREQ)
		 WRITE(CSTAT,*)STRING
		 CSTAT_LEN = LEN_TRIM(CSTAT)
		 WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', string length = '//CSTAT(1:CSTAT_LEN)
		 LOOP = LOOP + 1
	      END IF
	   END IF

C-----------------------------------------------------------------------------
C If the first permutation, load the output slot.
C-----------------------------------------------------------------------------
	   
	   IF (SAMPLE .EQ. 1) THEN
	      Y(COUNTER,1,SLOTOUT) = FREQ
	      Y(COUNTER,2,SLOTOUT) = STRING
	      Y(COUNTER,3,SLOTOUT) = 0.0D0
	   END IF
	   WK2(COUNTER) = STRING
	END DO

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     + 'string length vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = COUNTER
	SIG(1,SLOTOUT) = -1.0D0
	SIG(2,SLOTOUT) = -1.0D0
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = 1.0D+32
		DO I = 1, COUNTER
		IF (WK2(I) .LT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .LE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .LE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C Phase dispersion minimization (PDM) method.
C-----------------------------------------------------------------------------

800	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF 	
	WRITE(*,*)' '
801     WRITE(*,'(A,$)')'Enter number of bins : '
        READ(*,*,ERR=801)NBIN
	IF ((NBIN .LT. 2) .OR. (NBIN .GT. MXROW)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Number of bins must lie in the
     + range 2 to ',MXROW
		GOTO 10
	END IF
802     WRITE(*,'(A,$)')'Enter width of each bin : '
        READ(*,*,ERR=802)WBIN
	IF ((WBIN .LE. 0.) .OR. (WBIN .GE. 1.)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Width of bins must be greater
     + than 0 and less than 1'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	IF (FMIN .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No PDM statistic for zero frequency.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	ZEROPT = AVE

C-----------------------------------------------------------------------------
C Calculate variance of whole dataset.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,2,SLOT)
	END DO
	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Fold on trial frequencies and calculate PDM statistic.
C-----------------------------------------------------------------------------

	COUNTER = 0
	LOOP = 1
	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
	   DO I = 1, NDATA
	      XDATA(I) = Y(I,1,SLOT)
	      IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT)
	      YDATA(I) = YRAN(I)
	   END DO
	   PERIOD = 1.0D0 / FREQ
	   CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, 
     + ZEROPT, PERIOD)
	   IF (IFAIL .EQ. 1) THEN
	      GOTO 10			
	   END IF 	
	   CALL PERIOD_PDM (XDATA, YDATA, NDATA, NBIN, WBIN,
     + VARI, PDM, MXROW, IFAIL)
	   IF (IFAIL .EQ. 1) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)
     + '** ERROR: PDM statistic calculation unsuccessful.'
	      GOTO 10			
	   END IF 	
	   COUNTER = COUNTER + 1
	   IF (SAMPLE .EQ. 1) THEN
	      IF (COUNTER .EQ. (LOOP*ISTEP)+1) THEN
		 WRITE(CFREQ,*)FREQ
		 CFREQ_LEN = LEN_TRIM(CFREQ)
		 WRITE(CSTAT,*)PDM
		 CSTAT_LEN = LEN_TRIM(CSTAT)
		 WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', PDM statistic = '//CSTAT(1:CSTAT_LEN)
		 LOOP = LOOP + 1
	      END IF
	   END IF

C-----------------------------------------------------------------------------
C If the first permutation, load the output slot.
C-----------------------------------------------------------------------------

	   IF (SAMPLE .EQ. 1) THEN
	      Y(COUNTER,1,SLOTOUT) = FREQ
	      Y(COUNTER,2,SLOTOUT) = PDM
	      Y(COUNTER,3,SLOTOUT) = 0.0D0
	   END IF
	   WK2(COUNTER) = PDM
	END DO

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	YERRORARRAY(SLOTOUT) = .FALSE.
	DETRENDARRAY(SLOTOUT) = .FALSE.
	INFILEARRAY(SLOTOUT) = 
     + 'PDM statistic vs frequency, '//INFILEARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = COUNTER
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	IF (LSIG) THEN
	WRITE(*,*)' '
	END IF
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = 1.0D+32
		DO I = 1, COUNTER
		IF (WK2(I) .LT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .LE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	SIGNOISE = 0.0D0
	DO I = 2, NPERMS+1
		IF (STAT(I) .LE. STAT(1)) THEN
		SIGNOISE = SIGNOISE + 1.0D0
		END IF
	END DO
	SIG(1,SLOTOUT) = DBLE(NPERMS)
	SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C On-line help.
C-----------------------------------------------------------------------------

900	CALL PERIOD_HELP (COMMAND)
	GOTO 10

C-----------------------------------------------------------------------------
C Calculate periods from peaks (and troughs) in periodogram.
C-----------------------------------------------------------------------------

1000	WRITE(*,*)' '
1001	WRITE(*,'(A,$)')'Enter first and last slots containing'//
     + ' periodograms (0,0 to quit) : '
	READ(*,*,ERR=1001)SLOTFIRST, SLOTLAST
	IF (SLOTFIRST .LE. 0 .OR. SLOTLAST .LE. 0) THEN
		GOTO 10
	ELSE IF (SLOTFIRST .GT. MXSLOT .OR. SLOTLAST .GT. MXSLOT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
		GOTO 10
	END IF
1002	WRITE(*,'(A,$)')
     + 'Enter frequency range to analyse (0,0 for whole range) : '
	READ(*,*,ERR=1002)MINF, MAXF
	IF (LOG) THEN
1003	WRITE(*,'(A,$)')'Write results to the log file ? [Y] : '
	READ(*,'(A)',ERR=1003)REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.)
	IF (REPLY .EQ. 'N') THEN
		LOGWRITE = .FALSE.
	ELSE 
		LOGWRITE = .TRUE.
	END IF
	ELSE
		LOGWRITE = .FALSE.
	END IF

C-----------------------------------------------------------------------------
C Loop through periodograms.
C-----------------------------------------------------------------------------

	DO SLOT = SLOTFIRST, SLOTLAST	
	NDATA = NPTSARRAY(SLOT)
	IF (NDATA .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	IF (MINF .LE. 0) MINF = Y(1,1,SLOT)
	IF (MAXF .LE. 0) MAXF = Y(NDATA,1,SLOT)
	LFAP1 = .FALSE.
	IF (MINF .EQ. Y(1,1,SLOT)) THEN
	IF (MAXF .EQ. Y(NDATA,1,SLOT)) THEN
		LFAP1 = .TRUE.
	END IF
	END IF
	IF (INFILEARRAY(SLOT)(1:14) .EQ. 'reduced chi**2') THEN
		LPEAK = .FALSE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'Lomb-Scargle s') THEN
		LPEAK = .TRUE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'normalized pow') THEN
		LPEAK = .TRUE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'CLEANed power ') THEN
		LPEAK = .TRUE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'string length ') THEN
		LPEAK = .FALSE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'PDM statistic ') THEN
		LPEAK = .FALSE.
		GOTO 1010
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'amplitude vs f') THEN
		LPEAK = .TRUE.
		GOTO 1010
	ELSE
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Input data is not a periodogram.'
		GOTO 10
	END IF
	IF (MAXF .GT. Y(NDATA,1,SLOT)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Invalid frequency range.'
		GOTO 10
	ELSE IF (MINF .LT. Y(1,1,SLOT)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Invalid frequency range.'
		GOTO 10
	ELSE IF (MINF .GE. MAXF) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Invalid frequency range.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Search for highest (or lowest) peak in periodogram between given 
C frequency range.
C-----------------------------------------------------------------------------

1010	CONTINUE
	EL = 0
	PEAK = -1.0D+32
	TROUGH = 1.0D+32
	DO 1020 I = 1, NDATA
		IF (Y(I,1,SLOT) .LT. MINF) THEN
			GOTO 1020
		ELSE IF (Y(I,1,SLOT) .GT. MAXF) THEN
			GOTO 1030
		END IF
		IF (LPEAK) THEN
		IF (Y(I,2,SLOT) .GT. PEAK) THEN
			PEAK = Y(I,2,SLOT)
			EL = I
		END IF
		ELSE 
		IF (Y(I,2,SLOT) .LT. TROUGH) THEN
			TROUGH = Y(I,2,SLOT)
			EL = I
		END IF
		END IF
1020	CONTINUE
1030	CONTINUE
	ELMAX = MAX (EL-1, 1)
	ELMIN = MIN (EL+1, NDATA)
	IF (Y(EL,1,SLOT) .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Peak is at infinite period or
     + frequency range too small.'
		GOTO 10
	ELSE
		PERIOD = 1.0D0 / Y(EL,1,SLOT)
	END IF
	PERROR = 0.5D0 * DABS((1.0D0/Y(ELMIN,1,SLOT)) - 
     + (1.0D0/Y(ELMAX,1,SLOT)))

C-----------------------------------------------------------------------------
C Determine significances.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		IF (SIG(1,SLOT) .GE. 0.) THEN
			SIGNOISE = SIG(2,SLOT)
			IF (SIGNOISE .EQ. 0.) THEN
				ERRNOISE = -1.0D0
			ELSE 
				ERRNOISE = DSQRT ( (SIGNOISE *
     +                          (1.0D0 - SIGNOISE)) / SIG(1,SLOT) ) 
			END IF
			SIGPERIOD = Y(EL,3,SLOT) / SIG(1,SLOT)
			IF (SIGPERIOD .EQ. 0.) THEN
				ERRPERIOD = -1.0D0
			ELSE
				ERRPERIOD = DSQRT ( (SIGPERIOD * 
     +                          (1.0D0 - SIGPERIOD)) / SIG(1,SLOT) ) 
			END IF
		ELSE
			SIGNOISE = -1.0D0
			ERRNOISE = -1.0D0
			SIGPERIOD = -1.0D0
			ERRPERIOD = -1.0D0
		END IF
	END IF

C-----------------------------------------------------------------------------
C Output information to screen.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	WRITE(*,*)'** OK: Results for slot number = ',SLOT
	WRITE(*,*)'** OK: ',INFILEARRAY(SLOT)(1:65)
	WRITE(*,*)'** OK: Minimum frequency searched  = ',MINF
	WRITE(*,*)'** OK: Maximum frequency searched  = ',MAXF
	IF (INFILEARRAY(SLOT)(1:14) .EQ. 'string length ') THEN
	WRITE(*,*)'** OK: Minimum string-length       = ',TROUGH
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'PDM statistic ') THEN
	WRITE(*,*)'** OK: Minimum PDM statistic       = ',TROUGH
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'reduced chi**2') THEN
	WRITE(*,*)'** OK: Minimum reduced chi-squared = ',TROUGH
	END IF
	IF (LPEAK) THEN
	WRITE(*,*)'** OK: Maximum power               = ',PEAK
	END IF
	WRITE(*,*)'** OK: Frequency                   = ',Y(EL,1,SLOT)
	WRITE(*,*)'** OK: Period                      = ',PERIOD
	WRITE(*,*)'** OK: Minimum error in period     = ',PERROR
	IF (LSIG) THEN
	IF (LFAP1) THEN
	WRITE(*,*)'** OK: False alarm probability 1   = ',SIGNOISE
	WRITE(*,*)'** OK: Error in FAP1               = ',ERRNOISE
	END IF
	WRITE(*,*)'** OK: False alarm probability 2   = ',SIGPERIOD
	WRITE(*,*)'** OK: Error in FAP2               = ',ERRPERIOD
	END IF

C-----------------------------------------------------------------------------
C Output information to log file if requested.
C-----------------------------------------------------------------------------

	IF (LOGWRITE) THEN
	WRITE(LOGUNIT,*)' '
	WRITE(LOGUNIT,*)'** OK: Results for slot number = ',SLOT
	WRITE(LOGUNIT,*)'** OK: ',INFILEARRAY(SLOT)
	WRITE(LOGUNIT,*)'** OK: Minimum frequency searched  = ',MINF
	WRITE(LOGUNIT,*)'** OK: Maximum frequency searched  = ',MAXF
	IF (INFILEARRAY(SLOT)(1:14) .EQ. 'string length ') THEN
	WRITE(LOGUNIT,*)'** OK: Minimum string-length       = ',TROUGH
 	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'PDM statistic ') THEN
	WRITE(LOGUNIT,*)'** OK: Minimum PDM statistic       = ',TROUGH
	ELSE IF (INFILEARRAY(SLOT)(1:14) .EQ. 'reduced chi**2') THEN
	WRITE(LOGUNIT,*)'** OK: Minimum reduced chi-squared = ',TROUGH
	END IF
	IF (LPEAK) THEN
	WRITE(LOGUNIT,*)'** OK: Maximum power               = ',PEAK
	END IF
	WRITE(LOGUNIT,*)'** OK: Frequency                   = ',
     + Y(EL,1,SLOT)
	WRITE(LOGUNIT,*)'** OK: Period                      = ',PERIOD
	WRITE(LOGUNIT,*)'** OK: Minimum error in period     = ',PERROR
	IF (LSIG) THEN
	IF (LFAP1) THEN
	WRITE(LOGUNIT,*)'** OK: False alarm probability 1   = ',SIGNOISE
	WRITE(LOGUNIT,*)'** OK: Error in FAP1               = ',ERRNOISE
	END IF
	WRITE(LOGUNIT,*)'** OK: False alarm probability 2   = ',SIGPERIOD
	WRITE(LOGUNIT,*)'** OK: Error in FAP2               = ',ERRPERIOD
	END IF
	WRITE(LOGUNIT,'(A)')'.'
	END IF
	END DO
	GOTO 10	

C-----------------------------------------------------------------------------
C Calculate significance of period.
C-----------------------------------------------------------------------------

1100	WRITE(*,*)' '
	IF (.NOT. LSIG) THEN
1101	WRITE(*,'(A,$)')'Enter number of permutations in sample : '
	READ(*,*,ERR=1101)NPERMS
	IF (NPERMS .LT. 3) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Minimum number of permutations
     + = 3.'
		NPERMS = 0
		GOTO 10
	ELSE IF (NPERMS .GT. MAXPERMS) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Maximum number of permutations
     + = ',MAXPERMS
		NPERMS = 0
		GOTO 10
	END IF
        CALL DATE_AND_TIME (DATE, TIME, ZONE, VALUES)
        SEED = -ABS(1 + (VALUES(7) * VALUES(8)))
	CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
	LSIG = .TRUE.
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Significance calculation enabled.'
	ELSE
	LSIG = .FALSE.
	NPERMS = 0
	WRITE(*,*)'** OK: Significance calculation disabled.'
	END IF
	GOTO 10

C-----------------------------------------------------------------------------
C Call PLT. Turn off BASE subtraction, if on, to prevent the plotted
C frequencies from becoming messed up.
C-----------------------------------------------------------------------------

1200	BASE = .FALSE.
        CALL PERIOD_PLT (Y, MXROW, MXSLOT, 
     + 			 NPTSARRAY, YERRORARRAY, 
     +			 INFILEARRAY, COMMAND,
     +                   MXVEC, OVERPLOT, QDP, OFFSET,
     +                   BASE, CMD, NCMD, MXCMD)
	GOTO 10

C-----------------------------------------------------------------------------
C Sliding Scargle periodogram.
C-----------------------------------------------------------------------------

1300	WRITE(*,'(A,$)')
     +       'Enter slot for input (0 to quit) : '
        READ(*,*,ERR=1300)SLOT
	IF (SLOT .EQ. 0) THEN
           GOTO 10
        ELSE IF (SLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number =',MXSLOT
           GOTO 10
        END IF

C-----------------------------------------------------------------------------
C Prompt for slide parameters.
C-----------------------------------------------------------------------------

1310	WRITE(*,'(A,$)')'Enter fraction of x-axis to process : '
	READ(*,*,ERR=1310)PROCFRAC
	IF (PROCFRAC .GT. 1.D0 .OR. PROCFRAC .LE. 0.D0) THEN
	   GOTO 1310
	END IF
1320	WRITE(*,'(A,$)')
     + 'Enter fraction of x-axis to step (0 for every point) : '
	READ(*,*,ERR=1320)STEPFRAC
	IF (STEPFRAC .GT. 1.D0 .OR. STEPFRAC .LT. 0.D0) THEN
	   GOTO 1320
	END IF

C-----------------------------------------------------------------------------
C Check slot.
C-----------------------------------------------------------------------------

	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	NDATA = NPTSARRAY(SLOT)
	WRITE(*,*)'** OK: Number of data points loaded =',NDATA

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

 	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	WRITE(*,*)' '

C-----------------------------------------------------------------------------
C Setup step parameters.
C-----------------------------------------------------------------------------

	NUMPTS = INT(PROCFRAC*DBLE(NDATA))
	IF (STEPFRAC .EQ. 0.D0) THEN
	   STEP = 1
	ELSE
	   STEP = NINT(STEPFRAC*DBLE(NDATA))
	END IF
	NUMSTEPS = NINT(DBLE((NDATA-NUMPTS)/STEP))+1
	WRITE(*,*)'** OK: Number of data points per step  =',STEP
	WRITE(*,*)'** OK: Number of steps =',NUMSTEPS
	WRITE(*,*)' '

C-----------------------------------------------------------------------------
C Loop through steps.
C-----------------------------------------------------------------------------

1330	DO J = 1, NUMSTEPS

C-----------------------------------------------------------------------------
C Load data arrays.
C-----------------------------------------------------------------------------

	COUNTER = 0
	DO I = 1+(J*STEP-STEP), (J*STEP-STEP)+NUMPTS
	   COUNTER = COUNTER + 1
	   XDATA(COUNTER) = Y(I,1,SLOT)
	   YDATA(COUNTER) = Y(I,2,SLOT)
	END DO

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	SAMPLE = 0
	CALL PERIOD_SCARGLE (XDATA, YDATA, COUNTER, WORK1, WORK2, NWK, 
     +			     NOUT, IFAIL, FMIN, FMAX, FINT, SAMPLE)
	IF (IFAIL .EQ. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Lomb-Scargle statistic calculation
     + unsuccessful.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C If first step, initiate plot device.
C-----------------------------------------------------------------------------

	IF (J .EQ. 1) THEN
1340	IDEV = PGBEG (0,'?',0,0)
	IF (IDEV .NE. 1) GOTO 1340

	CALL PGQCOL (SHADE_MIN, SHADE_MAX)
	DO I = 16, 16+SHADE_MAX
	   CALL PGSCR(I, (I-16)*(1./SHADE_MAX), 
     +     (I-16)*(1./SHADE_MAX), (I-16)*(1./SHADE_MAX) )
	END DO

	WRITE(*,*)' '
	IF (.NOT. LREPLOT) THEN
	   XMIN = REAL(WORK1(1))
	   XMAX = REAL(WORK1(NOUT))
	   YMIN = 1.
	   YMAX = REAL(NUMSTEPS+1)
	END IF
	CALL PGSVP (0.1,0.9,0.1,0.7)
	CALL PGSWIN (XMIN, XMAX, YMIN, YMAX)
	END IF

C-----------------------------------------------------------------------------
C Plot results.
C-----------------------------------------------------------------------------

	IF (.NOT. LREPLOT) THEN
	   BLACK = 1.D32
	   WHITE = -1.D32
	   DO I = 1, NOUT
	      IF (WORK2(I) .GT. WHITE) THEN
		 WHITE = WORK2(I)
		 IF (WHITE .GT. MAXWHITE) THEN
		    MAXWHITE = WHITE
		 END IF
	      END IF
	      IF (WORK2(I) .LT. BLACK) THEN
		 BLACK = WORK2(I)
		 IF (BLACK .LT. MAXBLACK) THEN
		    MAXBLACK = BLACK
		 END IF
	      END IF
	   END DO
	END IF
	
	WRITE(*,*)'** OK: Plotting step number =',J
	DO I = 1, NOUT-1
	   XPTS(1) = REAL(WORK1(I))
	   XPTS(2) = REAL(WORK1(I))
	   XPTS(3) = REAL(WORK1(I+1))
	   XPTS(4) = REAL(WORK1(I+1))
	   YPTS(1) = REAL(J)
	   YPTS(2) = REAL(J+1)
	   YPTS(3) = REAL(J+1)
	   YPTS(4) = REAL(J)
c	   SHADE = INT(239.*REAL((WORK2(I)-BLACK)/(WHITE-BLACK)))
	   SHADE = INT((SHADE_MAX-16)*
     +     REAL((WORK2(I)-BLACK)/(WHITE-BLACK)))
	   IF (SHADE .LT. 0) SHADE = 0
	   CALL PGSCI(SHADE+16)
	   IF (XPTS(1) .LT. XMIN .OR. XPTS(3) .GT. XMAX .OR.
     +	   YPTS(1) .LT. YMIN .OR. YPTS(2) .GT. YMAX) THEN
	      CONTINUE
	   ELSE
	      CALL PGPOLY(4,XPTS,YPTS)
	   END IF
	END DO
	END DO
	CALL PGSCI(4)
	CALL PGBOX('BCNST',0.,0.,'BNST',0.,0.)
	CALL PGSCI(2)
	CALL PGLAB('frequency','step number',' ')
	CALL PGSCI(1)

C-----------------------------------------------------------------------------
C Plot y-axis scale in units of time. Subtract a base, otherwise the y-axis
C numbering can't handle the number of time digits in ULTRACAM data. 
C-----------------------------------------------------------------------------

	DBASE = AINT(Y(1,1,SLOT))
	YMIN = REAL(Y(1,1,SLOT)-DBASE)
	YMAX = REAL(Y(1+(NUMSTEPS*STEP-STEP),1,SLOT)-DBASE)
	CALL PGSWIN (XMIN, XMAX, YMIN, YMAX)
	CALL PGSCI(4)
	CALL PGBOX('',0.,0.,'CMST',0.,0.)
	CALL PGSCI(2)
        CALL PGMTXT ('R', 3., 0.5, 0.,'time')
	CALL PGSCI(1)

C-----------------------------------------------------------------------------
C Now plot Scargle periodogram of entire dataset.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
	   XDATA(I) = Y(I,1,SLOT)
	   YDATA(I) = Y(I,2,SLOT)
	END DO
	CALL PERIOD_SCARGLE (XDATA, YDATA, NDATA, WORK1, WORK2, NWK, 
     +			     NOUT, IFAIL, FMIN, FMAX, FINT, SAMPLE)
	IF (IFAIL .EQ. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Lomb-Scargle statistic calculation
     + unsuccessful.'
		GOTO 10
	END IF
	YMIN = 1.E32
	YMAX = -1.E32
	DO I = 1, NOUT
	   IF (REAL(WORK2(I)) .GT. YMAX) THEN
	      YMAX = REAL(WORK2(I))
	   END IF
	   IF (REAL(WORK2(I)) .LT. YMIN) THEN
	      YMIN = REAL(WORK2(I))
	   END IF
	END DO

	P = (YMAX-YMIN)*0.05
	IF (.NOT. LREPLOT) THEN
	   YMAX = YMAX + P
	   YMIN = YMIN - P
	   XMIN = REAL(WORK1(1))
	   XMAX = REAL(WORK1(NOUT))
	ELSE
	   YMAX = PMAX
	   YMIN = PMIN
	END IF
	CALL PGSVP (0.1,0.9,0.7,0.9)
	CALL PGSWIN (XMIN, XMAX, YMIN, YMAX)
	DO I = 1, NOUT
	   XPTS(1) = REAL(WORK1(I))
	   YPTS(1) = REAL(WORK2(I))
	   IF (I .EQ. 1) THEN
	      CALL PGMOVE (XPTS(1), YPTS(1))
	   ELSE
	      CALL PGDRAW (XPTS(1), YPTS(1))
	   END IF
	END DO
	CALL PGSCI(4)
	CALL PGBOX('BCST',0.,0.,'BCNST',0.,0.)
	CALL PGSCI(2)
	CALL PGLAB(' ','power',' ')
	CALL PGSCI(1)

C-----------------------------------------------------------------------------
C Prompt for a replot, changing the plot parameters.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	WRITE(*,*)'** OK: Minimum power in dataset = ',REAL(MAXBLACK)
	WRITE(*,*)'** OK: Maximum power in dataset = ',REAL(MAXWHITE)
	WRITE(*,*)' '
1350	WRITE(*,'(A,$)')'Enter black,white,xmin,xmax,ymin,ymax,'//
     +  'pmin,pmax to replot (0''s to quit) : '
	READ(*,*,ERR=1350)BLACK, WHITE, XMIN, XMAX, YMIN, YMAX,
     +  PMIN, PMAX
	IF (BLACK .EQ. 0. .AND. WHITE .EQ. 0. .AND. XMIN .EQ. 0. 
     +      .AND. XMAX .EQ. 0. .AND. YMIN .EQ. 0. .AND. YMAX 
     +      .EQ. 0. .AND. PMIN .EQ. 0. .AND. PMAX .EQ. 0.) THEN
	   LREPLOT = .FALSE.
	   CALL PGEND
	   GOTO 10
	ELSE
	   LREPLOT = .TRUE.
	END IF
	GOTO 1330

C-----------------------------------------------------------------------------
C Amplitude of sine fit versus frequency. Amplitude (actually, the 
C semi-amplitude) is calculated from normalised y data, so represents the
C fractional amplitude.
C-----------------------------------------------------------------------------

1400	IF (.NOT. LSELECT) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No slots selected.'
		GOTO 10
	END IF

	IF (LSIG) THEN 
	   WRITE(*,*)' '
	   WRITE(*,*)'** OK: Significance calculation to be performed'//
     + ' with samples = ',NPERMS
	   WRITE(*,*)' '
	   WRITE(*,*)'** OK: Output slot can contain either the'//
     + ' periodogram'
	   WRITE(*,*)'** OK: of the input slot or the highest peak'//
     + ' found at'
	   WRITE(*,*)'** OK: each frequency in the set of randomised'//
     + ' time series.'
	   WRITE(*,*)'** OK: The CDF of the latter can be calculated'//
     + ' using HIST'
	   WRITE(*,*)'** OK: to determine significances.'
	   WRITE(*,*)' '
	   WRITE(*,'(A,$)')'Output slot to contain [p]eriodogram or'//
     + ' [h]ighest peak ? [p] (q to quit) : '
	   READ(*,'(A)',ERR=10)REPLY
	   CALL PERIOD_CASE (REPLY, .TRUE.)
	   IF (REPLY .EQ. 'H') THEN
	      LCDF = .TRUE.
	   ELSE IF (REPLY .EQ. 'Q') THEN
	      GOTO 10
	   ELSE
	      LCDF = .FALSE.
	   END IF
	END IF

C-----------------------------------------------------------------------------
C Loop through slots to be processed.
C-----------------------------------------------------------------------------

	DO SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Loop through permutations. 
C-----------------------------------------------------------------------------

	DO SAMPLE = 1, NPERMS+1
	IF (SAMPLE .EQ. 1) THEN
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 10
	END IF
	SLOTOUT = FIRSTOUT + (SLOT - FIRSTSLOT)
	NDATA = NPTSARRAY(SLOT)

C-----------------------------------------------------------------------------
C Set frequency limits.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,1,SLOT)
	END DO
	WRITE(*,*)' '
	CALL PERIOD_AUTOLIM (FDATA, NDATA, MXROW, MINFREQ, MAXFREQ, 
     +		             FINTERVAL, FMIN, FMAX, FINT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 10
	END IF
	IF (FMIN .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: No sine fit for zero frequency.'
		GOTO 10
	END IF

C-----------------------------------------------------------------------------
C Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	ZEROPT = AVE

C-----------------------------------------------------------------------------
C Calculate mean of y data
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		FDATA(I) = Y(I,2,SLOT)
	END DO
	CALL PERIOD_MOMENT (FDATA, NDATA, AVE, ADEV, SDEV, VARI)
	WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Loop through trial frequencies.
C-----------------------------------------------------------------------------

	COUNTER = 0
	LOOP = 1
	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
	   DO I = 1, NDATA
	      XDATA(I) = Y(I,1,SLOT)
	      IF (SAMPLE .EQ. 1) YRAN(I) = Y(I,2,SLOT) / AVE
	      YDATA(I) = YRAN(I)
	      IF (.NOT. YERRORARRAY(SLOT)) THEN
		 YERR(I) = 1.0D0
	      ELSE 
		 IF (SAMPLE .EQ. 1) ERAN(I) = Y(I,3,SLOT) / AVE
		 YERR(I) = ERAN(I)
	      END IF
	   END DO
	   PERIOD = 1.0D0 / FREQ
	   CALL PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, 
     + ZEROPT, PERIOD)
	   IF (IFAIL .EQ. 1) THEN
	      GOTO 10			
	   END IF 	
	   CALL PERIOD_SINFIT (XDATA, YDATA, YERR, NDATA, 1.0D0,
     + GAMMA, KVEL, PHASE, VAR, NP, F, IFAIL)	
	   IF (IFAIL .EQ. 1) THEN
	      WRITE(*,*)ACHAR(7)
	      WRITE(*,*)'** ERROR: Sine fit unsuccessful.'
	      GOTO 10			
	   END IF
	   COUNTER = COUNTER + 1
	   IF (SAMPLE .EQ. 1) THEN
	      IF (COUNTER .EQ. (LOOP*ISTEP)+1) THEN
		 WRITE(CFREQ,*)FREQ
		 CFREQ_LEN = LEN_TRIM(CFREQ)
		 WRITE(CSTAT,*)KVEL
		 CSTAT_LEN = LEN_TRIM(CSTAT)
		 WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', fractional amplitude = '//CSTAT(1:CSTAT_LEN)
		 LOOP = LOOP + 1
	      END IF
	   END IF

C-----------------------------------------------------------------------------
C If the first permutation, load the output slot.
C-----------------------------------------------------------------------------
	   
	   IF (SAMPLE .EQ. 1) THEN
	      Y(COUNTER,1,SLOTOUT) = FREQ
	      Y(COUNTER,2,SLOTOUT) = KVEL
	      Y(COUNTER,3,SLOTOUT) = 0.0D0
	   END IF
	   WK2(COUNTER) = KVEL
	END DO

C-----------------------------------------------------------------------------
C Periodogram complete. If the first permutation, and if the periodogram is
C required, load the output arrays.
C-----------------------------------------------------------------------------

	IF (SAMPLE .EQ. 1) THEN
	   YERRORARRAY(SLOTOUT) = .FALSE.
	   DETRENDARRAY(SLOTOUT) = .FALSE.
	   INFILEARRAY(SLOTOUT) = 
     + 'amplitude vs frequency, '//INFILEARRAY(SLOT)
	   NPTSARRAY(SLOTOUT) = COUNTER
	   SIG(1,SLOTOUT) = -1.0D0
	   SIG(2,SLOTOUT) = -1.0D0
	   IF (.NOT. LCDF) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	   END IF
	   WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Determine significance information if requested.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
		STAT(SAMPLE) = -1.0D+32
		DO I = 1, COUNTER
		IF (WK2(I) .GT. STAT(SAMPLE)) THEN
			STAT(SAMPLE) = WK2(I)
		END IF
		IF (SAMPLE .GT. 1) THEN
		IF (WK2(I) .GE. Y(I,2,SLOTOUT)) THEN
			Y(I,3,SLOTOUT) = Y(I,3,SLOTOUT) + 1.0D0
		END IF
		END IF
		END DO
		IF (SAMPLE .GT. 1) THEN
		WRITE(*,*)'** OK: Processed permutation = ',SAMPLE-1
		END IF

C-----------------------------------------------------------------------------
C Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

		CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		DO J = 1, 1 + INT(RANDOM * 2.)
		   DO I = 1, NDATA
		      CALL PERIOD_RAN1(SEED, IR, RANDOM, IY)
		      EL = 1 + INT(RANDOM * REAL(NDATA))
		      SHUFFLE1 = YRAN(I) 
		      SHUFFLE2 = YRAN(EL) 
		      YRAN(I) = SHUFFLE2
		      YRAN(EL) = SHUFFLE1
		      SHUFFLE1 = ERAN(I) 
		      SHUFFLE2 = ERAN(EL)
		      ERAN(I) = SHUFFLE2
		      ERAN(EL) = SHUFFLE1
		   END DO
		END DO
	END IF
	END DO

C-----------------------------------------------------------------------------
C Permutations complete - calculate significance and load output arrays,
C overwriting the periodogram if the CDF is required.
C-----------------------------------------------------------------------------

	IF (LSIG) THEN
	   SIGNOISE = 0.0D0
	   DO I = 2, NPERMS+1
	      IF (STAT(I) .GE. STAT(1)) THEN
		 SIGNOISE = SIGNOISE + 1.0D0
	      END IF
	   END DO
	   SIG(1,SLOTOUT) = DBLE(NPERMS)
	   SIG(2,SLOTOUT) = SIGNOISE / DBLE(NPERMS)
	   
	   IF (LCDF) THEN
	      DO SAMPLE = 1, NPERMS+1
		 Y(SAMPLE,1,SLOTOUT) = SAMPLE
		 Y(SAMPLE,2,SLOTOUT) = STAT(SAMPLE)
		 Y(SAMPLE,3,SLOTOUT) = 0.0D0	      
	      END DO
	      YERRORARRAY(SLOTOUT) = .FALSE.
	      DETRENDARRAY(SLOTOUT) = .FALSE.
	      INFILEARRAY(SLOTOUT) = 
     + 'maximum amplitude vs permutation number, '//INFILEARRAY(SLOT)
	      NPTSARRAY(SLOTOUT) = NPERMS+1
	      SIG(1,SLOTOUT) = -1.0D0
	      SIG(2,SLOTOUT) = -1.0D0
	      WRITE(*,*)' '
	      WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
	      WRITE(*,*)' '
	   END IF

	END IF

C-----------------------------------------------------------------------------
C Process the next data slot.
C-----------------------------------------------------------------------------

	END DO
	GOTO 10	

	END
