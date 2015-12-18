
	PROGRAM PERIOD_MAIN

C=============================================================================
C Package to search for periodicities in data using a number of techniques. 
C
C Written by Vikram Singh Dhillon @LPO 24-January-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Removed repeated listing of menu, Vik Dhillon @Paranal 11-May-2005.
C=============================================================================

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
     +		   	  ICOMP(2*MXPAR,MXMOD),NTERM(MXMOD)

C-----------------------------------------------------------------------------
C PERIOD_MAIN declarations.
C Adjust MXROW to change the maximum number of times handled by PERIOD.
C Adjust MXSLOT to change the maximum number of slots in PERIOD.
C Adjust MXCOL to change the maximum number of columns in the input file.
C Adjust MXSIM to change the maximum number of slots which can be overplotted.
C To maximise the values of these parameters, type "unlimit stacksize". 
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXCOL, MXSLOT, MXVEC, MXSIM, MXCMD
	PARAMETER (MXROW=150000, MXSLOT=50, MXCOL=10, MXSIM=10)
	PARAMETER (MXVEC=MXSIM*3)
	PARAMETER (MXCMD=100)
	INTEGER NPTSARRAY(MXSLOT), LOGUNIT, I, NCMD
	DOUBLE PRECISION Y(MXROW, 3 , MXSLOT), OFFSET
	DOUBLE PRECISION JUNK1(MXROW), JUNK2(MXROW, MXCOL)
	CHARACTER*12 COMMAND
	CHARACTER*72 INFILEARRAY(MXSLOT), LOGFILE, CMD(MXCMD)
	LOGICAL PERIOD_PARSE, LOG, OVERPLOT, QDP, LCOMMAND, BASE
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
	DATA LOGUNIT, LOG, OFFSET /17, .FALSE., 0.D0/
	DATA OVERPLOT, QDP, BASE /.FALSE., .TRUE., .FALSE./

C-----------------------------------------------------------------------------
C Declarations for PERIOD_UINPUT and PERIOD_USLOTS.
C Adjust MAXAPER to change the maximum number of apertures in the reduce log 
C file.
C Adjust MAXHEADER to change the maximum number of header lines in the reduce
C log file.
C-----------------------------------------------------------------------------

	INTEGER NUM_APER(3), NDATA, MAXAPER, MAXCHARS
	INTEGER MAXLINES, IFAIL, MAXHEADER, IVERSION
	PARAMETER (MAXAPER=10, MAXCHARS=MAXAPER*103+70)
	PARAMETER (MAXHEADER=200, MAXLINES=MXROW*3+MAXHEADER)
	INTEGER RUN(MXROW), NSAT(MXROW)
        INTEGER NREJ(3,MXROW,MAXAPER)
        DOUBLE PRECISION MJD(3,MXROW)
        DOUBLE PRECISION COUNTS(3,MXROW,MAXAPER), SIGMA(3,MXROW,MAXAPER)
	REAL FWHM(3,MXROW), BETA(3,MXROW)
        REAL EXPOSE(3,MXROW), WORST(3,MXROW,MAXAPER) 
        REAL XPOS(3,MXROW,MAXAPER), YPOS(3,MXROW,MAXAPER)
	REAL XMPOS(3,MXROW,MAXAPER), YMPOS(3,MXROW,MAXAPER)
	REAL EXMPOS(3,MXROW,MAXAPER), EYMPOS(3,MXROW,MAXAPER)
        REAL SKY(3,MXROW,MAXAPER), NSKY(3,MXROW,MAXAPER)
        CHARACTER*(MAXCHARS) STRING(MAXLINES)
	CHARACTER*72 UINFILE
	LOGICAL LUINPUT
	DATA LUINPUT /.FALSE./

C-----------------------------------------------------------------------------
C Declarations for PERIOD_PERIOD.
C-----------------------------------------------------------------------------

	INTEGER NWK
	PARAMETER (NWK = MXROW*27)
	DOUBLE PRECISION WORK1(NWK), WORK2(NWK)
	DOUBLE PRECISION ONES(0:MXROW-1), WFREQ(0:(2*MXROW)-1)
        DOUBLE PRECISION DFREQ(0:MXROW-1)
        DOUBLE COMPLEX D(0:MXROW-1), W(0:(2*MXROW)-1)
	DOUBLE COMPLEX R(0:MXROW-1)
	DOUBLE COMPLEX B(0:MXROW-1), C(0:MXROW-1), S(0:MXROW-1)

C-----------------------------------------------------------------------------
C Initialise arrays.
C------------------------------------------------e-----------------------------

	DO I = 1, MXSLOT
	   NPTSARRAY(I) = 0
	END DO

C-----------------------------------------------------------------------------
C Show introductory info.
C-----------------------------------------------------------------------------

	CALL PERIOD_INTRO

C-----------------------------------------------------------------------------
C User prompt
C-----------------------------------------------------------------------------

 10	WRITE(*,'(A,$)')'period> '
	READ(*,'(A)',ERR=10) COMMAND
	CALL PERIOD_CASE (COMMAND, .TRUE.)
	IF( PERIOD_PARSE( COMMAND, 'INPUT')    )GOTO 100
	IF( PERIOD_PARSE( COMMAND, 'UINPUT')   )GOTO 200
	IF( PERIOD_PARSE( COMMAND, 'USLOTS')   )GOTO 300
	IF( PERIOD_PARSE( COMMAND, 'FAKE')     )GOTO 400
	IF( PERIOD_PARSE( COMMAND, 'NOISE')    )GOTO 500
	IF( PERIOD_PARSE( COMMAND, 'DETREND')  )GOTO 600
	IF( PERIOD_PARSE( COMMAND, 'WINDOW')   )GOTO 700
	IF( PERIOD_PARSE( COMMAND, 'OPEN')     )GOTO 800
	IF( PERIOD_PARSE( COMMAND, 'CLOSE')    )GOTO 900
	IF( PERIOD_PARSE( COMMAND, 'PERIOD')   )GOTO 1000
	IF( PERIOD_PARSE( COMMAND, 'FIT')      )GOTO 1100
	IF( PERIOD_PARSE( COMMAND, 'FOLD')     )GOTO 1200
	IF( PERIOD_PARSE( COMMAND, 'SINE')     )GOTO 1300
	IF( PERIOD_PARSE( COMMAND, 'ARITH')    )GOTO 1400
	IF( PERIOD_PARSE( COMMAND, 'CLIP')     )GOTO 1500
	IF( PERIOD_PARSE( COMMAND(1:3), 'PLT') )GOTO 1600
	IF( PERIOD_PARSE( COMMAND(1:2), 'SH')  )GOTO 1700
	IF( PERIOD_PARSE( COMMAND, 'OUTPUT')   )GOTO 1800
	IF( PERIOD_PARSE( COMMAND(1:4), 'HELP'))GOTO 1900
	IF( PERIOD_PARSE( COMMAND, 'BIN')      )GOTO 2000
	IF( PERIOD_PARSE( COMMAND, 'SMOOTH')   )GOTO 2100
	IF( PERIOD_PARSE( COMMAND, 'APPEND')   )GOTO 2200
	IF( PERIOD_PARSE( COMMAND, 'COPY')     )GOTO 2300
	IF( PERIOD_PARSE( COMMAND, 'DEL')      )GOTO 2400
	IF( PERIOD_PARSE( COMMAND, 'TIME')     )GOTO 2500
	IF( PERIOD_PARSE( COMMAND, 'DERIV')    )GOTO 2600
	IF( PERIOD_PARSE( COMMAND, 'XCOR')     )GOTO 2700
	IF( PERIOD_PARSE( COMMAND, 'UNBLUE')   )GOTO 2800
	IF( PERIOD_PARSE( COMMAND, 'HIST')     )GOTO 2900
	IF( PERIOD_PARSE( COMMAND, 'BOOT')     )GOTO 3000
	IF( PERIOD_PARSE( COMMAND, 'JIGGLE')   )GOTO 3100
	IF( PERIOD_PARSE( COMMAND, 'ERRORS')   )GOTO 3200
	IF( PERIOD_PARSE( COMMAND, 'POLY')     )GOTO 3300
	IF( PERIOD_PARSE( COMMAND, 'AIRMASS')  )GOTO 3400
	IF( PERIOD_PARSE( COMMAND, 'INTEG')    )GOTO 3500
	IF( PERIOD_PARSE( COMMAND, 'UCAL')     )GOTO 3600
	IF( PERIOD_PARSE( COMMAND, 'QUIT')     )GOTO 11111
	IF( PERIOD_PARSE( COMMAND, 'EXIT')     )GOTO 11111
	IF( PERIOD_PARSE( COMMAND, '?')        )GOTO 50
	IF (OVERPLOT) THEN
	   IF( PERIOD_PARSE( COMMAND(1:3), 'OFF') ) THEN
	      IF (COMMAND(3:4).EQ.'  '.OR.COMMAND(4:5).EQ.'  ') THEN
		 LCOMMAND = .FALSE.
	      ELSE
		 READ(COMMAND(4:12),*,ERR=10)OFFSET
		 LCOMMAND = .TRUE.
	      END IF
	      IF (.NOT. LCOMMAND) THEN
		 WRITE(*,*)' '
 20		 WRITE(*,'(A,$)')'Enter y offset for plots : '
		 READ(*,*,ERR=20)OFFSET
	      END IF
	   END IF
	END IF
	IF( PERIOD_PARSE( COMMAND,  'OVERPLOT') ) THEN
	   IF (OVERPLOT) THEN
	      OVERPLOT = .FALSE.
	   ELSE
	      OVERPLOT = .TRUE.
	   END IF
	END IF
	IF( PERIOD_PARSE( COMMAND,  'QDP') ) THEN
	   IF (QDP) THEN
	      QDP = .FALSE.
	      DO I = 10, MXCMD-1
30		 WRITE(*,'(A,$)')
     + 'Enter extra plot commands (<cr> to quit) : '
		 READ(*,'(A)',ERR=30) CMD(I)
		 IF (CMD(I) .EQ. ' ') THEN
		    CMD(I) = 'PL'
		    CMD(I+1) = 'EXIT'
		    NCMD = I+1
		    GOTO 10
		 END IF
	      END DO
	   ELSE
	      QDP = .TRUE.
	   END IF
	END IF
	IF( PERIOD_PARSE( COMMAND,  'BASE') ) THEN
	   IF (BASE) THEN
	      BASE = .FALSE.
	   ELSE
	      BASE = .TRUE.
	   END IF
	END IF
	
	GOTO 10

C-----------------------------------------------------------------------------
C Display menu.
C-----------------------------------------------------------------------------

50	CALL PERIOD_MENU (BASE, LOG, OVERPLOT, QDP, OFFSET)
	GOTO 10

C-----------------------------------------------------------------------------
C Input data.
C-----------------------------------------------------------------------------

100     CALL PERIOD_INPUT (Y, MXROW, MXSLOT, 
     +                     NPTSARRAY, YERRORARRAY,
     +			   INFILEARRAY, DETRENDARRAY,
     +			   JUNK1, JUNK2, MXCOL)
	GOTO 10

C-----------------------------------------------------------------------------
C Input ULTRACAM data.
C-----------------------------------------------------------------------------

200     CALL PERIOD_UINPUT (NDATA, MAXAPER, RUN, MJD, 
     +                      NSAT, EXPOSE, FWHM, BETA,
     +                      XPOS, YPOS, XMPOS, YMPOS,
     +                      EXMPOS, EYMPOS, COUNTS, 
     +                      SIGMA, SKY, NSKY, NREJ, WORST,
     +                      NUM_APER, UINFILE, MXROW,  
     +                      MAXCHARS, STRING, IFAIL, 
     +                      MAXHEADER, IVERSION)
	IF (IFAIL .EQ. 0) THEN
	   LUINPUT = .TRUE.
	ELSE
	   LUINPUT = .FALSE.
	END IF
        GOTO 10                

C-----------------------------------------------------------------------------
C Load slots with ULTRACAM data.
C-----------------------------------------------------------------------------

300	IF (LUINPUT) THEN
	CALL PERIOD_USLOTS (NDATA, MAXAPER, RUN, MJD,
     +                      NSAT, EXPOSE, FWHM, BETA, 
     +                      XPOS, YPOS, XMPOS, YMPOS, 
     +                      EXMPOS, EYMPOS, COUNTS, 
     +                      SIGMA, SKY, NSKY, NREJ, WORST, 
     +                      NUM_APER, UINFILE, IVERSION,
     +                      Y, MXROW, MXSLOT,
     +                      NPTSARRAY, YERRORARRAY, 
     +                      INFILEARRAY, DETRENDARRAY)
	ELSE
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)'** ERROR: No ULTRACAM data has been input.'
	END IF
	GOTO 10

C-----------------------------------------------------------------------------
C Create fake data.
C-----------------------------------------------------------------------------

400	CALL PERIOD_FAKE (Y, MXROW, MXSLOT, 
     +                    NPTSARRAY, YERRORARRAY,
     +                    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Randomise data.
C-----------------------------------------------------------------------------

500	CALL PERIOD_NOISE (Y, MXROW, MXSLOT,
     +			   NPTSARRAY, YERRORARRAY, 
     +			   INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Detrend the data.
C-----------------------------------------------------------------------------

600	CALL PERIOD_DETREND (Y, MXROW, MXSLOT,
     +			     NPTSARRAY, YERRORARRAY, 
     +			     INFILEARRAY, DETRENDARRAY) 
	GOTO 10

C-----------------------------------------------------------------------------
C Set data points to unity for window function generation.
C-----------------------------------------------------------------------------

700	CALL PERIOD_WINDOW (Y, MXROW, MXSLOT,
     +			    NPTSARRAY, YERRORARRAY,
     +			    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Open log file. 
C-----------------------------------------------------------------------------

800	LOG = .TRUE.
	CALL PERIOD_LOG (LOGFILE, LOG, LOGUNIT)
	GOTO 10

C-----------------------------------------------------------------------------
C Close log file.
C-----------------------------------------------------------------------------

900	LOG = .FALSE.
	CALL PERIOD_LOG (LOGFILE, LOG, LOGUNIT)
	GOTO 10

C-----------------------------------------------------------------------------
C Find periodicities.
C-----------------------------------------------------------------------------

1000	CALL PERIOD_PERIOD (Y, MXROW, MXSLOT, 
     +                      NPTSARRAY, YERRORARRAY,
     +                      INFILEARRAY, DETRENDARRAY, 
     +			    LOG, LOGUNIT, NWK, WORK1, WORK2,
     +                      ONES, WFREQ, DFREQ, D, W, R, B, C, S,
     +                      MXVEC, OVERPLOT, QDP, OFFSET,
     +                      CMD, NCMD, MXCMD)
	GOTO 10

C-----------------------------------------------------------------------------
C Fit sine curve to data.
C-----------------------------------------------------------------------------

1100	CALL PERIOD_FIT  (Y, MXROW, MXSLOT,
     +	                  NPTSARRAY, YERRORARRAY,
     +			  INFILEARRAY, DETRENDARRAY,
     +			  LOG, LOGUNIT)
	GOTO 10

C-----------------------------------------------------------------------------
C Fold data on given period.
C-----------------------------------------------------------------------------

1200	CALL PERIOD_PHASE (Y, MXROW, MXSLOT,
     +		           NPTSARRAY, YERRORARRAY, 
     +	   	           INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Add or subtract sine curves.
C-----------------------------------------------------------------------------

1300 	CALL PERIOD_SINE (Y, MXROW, MXSLOT,
     +		          NPTSARRAY, YERRORARRAY, 
     +                    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Slot arithmetic.
C-----------------------------------------------------------------------------

1400	CALL PERIOD_ARITH (Y, MXROW, MXSLOT,
     +                     NPTSARRAY, YERRORARRAY,
     +                     INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Clip data.
C-----------------------------------------------------------------------------

1500	CALL PERIOD_CLIP (Y, MXROW, MXSLOT,
     +                    NPTSARRAY, YERRORARRAY,
     +                    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Call PLT.
C-----------------------------------------------------------------------------

1600	CALL PERIOD_PLT (Y, MXROW, MXSLOT, 
     + 			 NPTSARRAY, YERRORARRAY, 
     +			 INFILEARRAY, COMMAND, 
     +                   MXVEC, OVERPLOT, QDP, OFFSET, 
     +                   BASE, CMD, NCMD, MXCMD)
	GOTO 10

C-----------------------------------------------------------------------------
C Info on data and fits.
C-----------------------------------------------------------------------------

1700	CALL PERIOD_SHOW (Y, MXROW, MXSLOT, 
     +                    NPTSARRAY, YERRORARRAY, 
     +	                  INFILEARRAY, COMMAND, DETRENDARRAY, 
     +		          LOGFILE, LOG, LOGUNIT)
	GOTO 10

C-----------------------------------------------------------------------------
C Output data.
C-----------------------------------------------------------------------------

1800	CALL PERIOD_OUTPUT (Y, MXROW, MXSLOT,
     +		       	    NPTSARRAY, YERRORARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C On-line help. 
C-----------------------------------------------------------------------------

1900	CALL PERIOD_HELP (COMMAND)
	GOTO 10

C-----------------------------------------------------------------------------
C Bin data.
C-----------------------------------------------------------------------------

2000	CALL PERIOD_BIN (Y, MXROW, MXSLOT,
     +	                 NPTSARRAY, YERRORARRAY, 
     +		         INFILEARRAY, DETRENDARRAY) 
	GOTO 10

C-----------------------------------------------------------------------------
C Smooth data.
C-----------------------------------------------------------------------------

2100	CALL PERIOD_SMOOTH (Y, MXROW, MXSLOT,
     +	                    NPTSARRAY, YERRORARRAY, 
     +		            INFILEARRAY, DETRENDARRAY) 
	GOTO 10

C-----------------------------------------------------------------------------
C Append slots.
C-----------------------------------------------------------------------------

2200	CALL PERIOD_APPEND (Y, MXROW, MXSLOT, 
     + 			    NPTSARRAY, YERRORARRAY, 
     +			    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Copy slots.
C-----------------------------------------------------------------------------

2300	CALL PERIOD_COPY (Y, MXROW, MXSLOT, 
     + 			  NPTSARRAY, YERRORARRAY, 
     +			  INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Delete slots.
C-----------------------------------------------------------------------------

2400	CALL PERIOD_DEL (MXSLOT, NPTSARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Barycentric/heliocentric correction.
C-----------------------------------------------------------------------------

2500	CALL PERIOD_TIME (Y, MXROW, MXSLOT, 
     + 		          NPTSARRAY, YERRORARRAY, 
     +		          INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Take derivative of data.
C-----------------------------------------------------------------------------

2600	CALL PERIOD_DERIV (Y, MXROW, MXSLOT,
     +			   NPTSARRAY, YERRORARRAY,
     +			   INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Cross-correlate data.
C-----------------------------------------------------------------------------

2700	CALL PERIOD_DCF (Y, MXROW, MXSLOT,
     +                   NPTSARRAY, YERRORARRAY,
     +			 INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Correct data for NBLUE>1
C-----------------------------------------------------------------------------

2800	CALL PERIOD_UNBLUE (Y, MXROW, MXSLOT,
     +                      NPTSARRAY, YERRORARRAY,
     +			    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Compute histogram or CDF
C-----------------------------------------------------------------------------

2900	CALL PERIOD_HIST (Y, MXROW, MXSLOT,
     +                    NPTSARRAY, YERRORARRAY,
     +			  INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Bootstrap data.
C-----------------------------------------------------------------------------

3000	CALL PERIOD_BOOT (Y, MXROW, MXSLOT,
     +			  NPTSARRAY, YERRORARRAY, 
     +			  INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Bootstrap data.
C-----------------------------------------------------------------------------

3100	CALL PERIOD_JIGGLE (Y, MXROW, MXSLOT,
     +			    NPTSARRAY, YERRORARRAY, 
     +			    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Calculate errors.
C-----------------------------------------------------------------------------

3200	CALL PERIOD_ERRORS (Y, MXROW, MXSLOT,
     +			    NPTSARRAY, YERRORARRAY, 
     +			    INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Polynomial fit to data
C-----------------------------------------------------------------------------

3300	CALL PERIOD_POLYFIT (Y, MXROW, MXSLOT,
     +	 		     NPTSARRAY, YERRORARRAY, 
     +			     INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Calculate airmass of object
C-----------------------------------------------------------------------------

3400	CALL PERIOD_AIRMASS (Y, MXROW, MXSLOT,
     +	 		     NPTSARRAY, YERRORARRAY, 
     +			     INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Integrate data
C-----------------------------------------------------------------------------

3500	CALL PERIOD_INTEG (Y, MXROW, MXSLOT,
     +	 		   NPTSARRAY, YERRORARRAY, 
     +			   INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Flux calibrate data
C-----------------------------------------------------------------------------

3600	CALL PERIOD_UCAL (Y, MXROW, MXSLOT,
     +	 		  NPTSARRAY, YERRORARRAY, 
     +			  INFILEARRAY, DETRENDARRAY)
	GOTO 10

C-----------------------------------------------------------------------------
C Exit period.
C-----------------------------------------------------------------------------

11111	CALL PERIOD_QUIT
	GOTO 10

	END
