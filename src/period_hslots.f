
	SUBROUTINE PERIOD_HSLOTS (NHDATA, MAXAPER, RUN, MJD, 
     +                            EXPOSE, MFWHM, MBETA, XMPOS, EXMPOS, 
     +                            YMPOS, EYMPOS, HFWHM, EFWHM, HBETA,
     +                            EBETA, COUNTS, SIGMA, SKY, ESKY,
     +                            NSKY, NREJ, NUM_APER, HINFILE, 
     +                            Y, MXROW, MXSLOT,
     +                            NPTSARRAY, YERRORARRAY, 
     +                            INFILEARRAY, DETRENDARRAY)

C=============================================================================
C Routine to load PERIOD slots with HiPERCAM data input using the HINPUT
C command. Prompts the user for which variables to use as the x- and y-axes
C and for which aperture and CCD numbers to load. 
C
C Written by Vik Dhillon @Sheffield 24-Oct-2017.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_SLOTS declarations.
C-----------------------------------------------------------------------------
	
	INTEGER H, I, J, K, MAXAPER, NCCD, NAPER, NUMSLOTS
	INTEGER NUM_APER(5), NHDATA(5), NDATA, HINFILE_LEN
	INTEGER FIRSTSLOT, LASTSLOT, SLOT, XAXIS, YAXIS, YAXISMAX
	CHARACTER*72 INFILEARRAY(MXSLOT), HINFILE
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT) 

C-----------------------------------------------------------------------------
C Declarations for parameters read from the ULTRACAM data reduction pipeline
C log file.
C-----------------------------------------------------------------------------

	INTEGER RUN(MXROW)
	INTEGER NREJ(5,MXROW,MAXAPER)
	DOUBLE PRECISION MJD(5,MXROW)
	DOUBLE PRECISION COUNTS(5,MXROW,MAXAPER), SIGMA(5,MXROW,MAXAPER)
	REAL HFWHM(5,MXROW,MAXAPER), HBETA(5,MXROW,MAXAPER)
	REAL EFWHM(5,MXROW,MAXAPER), EBETA(5,MXROW,MAXAPER)
	REAL EXPOSE(5,MXROW), SKY(5,MXROW,MAXAPER), NSKY(5,MXROW,MAXAPER)
	REAL XMPOS(5,MXROW,MAXAPER), YMPOS(5,MXROW,MAXAPER)
	REAL EXMPOS(5,MXROW,MAXAPER), EYMPOS(5,MXROW,MAXAPER)
 	REAL ESKY(5,MXROW,MAXAPER)
	REAL MFWHM(5,MXROW), MBETA(5,MXROW)
	
C-----------------------------------------------------------------------------
C Set length of HINFILE to avoid problems with concatenating strings.
C-----------------------------------------------------------------------------

	HINFILE_LEN = LEN_TRIM(HINFILE)
	
C-----------------------------------------------------------------------------
C Select variables to load as the x- and y-axes. 
C-----------------------------------------------------------------------------

	YAXISMAX = 11
	WRITE(*,*)' '
	WRITE(*,*)
     + '---------------------------------------------------------------'
	WRITE(*,*)'x-axis options   y-axis options'
	WRITE(*,*)
     + '---------------------------------------------------------------'
	WRITE(*,*)'1. MJD           1. Object counts (with errors)'
	WRITE(*,*)'2. Run number    2. Sky counts (with errors)'
	WRITE(*,*)
     + '                 3. x-position of aperture (with errors)'
	WRITE(*,*)
     + '                 4. y-position of aperture (with errors)'
	WRITE(*,*)'                 5. Number of sky pixels rejected'
	WRITE(*,*)
     + '                 6. FWHM for profile fits (with errors)'
	WRITE(*,*)
     + '                 7. Beta exponent for Moffat fits (with errors)'
	WRITE(*,*)'                 8. Number of sky pixels available'
	WRITE(*,*)'                 9. Exposure time'
	WRITE(*,*)'                10. Mean FWHM for profile fits'
	WRITE(*,*)
     + '                11. Mean Beta exponent for Moffat fits'	
	WRITE(*,*)
     + '---------------------------------------------------------------'
	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Enter desired axes [x,y] : '
	READ(*,*,ERR=10)XAXIS, YAXIS
	IF (XAXIS .LT. 1 .OR. XAXIS .GT. 2 .OR. 
     +      YAXIS .LT. 1 .OR. YAXIS .GT. YAXISMAX) THEN
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)'** ERROR: Invalid axes.'
	   WRITE(*,*)' '
	   GOTO 10
	END IF
 20	WRITE(*,'(A,$)')'Enter CCD number to load (0 for all) : '
	READ(*,*,ERR=20)NCCD
	IF (NCCD .LT. 0) GOTO 20
	IF (NCCD .GT. 5) GOTO 20
	IF (NCCD .EQ. 0) THEN
	   IF (NUM_APER(1) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: CCD1 has no apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   ELSE IF (NUM_APER(2) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: CCD2 has no apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   ELSE IF (NUM_APER(3) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: CCD3 has no apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   ELSE IF (NUM_APER(4) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: CCD4 has no apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   ELSE IF (NUM_APER(5) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: CCD5 has no apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   END IF
	   IF (NUM_APER(1) .NE. NUM_APER(2) .OR.
     +        NUM_APER(1) .NE. NUM_APER(3) .OR.
     +        NUM_APER(1) .NE. NUM_APER(4) .OR.
     +        NUM_APER(1) .NE. NUM_APER(5) .OR.
     +        NUM_APER(2) .NE. NUM_APER(3) .OR.
     +        NUM_APER(2) .NE. NUM_APER(4) .OR.
     +        NUM_APER(2) .NE. NUM_APER(5) .OR.
     +        NUM_APER(3) .NE. NUM_APER(4) .OR.
     +        NUM_APER(3) .NE. NUM_APER(5) .OR.
     +        NUM_APER(4) .NE. NUM_APER(5)) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)
     + '** ERROR: CCD''s have different numbers of apertures.'
	      WRITE(*,*)' '
	      GOTO 20
	   END IF
	ELSE
	   IF (NUM_APER(NCCD) .EQ. 0) THEN
	      WRITE(*,*)' '
	      WRITE(*,*)'** ERROR: No apertures in CCD number ',NCCD
	      WRITE(*,*)' '
	      GOTO 20	      
	   END IF
	END IF
	IF (YAXIS .LT. 9) THEN
30	   WRITE(*,'(A,$)')
     + 'Enter aperture number to load (0 for all) : '
	   READ(*,*,ERR=30)NAPER
	   IF (NAPER .LT. 0) GOTO 30
	END IF
	IF (YAXIS .LT. 9) THEN
	   IF (NCCD .EQ. 0) THEN
	      IF (NAPER .EQ. 0) THEN
		 NUMSLOTS = NUM_APER(1)+NUM_APER(2)+NUM_APER(3)+
     + NUM_APER(4)+NUM_APER(5)
	      ELSE
		 IF (NAPER .GT. NUM_APER(1) .OR. NAPER .GT.  
     + 		 NUM_APER(2) .OR. NAPER .GT. NUM_APER(3) .OR. 
     + 		 NAPER .GT. NUM_APER(4) .OR. 
     + 		 NAPER .GT. NUM_APER(5)) THEN 
		    NUMSLOTS = 0
		 ELSE
		    NUMSLOTS = 5
		 END IF
	      END IF
	   ELSE 
	      IF (NAPER .EQ. 0) THEN
		 NUMSLOTS = NUM_APER(NCCD)
	      ELSE
		 IF (NAPER .GT. NUM_APER(NCCD)) THEN
		    NUMSLOTS = 0
		 ELSE
		    NUMSLOTS = 1
		 END IF
	      END IF
	   END IF
	ELSE
	   IF (NCCD .EQ. 0) THEN
	      NUMSLOTS = 5
	   ELSE
	      NUMSLOTS = 1
	   END IF
	END IF
	IF (NUMSLOTS .LE. 0) THEN
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)'** ERROR: No data present in selected CCD/aperture.'
	   GOTO 99
	ELSE
	   WRITE(*,*)' '
	   WRITE(*,*)'** OK: Number of slots to be loaded = ',NUMSLOTS
	END IF

C-----------------------------------------------------------------------------
C Load slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
40	WRITE(*,'(A,$)')'Enter starting slot for input (0 to quit) : '
        READ(*,*,ERR=40)FIRSTSLOT
	IF (FIRSTSLOT .LT. 0) GOTO 40
	LASTSLOT = FIRSTSLOT + NUMSLOTS - 1
        IF (FIRSTSLOT .EQ. 0) THEN
	   GOTO 99
        ELSE IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
           GOTO 99
        END IF

C-----------------------------------------------------------------------------
C Now the tricky bit...load the x-axis and y-axis arrays, as well as the
C yerror, infile, detrend and ntps arrays. Also, check to see if the x-axis 
C data is in ascending order. Unless NSKIPS>1, it should be - so just warn the 
C user and don't offer to re-sort (as occurs in the related subroutine 
C PERIOD_INPUT).
C-----------------------------------------------------------------------------

	IF (NCCD .NE. 0) THEN
	IF (NAPER .NE. 0) THEN
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
	   IF (NCCD .EQ. 1) NDATA = NHDATA(1)
	   IF (NCCD .EQ. 2) NDATA = NHDATA(2)
	   IF (NCCD .EQ. 3) NDATA = NHDATA(3)
	   IF (NCCD .EQ. 4) NDATA = NHDATA(4)
	   IF (NCCD .EQ. 5) NDATA = NHDATA(5)
	   DO J = 1, NDATA
	      IF (XAXIS .EQ. 1) THEN
		 Y(J,1,SLOT) = MJD(NCCD,J)
	      ELSE IF (XAXIS .EQ. 2) THEN
		 Y(J,1,SLOT) = RUN(J)
	      END IF
	      IF (YAXIS .EQ. 1) THEN
		 Y(J,2,SLOT) = COUNTS(NCCD,J,NAPER)
		 Y(J,3,SLOT) = SIGMA(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'obj cts, CCD',NCCD,', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(NCCD,J,NAPER)
		 Y(J,3,SLOT) = ESKY(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky cts, CCD',NCCD,', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XMPOS(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EXMPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap x-pos, CCD',NCCD,
     + ', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YMPOS(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EYMPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap y-pos, CCD',NCCD,
     + ', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix reject, CCD',NCCD,', ap',
     + NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = HFWHM(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EFWHM(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     + 'profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = HBETA(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EBETA(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     + 'profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSKY(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix avail, CCD',NCCD,', ap',
     + NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'exp time, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = MFWHM(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = MBETA(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NSKIPS>1? Check the input data file.'
	      END IF

	   END DO
	   DETRENDARRAY(SLOT) = .FALSE.
	   NPTSARRAY(SLOT) = NDATA
	   WRITE(*,*)'** OK: Filled Slot = ',SLOT
	END DO
	END IF
	END IF
	
	IF (NCCD .EQ. 0) THEN
	IF (NAPER .NE. 0) THEN
	K = 0
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
	   K = K + 1
	   DO J = 1, NDATA
	      IF (XAXIS .EQ. 1) THEN
		 Y(J,1,SLOT) = MJD(K,J)
	      ELSE IF (XAXIS .EQ. 2) THEN
		 Y(J,1,SLOT) = RUN(J)
	      END IF
	      IF (YAXIS .EQ. 1) THEN
		 Y(J,2,SLOT) = COUNTS(K,J,NAPER)
		 Y(J,3,SLOT) = SIGMA(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'obj cts, CCD',K,', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(K,J,NAPER)
		 Y(J,3,SLOT) = ESKY(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky cts, CCD',K,', ap',NAPER,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XMPOS(K,J,NAPER)
		 Y(J,3,SLOT) = EXMPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap x-pos, CCD',K,
     + ', ap',NAPER,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YMPOS(K,J,NAPER)
		 Y(J,3,SLOT) = EYMPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap y-pos, CCD',K,
     + ', ap',NAPER,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix reject, CCD',K,', ap',
     + NAPER,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = HFWHM(K,J,NAPER)
		 Y(J,3,SLOT) = EFWHM(K,J,NAPER)		 
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     +                'profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = HBETA(K,J,NAPER)
		 Y(J,3,SLOT) = EBETA(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     + 'profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSKY(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix avail, CCD',K,', ap',
     + NAPER,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exp time, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = MFWHM(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = MBETA(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NSKIPS>1? Check the input data file.'
	      END IF

	   END DO
	   DETRENDARRAY(SLOT) = .FALSE.
	   NPTSARRAY(SLOT) = NDATA
	   WRITE(*,*)'** OK: Filled Slot = ',SLOT
	END DO
	END IF
	END IF

	IF (NCCD .NE. 0) THEN
	IF (NAPER .EQ. 0) THEN
	K = 0
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
	   K = K + 1
	   IF (NCCD .EQ. 1) NDATA = NHDATA(1)
	   IF (NCCD .EQ. 2) NDATA = NHDATA(2)
	   IF (NCCD .EQ. 3) NDATA = NHDATA(3)
	   IF (NCCD .EQ. 4) NDATA = NHDATA(4)
	   IF (NCCD .EQ. 5) NDATA = NHDATA(5)
	   DO J = 1, NDATA
	      IF (XAXIS .EQ. 1) THEN
		 Y(J,1,SLOT) = MJD(NCCD,J)
	      ELSE IF (XAXIS .EQ. 2) THEN
		 Y(J,1,SLOT) = RUN(J)
	      END IF
	      IF (YAXIS .EQ. 1) THEN
		 Y(J,2,SLOT) = COUNTS(NCCD,J,K)
		 Y(J,3,SLOT) = SIGMA(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'obj cts, CCD',NCCD,', ap',K,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(NCCD,J,K)
		 Y(J,3,SLOT) = ESKY(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky cts, CCD',NCCD,', ap',K,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XMPOS(NCCD,J,K)
		 Y(J,3,SLOT) = EXMPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap x-pos, CCD',NCCD,
     + ', ap',K,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YMPOS(NCCD,J,K)
		 Y(J,3,SLOT) = EYMPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap y-pos, CCD',NCCD,
     + ', ap',K,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix reject, CCD',NCCD,', ap',
     + K,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = HFWHM(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EFWHM(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     +                'profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = HBETA(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EBETA(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     + 'profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSKY(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix avail, CCD',NCCD,', ap',
     + K,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exp time, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = MFWHM(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = MBETA(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NSKIPS>1? Check the input data file.'
	      END IF

	   END DO
	   DETRENDARRAY(SLOT) = .FALSE.
	   NPTSARRAY(SLOT) = NDATA
	   WRITE(*,*)'** OK: Filled Slot = ',SLOT
	END DO
	END IF
	END IF

	IF (NCCD .EQ. 0) THEN
	IF (NAPER .EQ. 0) THEN
	K = 0
	H = 1
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
	   K = K + 1
	   IF (K .EQ. 6) THEN
	      H = H + 1
	      K = 1
	   END IF
	   IF (K .EQ. 1) NDATA = NHDATA(1)
	   IF (K .EQ. 2) NDATA = NHDATA(2)
	   IF (K .EQ. 3) NDATA = NHDATA(3)
	   IF (K .EQ. 4) NDATA = NHDATA(4)
	   IF (K .EQ. 5) NDATA = NHDATA(5)
	   DO J = 1, NDATA
	      IF (XAXIS .EQ. 1) THEN
		 Y(J,1,SLOT) = MJD(K,J)
	      ELSE IF (XAXIS .EQ. 2) THEN
		 Y(J,1,SLOT) = RUN(J)
	      END IF
	      IF (YAXIS .EQ. 1) THEN
		 Y(J,2,SLOT) = COUNTS(K,J,H)
		 Y(J,3,SLOT) = SIGMA(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'obj cts, CCD',K,', ap',H,
     + ', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(K,J,H)
		 Y(J,3,SLOT) = ESKY(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky cts, CCD',K,', ap',H,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XMPOS(K,J,H)
		 Y(J,3,SLOT) = EXMPOS(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap x-pos, CCD',K,
     + ', ap',H,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YMPOS(K,J,H)
		 Y(J,3,SLOT) = EYMPOS(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'ap y-pos, CCD',K,
     + ', ap',H,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix reject, CCD',K,', ap',
     + H,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = HFWHM(K,J,NAPER)
		 Y(J,3,SLOT) = EFWHM(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     +                'profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = HBETA(K,J,NAPER)
		 Y(J,3,SLOT) = EBETA(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
		 INFILEARRAY(SLOT) = 
     + 'profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSKY(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'num sky pix avail, CCD',K,', ap',
     + H,', '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exp time, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = MFWHM(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit FWHM, '//HINFILE(1:HINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = MBETA(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'mean profile fit Beta, '//HINFILE(1:HINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NSKIPS>1? Check the input data file.'
	      END IF

	   END DO
	   DETRENDARRAY(SLOT) = .FALSE.
	   NPTSARRAY(SLOT) = NDATA
 	   WRITE(*,*)'** OK: Filled Slot = ',SLOT
	END DO
	END IF
	END IF

99	RETURN
	END
