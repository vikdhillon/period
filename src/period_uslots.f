 
	SUBROUTINE PERIOD_USLOTS (NDATA, MAXAPER, RUN, MJD,
     +                            NSAT, EXPOSE, FWHM, BETA, 
     +                            XPOS, YPOS, XMPOS, YMPOS, 
     +                            EXMPOS, EYMPOS, COUNTS, 
     +                            SIGMA, SKY, NSKY, NREJ, WORST,
     +                            NUM_APER, UINFILE, IVERSION,
     +                            Y, MXROW, MXSLOT,
     +                            NPTSARRAY, YERRORARRAY, 
     +                            INFILEARRAY, DETRENDARRAY)

C=============================================================================
C Routine to load PERIOD slots with ULTRACAM data input using the UINPUT
C command. Prompts the user for which variables to use as the x- and y-axes
C and for which aperture and CCD numbers to load. 
C
C Written by Vik Dhillon @Sheffield 13-May-2003.
C Modified by Vik Dhillon @Nether Edge 01-Feb-2004 to accommodate the FWHM
C and BETA parameters output by v2.4 of the pipeline. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Now handles ultracam3.0.0 log files, Vik Dhillon @Nether Edge 20-April-2004.
C Removed the subtraction of BASE, which was unecessary to preserve 
C accuracy. Also check to see if CCD's have different numbers of apertures
C (in which case it is not possible to process all three CCD's 
C simultaneously), Vik Dhillon @Paranal 06-May-2005.
C Now handles NBLUE, Vik Dhillon @ING 17-October-2007.
C Now handles EXPOSE, Vik Dhillon @Sheffield 27-August-2009.
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
	INTEGER NUM_APER(3), NDATA, UINFILE_LEN
	INTEGER FIRSTSLOT, LASTSLOT, SLOT, XAXIS, YAXIS, YAXISMAX
	CHARACTER*72 INFILEARRAY(MXSLOT), UINFILE
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT) 

C-----------------------------------------------------------------------------
C Declarations for parameters read from the ULTRACAM data reduction pipeline
C log file.
C-----------------------------------------------------------------------------

	INTEGER RUN(MXROW), NSAT(MXROW)
	INTEGER NREJ(3,MXROW,MAXAPER), IVERSION
	DOUBLE PRECISION MJD(3,MXROW)
	DOUBLE PRECISION COUNTS(3,MXROW,MAXAPER), SIGMA(3,MXROW,MAXAPER)
	REAL FWHM(3,MXROW), BETA(3,MXROW), WORST(3,MXROW,MAXAPER)
	REAL EXPOSE(3,MXROW), SKY(3,MXROW,MAXAPER), NSKY(3,MXROW,MAXAPER)
	REAL XPOS(3,MXROW,MAXAPER), YPOS(3,MXROW,MAXAPER)
	REAL XMPOS(3,MXROW,MAXAPER), YMPOS(3,MXROW,MAXAPER)
	REAL EXMPOS(3,MXROW,MAXAPER), EYMPOS(3,MXROW,MAXAPER)

C-----------------------------------------------------------------------------
C Set length of UINFILE to avoid problems with concatenating strings.
C-----------------------------------------------------------------------------

	UINFILE_LEN = LEN_TRIM(UINFILE)

C-----------------------------------------------------------------------------
C Select variables to load as the x- and y-axes. 
C-----------------------------------------------------------------------------

	IF (IVERSION .LT. 20040223) THEN
	   YAXISMAX = 9
        ELSE IF (IVERSION .GE. 2004023 .AND. 
     + 	   IVERSION .LT. 20040629) THEN
	   YAXISMAX = 12
	ELSE
	   YAXISMAX = 13
	END IF
	WRITE(*,*)' '
	WRITE(*,*)'------------------------------------------------------'
	WRITE(*,*)'x-axis options    y-axis options'
	WRITE(*,*)'------------------------------------------------------'
	WRITE(*,*)'1. MJD            1. Object counts (with errors)'
	WRITE(*,*)'2. Run number     2. Sky counts'
	WRITE(*,*)'                  3. x-position of aperture used'
	WRITE(*,*)'                  4. y-position of aperture used'
	WRITE(*,*)'                  5. Number of sky pixels rejected'
	WRITE(*,*)'                  6. FWHM for profile fits'
	WRITE(*,*)'                  7. Beta exponent for Moffat fits'
	WRITE(*,*)'                  8. Number of GPS satellites detected'
	WRITE(*,*)'                  9. Exposure time'
	IF (YAXISMAX .GE. 12) THEN
	WRITE(*,*)'                 10. x-position of aperture measured'
	WRITE(*,*)'                 11. y-position of aperture measured'
	WRITE(*,*)'                 12. Number of sky pixels available'
	END IF
	IF (YAXISMAX .EQ. 13) THEN
	WRITE(*,*)'                 13. Worst bad pixel value'
	END IF
	WRITE(*,*)'------------------------------------------------------'
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
	IF (YAXIS .NE. 8) THEN
20	   WRITE(*,'(A,$)')'Enter CCD number to load (0 for all) : '
	   READ(*,*,ERR=20)NCCD
	   IF (NCCD .LT. 0) GOTO 20
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
	      END IF
	      IF (NUM_APER(1) .NE. NUM_APER(2) .OR.
     +           NUM_APER(1) .NE. NUM_APER(3) .OR.
     +           NUM_APER(2) .NE. NUM_APER(3)) THEN
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
	ELSE
	   NCCD = 1
	END IF
	IF (YAXIS .LT. 6 .OR. YAXIS .GT. 9) THEN
30	   WRITE(*,'(A,$)')
     + 'Enter aperture number to load (0 for all) : '
	   READ(*,*,ERR=30)NAPER
	   IF (NAPER .LT. 0) GOTO 30
	END IF
	IF (YAXIS .LT. 6 .OR. YAXIS .GT. 9) THEN
	   IF (NCCD .EQ. 0) THEN
	      IF (NAPER .EQ. 0) THEN
		 NUMSLOTS = NUM_APER(1)+NUM_APER(2)+NUM_APER(3)
	      ELSE
		 IF (NAPER .GT. NUM_APER(1) .OR. NAPER .GT.  
     + 		 NUM_APER(2) .OR. NAPER .GT. NUM_APER(3)) THEN 
		    NUMSLOTS = 0
		 ELSE
		    NUMSLOTS = 3
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
	   IF (YAXIS .EQ. 8) THEN
	      NUMSLOTS = 1
	   ELSE
	      IF (NCCD .EQ. 0) THEN
		 NUMSLOTS = 3
	      ELSE
		 NUMSLOTS = 1
	      END IF
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
	LASTSLOT = FIRSTSLOT + NUMSLOTS - 1
        IF (FIRSTSLOT .EQ. 0) THEN
	   GOTO 99
        ELSE IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
           GOTO 99
        END IF

C-----------------------------------------------------------------------------
C First load the detrend and npts arrays. 
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
	   DO J = 1, NDATA
	   END DO
	   DETRENDARRAY(SLOT) = .FALSE.
	   NPTSARRAY(SLOT) = NDATA
	END DO

C-----------------------------------------------------------------------------
C Now the tricky bit...load the x-axis and y-axis arrays, as well as the
C yerror, infile, detrend and ntps arrays. Also, check to see if the x-axis 
C data is in ascending order. Unless NBLUE>1, it should be - so just warn the 
C user and don't offer to resort (as occurs in the related subroutine 
C PERIOD_INPUT).
C-----------------------------------------------------------------------------

	IF (NCCD .NE. 0) THEN
	IF (NAPER .NE. 0) THEN
	DO I = FIRSTSLOT, LASTSLOT
	   SLOT = I
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
     + 'object counts, CCD',NCCD,', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky counts, CCD',NCCD,', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position used, CCD',NCCD,
     + ', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position used, CCD',NCCD,
     + ', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels rejected, CCD',NCCD,', aperture',
     + NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = FWHM(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'FWHM for profile fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = BETA(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'Beta exponent for Moffat fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSAT(J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'number of GPS satellites detected, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'exposure time, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = XMPOS(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EXMPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position measured, CCD',NCCD,
     + ', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = YMPOS(NCCD,J,NAPER)
		 Y(J,3,SLOT) = EYMPOS(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position measured, CCD',NCCD,
     + ', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 12) THEN
		 Y(J,2,SLOT) = NSKY(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels available, CCD',NCCD,', aperture',
     + NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 13) THEN
		 Y(J,2,SLOT) = WORST(NCCD,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'worst bad pixel value, CCD',NCCD,', aperture',
     + NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NBLUE>1? Check the input data file.'
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
     + 'object counts, CCD',K,', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky counts, CCD',K,', aperture',NAPER,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position used, CCD',K,
     + ', aperture',NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position used, CCD',K,
     + ', aperture',NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels rejected, CCD',K,', aperture',
     + NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = FWHM(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'FWHM for profile fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = BETA(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'Beta exponent for Moffat fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSAT(J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'number of GPS satellites detected, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exposure time, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = XMPOS(K,J,NAPER)
		 Y(J,3,SLOT) = EXMPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position measured, CCD',K,
     + ', aperture',NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = YMPOS(K,J,NAPER)
		 Y(J,3,SLOT) = EYMPOS(K,J,NAPER)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position measured, CCD',K,
     + ', aperture',NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 12) THEN
		 Y(J,2,SLOT) = NSKY(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels available, CCD',K,', aperture',
     + NAPER,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 13) THEN
		 Y(J,2,SLOT) = WORST(K,J,NAPER)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'worst bad pixel value, CCD',K,', aperture',
     + NAPER,', '//UINFILE(1:UINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NBLUE>1? Check the input data file.'
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
     + 'object counts, CCD',NCCD,', aperture',K,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky counts, CCD',NCCD,', aperture',K,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position used, CCD',NCCD,
     + ', aperture',K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position used, CCD',NCCD,
     + ', aperture',K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels rejected, CCD',NCCD,', aperture',
     + K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = FWHM(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'FWHM for profile fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = BETA(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'Beta exponent for Moffat fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSAT(J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'number of GPS satellites detected, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(NCCD,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exposure time, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = XMPOS(NCCD,J,K)
		 Y(J,3,SLOT) = EXMPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position measured, CCD',NCCD,
     + ', aperture',K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = YMPOS(NCCD,J,K)
		 Y(J,3,SLOT) = EYMPOS(NCCD,J,K)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position measured, CCD',NCCD,
     + ', aperture',K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 12) THEN
		 Y(J,2,SLOT) = NSKY(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels available, CCD',NCCD,', aperture',
     + K,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 13) THEN
		 Y(J,2,SLOT) = WORST(NCCD,J,K)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'worst bad pixel value, CCD',NCCD,', aperture',
     + K,', '//UINFILE(1:UINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NBLUE>1? Check the input data file.'
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
	   IF (K .EQ. 4) THEN
	      H = H + 1
	      K = 1
	   END IF
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
     + 'object counts, CCD',K,', aperture',H,
     + ', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 2) THEN
		 Y(J,2,SLOT) = SKY(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'sky counts, CCD',K,', aperture',H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 3) THEN
		 Y(J,2,SLOT) = XPOS(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position used, CCD',K,
     + ', aperture',H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 4) THEN
		 Y(J,2,SLOT) = YPOS(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position used, CCD',K,
     + ', aperture',H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 5) THEN
		 Y(J,2,SLOT) = NREJ(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels rejected, CCD',K,', aperture',
     + H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 6) THEN
		 Y(J,2,SLOT) = FWHM(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'FWHM for profile fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 7) THEN
		 Y(J,2,SLOT) = BETA(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'Beta exponent for Moffat fits, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 8) THEN
		 Y(J,2,SLOT) = NSAT(J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     + 'number of GPS satellites detected, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 9) THEN
		 Y(J,2,SLOT) = EXPOSE(K,J)
		 YERRORARRAY(SLOT) = .FALSE.
		 INFILEARRAY(SLOT) = 
     +                'exposure time, '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 10) THEN
		 Y(J,2,SLOT) = XMPOS(K,J,H)
		 Y(J,3,SLOT) = EXMPOS(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture x-position measured, CCD',K,
     + ', aperture',H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 11) THEN
		 Y(J,2,SLOT) = YMPOS(K,J,H)
		 Y(J,3,SLOT) = EYMPOS(K,J,H)
		 YERRORARRAY(SLOT) = .TRUE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'aperture y-position measured, CCD',K,
     + ', aperture',H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 12) THEN
		 Y(J,2,SLOT) = NSKY(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'number of sky pixels available, CCD',K,', aperture',
     + H,', '//UINFILE(1:UINFILE_LEN)
	      ELSE IF (YAXIS .EQ. 13) THEN
		 Y(J,2,SLOT) = WORST(K,J,H)
		 YERRORARRAY(SLOT) = .FALSE.
  		 WRITE(INFILEARRAY(SLOT),'(A,I1,A,I2,A)')
     + 'worst bad pixel value, CCD',K,', aperture',
     + H,', '//UINFILE(1:UINFILE_LEN)
	      END IF

	      IF (Y(J,1,SLOT) .LE. Y(J-1,1,SLOT)) THEN
		 IF (I .NE. FIRSTSLOT) WRITE(*,*)' '
		 WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at run number =',J
		 WRITE(*,*)'** WARNING: NBLUE>1? Check the input data file.'
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
