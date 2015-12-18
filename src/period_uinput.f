
	SUBROUTINE PERIOD_UINPUT (NDATA, MAXAPER, RUN, MJD, 
     +                            NSAT, EXPOSE, FWHM, BETA,
     +                            XPOS, YPOS, XMPOS, YMPOS,
     +                            EXMPOS, EYMPOS, COUNTS, 
     +                            SIGMA, SKY, NSKY, NREJ, WORST, 
     +                            NUM_APER, UINFILE, MXROW,  
     +                            MAXCHARS, STRING, IFAIL, 
     +                            MAXHEADER, IVERSION)

C=============================================================================
C Routine to input ULTRACAM data into the PERIOD program. The data must be 
C read from the log files output by the 'reduce' program of the ULTRACAM data
C reduction pipeline. IFAIL=1 if an error has occured, otherwise IFAIL=0.
C
C Written by Vik Dhillon @Sheffield 12-May-2003.
C Modified by Vik Dhillon @Nether Edge 01-Feb-2004 to cope with ultracam-v2.4
C reduce files
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C Now handles ultracam3.0.0 log files, Vik Dhillon @Nether Edge 20-April-2004.
C Now handles ultracam4.0.0 log files, Vik Dhillon @Sheffield 12-July-2004.
C Now handles multiple log files, Vik Dhillon @WHT 10-August-2005.
C Now handles ultracam8.0.0 log files, Vik Dhillon @Mons 20-April-2006.
C Now handles NBLUE, Vik Dhillon @ING 17-October-2007.
C Now handles 'r0' and 'run' in run numbers properly, VSD @ING 27-June-2014.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_UINPUT declarations.
C-----------------------------------------------------------------------------
	
	INTEGER I, J, K, L, MAXCHARS, MAXAPER, MXROW, MAXHEADER
	INTEGER NTOTAL, NCOLS, NHEADS, NUM_APER(3), NDATA, IUNIT
	INTEGER IFAIL, IVERSION, NDIV, NCCD, LENGTH
	REAL JUNK
	CHARACTER*(*) STRING(1)
	CHARACTER*72 UINFILE, INFILE
	CHARACTER*8 CVERSION
	CHARACTER*1 REPLY
	LOGICAL LFATAL, LNONFATAL, LBLANK, LNSAT
	DATA IUNIT /10/

C-----------------------------------------------------------------------------
C Declarations for parameters read from the ULTRACAM data reduction pipeline
C log file.
C-----------------------------------------------------------------------------

	INTEGER CCD, NAPER
	INTEGER RUN(MXROW), FLAG(MXROW), NSAT(MXROW)
	INTEGER NREJ(3,MXROW,MAXAPER), ERROR_FLAG(3,MXROW,MAXAPER)
	DOUBLE PRECISION MJD(3,MXROW)
	DOUBLE PRECISION COUNTS(3,MXROW,MAXAPER), SIGMA(3,MXROW,MAXAPER)
	REAL FWHM(3,MXROW), BETA(3,MXROW), WORST(3,MXROW,MAXAPER)
	REAL EXPOSE(3,MXROW), SKY(3,MXROW,MAXAPER), NSKY(3,MXROW,MAXAPER)
	REAL XPOS(3,MXROW,MAXAPER), YPOS(3,MXROW,MAXAPER)
	REAL XMPOS(3,MXROW,MAXAPER), YMPOS(3,MXROW,MAXAPER)
	REAL EXMPOS(3,MXROW,MAXAPER), EYMPOS(3,MXROW,MAXAPER)

C-----------------------------------------------------------------------------
C Prompt for name of data file and open it.
C-----------------------------------------------------------------------------

	K = 0
10	WRITE(*,'(A,$)')'Enter name of data file (<CR> to quit) : '
	READ(*,'(A)',ERR=10)INFILE
	IF (INFILE .EQ. ' ') THEN
	   IF (K .EQ. 0) THEN
	      IFAIL = 1
	   ELSE
	      IFAIL = 0
	   END IF
	   GOTO 99
	END IF
	UINFILE = INFILE
        OPEN(UNIT=10,FILE=UINFILE,STATUS='OLD',ERR=10)

C-----------------------------------------------------------------------------
C Read in data.
C-----------------------------------------------------------------------------

	DO I = 1, MXROW*3+MAXHEADER
	   READ(IUNIT,'(A)',END=20)STRING(I)
	END DO
20	IF (I .EQ. MXROW*3) THEN
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)
     + '** WARNING: Maximum number of rows reached = ',MXROW*3+MAXHEADER
	   WRITE(*,*)
     + '** WARNING: Change dimension of MXROW in PERIOD.'
	   IFAIL = 1
	   GOTO 99
	ELSE 
	   NTOTAL = I - 1
	END IF
	CLOSE (UNIT=IUNIT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Total number of lines read = ',NTOTAL

C-----------------------------------------------------------------------------
C Determine number of header lines.
C-----------------------------------------------------------------------------

	NHEADS = 0
	DO I = 1, NTOTAL
	   IF (STRING(I)(1:1) .EQ. '#') THEN
	      NHEADS = NHEADS + 1
	   END IF
	END DO
	WRITE(*,*)'** OK: Number of header lines found = ',NHEADS
	IF (NHEADS .GT. MAXHEADER) THEN
	   WRITE(*,*)' '
	   WRITE(*,*)'** WARNING: Too many header lines.'
	   WRITE(*,*)
     + '** WARNING: Modify dimension of MAXHEADER in PERIOD_MAIN.'
	   WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Determine version of software. Version 8 has a date of 19/12/2005 or later.
C Version 4 has a date between 2004/06/29 and 19/12/2005. Version 3 has a date 
C between 2004/02/23 and 2004/06/29. The version number is used to set the 
C number of columns for each aperture in the reduce log file. Note that it is
C possible I have missed some reduce log file changes (especially between 
C versions 4 and 8), so UINPUT may state a pipeline version number of 4 even
C if it is actually 5, 6 or 7.
C
C Pipeline version 8.1.32 onwards does not output the number of satellites to
C the reduce log file as this parameter is not written by the new data 
C acquisition system (which was first used on-sky on the NTT on 2010 April 19).
C Unfortunately, the reduce log file version remained at 20051219, so it is 
C necessary to find the line defining the contents of the columns of the reduce 
C log file and search for the absence of the characters "nsat".
C-----------------------------------------------------------------------------

	DO I = 1, 100
	   IF (STRING(I)(3:9) .EQ. 'Version') THEN
	      CVERSION(1:4) = STRING(I)(62:65)
	      CVERSION(5:6) = STRING(I)(59:60)
	      CVERSION(7:8) = STRING(I)(56:57)
	      J = I
	      READ(CVERSION,*)IVERSION
	   END IF
	   IF (STRING(I)(3:13) .EQ. 'name/number') THEN
	      IF (STRING(I)(24:28) .EQ. 'nsat') THEN
		 LNSAT = .TRUE.
	      ELSE
		 LNSAT = .FALSE.
	      END IF
	   END IF
	END DO
	IF (IVERSION .LT. 20020516 .OR. IVERSION .GT. 20051219) THEN 
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)'** ERROR: Unrecognised version = ',IVERSION
	   IFAIL = 1
	   GOTO 99
	END IF
	IF (IVERSION .LT. 20040223) THEN
	   WRITE(*,*)
     + '** OK: Early pipeline version, reduce file = ',STRING(J)(56:65)
	   NDIV = 8
	ELSE IF (IVERSION .GE. 20040223 .AND. IVERSION .LT. 20040629) THEN
	   WRITE(*,*)
     + '** OK: Pipeline version 3, reduce file = ',STRING(J)(56:65)
	   NDIV = 13
 	ELSE IF (IVERSION .GE. 20040629 .AND. IVERSION .LT. 20051219) THEN
	   WRITE(*,*)
     + '** OK: Pipeline version 4, reduce file = ',STRING(J)(56:65)
	   NDIV = 14
	ELSE 
	   IF (.NOT. LNSAT) THEN
	      WRITE(*,*)
     + '** OK: Pipeline version 8.1.32 (or later), reduce file = ',
     + STRING(J)(56:65)
	      NDIV = 14
	   ELSE
	      WRITE(*,*)
     + '** OK: Pipeline version 8, reduce file = ',STRING(J)(56:65)
	      NDIV = 14
	   END IF
	END IF

C-----------------------------------------------------------------------------
C If the "u" option has been used, there will be filenames present in the 
C first column instead of run numbers. This next section strips out any
C characters from this first column to avoid crashing the READ command
C below (which can only handle numbers). It therefore makes sense to name 
C .ucm files as simply as possible, preferably after their run number.
C I also strip out any occurrences of "nan" and "inf".
C-----------------------------------------------------------------------------

        DO I = NHEADS+1, NTOTAL
           DO J = 1, MAXCHARS
              IF (STRING(I)(J:J) .EQ. '_') THEN
		 WRITE(*,*)'** WARNING: Stripping out "_"'
                 STRING(I)(J:J) = '0'
	      ELSE IF (STRING(I)(J:J) .EQ. '.') THEN
		 IF (STRING(I)(J+1:J+3) .EQ. 'ucm') THEN
		    WRITE(*,*)'** WARNING: Stripping out ".ucm"'
		    STRING(I)(J:J+3) = '    '
		 END IF
	      ELSE IF (STRING(I)(J:J) .EQ. 'n') THEN
		 IF (STRING(I)(J:J+2) .EQ. 'nan') THEN
		    WRITE(*,*)'** WARNING: Stripping out "nan"'
		    STRING(I)(J:J+2) = '000'
	         END IF
	      ELSE IF (STRING(I)(J:J) .EQ. 'i') THEN
		 IF (STRING(I)(J:J+2) .EQ. 'inf') THEN
		    WRITE(*,*)'** WARNING: Stripping out "inf"'
		    STRING(I)(J:J+2) = '000'
	         END IF
	      ELSE IF (STRING(I)(J:J) .EQ. 'r') THEN
		 IF (STRING(I)(J:J+1) .EQ. 'r0') THEN
		    WRITE(*,*)'** WARNING: Stripping out "r"'
		    STRING(I)(J:J+1) = '  '
		 ELSE IF (STRING(I)(J:J+2) .EQ. 'run') THEN
		    WRITE(*,*)'** WARNING: Stripping out "run"'
		    STRING(I)(J:J+2) = '  '
	         END IF
              END IF
           END DO
        END DO

C-----------------------------------------------------------------------------
C Determine number of apertures present for each CCD. Annoyingly, from 
C pipeline version 8 (and maybe earlier), Tom stopped writing a line to the 
C log file for each CCD if the CCD had no apertures reduced. I guess this is 
C because it saves space in the log file. So, I must now determine the number
C of CCDs and set the number of apertures of any missing CCDs to 0. In this
C way, the rest of period can remain unchanged.
C-----------------------------------------------------------------------------

	NCCD = 0
	DO J = 1, 3
	   NUM_APER(J) = 0

C-----------------------------------------------------------------------------
C The code below works out the number of columns in the string - copied from
C PERIOD_READFREE
C-----------------------------------------------------------------------------

	   NCOLS = 0
	   LBLANK = .TRUE.
	   LENGTH = LEN(STRING(NHEADS+J))
	   DO I = 1, LENGTH
	      IF(LBLANK .AND. STRING(NHEADS+J)(I:I) .NE.' ' ) THEN
		 NCOLS = NCOLS + 1
		 LBLANK = .FALSE.
	      ELSE IF(.NOT. LBLANK .AND. STRING(NHEADS+J)(I:I) 
     + .EQ. ' ') THEN
		 LBLANK = .TRUE.
	      END IF
	   END DO

C-----------------------------------------------------------------------------
C Now work out the number of apertures and CCDs
C-----------------------------------------------------------------------------

	   IF (.NOT. LNSAT) THEN
	      READ(STRING(NHEADS+J),*) (JUNK, I=1, 4), CCD, 
     +     (JUNK, I=6, NCOLS)	   
	      NUM_APER(CCD) = (NCOLS-7)/NDIV
	   ELSE
	      READ(STRING(NHEADS+J),*) (JUNK, I=1, 5), CCD, 
     +     (JUNK, I=7, NCOLS)
	      NUM_APER(CCD) = (NCOLS-8)/NDIV	      
	   END IF

	   IF (NUM_APER(J) .GE. 1) NCCD = NCCD + 1
           WRITE(*,*)'** OK: Number of apertures for CCD',J,
     +     ' = ',NUM_APER(J)	   
	END DO	
	WRITE(*,*)'** OK: Number of CCD''s = ',NCCD

	IF (NUM_APER(1) .GT. MAXAPER .OR. 
     +      NUM_APER(2) .GT. MAXAPER .OR.
     +      NUM_APER(3) .GT. MAXAPER) THEN
	   WRITE(*,*)ACHAR(7)	   
	   WRITE(*,*)
     +     '** ERROR: Maximum number of apertures = ',MAXAPER
	   IFAIL = 1
	   GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Load data arrays. 
C For pre-2004/02/23, format of reduce log file is: 
C name/number mjd flag nsat expose ccd fwhm beta [naper x y counts sigma sky 
C nrej error_flag]*num_aper
C For post-2004/02/23 but pre-2004/06/29, format of reduce log file is: 
C name/number mjd flag nsat expose ccd fwhm beta [naper x y xm ym exm eym 
C counts sigma sky nsky nrej error_flag]*num_aper
C For post-2004/06/29, format of reduce log file is: 
C name/number mjd flag nsat expose ccd fwhm beta [naper x y xm ym exm eym 
C counts sigma sky nsky nrej worst error_flag]*num_aper
C-----------------------------------------------------------------------------

	IF (IVERSION .LT. 20051219) THEN

	DO I = NHEADS+1, NTOTAL-2, 3
	   K = K + 1

	   IF (IVERSION .LT. 20040223) THEN
	      READ(STRING(I),*)RUN(K),MJD(1,K),FLAG(K),NSAT(K),
     +                EXPOSE(1,K),CCD,FWHM(1,K),BETA(1,K),
     +                (NAPER,XPOS(1,K,J),YPOS(1,K,J),
     +                COUNTS(1,K,J),SIGMA(1,K,J),SKY(1,K,J), 
     +                NREJ(1,K,J),ERROR_FLAG(1,K,J),J=1,NUM_APER(1))
	   ELSE IF (IVERSION .GE. 2004023 .AND. IVERSION 
     + .LT. 20040629) THEN
	      READ(STRING(I),*)RUN(K),MJD(1,K),FLAG(K),NSAT(K),
     +                EXPOSE(1,K),CCD,FWHM(1,K),BETA(1,K),
     +                (NAPER,XPOS(1,K,J),YPOS(1,K,J),
     +                XMPOS(1,K,J),YMPOS(1,K,J), 
     +                EXMPOS(1,K,J),EYMPOS(1,K,J),
     +                COUNTS(1,K,J),SIGMA(1,K,J),SKY(1,K,J), 
     +                NSKY(1,K,J),
     +                NREJ(1,K,J),ERROR_FLAG(1,K,J),J=1,NUM_APER(1))
	   ELSE 
	      READ(STRING(I),*)RUN(K),MJD(1,K),FLAG(K),NSAT(K),
     +                EXPOSE(1,K),CCD,FWHM(1,K),BETA(1,K),
     +                (NAPER,XPOS(1,K,J),YPOS(1,K,J),
     +                XMPOS(1,K,J),YMPOS(1,K,J), 
     +                EXMPOS(1,K,J),EYMPOS(1,K,J),
     +                COUNTS(1,K,J),SIGMA(1,K,J),SKY(1,K,J), 
     +                NSKY(1,K,J),NREJ(1,K,J),		 
     +                WORST(1,K,J),ERROR_FLAG(1,K,J),J=1,NUM_APER(1))
	   END IF

	   IF (RUN(K) .LT. 0) THEN
	      RUN(K) = K
	   END IF

	   IF (CCD .NE. 1) THEN
	      WRITE(*,*)'** ERROR: Expected CCD = 1 but read CCD =',CCD
	      IFAIL = 1
	      GOTO 99
	   END IF

	   IF (IVERSION .LT. 20040223) THEN
	      READ(STRING(I+1),*)RUN(K),MJD(2,K),FLAG(K),NSAT(K),
     +                EXPOSE(2,K),CCD,FWHM(2,K),BETA(2,K),
     +                (NAPER,XPOS(2,K,J),YPOS(2,K,J),
     +                COUNTS(2,K,J),SIGMA(2,K,J),SKY(2,K,J),
     +                NREJ(2,K,J),ERROR_FLAG(2,K,J),J=1,NUM_APER(2))
	   ELSE IF (IVERSION .GE. 2004023 .AND. IVERSION 
     + .LT. 20040629) THEN
	      READ(STRING(I+1),*)RUN(K),MJD(2,K),FLAG(K),NSAT(K),
     +                EXPOSE(2,K),CCD,FWHM(2,K),BETA(2,K),
     +                (NAPER,XPOS(2,K,J),YPOS(2,K,J),
     +                XMPOS(2,K,J),YMPOS(2,K,J), 
     +                EXMPOS(2,K,J),EYMPOS(2,K,J),
     +                COUNTS(2,K,J),SIGMA(2,K,J),SKY(2,K,J),
     +                NSKY(2,K,J), 
     +                NREJ(2,K,J),ERROR_FLAG(2,K,J),J=1,NUM_APER(2))
	   ELSE 
	      READ(STRING(I+1),*)RUN(K),MJD(2,K),FLAG(K),NSAT(K),
     +                EXPOSE(2,K),CCD,FWHM(2,K),BETA(2,K),
     +                (NAPER,XPOS(2,K,J),YPOS(2,K,J),
     +                XMPOS(2,K,J),YMPOS(2,K,J), 
     +                EXMPOS(2,K,J),EYMPOS(2,K,J),
     +                COUNTS(2,K,J),SIGMA(2,K,J),SKY(2,K,J),
     +                NSKY(2,K,J),NREJ(2,K,J), 
     +                WORST(2,K,J),ERROR_FLAG(2,K,J),J=1,NUM_APER(2))
	   END IF

	   IF (RUN(K) .LT. 0) THEN
	      RUN(K) = K
	   END IF

	   IF (CCD .NE. 2) THEN
	      WRITE(*,*)'** ERROR: Expected CCD = 2 but read CCD =',CCD
	      IFAIL = 1
	      GOTO 99
	   END IF

	   IF (IVERSION .LT. 20040223) THEN
	      READ(STRING(I+2),*)RUN(K),MJD(3,K),FLAG(K),NSAT(K),
     +                EXPOSE(3,K),CCD,FWHM(3,K),BETA(3,K),
     +                (NAPER,XPOS(3,K,J),YPOS(3,K,J),
     +                COUNTS(3,K,J),SIGMA(3,K,J),SKY(3,K,J),
     +                NREJ(3,K,J),ERROR_FLAG(3,K,J),J=1,NUM_APER(3))
	   ELSE IF (IVERSION .GE. 2004023 .AND. IVERSION 
     + .LT. 20040629) THEN
	      READ(STRING(I+2),*)RUN(K),MJD(3,K),FLAG(K),NSAT(K),
     +                EXPOSE(3,K),CCD,FWHM(3,K),BETA(3,K),
     +                (NAPER,XPOS(3,K,J),YPOS(3,K,J),
     +                XMPOS(3,K,J),YMPOS(3,K,J), 
     +                EXMPOS(3,K,J),EYMPOS(3,K,J),
     +                COUNTS(3,K,J),SIGMA(3,K,J),SKY(3,K,J),
     +                NSKY(3,K,J),
     +                NREJ(3,K,J),ERROR_FLAG(3,K,J),J=1,NUM_APER(3))
	   ELSE
	      READ(STRING(I+2),*)RUN(K),MJD(3,K),FLAG(K),NSAT(K),
     +                EXPOSE(3,K),CCD,FWHM(3,K),BETA(3,K),
     +                (NAPER,XPOS(3,K,J),YPOS(3,K,J),
     +                XMPOS(3,K,J),YMPOS(3,K,J), 
     +                EXMPOS(3,K,J),EYMPOS(3,K,J),
     +                COUNTS(3,K,J),SIGMA(3,K,J),SKY(3,K,J),
     +                NSKY(3,K,J),NREJ(3,K,J),
     +                WORST(3,K,J),ERROR_FLAG(3,K,J),J=1,NUM_APER(3))
	   END IF

	   IF (RUN(K) .LT. 0) THEN
	      RUN(K) = K
	   END IF

	   IF (CCD .NE. 3) THEN
	      WRITE(*,*)'** ERROR: Expected CCD = 3 but read CCD =',CCD
	      IFAIL = 1
	      GOTO 99
	   END IF

	END DO

C-----------------------------------------------------------------------------
C For post-20051219, handle the data input separately. I probably could merge
C this next bit with that above so that the same block of code will read in
C all reduce file formats, but I can't be bothered at the moment.
C-----------------------------------------------------------------------------

	ELSE

	DO I = NHEADS+1, NTOTAL-NCCD+1, NCCD
	   L = 0
	   K = K + 1

	   IF (NUM_APER(1) .GE. 1) THEN
	      IF (.NOT. LNSAT) THEN
		 READ(STRING(I),*)RUN(K),MJD(1,K),FLAG(K),
     +                EXPOSE(1,K),CCD,FWHM(1,K),BETA(1,K),
     +                (NAPER,XPOS(1,K,J),YPOS(1,K,J),
     +                XMPOS(1,K,J),YMPOS(1,K,J), 
     +                EXMPOS(1,K,J),EYMPOS(1,K,J),
     +                COUNTS(1,K,J),SIGMA(1,K,J),SKY(1,K,J), 
     +                NSKY(1,K,J),NREJ(1,K,J),
     +                WORST(1,K,J),ERROR_FLAG(1,K,J),J=1,NUM_APER(1))
		 NSAT(K) = -1
		 L = L + 1
	      ELSE
		 READ(STRING(I),*)RUN(K),MJD(1,K),FLAG(K),NSAT(K),
     +                EXPOSE(1,K),CCD,FWHM(1,K),BETA(1,K),
     +                (NAPER,XPOS(1,K,J),YPOS(1,K,J),
     +                XMPOS(1,K,J),YMPOS(1,K,J), 
     +                EXMPOS(1,K,J),EYMPOS(1,K,J),
     +                COUNTS(1,K,J),SIGMA(1,K,J),SKY(1,K,J), 
     +                NSKY(1,K,J),NREJ(1,K,J),
     +                WORST(1,K,J),ERROR_FLAG(1,K,J),J=1,NUM_APER(1))
		 L = L + 1
	      END IF

	      IF (RUN(K) .LT. 0) THEN
		 RUN(K) = K
	      END IF

	      IF (CCD .NE. 1) THEN
		 WRITE(*,*)'** ERROR: Expected CCD = 1 but read CCD =',CCD
		 IFAIL = 1
		 GOTO 99
	      END IF

	   END IF


	   IF (NUM_APER(2) .GE. 1) THEN
	      IF (.NOT. LNSAT) THEN
		 READ(STRING(I+L),*)RUN(K),MJD(2,K),FLAG(K),
     +                EXPOSE(2,K),CCD,FWHM(2,K),BETA(2,K),
     +                (NAPER,XPOS(2,K,J),YPOS(2,K,J),
     +                XMPOS(2,K,J),YMPOS(2,K,J), 
     +                EXMPOS(2,K,J),EYMPOS(2,K,J),
     +                COUNTS(2,K,J),SIGMA(2,K,J),SKY(2,K,J),
     +                NSKY(2,K,J),NREJ(2,K,J), 
     +                WORST(2,K,J),ERROR_FLAG(2,K,J),J=1,NUM_APER(2))
		 NSAT(K) = -1
		 L = L + 1
	      ELSE
		 READ(STRING(I+L),*)RUN(K),MJD(2,K),FLAG(K),NSAT(K),
     +                EXPOSE(2,K),CCD,FWHM(2,K),BETA(2,K),
     +                (NAPER,XPOS(2,K,J),YPOS(2,K,J),
     +                XMPOS(2,K,J),YMPOS(2,K,J), 
     +                EXMPOS(2,K,J),EYMPOS(2,K,J),
     +                COUNTS(2,K,J),SIGMA(2,K,J),SKY(2,K,J),
     +                NSKY(2,K,J),NREJ(2,K,J), 
     +                WORST(2,K,J),ERROR_FLAG(2,K,J),J=1,NUM_APER(2))
		 L = L + 1
	      END IF

	      IF (RUN(K) .LT. 0) THEN
		 RUN(K) = K
	      END IF
	      
	      IF (CCD .NE. 2) THEN
		 WRITE(*,*)'** ERROR: Expected CCD = 2 but read CCD =',CCD
		 IFAIL = 1
		 GOTO 99
	      END IF

	   END IF

	   IF (NUM_APER(3) .GE. 1) THEN
	      IF (.NOT. LNSAT) THEN
		 READ(STRING(I+L),*)RUN(K),MJD(3,K),FLAG(K),
     +                EXPOSE(3,K),CCD,FWHM(3,K),BETA(3,K),
     +                (NAPER,XPOS(3,K,J),YPOS(3,K,J),
     +                XMPOS(3,K,J),YMPOS(3,K,J), 
     +                EXMPOS(3,K,J),EYMPOS(3,K,J),
     +                COUNTS(3,K,J),SIGMA(3,K,J),SKY(3,K,J),
     +                NSKY(3,K,J),NREJ(3,K,J),
     +                WORST(3,K,J),ERROR_FLAG(3,K,J),J=1,NUM_APER(3))
		 NSAT(K) = -1
	      ELSE
		 READ(STRING(I+L),*)RUN(K),MJD(3,K),FLAG(K),NSAT(K),
     +                EXPOSE(3,K),CCD,FWHM(3,K),BETA(3,K),
     +                (NAPER,XPOS(3,K,J),YPOS(3,K,J),
     +                XMPOS(3,K,J),YMPOS(3,K,J), 
     +                EXMPOS(3,K,J),EYMPOS(3,K,J),
     +                COUNTS(3,K,J),SIGMA(3,K,J),SKY(3,K,J),
     +                NSKY(3,K,J),NREJ(3,K,J),
     +                WORST(3,K,J),ERROR_FLAG(3,K,J),J=1,NUM_APER(3))
	      END IF
	      IF (RUN(K) .LT. 0) THEN
		 RUN(K) = K
	      END IF

	      IF (CCD .NE. 3) THEN
		 WRITE(*,*)'** ERROR: Expected CCD = 3 but read CCD =',CCD
		 IFAIL = 1
		 GOTO 99
	      END IF

	   END IF

	END DO

	END IF

	NDATA = K
	WRITE(*,*)'** OK: Number of runs = ',K
	WRITE(*,'(A,F17.11)')' ** OK: First MJD = ',MJD(1,1)
	WRITE(*,'(A,F17.11)')' ** OK: Last MJD = ',MJD(1,NDATA)

C-----------------------------------------------------------------------------
C Report warnings and errors if requested.
C-----------------------------------------------------------------------------
	
	WRITE(*,*)' '
	WRITE(*,'(A,$)')
     + 'Do you want to see a listing of the fatal errors? [N] : '
	READ(*,'(A)')REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.)
	IF (REPLY .EQ. 'Y') THEN 
	   LFATAL = .TRUE.
	ELSE 
	   LFATAL = .FALSE.
	END IF

	WRITE(*,'(A,$)')
     + 'Do you want to see a listing of the non-fatal errors? [N] : '
	READ(*,'(A)')REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.)
	IF (REPLY .EQ. 'Y') THEN 
	   LNONFATAL = .TRUE.
	ELSE 
	   LNONFATAL = .FALSE.
	END IF

	IF (LNONFATAL .OR. LFATAL) WRITE(*,*)' '
	DO I = 1, NDATA

	   IF (LNONFATAL) THEN
	      IF (FLAG(I) .EQ. 0) WRITE(*,*)
     + '** WARNING: MJD unreliable at run = ',RUN(I)
	      IF (NSAT(I) .LT. 4) WRITE(*,*)	      
     + '** WARNING: NSAT < 4 at run = ',RUN(I)
	      DO J = 1, NUM_APER(1)
		 IF (ERROR_FLAG(1,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(1,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(1,I,J),', aperture =',J,', CCD1, run =',RUN(I)
		 END IF
	      END DO

	      DO J = 1, NUM_APER(2)
		 IF (ERROR_FLAG(2,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(2,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(2,I,J),', aperture =',J,', CCD2, run =',RUN(I)
		 END IF
	      END DO

	      DO J = 1, NUM_APER(3)
		 IF (ERROR_FLAG(3,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(3,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(3,I,J),', aperture =',J,', CCD3, run =',RUN(I)
		 END IF
	      END DO
	   END IF

	   IF (LFATAL) THEN
	      DO J = 1, NUM_APER(1)
		 IF (ERROR_FLAG(1,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(1,I,J),', aperture =',J,', CCD1, run =',RUN(I)
		 END IF
	      END DO

	      DO J = 1, NUM_APER(2)
		 IF (ERROR_FLAG(2,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(2,I,J),', aperture =',J,', CCD2, run =',RUN(I)
		 END IF
	      END DO

	      DO J = 1, NUM_APER(3)
		 IF (ERROR_FLAG(3,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(3,I,J),', aperture =',J,', CCD3, run =',RUN(I)
		 END IF
	      END DO
	   END IF

	END DO

C Get rid of annoying g95 warning by doing something meaningless with these 
C unused variables!

	NAPER = INT(JUNK)
	JUNK = REAL(NAPER)

	WRITE(*,*)' '
	GOTO 10
99	RETURN
	END
