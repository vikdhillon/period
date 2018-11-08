
	SUBROUTINE PERIOD_HINPUT (NHDATA, MAXAPER, RUN, MJD, 
     +                            EXPOSE, MFWHM, MBETA, XMPOS, EXMPOS, 
     +                            YMPOS, EYMPOS, HFWHM, EFWHM, HBETA,
     +                            EBETA, COUNTS, SIGMA, SKY, ESKY,
     +                            NSKY, NREJ, NUM_APER, HINFILE, 
     +                            MXROW, MAXCHARS, STRING, IFAIL, 
     +                            MAXHEADER, IVERSION)

C=============================================================================
C Routine to input HiPERCAM data into the PERIOD program. The data must be 
C read from the log files output by the 'reduce' program of the HiPERCAM data
C reduction pipeline. IFAIL=1 if an error has occured, otherwise IFAIL=0.
C
C Written by Vik Dhillon @Sheffield 24-Oct-2017.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_HINPUT declarations.
C-----------------------------------------------------------------------------
	
	INTEGER I, J, K, MAXCHARS, MAXAPER, MXROW, MAXHEADER
	INTEGER N1, N2, N3, N4, N5
	INTEGER NTOTAL, NCOLS, NHEADS, NUM_APER(5), NHDATA(5)
	INTEGER IFAIL, IVERSION, NDIV, LENGTH, IUNIT
	INTEGER NCCD
	CHARACTER*(*) STRING(1)
	CHARACTER*72 HINFILE, INFILE
	CHARACTER*8 CVERSION
	CHARACTER*1 REPLY
	LOGICAL LFATAL, LNONFATAL, LBLANK
	DATA IUNIT /10/

C-----------------------------------------------------------------------------
C Declarations for parameters read from the HiPERCAM data reduction pipeline
C log file.
C-----------------------------------------------------------------------------

	INTEGER CCD
	INTEGER RUN(MXROW), MJDFLAG(5,MXROW)
	INTEGER NREJ(5,MXROW,MAXAPER), ERROR_FLAG(5,MXROW,MAXAPER)
	DOUBLE PRECISION MJD(5,MXROW)
	DOUBLE PRECISION COUNTS(5,MXROW,MAXAPER), SIGMA(5,MXROW,MAXAPER)
	REAL HFWHM(5,MXROW,MAXAPER), HBETA(5,MXROW,MAXAPER)
	REAL EFWHM(5,MXROW,MAXAPER), EBETA(5,MXROW,MAXAPER)
	REAL EXPOSE(5,MXROW), SKY(5,MXROW,MAXAPER), NSKY(5,MXROW,MAXAPER)
 	REAL ESKY(5,MXROW,MAXAPER)
	REAL XMPOS(5,MXROW,MAXAPER), YMPOS(5,MXROW,MAXAPER)
	REAL EXMPOS(5,MXROW,MAXAPER), EYMPOS(5,MXROW,MAXAPER)
	REAL MFWHM(5,MXROW), MBETA(5,MXROW)
	
C-----------------------------------------------------------------------------
C Prompt for name of data file and open it.
C-----------------------------------------------------------------------------

	N1 = 1
	N2 = 1
	N3 = 1
	N4 = 1
	N5 = 1
10	WRITE(*,'(A,$)')'Enter name of data file (<CR> to quit) : '
	READ(*,'(A)',ERR=10)INFILE
	IF (INFILE .EQ. ' ') THEN
	   IF (N1 .EQ. 1 .AND. N2 .EQ. 1 .AND. N3
     + .EQ. 1 .AND. N4 .EQ. 1 .AND. N5 .EQ. 1) THEN
	      IFAIL = 1
	   ELSE
	      IFAIL = 0
	   END IF
	   GOTO 50
	END IF
	HINFILE = INFILE
        OPEN(UNIT=10,FILE=HINFILE,STATUS='OLD',ERR=10)

C-----------------------------------------------------------------------------
C Read in data.
C-----------------------------------------------------------------------------

	DO I = 1, MXROW*5+MAXHEADER
	   READ(IUNIT,'(A)',END=20)STRING(I)
	END DO
20	IF (I .EQ. MXROW*5) THEN
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)
     + '** WARNING: Maximum number of rows reached = ',MXROW*5+MAXHEADER
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

	DO I = 1, NTOTAL
	   IF (STRING(I)(1:30) .EQ. '# End of data type definitions') THEN
	      GOTO 30
	   END IF
	END DO
30	NHEADS = I + 2
	WRITE(*,*)'** OK: Number of header lines found = ',NHEADS
	IF (NHEADS .GT. MAXHEADER) THEN
	   WRITE(*,*)' '
	   WRITE(*,*)'** WARNING: Too many header lines.'
	   WRITE(*,*)
     + '** WARNING: Modify dimension of MAXHEADER in PERIOD_MAIN.'
	   WRITE(*,*)' '
	END IF

C-----------------------------------------------------------------------------
C Check version of reduce file.
C-----------------------------------------------------------------------------

	DO I = 1, 100
	   IF (STRING(I)(5:11) .EQ. 'version') THEN
	      CVERSION(1:4) = STRING(I)(17:20)
	      CVERSION(5:6) = STRING(I)(21:22)
	      CVERSION(7:8) = STRING(I)(23:24)
	      J = I
	      READ(CVERSION,*)IVERSION
	   END IF
	END DO
	IF (IVERSION .EQ. 20180211) THEN 	
	   WRITE(*,*)
     + '** OK: reduce file version = ',IVERSION
	   NDIV = 15
	ELSE IF (IVERSION .EQ. 20180519) THEN 	
	   WRITE(*,*)
     + '** OK: reduce file version = ',IVERSION
	   NDIV = 15
	ELSE IF (IVERSION .EQ. 180608) THEN
	   WRITE(*,*)
     + '** OK: reduce file version = ',IVERSION
	   NDIV = 15
	ELSE IF (IVERSION .EQ. 180625) THEN
	   WRITE(*,*)
     + '** OK: reduce file version = ',IVERSION
	   NDIV = 15
	ELSE
	   WRITE(*,*)ACHAR(7)
	   WRITE(*,*)'** ERROR: Unrecognised version = ',IVERSION
	   IFAIL = 1
	   GOTO 99
	END IF

C-----------------------------------------------------------------------------
C If the hcm file option has been used, there will be filenames present in the 
C first column instead of run numbers. This next section strips out any
C characters from this first column to avoid crashing the READ command
C below (which can only handle numbers). It therefore makes sense to name 
C .hcm files as simply as possible, preferably after their run number.
C I also strip out any occurrences of "nan" and "inf".
C-----------------------------------------------------------------------------

        DO I = NHEADS+1, NTOTAL
           DO J = 1, MAXCHARS
              IF (STRING(I)(J:J) .EQ. '_') THEN
		 WRITE(*,*)'** WARNING: Stripping out "_"'
                 STRING(I)(J:J) = '0'
	      ELSE IF (STRING(I)(J:J) .EQ. '.') THEN
		 IF (STRING(I)(J+1:J+3) .EQ. 'hcm') THEN
		    WRITE(*,*)'** WARNING: Stripping out ".hcm"'
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
C Initialise arrays
C-----------------------------------------------------------------------------

	DO I = 1, 5
	   NUM_APER(I) = 0
	END DO

C-----------------------------------------------------------------------------
C Determine the number of CCDs and apertures present
C-----------------------------------------------------------------------------

	DO I = 1, NHEADS
	   IF(STRING(I)(3:22) .EQ. 'Start of column name' ) THEN
	      DO J = 1, 8
		 IF (STRING(I+J)(3:20) .NE.
     + 'End of column name') THEN
		    IF (STRING(I+J)(3:3) .NE. ' ') THEN
		       NCOLS = 0
		       LBLANK = .TRUE.
		       LENGTH = LEN(STRING(I+J))
		       DO K = 1, LENGTH
			  IF (LBLANK .AND. STRING(I+J)(K:K)
     + .NE. ' ' ) THEN
			     NCOLS = NCOLS + 1
			     LBLANK = .FALSE.
			  ELSE IF (.NOT. LBLANK .AND. STRING(I+J)(K:K) 
     + .EQ. ' ') THEN
			     LBLANK = .TRUE.
			  END IF
		       END DO
		       READ(STRING(I+J)(3:3),*)CCD
		       NUM_APER(CCD) = (NCOLS-7)/NDIV
		       WRITE(*,*)'** OK: Number of apertures for CCD',CCD,
     + ' = ',NUM_APER(CCD)
		    END IF
		 ELSE
		    GOTO 40
		 END IF
	      END DO
	   END IF
	END DO

 40	NCCD = 0
	DO I = 1, 5
	   IF (NUM_APER(I) .GT. 0) THEN
	      NCCD = NCCD + 1
	   END IF
	END DO
	IF (NCCD .EQ. 0) THEN
	   WRITE(*,*)'** ERROR: No CCDs present in log file'
	   IFAIL = 1
	   GOTO 99
	ELSE
	   WRITE(*,*)'** OK: Number of CCDs = ',NCCD	   
	END IF	   	
	DO I = 1, 5
	   IF (NUM_APER(I) .GT. MAXAPER) THEN
	      WRITE(*,*)ACHAR(7)	   
	      WRITE(*,*)
     + '** ERROR: Maximum number of apertures = ',MAXAPER
	      IFAIL = 1
	      GOTO 99
	   END IF
	END DO
		
C-----------------------------------------------------------------------------
C Load data arrays.
C Format of reduce log file pre 180625 is:
C CCD nframe MJD MJDok Exptim [x xe y ye fwhm fwhme beta betae counts
c countse sky skye nsky nrej flag]*num_aper
C Format of reduce log file 180625 onwards is:
C CCD nframe MJD MJDok Exptim mfwhm mbeta [x xe y ye fwhm fwhme beta betae
C counts countse sky skye nsky nrej flag]*num_aper
C-----------------------------------------------------------------------------

	DO I = NHEADS+1, NTOTAL
	   
	   IF (STRING(I)(1:1) .NE. '#') THEN
	      
	      IF(STRING(I)(1:1) .EQ. '1') THEN	      
		 IF (NUM_APER(1) .GE. 1) THEN
		    IF (IVERSION .EQ. 180625) THEN		    
		    READ(STRING(I),*)CCD,RUN(N1),MJD(1,N1),MJDFLAG(1,N1),
     + EXPOSE(1,N1),MFWHM(1,N1), MBETA(1,N1),
     + (XMPOS(1,N1,J),EXMPOS(1,N1,J),YMPOS(1,N1,J),
     + EYMPOS(1,N1,J),HFWHM(1,N1,J),EFWHM(1,N1,J),HBETA(1,N1,J),
     + EBETA(1,N1,J),COUNTS(1,N1,J),SIGMA(1,N1,J),SKY(1,N1,J),
     + ESKY(1,N1,J),NSKY(1,N1,J),NREJ(1,N1,J),
     + ERROR_FLAG(1,N1,J),J=1,NUM_APER(1))
		    ELSE
		    READ(STRING(I),*)CCD,RUN(N1),MJD(1,N1),MJDFLAG(1,N1),
     + EXPOSE(1,N1),(XMPOS(1,N1,J),EXMPOS(1,N1,J),YMPOS(1,N1,J),
     + EYMPOS(1,N1,J),HFWHM(1,N1,J),EFWHM(1,N1,J),HBETA(1,N1,J),
     + EBETA(1,N1,J),COUNTS(1,N1,J),SIGMA(1,N1,J),SKY(1,N1,J),
     + ESKY(1,N1,J),NSKY(1,N1,J),NREJ(1,N1,J),
     + ERROR_FLAG(1,N1,J),J=1,NUM_APER(1))		    
		    END IF
		    IF (CCD .NE. 1) THEN
		       WRITE(*,*)'** ERROR: Expected CCD = 1 but read CCD =',CCD
		       IFAIL = 1
		       GOTO 99
		    END IF
		    N1 = N1 + 1
		 ELSE
		    WRITE(*,*)'** ERROR: CCD = 1 should not have any apertures.'
		    write(*,*)I
		    IFAIL = 1
		    GOTO 99		    
		 END IF

	      ELSE IF(STRING(I)(1:1) .EQ. '2') THEN	      
		 IF (NUM_APER(2) .GE. 1) THEN
		    IF (IVERSION .EQ. 180625) THEN		    
		    READ(STRING(I),*)CCD,RUN(N2),MJD(2,N2),MJDFLAG(2,N2),
     + EXPOSE(2,N2),MFWHM(2,N2), MBETA(2,N2),
     + (XMPOS(2,N2,J),EXMPOS(2,N2,J),YMPOS(2,N2,J),
     + EYMPOS(2,N2,J),HFWHM(2,N2,J),EFWHM(2,N2,J),HBETA(2,N2,J),
     + EBETA(2,N2,J),COUNTS(2,N2,J),SIGMA(2,N2,J),SKY(2,N2,J),
     + ESKY(2,N2,J),NSKY(2,N2,J),NREJ(2,N2,J),
     + ERROR_FLAG(2,N2,J),J=1,NUM_APER(2))
		    ELSE
		    READ(STRING(I),*)CCD,RUN(N2),MJD(2,N2),MJDFLAG(2,N2),
     + EXPOSE(2,N2),(XMPOS(2,N2,J),EXMPOS(2,N2,J),YMPOS(2,N2,J),
     + EYMPOS(2,N2,J),HFWHM(2,N2,J),EFWHM(2,N2,J),HBETA(2,N2,J),
     + EBETA(2,N2,J),COUNTS(2,N2,J),SIGMA(2,N2,J),SKY(2,N2,J),
     + ESKY(2,N2,J),NSKY(2,N2,J),NREJ(2,N2,J),
     + ERROR_FLAG(2,N2,J),J=1,NUM_APER(2))
		    END IF
		    IF (CCD .NE. 2) THEN
		       WRITE(*,*)'** ERROR: Expected CCD = 2 but read CCD =',CCD
		       IFAIL = 1
		       GOTO 99
		    END IF
		    N2 = N2 + 1
		 ELSE
		    WRITE(*,*)'** ERROR: CCD = 2 should not have any apertures.'
		    IFAIL = 1
		    GOTO 99		    
		 END IF

	      ELSE IF(STRING(I)(1:1) .EQ. '3') THEN	      
		 IF (NUM_APER(3) .GE. 1) THEN
		    IF (IVERSION .EQ. 180625) THEN		    
		    READ(STRING(I),*)CCD,RUN(N3),MJD(3,N3),MJDFLAG(3,N3),
     + EXPOSE(3,N3),MFWHM(3,N3), MBETA(3,N3),
     + (XMPOS(3,N3,J),EXMPOS(3,N3,J),YMPOS(3,N3,J),
     + EYMPOS(3,N3,J),HFWHM(3,N3,J),EFWHM(3,N3,J),HBETA(3,N3,J),
     + EBETA(3,N3,J),COUNTS(3,N3,J),SIGMA(3,N3,J),SKY(3,N3,J),
     + ESKY(3,N3,J),NSKY(3,N3,J),NREJ(3,N3,J),
     + ERROR_FLAG(3,N3,J),J=1,NUM_APER(3))
		    ELSE
		    READ(STRING(I),*)CCD,RUN(N3),MJD(3,N3),MJDFLAG(3,N3),
     + EXPOSE(3,N3),(XMPOS(3,N3,J),EXMPOS(3,N3,J),YMPOS(3,N3,J),
     + EYMPOS(3,N3,J),HFWHM(3,N3,J),EFWHM(3,N3,J),HBETA(3,N3,J),
     + EBETA(3,N3,J),COUNTS(3,N3,J),SIGMA(3,N3,J),SKY(3,N3,J),
     + ESKY(3,N3,J),NSKY(3,N3,J),NREJ(3,N3,J),
     + ERROR_FLAG(3,N3,J),J=1,NUM_APER(3))
		    END IF
		    IF (CCD .NE. 3) THEN
		       WRITE(*,*)'** ERROR: Expected CCD = 3 but read CCD =',CCD
		       IFAIL = 1
		       GOTO 99
		    END IF
		    N3 = N3 + 1
		 ELSE
		    WRITE(*,*)'** ERROR: CCD = 3 should not have any apertures.'
		    IFAIL = 1
		    GOTO 99		    
		 END IF

	      ELSE IF(STRING(I)(1:1) .EQ. '4') THEN	      
		 IF (NUM_APER(4) .GE. 1) THEN
		    IF (IVERSION .EQ. 180625) THEN		    
		    READ(STRING(I),*)CCD,RUN(N4),MJD(4,N4),MJDFLAG(4,N4),
     + EXPOSE(4,N4),MFWHM(4,N4), MBETA(4,N4),
     + (XMPOS(4,N4,J),EXMPOS(4,N4,J),YMPOS(4,N4,J),
     + EYMPOS(4,N4,J),HFWHM(4,N4,J),EFWHM(4,N4,J),HBETA(4,N4,J),
     + EBETA(4,N4,J),COUNTS(4,N4,J),SIGMA(4,N4,J),SKY(4,N4,J),
     + ESKY(4,N4,J),NSKY(4,N4,J),NREJ(4,N4,J),
     + ERROR_FLAG(4,N4,J),J=1,NUM_APER(4))
		    ELSE
		    READ(STRING(I),*)CCD,RUN(N4),MJD(4,N4),MJDFLAG(4,N4),
     + EXPOSE(4,N4),(XMPOS(4,N4,J),EXMPOS(4,N4,J),YMPOS(4,N4,J),
     + EYMPOS(4,N4,J),HFWHM(4,N4,J),EFWHM(4,N4,J),HBETA(4,N4,J),
     + EBETA(4,N4,J),COUNTS(4,N4,J),SIGMA(4,N4,J),SKY(4,N4,J),
     + ESKY(4,N4,J),NSKY(4,N4,J),NREJ(4,N4,J),
     + ERROR_FLAG(4,N4,J),J=1,NUM_APER(4))
		    END IF
		    IF (CCD .NE. 4) THEN
		       WRITE(*,*)'** ERROR: Expected CCD = 4 but read CCD =',CCD
		       IFAIL = 1
		       GOTO 99
		    END IF
		    N4 = N4 + 1
		 ELSE
		    WRITE(*,*)'** ERROR: CCD = 4 should not have any apertures.'
		    IFAIL = 1
		    GOTO 99		    
		 END IF

	      ELSE IF(STRING(I)(1:1) .EQ. '5') THEN	      
		 IF (NUM_APER(5) .GE. 1) THEN
		    IF (IVERSION .EQ. 180625) THEN		    
		    READ(STRING(I),*)CCD,RUN(N5),MJD(5,N5),MJDFLAG(5,N5),
     + EXPOSE(5,N5),MFWHM(5,N5), MBETA(5,N5),
     + (XMPOS(5,N5,J),EXMPOS(5,N5,J),YMPOS(5,N5,J),
     + EYMPOS(5,N5,J),HFWHM(5,N5,J),EFWHM(5,N5,J),HBETA(5,N5,J),
     + EBETA(5,N5,J),COUNTS(5,N5,J),SIGMA(5,N5,J),SKY(5,N5,J),
     + ESKY(5,N5,J),NSKY(5,N5,J),NREJ(5,N5,J),
     + ERROR_FLAG(5,N5,J),J=1,NUM_APER(5))
		    ELSE
		    READ(STRING(I),*)CCD,RUN(N5),MJD(5,N5),MJDFLAG(5,N5),
     + EXPOSE(5,N5),(XMPOS(5,N5,J),EXMPOS(5,N5,J),YMPOS(5,N5,J),
     + EYMPOS(5,N5,J),HFWHM(5,N5,J),EFWHM(5,N5,J),HBETA(5,N5,J),
     + EBETA(5,N5,J),COUNTS(5,N5,J),SIGMA(5,N5,J),SKY(5,N5,J),
     + ESKY(5,N5,J),NSKY(5,N5,J),NREJ(5,N5,J),
     + ERROR_FLAG(5,N5,J),J=1,NUM_APER(5))
		    END IF
		    IF (CCD .NE. 5) THEN
		       WRITE(*,*)'** ERROR: Expected CCD = 5 but read CCD =',CCD
		       IFAIL = 1
		       GOTO 99
		    END IF
		    N5 = N5 + 1
		 ELSE
		    WRITE(*,*)'** ERROR: CCD = 5 should not have any apertures.'
		    IFAIL = 1
		    GOTO 99		    
		 END IF

	      END IF
	   END IF
	END DO	      

	NHDATA(1) = N1-1
	NHDATA(2) = N2-1
	NHDATA(3) = N3-1
	NHDATA(4) = N4-1
	NHDATA(5) = N5-1
	WRITE(*,*)'** OK: Number of CCD1 runs = ',NHDATA(1)
	WRITE(*,*)'** OK: Number of CCD2 runs = ',NHDATA(2)
	WRITE(*,*)'** OK: Number of CCD3 runs = ',NHDATA(3)
	WRITE(*,*)'** OK: Number of CCD4 runs = ',NHDATA(4)
	WRITE(*,*)'** OK: Number of CCD5 runs = ',NHDATA(5)
	WRITE(*,*)' '

C-----------------------------------------------------------------------------
C Catch instances when apertures are defined in CCDs using setaper which are
C then not reduced in reduce.
C-----------------------------------------------------------------------------
	
	IF (NHDATA(1) .EQ. 0) NUM_APER(1) = 0
	IF (NHDATA(2) .EQ. 0) NUM_APER(2) = 0
	IF (NHDATA(3) .EQ. 0) NUM_APER(3) = 0
	IF (NHDATA(4) .EQ. 0) NUM_APER(4) = 0
	IF (NHDATA(5) .EQ. 0) NUM_APER(5) = 0
	   
	GOTO 10
	
C-----------------------------------------------------------------------------
C Report warnings and errors if requested.
C-----------------------------------------------------------------------------

 50	IF (N1 .EQ. 1 .AND. N2 .EQ. 1 .AND. N3
     + .EQ. 1 .AND. N4 .EQ. 1 .AND. N5 .EQ. 1) THEN
	   GOTO 99
	ELSE
	
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

	DO I = 1, NHDATA(1)
	   IF (LNONFATAL) THEN
	      DO J = 1, NUM_APER(1)
 		 IF (MJDFLAG(1,I) .EQ. 0) WRITE(*,*)
     + '** WARNING: CCD1 MJD unreliable at run = ',RUN(I)
		 IF (ERROR_FLAG(1,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(1,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(1,I,J),', aperture =',J,', CCD1, run =',RUN(I)
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
	   END IF
	END DO

	DO I = 1, NHDATA(2)
	   IF (LNONFATAL) THEN	
	      DO J = 1, NUM_APER(2)
		 IF (MJDFLAG(2,I) .EQ. 0) WRITE(*,*)
     + '** WARNING: CCD2 MJD unreliable at run = ',RUN(I)
		 IF (ERROR_FLAG(2,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(2,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(2,I,J),', aperture =',J,', CCD2, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	   IF (LFATAL) THEN
	      DO J = 1, NUM_APER(2)
		 IF (ERROR_FLAG(2,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(2,I,J),', aperture =',J,', CCD2, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	END DO

	DO I = 1, NHDATA(3)
	   IF (LNONFATAL) THEN	
	      DO J = 1, NUM_APER(3)
		 IF (MJDFLAG(3,I) .EQ. 0) WRITE(*,*)
     + '** WARNING: CCD3 MJD unreliable at run = ',RUN(I)
		 IF (ERROR_FLAG(3,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(3,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(3,I,J),', aperture =',J,', CCD3, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	   IF (LFATAL) THEN
	      DO J = 1, NUM_APER(3)
		 IF (ERROR_FLAG(3,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(3,I,J),', aperture =',J,', CCD3, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	END DO	      
	
	DO I = 1, NHDATA(4)
	   IF (LNONFATAL) THEN		      
	      DO J = 1, NUM_APER(4)
		 IF (MJDFLAG(4,I) .EQ. 0) WRITE(*,*)
     + '** WARNING: CCD4 MJD unreliable at run = ',RUN(I)
		 IF (ERROR_FLAG(4,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(4,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(4,I,J),', aperture =',J,', CCD4, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	   IF (LFATAL) THEN
	      DO J = 1, NUM_APER(4)
		 IF (ERROR_FLAG(4,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(4,I,J),', aperture =',J,', CCD4, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	END DO	      

	DO I = 1, NHDATA(5)
	   IF (LNONFATAL) THEN	
	      DO J = 1, NUM_APER(5)
		 IF (MJDFLAG(5,I) .EQ. 0) WRITE(*,*)
     + '** WARNING: CCD5 MJD unreliable at run = ',RUN(I)
		 IF (ERROR_FLAG(5,I,J) .GT. 0 .AND. 
     +               ERROR_FLAG(5,I,J) .LT. 9) THEN
		    WRITE(*,*)'** WARNING: Error code =',
     + ERROR_FLAG(5,I,J),', aperture =',J,', CCD5, run =',RUN(I)
		 END IF
	      END DO
	   END IF	   
	   IF (LFATAL) THEN
	      DO J = 1, NUM_APER(5)
		 IF (ERROR_FLAG(5,I,J) .GE. 9) THEN
		    WRITE(*,*)'** ERROR: Error code =',
     + ERROR_FLAG(5,I,J),', aperture =',J,', CCD5, run =',RUN(I)
		 END IF
	      END DO
	   END IF
	END DO	      
	END IF
	
 99	RETURN
	END
