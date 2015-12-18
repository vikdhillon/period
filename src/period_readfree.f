
	SUBROUTINE PERIOD_READFREE (DATA, NX, NY, IUNIT, IFAIL)

C=========================================================================
C General purpose routine to read free format real data stored in the form 
C of columns. It will ignore all blank lines and lines starting with an 
C exclamation mark
C
C DATA(NX*NY) -- The data array. NX entries/line, NY lines.
C NX          -- the number of entries to be read/line. An error will
C                occur if this is too large. Enter a value of zero
C                and the routine will try to determine the number of
C                entries automatically. NX will then be returned.
C                Can be changed on exit.
C NY          -- The number of lines to be read. Enter 0 to read all
C                the lines. On exit contains the number of lines read.
C IUNIT       -- Reads from IUNIT.
C IFAIL       -- 0 for normal exit, anything else an error has occurred.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C=========================================================================

	DOUBLE PRECISION DATA(1)
	INTEGER NX, NY, IFAIL
	CHARACTER*100 STRING
	LOGICAL BLANK
	DATA MAXNY /1000000000/

	LENGTH = LEN(STRING)
	IFAIL = 0
	NLINES = 0

C-------------------------------------------------------------------------
C Find number of columns.
C-------------------------------------------------------------------------

 	IF(NX.LE.0) THEN
	  IFAIL = 0
	  NBLOCK = 0
	  DO WHILE(IFAIL.EQ.0 .AND. NBLOCK.EQ.0)
	    STRING(1:1) = '!'
	    DO WHILE(STRING(1:1).EQ.'!')
	      READ(IUNIT,'(A)',IOSTAT=IFAIL) STRING
	    END DO

C-------------------------------------------------------------------------
C Search for number of contiguous blocks of data.
C-------------------------------------------------------------------------

	    IF(IFAIL.EQ.0) THEN
	      NBLOCK = 0
	      BLANK = .TRUE.
	      DO I = 1, LENGTH
		IF(BLANK .AND. (STRING(I:I).NE.' ' 
     &			.AND. STRING(I:I).NE.CHAR(9)) ) THEN
		  NBLOCK = NBLOCK + 1
		  BLANK = .FALSE.
		ELSE IF(.NOT.BLANK .AND. (STRING(I:I).EQ.' ' .OR. 
     &			STRING(I:I).EQ.CHAR(9)) ) THEN
		  BLANK = .TRUE.
		END IF
	      END DO
	    END IF
	  END DO
	  IF(IFAIL.NE.0) RETURN

C-------------------------------------------------------------------------
C Translate first line.
C-------------------------------------------------------------------------

	  NX = NBLOCK
	  READ(STRING,*,IOSTAT=IFAIL) (DATA(I),I=1,NX)
	  IF(IFAIL.NE.0) RETURN
	  NLINES = 1
	END IF
C-------------------------------------------------------------------------
C Read data.
C-------------------------------------------------------------------------

	IF(NY.LE.0) NY = MAXNY
	DO WHILE(IFAIL.EQ.0 .AND. NLINES.LT.NY)
	  READ(IUNIT,'(A)',IOSTAT=IFAIL) STRING
	  IF(IFAIL.EQ.0 .AND. STRING(1:1).NE.'!' .AND. STRING.NE.' ') THEN
	    NLINES = NLINES + 1
	    READ(STRING,*,IOSTAT=IFAIL) (DATA(NX*(NLINES-1)+I),I=1,NX)
	  END IF
	END DO

C-------------------------------------------------------------------------
C Error check.
C-------------------------------------------------------------------------

	IF(IFAIL.GT.0) THEN
	  NY = 0
	  RETURN
	END IF
	IFAIL = 0
	NY = NLINES

	RETURN
	END
