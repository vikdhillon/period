
	SUBROUTINE PERIOD_INPUT (Y, MXROW, MXSLOT, 
     +				 NPTSARRAY, YERRORARRAY, 
     +			         INFILEARRAY, DETRENDARRAY,
     +                           JUNK1, JUNK2, MXCOL)

C=============================================================================
C Routine to input data into the PERIOD program. The data must be read from
C an ASCII file. Input of Y axis errors is optional.
C
C Written by Vikram Singh Dhillon @Sussex 31-May-1991.
C ANTARES option removed by Vik Dhillon @ING 20-May-2003.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT, MXCOL
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_INPUT declarations.
C-----------------------------------------------------------------------------

	DOUBLE PRECISION JUNK1(MXROW), JUNK2(MXROW, MXCOL)
	INTEGER NUMCOLS, NUMROWS, IFAIL, XCOL, YCOL, YCOLERR
	INTEGER I, J, K, N, SLOT, FIRSTSLOT, LASTSLOT, IUNIT
	INTEGER KEY(MXROW)
	CHARACTER*100 STRING(MXROW)
	CHARACTER*72 INFILEARRAY(MXSLOT), INFILE
	CHARACTER*1 REPLY, SORT
	LOGICAL YERRORARRAY(MXSLOT), YERROR, DETRENDARRAY(MXSLOT) 
	DATA IUNIT /12/
	DATA NUMCOLS, NUMROWS /0, 0/

C-----------------------------------------------------------------------------
C Select slots to load.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
1	WRITE(*,'(A,$)')'Enter first and last slots for input
     + (0,0 to quit) : '
	READ(*,*,ERR=1)FIRSTSLOT, LASTSLOT
	IF (FIRSTSLOT .EQ. 0 .OR. LASTSLOT .EQ. 0) GOTO 99
	IF (FIRSTSLOT .GT. MXSLOT .OR. LASTSLOT .GT. MXSLOT) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Maximum slot number = ',MXSLOT
	   GOTO 99
	END IF
	DO J = FIRSTSLOT, LASTSLOT
	SLOT = J 

C-----------------------------------------------------------------------------
C Read in the data from a file using PERIOD_READFREE.
C-----------------------------------------------------------------------------

	IF (J .NE. FIRSTSLOT) WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Enter name of data file (<CR> to quit) : '
	READ(*,'(A)',ERR=10)INFILEARRAY(SLOT)
	IF (INFILEARRAY(SLOT)(1:1) .EQ. ' ') THEN
		GOTO 99
	END IF
	INFILE = INFILEARRAY(SLOT)
	INFILEARRAY(SLOT) = INFILEARRAY(SLOT)
        OPEN(UNIT=IUNIT,FILE=INFILE,STATUS='OLD',ERR=10)
        CALL PERIOD_READFREE(JUNK1, NUMCOLS, NUMROWS, IUNIT, IFAIL)
	REWIND IUNIT
        IF (IFAIL .EQ. 1) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: Something wrong with input file.'
    	   CLOSE (UNIT=IUNIT)
    	   GOTO 99
        END IF
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Number of columns found = ',NUMCOLS
	WRITE(*,*)'** OK: Number of rows found    = ',NUMROWS
	IF ( (NUMCOLS .GT. 0) .AND. (NUMROWS .EQ. 0)) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Something wrong with input file.'
		CLOSE (UNIT=IUNIT)
		GOTO 99
	ELSE IF (NUMCOLS .GT. MXCOL) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Too many columns.'
		CLOSE (UNIT=IUNIT)
		GOTO 99
	ELSE IF (NUMROWS .GT. MXROW) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Too many rows.'
		CLOSE (UNIT=IUNIT)
		GOTO 99
	ELSE IF (NUMCOLS .EQ. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Only one column found.'
		CLOSE (UNIT=IUNIT)
		GOTO 99
	ELSE IF (NUMCOLS .EQ. 2) THEN
		WRITE(*,*)' '
		WRITE(*,*)'** WARNING: Only two columns found.'
		WRITE(*,*)'** WARNING: Will assume there are no errors.' 
		WRITE(*,*)' '
16		WRITE(*,'(A,$)')'Enter number of column containing X data : '
		READ(*,*,ERR=16)XCOL
		IF (XCOL .GT. NUMCOLS) THEN
			GOTO 16
		ELSE IF (XCOL .LE. 0) THEN
			GOTO 16
		END IF
17		WRITE(*,'(A,$)')'Enter number of column containing Y data : '
		READ(*,*,ERR=17)YCOL
		IF (YCOL .GT. NUMCOLS) THEN
			GOTO 17
		ELSE IF (YCOL .LE. 0) THEN
			GOTO 17
		END IF
		YERROR = .FALSE.
		GOTO 20
	ELSE IF (NUMCOLS .EQ. 3) THEN
		WRITE(*,*)' '
		WRITE(*,*)'** OK: Three data columns have been found.'
		WRITE(*,*)'** OK: The default column assignment is: '
		WRITE(*,*)' '
		WRITE(*,*)'** OK: COLUMN 1: X axis data.'
		WRITE(*,*)'** OK: COLUMN 2: Y axis data.'
		WRITE(*,*)'** OK: COLUMN 3: Y axis errors.'
		WRITE(*,*)' '
30		WRITE(*,'(A,$)')'Does this agree with the data file ? [Y] : '
		READ(*,'(A)',ERR=30)REPLY
		CALL PERIOD_CASE (REPLY, .TRUE.)
		IF (REPLY .EQ. 'Y' .OR. REPLY .EQ. ' ') THEN
			XCOL = 1
			YCOL = 2
			YCOLERR = 3
			YERROR = .TRUE.
			GOTO 20
		ELSE
		WRITE(*,*)' '
18		WRITE(*,'(A,$)')'Enter number of column containing X data  : '
		READ(*,*,ERR=18)XCOL
		IF (XCOL .GT. NUMCOLS) THEN
			GOTO 18
		ELSE IF (XCOL .LE. 0) THEN
			GOTO 18
		END IF
19		WRITE(*,'(A,$)')'Enter number of column containing Y data  : '
		READ(*,*,ERR=19)YCOL
		IF (YCOL .GT. NUMCOLS) THEN
			GOTO 19
		ELSE IF (YCOL .LE. 0) THEN
			GOTO 19
		END IF
31		WRITE(*,'(A,$)')'Are there errors on the Y axis data ? [Y] : '
		READ(*,'(A)',ERR=31)REPLY
		CALL PERIOD_CASE (REPLY, .TRUE.)
		IF (REPLY .EQ. 'Y' .OR. REPLY .EQ. ' ') THEN
21		WRITE(*,'(A,$)')'Enter number of column containing Y data
     + errors : '
			READ(*,*,ERR=21)YCOLERR
			IF (YCOLERR .GT. NUMCOLS) THEN
				GOTO 21
			ELSE IF (YCOLERR .LE. 0) THEN
				GOTO 21
			END IF
			YERROR = .TRUE.
			GOTO 20
		ELSE
			YERROR = .FALSE.
			GOTO 20
		END IF
		END IF
	ELSE IF (NUMCOLS .GE. 4) THEN
		WRITE(*,*)' '
22		WRITE(*,'(A,$)')'Enter number of column containing X data  : '
		READ(*,*,ERR=22)XCOL
		IF (XCOL .GT. NUMCOLS) THEN
			GOTO 22
		ELSE IF (XCOL .LE. 0) THEN
			GOTO 22
		END IF
23		WRITE(*,'(A,$)')'Enter number of column containing Y data  : '
		READ(*,*,ERR=23)YCOL
		IF (YCOL .GT. NUMCOLS) THEN
			GOTO 23
		ELSE IF (YCOL .LE. 0) THEN
			GOTO 23
		END IF
32		WRITE(*,'(A,$)')'Are there errors on the Y axis data ? [Y] : '
		READ(*,'(A)',ERR=32)REPLY
		CALL PERIOD_CASE (REPLY, .TRUE.)
		IF (REPLY .EQ. 'Y' .OR. REPLY .EQ. ' ') THEN
24		WRITE(*,'(A,$)')'Enter number of column containing Y data
     + errors : '
			READ(*,*,ERR=24)YCOLERR
			IF (YCOLERR .GT. NUMCOLS) THEN
				GOTO 24
			ELSE IF (YCOLERR .LE. 0) THEN
				GOTO 24
			END IF
			YERROR = .TRUE.
			GOTO 20
		ELSE
			YERROR = .FALSE.
			GOTO 20
		END IF
	END IF

C-----------------------------------------------------------------------------
C Now create a data array ready for input to PLT.
C-----------------------------------------------------------------------------

20	DO I = 1, NUMROWS
		READ (IUNIT,'(A)')STRING(I)
		READ(STRING(I),*)(JUNK2(I,N),N=1,NUMCOLS)
		Y(I,1,SLOT) = JUNK2(I,XCOL)
		Y(I,2,SLOT) = JUNK2(I,YCOL)
		IF (YERROR) THEN
			Y(I,3,SLOT) = JUNK2(I,YCOLERR)
			IF (Y(I,3,SLOT) .LT. 0.D0) THEN
			WRITE(*,*)'** WARNING: negative error
     + at line number =',I
			WRITE(*,*)'** WARNING: Forcing positivity.'
			Y(I,3,SLOT) = DSQRT(Y(I,3,SLOT)**2.0D0)
			END IF
		END IF
	END DO

C-----------------------------------------------------------------------------
C Check through the x-axis data to see if data is in ascending order. If not,
C warn the user and offer to sort it. 
C-----------------------------------------------------------------------------

	DO I = 2, NUMROWS
		IF (Y(I,1,SLOT) .LE. Y(I-1,1,SLOT)) THEN
			WRITE(*,*)ACHAR(7)
			WRITE(*,*)'** WARNING: x-axis data not in
     + ascending order at line number =',I
			WRITE(*,*)' '
33			WRITE(*,'(A,$)')'Would you like to sort
     + the data ? [N] : '
			READ(*,'(A)',ERR=33)SORT
			CALL PERIOD_CASE (SORT, .TRUE.)
			IF (SORT .EQ. 'Y') THEN
			DO K = 1, NUMROWS
				JUNK1(K) = JUNK2(K,XCOL)
			END DO				
			CALL PERIOD_SHELLSORT (NUMROWS, JUNK1, KEY)
			DO K = 1, NUMROWS
				Y(K,1,SLOT) = JUNK2(KEY(K),XCOL)
				Y(K,2,SLOT) = JUNK2(KEY(K),YCOL)
				IF (YERROR) THEN
				Y(K,3,SLOT) = JUNK2(KEY(K),YCOLERR)	
				END IF
			END DO
			WRITE(*,*)' '
			GOTO 25
			ELSE 
			CLOSE (UNIT=IUNIT)
			NPTSARRAY(SLOT) = 0
			GOTO 99
			END IF
		END IF
	END DO						
	WRITE(*,*)' '
25	WRITE(*,*)'** OK: Filled Slot = ',SLOT
	YERRORARRAY(SLOT) = YERROR	
	DETRENDARRAY(SLOT) = .FALSE.
	NPTSARRAY(SLOT) = NUMROWS
	NUMCOLS = 0
	NUMROWS = 0
	CLOSE (UNIT=IUNIT)
	END DO

99	RETURN
	END
