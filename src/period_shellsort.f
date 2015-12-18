
	SUBROUTINE PERIOD_SHELLSORT (NDATA, XDATA, KEY)

C===========================================================================
C Shell sort routine:
C INPUT:
C	NDATA = NUMBER OF DATA VALUES
C	XDATA = DATA VALUES
C OUTPUT:
C	KEY   = KEY TO SORTED DATA VALUES (I.E. XDATA(KEY(I)) ASCENDS)
C
C Mike Irwin routine substantially modified by T.R. Marsh and included
C in PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C===========================================================================

	DOUBLE PRECISION XDATA(1), XX
	INTEGER KEY(1)

C---------------------------------------------------------------------------
C Evaluate jump step to be used in shell sort.
C---------------------------------------------------------------------------

	JUMP = 2
10	JUMP = 2*JUMP
	IF( JUMP .LT. NDATA ) GOTO 10
	JUMP = MIN0( NDATA, (3*JUMP)/4-1 )

C---------------------------------------------------------------------------
C Initialise key array.
C---------------------------------------------------------------------------

	DO I = 1, NDATA
	  KEY(I) = I
	END DO
	IF(NDATA.EQ.1) RETURN
	DO WHILE(JUMP .GT. 1)
	  JUMP = JUMP/2
	  IEND = NDATA - JUMP
	  DO I = 1, IEND
	    I1 = I
	    I2 = I + JUMP

C---------------------------------------------------------------------------
C Compare values JUMP apart in the current sorted array a value is moved 
C in the array if it is less than the value JUMP bins before it. It will 
C carry on jumping up the array until it meets a smaller value or runs out
C of space.
C---------------------------------------------------------------------------

	    IF( XDATA(KEY(I1)) .GT. XDATA(KEY(I2)) ) THEN
	      KEEP = KEY(I2)
	      XX = XDATA(KEEP)
20	      KEY(I2) = KEY(I1)
	      I2 = I1
	      I1 = I1 - JUMP
	      IF( I1.LE.0 ) GOTO 30
	      IF( XDATA(KEY(I1)) .GT. XX ) GOTO 20
30	      KEY(I2) = KEEP
	    END IF
	  END DO
	END DO

	RETURN
	END
