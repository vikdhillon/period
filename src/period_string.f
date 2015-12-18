
	SUBROUTINE PERIOD_STRING (XDATA, YDATA, NDATA, STRING, IFAIL)

C===========================================================================
C This routine calculates the string-length of XDATA(NDATA), YDATA(NDATA)
C following the method described by Dworetsky (1983, MNRAS, 203, 917) and
C Friend et al. (1990, MNRAS, 246, 637).
C
C Written by Vikram Singh Dhillon @Sussex 7-July-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_STRING declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA, IFAIL, I
	DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
	DOUBLE PRECISION STRING
	DOUBLE PRECISION YMAX, YMIN
	DOUBLE PRECISION XDIFF, YDIFF, LENGTH

C-----------------------------------------------------------------------------
C Find the maximum and minimum data points.
C-----------------------------------------------------------------------------

	YMAX = -1.0D+32
	YMIN =  1.0D+32
	DO I = 1, NDATA
		IF (YDATA(I) .GT. YMAX) THEN
			YMAX = YDATA(I)
		END IF
		IF (YDATA(I) .LT. YMIN) THEN
			YMIN = YDATA(I)
		END IF
	END DO

C-----------------------------------------------------------------------------
C If YMAX = YMIN (ie. if windowed data), report an error and abort.
C-----------------------------------------------------------------------------

	IF (YMIN .EQ. YMAX) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: YMIN .EQ. YMAX in PERIOD_STRING.'
		IF (YMIN .EQ. 1.) THEN
		WRITE(*,*)'** ERROR: Cannot process windowed data.'
		END IF
		IFAIL = 1
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Scale all of the y-axis data points so that when determining the string
C length they will have equal weight to the phases. 
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		YDATA(I) = (YDATA(I)-YMIN)/(2.0D0*(YMAX-YMIN))-0.25D0
	END DO

C-----------------------------------------------------------------------------
C Calculate the string-length.
C-----------------------------------------------------------------------------

	STRING = 0.0D0
	DO I = 1, NDATA-1
		YDIFF = YDATA(I+1) - YDATA(I)
		XDIFF = XDATA(I+1) - XDATA(I)
		LENGTH = DSQRT((XDIFF*XDIFF)+(YDIFF*YDIFF))
		STRING = STRING + LENGTH
	END DO

C-----------------------------------------------------------------------------
C Complete the string length calculation by adding on a piece of string for
C the first and last observations.
C-----------------------------------------------------------------------------

	YDIFF = YDATA(1) - YDATA(NDATA)
	XDIFF = XDATA(1) + 1.0D0 - XDATA(NDATA)
	LENGTH = DSQRT((XDIFF*XDIFF)+(YDIFF*YDIFF))
	STRING = STRING + LENGTH

C-----------------------------------------------------------------------------
C And return.
C-----------------------------------------------------------------------------

	IFAIL = 0
99	RETURN
	END
