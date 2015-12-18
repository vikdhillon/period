        
	SUBROUTINE PERIOD_FOLD (XDATA, YDATA, YERR, NDATA, ZEROPT, PERIOD)

C===========================================================================
C Folds data XDATA(NDATA) on PERIOD about ZEROPT.
C
C Written by Vikram Singh Dhillon @Sussex 29-March-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C===========================================================================

	IMPLICIT NONE

	INTEGER NDATA
	DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA), YERR(NDATA)
	DOUBLE PRECISION XWORK(NDATA), YWORK(NDATA), EWORK(NDATA)
	DOUBLE PRECISION ZEROPT, PERIOD
	INTEGER KEY(NDATA), I

C-----------------------------------------------------------------------------
C Load work arrays.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		XWORK(I) = XDATA(I)
		YWORK(I) = YDATA(I)
		EWORK(I) = YERR(I)
	END DO

C-----------------------------------------------------------------------------
C Fold data.
C-----------------------------------------------------------------------------

	DO I = 1, NDATA
		XWORK(I) = ((XWORK(I) - ZEROPT) / PERIOD) - 
     +			   AINT((XWORK(I) - ZEROPT) / PERIOD)
		IF (XWORK(I) .LE. 0.) THEN
			XWORK(I) = XWORK(I) + 1.0D0
		END IF
	END DO

C-----------------------------------------------------------------------------
C Sort data into ascending order.
C-----------------------------------------------------------------------------

	CALL PERIOD_SHELLSORT (NDATA, XWORK, KEY)
	DO I = 1, NDATA
		XDATA(I) = XWORK(KEY(I)) 
		YDATA(I) = YWORK(KEY(I))
		YERR(I)  = EWORK(KEY(I)) 
	END DO

	RETURN
	END
