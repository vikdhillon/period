
	SUBROUTINE PERIOD_MEDIAN (X, N, XMED)

C=============================================================================
C Subroutine to determine median of a dataset.
C Given an array X of N numbers, this routine returns their median value
C XMED. The array X is modified and returned sorted into ascending order.
C
C Adapted from Numerical Recipes by Vik Dhillon, ING SAG, 21-May-1994.
C Converted to DOUBLE PRECISION by Vik Dhillon @Sheffield 07-October-2004.
C=============================================================================

C------------------------------------------------------------------------------
C PERIOD_MEDIAN declarations.
C------------------------------------------------------------------------------
	
	IMPLICIT NONE

	INTEGER N, N2
	DOUBLE PRECISION X(N), XMED

	CALL PERIOD_SORT(N,X)
	N2 = N / 2
	IF (2*N2 .EQ. N) THEN
		XMED = 0.5D0 * ( X(N2) + X(N2+1) )
	ELSE
		XMED = X(N2+1)
	END IF 

	RETURN
	END
