
	SUBROUTINE PERIOD_SORT (N, RA)

C=============================================================================
C Sorts an array RA of length N into ascending numerical order using the 
C Heapsort algorithm. N is input; RA is replaced on output by its sorted
C rearrangement.
C
C Adapted from Numerical Recipes by Vik Dhillon, ING SAG, 21-May-1994.
C Converted to DOUBLE PRECISION by Vik Dhillon @Sheffield 07-October-2004.
C=============================================================================

C------------------------------------------------------------------------------
C PERIOD_SORT declarations.
C------------------------------------------------------------------------------
	
	IMPLICIT NONE

	INTEGER N, L, IR, I, J
	DOUBLE PRECISION RA(N), RRA

	L = N / 2+1
	IR = N

C-----------------------------------------------------------------------------
C The index L will be decremented from its initial value down to 1 during the
C "hiring" (heap creation) phase. Once it reaches 1, the index IR will be 
C decremented from its initial value down to 1 during the "retirement-and-
C promotion" (heap selection) phase.
C-----------------------------------------------------------------------------

10 	CONTINUE
	IF (L .GT. 1) THEN
		L = L - 1
		RRA = RA(L)
	ELSE
		RRA = RA(IR)
		RA(IR) = RA(1)
		IR = IR - 1
		IF (IR .EQ. 1) THEN
			RA(1) = RRA
			RETURN
		END IF
	END IF
	I = L
	J = L + L
20 	IF (J .LE. IR) THEN
		IF (J .LT. IR) THEN
			IF (RA(J) .LT. RA(J+1)) J = J + 1
		END IF
		IF (RRA .LT. RA(J)) THEN
			RA(I) = RA(J)
			I = J
			J = J + J
		ELSE
			J = IR + 1
		END IF
		GOTO 20
	END IF
	RA(I) = RRA
	GOTO 10

	END
