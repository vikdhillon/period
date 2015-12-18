
	SUBROUTINE PERIOD_FOUR1 (DATA, NN, ISIGN)

C==============================================================================
C Replaces data by its discrete Fourier transform, if ISIGN is input as 1; or
C replaces DATA by NN times its inverse discrete Fourier transform, if ISIGN
C is input as -1. DATA is a complex array of length NN or, equivalently, a real
C array of length 2*NN. NN MUST be an integer power of 2 (this is not checked
C for!).
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 11-Feb-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C==============================================================================

	IMPLICIT NONE

	INTEGER NN, ISIGN, N, J, I, M, MMAX, ISTEP
	DOUBLE PRECISION DATA(2*NN)
	DOUBLE PRECISION WR, WI, WPR, WPI, WTEMP, THETA
	DOUBLE PRECISION TEMPR, TEMPI

C------------------------------------------------------------------------------
C This is the bit-reversal section of the routine.
C------------------------------------------------------------------------------

	N = 2 * NN
	J = 1
	DO 10 I = 1, N, 2
		IF (J .GT. I) THEN

C------------------------------------------------------------------------------
C Exchange the two complex numbers.
C------------------------------------------------------------------------------

			TEMPR = DATA(J)
			TEMPI = DATA(J+1)
			DATA(J) = DATA(I)
			DATA(J+1) = DATA(I+1)
			DATA(I) = TEMPR
			DATA(I+1) = TEMPI
		END IF
		M = N / 2
1		IF ((M .GE. 2) .AND. (J .GT. M)) THEN
			J = J - M
			M = M / 2
			GO TO 1
		END IF
		J = J + M
10	CONTINUE 

C------------------------------------------------------------------------------
C Here begins the Danielson-Lanczos section of the routine.
C------------------------------------------------------------------------------

	MMAX = 2

C------------------------------------------------------------------------------
C Outer loop executed log_2(NN) times.
C------------------------------------------------------------------------------

2	IF (N .GT. MMAX) THEN
		ISTEP = 2 * MMAX

C------------------------------------------------------------------------------
C Initialise for the trigonometric recurrence.
C------------------------------------------------------------------------------

		THETA = 6.28318530717959D0 / (DBLE(ISIGN*MMAX))
		WPR = -2.0D0 * DSIN(0.5D0 * THETA)**2.0D0
		WPI = DSIN(THETA)
		WR = 1.0D0
		WI = 0.0D0

C------------------------------------------------------------------------------
C Here are the two nested inner loops.
C------------------------------------------------------------------------------

		DO 30 M = 1, MMAX, 2
			DO 20 I = M, N, ISTEP
				
C------------------------------------------------------------------------------
C This is the Danielson-Lanczos formula.
C------------------------------------------------------------------------------

				J = I + MMAX
				TEMPR = WR * DATA(J) - WI * DATA(J+1)
				TEMPI = WR * DATA(J+1) + WI * DATA(J)
				DATA(J) = DATA(I) - TEMPR
				DATA(J+1) = DATA(I+1) - TEMPI
				DATA(I) = DATA(I) + TEMPR
				DATA(I+1) = DATA(I+1) + TEMPI
20			CONTINUE

C------------------------------------------------------------------------------
C Trigonometric recurrence.
C------------------------------------------------------------------------------

			WTEMP = WR
			WR = WR * WPR - WI * WPI + WR
			WI = WI * WPR + WTEMP * WPI + WI
30		CONTINUE
		MMAX = ISTEP

C------------------------------------------------------------------------------
C Not yet done.
C------------------------------------------------------------------------------

		GOTO 2

C------------------------------------------------------------------------------
C All done.
C------------------------------------------------------------------------------

	END IF

	RETURN
	END
