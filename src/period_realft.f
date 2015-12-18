
	SUBROUTINE PERIOD_REALFT (DATA, N, ISIGN)

C=============================================================================
C Calculate the Fourier Transform of a set of 2N real-valued data points. 
C Replaces this data (which is stored in array DATA) by the positive frequency 
C half of its complex Fourier Transform. The real-valued first and last
C components of the complex transform are returned as elements DATA(1) and
C DATA(2) respectively. N must be a power of 2. This routine also calculates
C the inverse transform of a complex data array if it is the transform of
C real data. (Result in this case must be multipled by 1/N.)
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 1-July-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C=============================================================================
	
	IMPLICIT NONE

	INTEGER N, ISIGN, N2P3, I, I1, I2, I3, I4
	DOUBLE PRECISION DATA(2*N)
	DOUBLE PRECISION WR, WI, WPR, WPI, WTEMP, THETA
	DOUBLE PRECISION C1, C2, H1R, H1I, H2R, H2I

C-----------------------------------------------------------------------------
C Initialize the recurrence.
C-----------------------------------------------------------------------------

	THETA = 3.1415926535897932384626433832795028841971693993751D0 / 
     + DBLE(N)
	C1 = 0.5D0
	IF (ISIGN .EQ. 1) THEN
		C2 = -0.5D0

C-----------------------------------------------------------------------------
C The forward transform is here.
C-----------------------------------------------------------------------------

		CALL PERIOD_FOUR1 (DATA, N, +1)
	ELSE

C-----------------------------------------------------------------------------
C Otherwise set up for an inverse transform.
C-----------------------------------------------------------------------------

		C2 = 0.5D0
		THETA = -THETA
	END IF
	WPR = -2.0D0 * DSIN(0.5D0 * THETA)**2.0D0
	WPI = DSIN(THETA)
	WR = 1.0D0 + WPR
	WI = WPI
	N2P3 = 2 * N + 3

C-----------------------------------------------------------------------------
C Case I = 1 done separately below.
C-----------------------------------------------------------------------------

	DO I = 2, N/2+1
		I1 = 2 * I - 1
		I2 = I1 + 1
		I3 = N2P3 - I2
		I4 = I3 + 1

C-----------------------------------------------------------------------------
C The two separate transforms are separated out of Z.
C-----------------------------------------------------------------------------

		H1R = C1 * (DATA(I1) + DATA(I3))
		H1I = C1 * (DATA(I2) - DATA(I4))
		H2R = -C2 * (DATA(I2) + DATA(I4))
		H2I = C2 * (DATA(I1) - DATA(I3))

C-----------------------------------------------------------------------------
C Here they are recombined to form the true transform of the original 
C real data.
C-----------------------------------------------------------------------------

		DATA(I1) = H1R + WR * H2R - WI * H2I
		DATA(I2) = H1I + WR * H2I + WI * H2R
		DATA(I3) = H1R - WR * H2R + WI * H2I
		DATA(I4) = -H1I + WR * H2I + WI * H2R

C-----------------------------------------------------------------------------
C The recurrence.
C-----------------------------------------------------------------------------

		WTEMP = WR
		WR = WR * WPR - WI * WPI + WR
		WI = WI * WPR + WTEMP * WPI + WI
	END DO
	IF (ISIGN .EQ. 1) THEN
		H1R = DATA(1)
		DATA(1) = H1R + DATA(2)

C-----------------------------------------------------------------------------
C Squeeze the first and last data together to get them all within the
C original array.
C-----------------------------------------------------------------------------

		DATA(2) = H1R - DATA(2)
	ELSE
		H1R = DATA(1)
		DATA(1) = C1 * (H1R + DATA(2))
		DATA(2) = C1 * (H1R - DATA(2))

C-----------------------------------------------------------------------------
C This is the inverse transform for the case ISIGN = -1.
C-----------------------------------------------------------------------------

		CALL PERIOD_FOUR1 (DATA, N, -1)
	END IF

	RETURN
	END
