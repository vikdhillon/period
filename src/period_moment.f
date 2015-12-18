
	SUBROUTINE PERIOD_MOMENT (FDATA, N, AVE, ADEV, SDEV, VAR)

C==============================================================================
C Given an array of FDATA of length N, this routine returns its mean AVE,
C average deviation ADEV, standard deviation SDEV and variance VAR.
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 29-April-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C==============================================================================

C------------------------------------------------------------------------------
C PERIOD_MOMENT declarations.
C------------------------------------------------------------------------------

	IMPLICIT NONE
	INTEGER N, J
	DOUBLE PRECISION FDATA(N), AVE, ADEV, SDEV, VAR
	DOUBLE PRECISION S, P

	IF (N .LE. 1) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: N must be at least 2 in PERIOD_MOMENT.'
		GOTO 99
	END IF

C------------------------------------------------------------------------------
C First pass to get the mean.
C------------------------------------------------------------------------------

	S = 0.0D0
	DO J = 1, N
		S = S + FDATA(J)
	END DO
	AVE = S / DBLE(N)

C------------------------------------------------------------------------------
C Second pass to get the first (absolute) and second moments of the deviation 
C from the mean.
C------------------------------------------------------------------------------

	ADEV = 0.0D0
	VAR = 0.0D0
	DO J = 1, N
		S = FDATA(J) - AVE
		ADEV = ADEV + DABS(S)
		P = S * S
		VAR = VAR + P
	END DO

C------------------------------------------------------------------------------
C Put the pieces together according to the conventional definitions.
C------------------------------------------------------------------------------

	ADEV = ADEV / DBLE(N)
	VAR = VAR / (DBLE(N) - 1.0D0)
	SDEV = DSQRT (VAR)

99	RETURN
	END
