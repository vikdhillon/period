
	SUBROUTINE PERIOD_SCARGLE (X, Y, N, WORK1, WORK2, NWK, NOUT, 
     +			           IFAIL, FMIN, FMAX, FINT, INFO)

C==============================================================================
C Subroutine for fast evaluation of the Lomb-Scargle statistic using the method
C described by Press, W. H. & Rybicki, G. B., 1989, Astrophysical Journal, 338,
C 277-280. Given N data points with abscissas X (which need not be equally 
C spaced) and ordinates Y, this routine fills array WORK1 with a sequence of 
C NOUT increasing frequencies (not angular frequencies) from FMIN up to FMAX
C (with frequency interval FMIN) and fills array WORK2 with the values of 
C the Lomb-Scargle normalized periodogram at those frequencies. The arrays X 
C and Y are not altered. NWK, the dimension of WORK1 and WORK2, must be large 
C enough for intermediate work space, or an error results. If any errors occur,
C IFAIL is set to 1, otherwise IFAIL = 0. If INFO=1 then loop information is 
C output to the screen.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 29-April-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C==============================================================================

	IMPLICIT NONE

C------------------------------------------------------------------------------
C Number of interpolation points per 1/4 cycle of highest frequency.
C------------------------------------------------------------------------------

	INTEGER MACC
	PARAMETER (MACC=4)

C------------------------------------------------------------------------------
C PERIOD_SCARGLE declarations.
C------------------------------------------------------------------------------

	INTEGER N, NWK, NOUT, IFAIL, INFO, J, NFREQ, NFREQT, NDIM, K
	INTEGER IEL, LOOP, ISTEP
        INTEGER CFREQ_LEN, CSTAT_LEN
	DOUBLE PRECISION X(N), Y(N), WORK1(NWK), WORK2(NWK)
	DOUBLE PRECISION FMIN, FMAX, FINT
	DOUBLE PRECISION AVE, ADEV, SDEV, VAR
	DOUBLE PRECISION XMIN, XMAX, XDIF, OFAC, HIFAC, FAC, FNDIM
	DOUBLE PRECISION CK, CKK, DF, HYPO, HC2WT, HS2WT, CWT, SWT
	DOUBLE PRECISION CTERM, STERM, DEN, PRODUCT
        CHARACTER*32 CFREQ, CSTAT
	DATA ISTEP /50/

C------------------------------------------------------------------------------
C Compute the mean, variance, and range of the data.
C------------------------------------------------------------------------------
	
        CALL PERIOD_MOMENT (Y, N, AVE, ADEV, SDEV, VAR)
	IF (VAR .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Zero variance in PERIOD_SCARGLE.'
		IFAIL = 1
		GOTO 99
	END IF
	IFAIL = 0
	XMIN = X(1)
	XMAX = XMIN
	DO 10 J = 2, N
		IF (X(J) .LT. XMIN) XMIN = X(J)
		IF (X(J) .GT. XMAX) XMAX = X(J)	
10	CONTINUE
	XDIF = XMAX - XMIN

C------------------------------------------------------------------------------
C Compute OFAC and HIFAC. 
C Fixed problem with NOUT found by Kieran O'Brien, VSD@Sheffield, 26-May-2004
C------------------------------------------------------------------------------

	OFAC = 1.0D0 / (FINT * XDIF)
	HIFAC = (FMAX / FINT) / (0.5D0 * OFAC * DBLE(N))
	PRODUCT = 0.5D0 * OFAC * HIFAC * DBLE(N)
	NOUT = INT(PRODUCT)+1

C------------------------------------------------------------------------------
C Size the FFT as next power of 2 above NFREQT.
C------------------------------------------------------------------------------

	NFREQT = INT(OFAC * HIFAC * DBLE(N) * DBLE(MACC))

	NFREQ = 64
11	IF (NFREQ .LT. NFREQT) THEN
		NFREQ = NFREQ * 2
	GOTO 11
	END IF
	NDIM = 2 * NFREQ
	IF (NDIM .GT. NWK) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Workspaces too small in PERIOD_SCARGLE.'
		IFAIL = 1
		GOTO 99
	END IF
	IFAIL = 0

C------------------------------------------------------------------------------
C Zero the workspaces.
C------------------------------------------------------------------------------
	
	DO 12 J = 1, NDIM
		WORK1(J) = 0.0D0
		WORK2(J) = 0.0D0
12	CONTINUE
	FAC = DBLE(NDIM) / (XDIF * OFAC)
	FNDIM = DBLE(NDIM)

C------------------------------------------------------------------------------
C Extirpolate the data into the workspaces.
C------------------------------------------------------------------------------
	
	DO 13 J = 1, N
		CK = 1.0D0 + DMOD ((X(J) - XMIN) * FAC, FNDIM)
		CKK = 1.0D0 + DMOD (2.0D0 * (CK - 1.0D0), FNDIM)
		CALL PERIOD_SPREAD (Y(J) - AVE, WORK1, NDIM, CK, MACC, IFAIL)
		IF (IFAIL .EQ. 1) GOTO 99
		CALL PERIOD_SPREAD (1.0D0, WORK2, NDIM, CKK, MACC, IFAIL)
		IF (IFAIL .EQ. 1) GOTO 99
13 	CONTINUE

C------------------------------------------------------------------------------
C Take the Fast Fourier Transforms.
C------------------------------------------------------------------------------
	
	CALL PERIOD_REALFT (WORK1, NFREQ, 1)
	CALL PERIOD_REALFT (WORK2, NFREQ, 1)
	DF = 1.0D0 / (XDIF * OFAC)
	K = 3

C------------------------------------------------------------------------------
C Compute the Lomb-Scargle value for each frequency between FMIN, FMAX
C with step size FINT.
C------------------------------------------------------------------------------
			
	IEL = 0
	LOOP = 1
	DO 14 J = 1, NOUT
		IF ((DBLE(J) * DF) .LT. FMIN) THEN
		K = K + 2
		GOTO 14
		ELSE 
		HYPO = DSQRT (WORK2(K)**2.0D0 + WORK2(K+1)**2.0D0)
		HC2WT = 0.5D0 * WORK2(K) / HYPO
		HS2WT = 0.5D0 * WORK2(K+1) / HYPO
		CWT = DSQRT (0.5D0 + HC2WT)
		SWT = DSIGN (DSQRT(0.5D0-HC2WT), HS2WT)
		DEN = 0.5D0 * DBLE(N) + HC2WT * WORK2(K) + HS2WT * WORK2(K+1)
		CTERM = (CWT * WORK1(K) + SWT * WORK1(K+1))**2.0D0 / DEN
		STERM = (CWT*WORK1(K+1) - SWT*WORK1(K))**2.0D0 / (DBLE(N)-DEN)
		IEL = IEL + 1
		WORK1(IEL) = DBLE(J) * DF
		WORK2(IEL) = (CTERM + STERM) / (2.0D0 * VAR)
		IF (INFO .EQ. 1) THEN
		IF (IEL .EQ. (LOOP*ISTEP)+1) THEN
                   WRITE(CFREQ,*)WORK1(IEL)
                   CFREQ_LEN = LEN_TRIM(CFREQ)
                   WRITE(CSTAT,*)WORK2(IEL)
                   CSTAT_LEN = LEN_TRIM(CSTAT)
                   WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', Lomb-Scargle statistic = '//CSTAT(1:CSTAT_LEN)
	           LOOP = LOOP + 1
		END IF
		END IF
		K = K + 2
		END IF
14 	CONTINUE
	NOUT = IEL

99	RETURN
	END

C==============================================================================

	SUBROUTINE PERIOD_SPREAD (Y, YY, N, X, M, IFAIL)

C==============================================================================
C Given an array YY of length N, extirpolate (spread) a value Y into M actual
C array elements that best approximate the "fictional" (ie. possibly
C non-integer) array element number X. The weights used are coefficients of
C the Lagrange interpolating polynomial.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 29-April-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C==============================================================================

	IMPLICIT NONE

C------------------------------------------------------------------------------
C PERIOD_SPREAD declarations.
C------------------------------------------------------------------------------

	INTEGER M, IFAIL, IX, ILO, IHI, NDEN, J, NFAC(10), N
	DOUBLE PRECISION YY(N)
	DOUBLE PRECISION X, Y, FAC
	DATA NFAC /1,1,2,6,24,120,720,5040,40320,362880/

	IF (M .GT. 10) THEN
		WRITE(*,*)ACHAR(7)
	WRITE(*,*)'** ERROR: Factorial table too small in PERIOD_SPREAD.'
		IFAIL = 1
		GOTO 99
	END IF
	IFAIL = 0
	IX = INT(X)
	IF (X .EQ. DBLE(IX)) THEN
		YY(IX) = YY(IX) + Y
	ELSE
		ILO = MIN (MAX (INT(X-0.5D0*DBLE(M)+1.0D0), 1), N-M+1)
		IHI = ILO + M - 1
		NDEN = NFAC(M)
		FAC = X - DBLE(ILO)
		DO 15 J = ILO + 1, IHI
			FAC = FAC * (X - DBLE(J))
15		CONTINUE
		YY(IHI) = YY(IHI)+Y*FAC/(DBLE(NDEN)*(X-DBLE(IHI)))
		DO 16 J = IHI - 1, ILO, -1
			NDEN = (NDEN / (J + 1 - ILO)) * (J - IHI)
			YY(J) = YY(J)+Y*FAC/(DBLE(NDEN)*(X-DBLE(J)))
16		CONTINUE
	END IF

99	RETURN
	END
