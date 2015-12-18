								       
	SUBROUTINE PERIOD_CLEAN (XDATA, YDATA, NDATA, MXROW, FMIN, FMAX,
     +			         FINT, NCL, GAIN, FREQUENCY, POWER, NOUT,
     +                           INFO, ONES, WFREQ, DFREQ, D, W, R, B, 
     +                           C, S)

C===========================================================================
C Calculates the CLEANed power spectrum of a dataset of length NDATA stored 
C in the arrays XDATA and YDATA. The spectrum will be calculated between the
C frequencies FMIN and FMAX, with a step size given by FINT. The number
C of CLEAN iterations is set by NCL and the loop gain is set by GAIN.
C PERIOD_CLEAN returns POWER (the CLEANed power), FREQUENCY (the corresponding
C frequencies), and NOUT (the number of points in POWER and FREQUENCY, which 
C cannot be greater than MXROW). If INFO=1s then loop information is output
C to the screen.
C
C BRIEF DESCRIPTION OF USE:
C
C PERIOD_CLEAN deconvolves the spectral window from the dirty spectrum by 
C using an iterative, one-dimensional, Hoegbom CLEAN algorithm. To produce 
C the clean spectrum, a Gaussian beam is used which has been fitted to the 
C HWHM of the primary peak in the spectral window; the phases are determined
C by the offset of the mean time TMEAN from the "origin" TZERO. Since all 
C spectra from real data are Hermitian, we define only the non-negative 
C frequencies.  The negative frequency elements are recovered by the use of 
C the function PERIOD_CVAL, which returns the complex conjugate for negative 
C frequencies, and zero for frequencies outside the defined range.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992, based on 
C the original code written by Joseph Lehar (JLEHAR@MAIL.AST.CAM.AC.UK).
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_CLEAN declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA, MXROW, NCL, NOUT, I, J, NFREQ
	INTEGER NDFREQ, NWFREQ, NBFREQ
	INTEGER ICL, L, PERIOD_MAXLOC
	INTEGER ISTEP, LOOP, INFO
        INTEGER CFREQ_LEN, CSTAT_LEN
	DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
	DOUBLE PRECISION POWER(MXROW), FREQUENCY(MXROW)
	DOUBLE PRECISION ONES(0:MXROW-1), WFREQ(0:(2*MXROW)-1)
        DOUBLE PRECISION DFREQ(0:MXROW-1)
	DOUBLE PRECISION FMIN, FMAX, FINT, FREQ
	DOUBLE PRECISION GAIN
	DOUBLE PRECISION PI
	DOUBLE PRECISION PHINC, PERIOD_HWHM, HWIDTH, PERIOD_FILLB
	DOUBLE PRECISION TZERO, TMEAN, DMEAN, ADEV, SDEV, VAR
	DOUBLE COMPLEX PERIOD_DFOUR, CC, PERIOD_ALPHA
        DOUBLE COMPLEX D(0:MXROW-1), W(0:(2*MXROW)-1)
	DOUBLE COMPLEX R(0:MXROW-1)
	DOUBLE COMPLEX B(0:MXROW-1), C(0:MXROW-1), S(0:MXROW-1)
        CHARACTER*32 CFREQ, CSTAT
	DATA ISTEP /50/
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/

C----------------------------------------------------------------------------
C Initialise the C array.
C----------------------------------------------------------------------------

	DO I = 0, MXROW-1
		C(I) = 0.0D0
	END DO

C----------------------------------------------------------------------------
C Find the mean time and data mean and subtract (for simplicity). Also fill
C the ONES array with 1's in order to calculate the spectral window. The
C time information TZERO and TMEAN are used later in the CLEANing for beam
C phases.
C-----------------------------------------------------------------------------

	CALL PERIOD_MOMENT (XDATA, NDATA, TMEAN, ADEV, SDEV, VAR)
	CALL PERIOD_MOMENT (YDATA, NDATA, DMEAN, ADEV, SDEV, VAR)
	TZERO = TMEAN
        DO I = 1, NDATA
	   XDATA(I) = XDATA(I) - TMEAN 
	   YDATA(I) = YDATA(I) - DMEAN 
	   ONES(I) = 1.0D0
        END DO

C-----------------------------------------------------------------------------
C Calculate the dirty spectrum D (using a normal discrete Fourier transform). 
C Fill the residual array R with the dirty spectrum.
C-----------------------------------------------------------------------------

	IF (INFO .EQ. 1) THEN
	WRITE(*,*)'** OK: Calculating window and dirty spectra...'
	WRITE(*,*)' '
	END IF
	LOOP = 0
	I = 0
	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
           WFREQ(I)= FREQ
           DFREQ(I)= WFREQ(I)
           W(I) = PERIOD_DFOUR (NDATA, XDATA, ONES, WFREQ(I))  ! Spectral window.
           D(I) = PERIOD_DFOUR (NDATA, XDATA, YDATA, DFREQ(I)) ! Dirty spectrum.
	   R(I) = D(I)
	   IF (INFO .EQ. 1) THEN   
	   IF (I .EQ. LOOP*ISTEP) THEN
	      WRITE(CFREQ,*)DFREQ(I)
	      CFREQ_LEN = LEN_TRIM(CFREQ)
	      WRITE(CSTAT,*)(CDABS(D(I)))**2.0D0
	      CSTAT_LEN = LEN_TRIM(CSTAT)
	      WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', dirty power = '//CSTAT(1:CSTAT_LEN)
		LOOP = LOOP + 1
	   END IF
	   END IF
	   I = I + 1
        END DO
	NDFREQ = I - 1
 
C-----------------------------------------------------------------------------
C Complete the spectral window.
C-----------------------------------------------------------------------------

	IF (INFO .EQ. 1) THEN
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Completing spectral window...'
	WRITE(*,*)' '
	END IF
	NFREQ = NINT(((2.0D0*FMAX)-FMAX) / FINT)
	DO J = 1, NFREQ
	   FREQ = FMAX + FINT + ((DBLE(J)-1.D0)*FINT)
           WFREQ(I) = FREQ
           W(I) = PERIOD_DFOUR (NDATA, XDATA, ONES, WFREQ(I))
	   IF (INFO .EQ. 1) THEN
	   IF (I .EQ. LOOP*ISTEP) THEN
	      WRITE(CFREQ,*)WFREQ(I)
	      CFREQ_LEN = LEN_TRIM(CFREQ)
	      WRITE(CSTAT,*)(CDABS(W(I)))**2.0D0
	      CSTAT_LEN = LEN_TRIM(CSTAT)
	      WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', window power = '//CSTAT(1:CSTAT_LEN)
	      LOOP = LOOP + 1
	   END IF
	   END IF
	   I = I + 1
        END DO
	NWFREQ = I - 1
 	IF (2*NDFREQ .GT. NWFREQ) NDFREQ = NWFREQ/2     ! Ensure that 
							! NDFREQ = NWFREQ/2

C-----------------------------------------------------------------------------
C Fit a restoring beam B to the spectral window peak.
C-----------------------------------------------------------------------------

        PHINC = 2.0D0*PI*FINT*(TZERO-TMEAN)      ! Phase increment per element.
        HWIDTH = PERIOD_HWHM(NWFREQ, W)          ! HWHM of W (index units).
	IF (HWIDTH .LE. 0.0D0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Could not find half-width and'
		WRITE(*,*)'** ERROR: half-maximum in PERIOD_HWHM.'
		NOUT = 0
		GOTO 99
	END IF
        NBFREQ = INT(PERIOD_FILLB(MXROW, B, HWIDTH, PHINC)) ! Fill the restoring beam.

C----------------------------------------------------------------------------- 
C CLEAN the residual spectrum, storing the components in C(0:MS).
C-----------------------------------------------------------------------------

	IF (INFO .EQ. 1) THEN
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Performing CLEAN iterations...'
	WRITE(*,*)' '
	END IF
        DO ICL = 1, NCL
	   L = PERIOD_MAXLOC(NDFREQ, R)            ! Element location of max peak in R.
           CC = GAIN*PERIOD_ALPHA(L, NDFREQ, R, W) ! Estimate the component to remove.
           CALL PERIOD_SUBCMP(NDFREQ, R, W, L, CC) ! Subtract the component from R.
           C(L) = C(L) + CC                        ! Store the component in C.
   	   IF (INFO .EQ. 1) THEN
	      WRITE(*,*)'Clean iteration = ',ICL
	   END IF
        END DO

C----------------------------------------------------------------------------- 
C Generate the clean spectrum S(0:MS).
C-----------------------------------------------------------------------------

	IF (INFO .EQ. 1) THEN
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Generating CLEANed spectrum...'
	WRITE(*,*)' '
	END IF
        CALL PERIOD_CONVOLV (NDFREQ, S, NDFREQ, C, NBFREQ, B, ! Convolve C with
     +                       DFREQ, INFO)                     ! B to form S.
        CALL PERIOD_ADD (NDFREQ, S, NDFREQ, S, NDFREQ, R)     ! Then add R to S.

C----------------------------------------------------------------------------- 
C Finally, load the output arrays with the POWER of the CLEANed spectrum and
C their corresponding FREQUENCIES.
C-----------------------------------------------------------------------------

	DO I = 0, NDFREQ
		POWER(I+1) = (CDABS(S(I)))**2.0D0
		FREQUENCY(I+1) = DFREQ(I)
	END DO
	NOUT = NDFREQ + 1

99	RETURN
	END

C===========================================================================

        DOUBLE COMPLEX FUNCTION PERIOD_DFOUR (N, TIME, DATA, FREQ)

C===========================================================================
C Returns the Fourier transform of the time series specified by TIME(1:N)
C and DATA(1:N), evaluated at the frequency FREQ. The FT is normalized to 
C have the data mean at DC.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE

	INTEGER N, K
        DOUBLE PRECISION DATA(N), TIME(N), FREQ, TWOPI, PI, PHASE
        DOUBLE COMPLEX FT
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/
	TWOPI = 2.0D0*PI

C---------------------------------------------------------------------------  
C Evaluate FT at FREQ.
C--------------------------------------------------------------------------- 

        FT = (0.0D0,0.0D0)
	DO K = 1, N
           PHASE = -TWOPI * FREQ * TIME(K)
           FT = FT + DATA(K) * DCMPLX(DCOS(PHASE),DSIN(PHASE))
        END DO

C---------------------------------------------------------------------------  
C Return with FT properly normalized.
C--------------------------------------------------------------------------- 

        PERIOD_DFOUR = FT/DBLE(N)

        RETURN
        END

C===========================================================================
 
        INTEGER FUNCTION PERIOD_MAXLOC (M, ARRAY)

C===========================================================================
C Returns the array index of the ARRAY(0:M) element with maximum CDABS value.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER LMAX, M, I
	DOUBLE PRECISION AMAX, ARRAYI
        DOUBLE COMPLEX ARRAY(0:M) 

C--------------------------------------------------------------------------- 
C Find the maximum location.
C---------------------------------------------------------------------------

        LMAX = 0
        AMAX = CDABS(ARRAY(0))
        DO I = 1, M
           ARRAYI = CDABS(ARRAY(I))
           IF (ARRAYI .GT. AMAX) THEN
              AMAX = ARRAYI
              LMAX = I
           END IF
        END DO
        PERIOD_MAXLOC = LMAX

        RETURN
        END

C===========================================================================
 
        DOUBLE COMPLEX FUNCTION PERIOD_ALPHA (L, M, SPEC, WIND)

C===========================================================================
C Returns an estimate for the component A, responsible for SPEC(L) through
C the relation:
C                  SPEC(L) = A + (A*)WIND(2L)
C SPEC is defined (0:M), and WIND is defined (0:2M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER L, M
	DOUBLE PRECISION WNORM, ERROR
        DOUBLE COMPLEX SPEC(0:M), WIND(0:2*M), WIN2L, PERIOD_CVAL
        DATA ERROR /0.0001D0/                        ! Allowed error in WNORM.

C--------------------------------------------------------------------------- 
C Find the (L,-L) components which produce SPEC(L) through WIND.
C---------------------------------------------------------------------------

        WIN2L = WIND(2*L)                                 ! (L,-L) interference.
        WNORM = 1.0D0-CDABS(WIN2L)**2.0D0
        IF (WNORM .LT. ERROR) PERIOD_ALPHA = 0.5D0*SPEC(L)  ! Avoid singularities.
        IF (WNORM .GE. ERROR) 
     +  PERIOD_ALPHA = (SPEC(L)-WIN2L*
     +  PERIOD_CVAL(SPEC,-L,M))/WNORM

C--------------------------------------------------------------------------- 
C Return with the estimate of A.
C---------------------------------------------------------------------------

        RETURN
        END

C===========================================================================
 
        SUBROUTINE PERIOD_SUBCMP (M, SPEC, WIND, L, CPOS)

C===========================================================================
C Subtracts the complex component CPOS at array location L, and
C its complex conjugate, CNEG, at -L from the spectrum SPEC(0:M).
C The spectral window WIND(0:2M) is matched to the component and
C subtracted from the entire spectral array.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER M, L, I
        DOUBLE COMPLEX SPEC(0:M), WIND(0:2*M), CPOS, CNEG, PERIOD_CVAL

C--------------------------------------------------------------------------- 
C Specify the -L component.
C---------------------------------------------------------------------------

        CNEG = CONJG(CPOS)

C--------------------------------------------------------------------------- 
C Remove effects of both +L and -L components.
C---------------------------------------------------------------------------

        DO I = 0, M
           SPEC(I) = SPEC(I)-CPOS*
     +               PERIOD_CVAL(WIND,I-L,2*M)-CNEG*WIND(I+L)
        END DO               !     (From L)            (From -L)

C--------------------------------------------------------------------------- 
C Return to the program.
C---------------------------------------------------------------------------

        RETURN
        END

C===========================================================================
 
        SUBROUTINE PERIOD_CONVOLV (M, A, M1, A1, M2, A2, FREQ, INFO)

C===========================================================================
C Convolves complex arrays A1(0:M1) and A2(0:M2) to form A(0:M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER M, M1, M2, J, I
	INTEGER LOOP, ISTEP, INFO
        INTEGER CFREQ_LEN, CSTAT_LEN
        DOUBLE COMPLEX A(0:M), A1(0:M1), A2(0:M2), PERIOD_CVAL
	DOUBLE PRECISION FREQ(0:M)
        CHARACTER*32 CFREQ, CSTAT
	DATA ISTEP /50/

C--------------------------------------------------------------------------- 
C Convolve A1 with A2 to form A.
C---------------------------------------------------------------------------

	LOOP = 0
        DO J = 0, M
           A(J) = (0.0D0,0.0D0)    ! Reset A(J).
           DO I = -M2, M2
              A(J) = A(J)+PERIOD_CVAL(A2,I,M2)*
     +               PERIOD_CVAL(A1,J-I,M1)
           END DO
	   IF (INFO .EQ. 1) THEN
	   IF (J .EQ. LOOP*ISTEP) THEN
	      WRITE(CFREQ,*)FREQ(J)
	      CFREQ_LEN = LEN_TRIM(CFREQ)
	      WRITE(CSTAT,*)(CDABS(A(J)))**2.0D0
	      CSTAT_LEN = LEN_TRIM(CSTAT)
	      WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', CLEANed power = '//CSTAT(1:CSTAT_LEN)
	      LOOP = LOOP + 1
	   END IF
	   END IF
        END DO

        RETURN
        END

C===========================================================================
 
        SUBROUTINE PERIOD_ADD (M, A, M1, A1, M2, A2)

C===========================================================================
C Adds complex arrays A1(0:M1) and A2(0:M2) to form A(0:M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER M, M1, M2, J
        DOUBLE COMPLEX A(0:M), A1(0:M1), A2(0:M2), PERIOD_CVAL

C--------------------------------------------------------------------------- 
C Add A1 to A2, forming A.
C---------------------------------------------------------------------------

        DO J = 0, M
           A(J) = PERIOD_CVAL(A1,J,M1) + PERIOD_CVAL(A2,J,M2)
        END DO
        
	RETURN
        END

C===========================================================================
 
        DOUBLE PRECISION FUNCTION PERIOD_HWHM (M, ARRAY)

C===========================================================================
C Finds the half-width, half-maximum of the CDABS of ARRAY(0:M).
C This is done by linear interpolation between successive elements of ARRAY.
C Returns the HWHM in the units of the array elements.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER M, I
        DOUBLE COMPLEX ARRAY(0:M)
        DOUBLE PRECISION HALFMX, HALFWD, LAST, CURRENT, XLAST, SLOPE

C--------------------------------------------------------------------------- 
C Initial conditions for search.
C---------------------------------------------------------------------------

        HALFMX = CDABS(ARRAY(0))/2.0D0 ! Half-maximum (assume maximum at I=0).
        HALFWD = 0.0D0                 ! Half-width not yet found.

C--------------------------------------------------------------------------- 
C Loop through until less than HALFMX.
C---------------------------------------------------------------------------

        DO I = 1, M
           CURRENT = CDABS(ARRAY(I))             ! Current array value.
           IF (CURRENT .LT. HALFMX) THEN
              LAST = CDABS(ARRAY(I-1))           ! Last array value.
              XLAST = DBLE(I-1)                ! Last "x" co-ordinate.
              SLOPE = 1.0D0/(CURRENT-LAST)       ! Inverse slope between them.
              HALFWD = XLAST+SLOPE*(HALFMX-LAST) ! Interpolate to HALFMX.
              GOTO 10                            ! Pop out of loop.
           END IF
        END DO

C--------------------------------------------------------------------------- 
C Return with the result.
C---------------------------------------------------------------------------

10      PERIOD_HWHM = HALFWD
        
	RETURN
        END

C===========================================================================
 
	DOUBLE PRECISION FUNCTION PERIOD_FILLB (MB, B, HWIDTH, PINCR)

C===========================================================================
C Fills the restoring beam B(0:MB) with a Gaussian of half-width HWIDTH
C (in Index units). The complex phase is zero for I=0 and increases
C by PINCR for each element. Returns the maximum filled element.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER MB, I, MFILL
        DOUBLE COMPLEX B(0:MB)
	DOUBLE PRECISION SIGMA, HWIDTH, CONST, X, GAUSS
	DOUBLE PRECISION PHASEI, PINCR

C--------------------------------------------------------------------------- 
C Set up some initial conditions.
C---------------------------------------------------------------------------

        DO I = 0, MB
           B(I) = (0.0D0,0.0D0)
        END DO

C--------------------------------------------------------------------------- 
C If Gaussian, fit a Gaussian beam.
C Calculate SIGMA and the normalization constant.
C---------------------------------------------------------------------------

        SIGMA = HWIDTH/DSQRT(2.0D0*DLOG(2.0D0)) ! Sigma in element units.
        CONST = 1.0D0/(2.0D0*SIGMA*SIGMA)       ! Normalization constant.
        MFILL = INT(5.0D0*SIGMA)+1                  ! Maximum filled element.
        IF (MFILL .GT. MB) MFILL = MB

C---------------------------------------------------------------------------
C Fill B with the Gaussian.
C---------------------------------------------------------------------------

        DO I = 0, MFILL
           X = DBLE(I)
           GAUSS = DEXP(-CONST*X*X)
           B(I) = DCMPLX(GAUSS,0.0D0)
        END DO

C---------------------------------------------------------------------------
C Include the phase information.
C---------------------------------------------------------------------------

        DO I = 1, MFILL
           PHASEI = DBLE(I)*PINCR
           B(I) = B(I)*DCMPLX(DCOS(PHASEI),DSIN(PHASEI))
        END DO

C--------------------------------------------------------------------------- 
C Return the maximum filled element.
C---------------------------------------------------------------------------

        PERIOD_FILLB = DBLE(MFILL)
        
	RETURN
        END

C===========================================================================
 
        DOUBLE COMPLEX FUNCTION PERIOD_CVAL (ARRAY, I, MARR)

C===========================================================================
C Returns the "value" of the complex ARRAY at the "location" I.
C If I > 0 then it is equivalent to ARRAY(I), but if I < 0,
C it returns the complex conjugate of ARRAY(-I).  ARRAY must be
C indexed from 0 to at least ABS(I) and COMPLEX.
C If ABS(I) > MARR (maximum element in array), PERIOD_CVAL=0.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992. 
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE
	INTEGER I, LOC, MARR
        DOUBLE COMPLEX ARRAY(0:MARR)

C--------------------------------------------------------------------------- 
C Set value to conjugate of absolute location if necessary.
C---------------------------------------------------------------------------

        LOC = ABS(I)
        IF (LOC .GT. MARR) THEN
           PERIOD_CVAL = (0.0D0,0.0D0)
           RETURN
        END IF
        IF (I .GE. 0) PERIOD_CVAL = ARRAY(LOC)
        IF (I .LT. 0) PERIOD_CVAL = DCONJG(ARRAY(LOC))
        
	RETURN
        END
