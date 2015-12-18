	
	SUBROUTINE PERIOD_FT (XDATA, YDATA, NDATA, MXROW, FMIN, FMAX, 
     +			      FINT, FREQUENCY, POWER, NOUT, INFO)

C===========================================================================
C Calculates the Fourier transform of a dataset of length NDATA stored in 
C the arrays XDATA and YDATA. The FT will be calculated between the
C frequencies FMIN and FMAX, with a step size given by FINT. PERIOD_FT returns
C POWER (the amplitude squared divided by NDATA**2 for normalisation), 
C FREQUENCY (the corresponding frequencies) and NOUT (the number of points
C in POWER and FREQUENCY, which cannot be greater than MXROW). If INFO=1,
C then loop information is output to the screen.
C
C Written by Vikram Singh Dhillon @Sussex 24-June-1992.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C Removed loop over real variables, VSD @Sheffield 27-August-2009
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_FT declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA, MXROW, NOUT
	INTEGER COUNTER, LOOP, ISTEP, I, J, INFO, NFREQ
        INTEGER CFREQ_LEN, CSTAT_LEN
	DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
	DOUBLE PRECISION POWER(MXROW), FREQUENCY(MXROW)
	DOUBLE PRECISION FT_REAL(MXROW), FT_IMAG(MXROW)
	DOUBLE PRECISION FMIN, FMAX, FINT
	DOUBLE PRECISION PI, OMEGA, FREQ
	DOUBLE PRECISION EXPO, C, S
        CHARACTER*32 CFREQ, CSTAT
        DATA PI /3.1415926535897932384626433832795028841971693993751D0/
	DATA ISTEP /50/

C-----------------------------------------------------------------------------
C Initialise counters.
C-----------------------------------------------------------------------------

	LOOP = 1
	COUNTER = 1

C-----------------------------------------------------------------------------
C Loop over frequency range.
C-----------------------------------------------------------------------------

	NFREQ = 1+NINT((FMAX - FMIN)/FINT)
	DO J = 1, NFREQ
	   FREQ = FMIN + ((DBLE(J)-1.D0)*FINT)
				
C-----------------------------------------------------------------------------
C Initialise sums for each new frequency. The real part of the Fourier 
C transform is stored in FT_REAL and the imaginary part in FT_IMAG.
C-----------------------------------------------------------------------------

	   FT_REAL(COUNTER) = 0.0D0
	   FT_IMAG(COUNTER) = 0.0D0

C-----------------------------------------------------------------------------
C Calculate the angular frequency OMEGA.
C-----------------------------------------------------------------------------

	   OMEGA = 2.0D0 * PI * FREQ

C-----------------------------------------------------------------------------
C Loop over number of data points.
C-----------------------------------------------------------------------------

	   DO I = 1, NDATA

C-----------------------------------------------------------------------------
C Calculate the Fourier transform at frequency FREQ.
C-----------------------------------------------------------------------------

	      EXPO = OMEGA * XDATA(I)
	      C = DCOS (EXPO)
	      S = DSIN (EXPO)				
	      FT_REAL(COUNTER) = FT_REAL(COUNTER) + (YDATA(I) * C)
	      FT_IMAG(COUNTER) = FT_IMAG(COUNTER) + (YDATA(I) * S)
	   END DO

C-----------------------------------------------------------------------------
C Accumulate the power spectrum by summing the amplitude squared of the
C Fourier transform over the full frequency range. Divide by the number of
C data points squared in order to normalize the spectral window to unit
C amplitude at zero frequency.
C-----------------------------------------------------------------------------
	
	   POWER(COUNTER) = ((FT_REAL(COUNTER)**2.0D0) + 
     + (FT_IMAG(COUNTER)**2.0D0)) / (NDATA**2.0D0)

C-----------------------------------------------------------------------------
C Fill the frequency array, ouput some loop information and return.
C-----------------------------------------------------------------------------

	   FREQUENCY(COUNTER) = FREQ
	   IF (INFO .EQ. 1) THEN
	      IF (COUNTER .EQ. (LOOP*ISTEP)+1) THEN
		 WRITE(CFREQ,*)FREQUENCY(COUNTER)
		 CFREQ_LEN = LEN_TRIM(CFREQ)
		 WRITE(CSTAT,*)POWER(COUNTER)
		 CSTAT_LEN = LEN_TRIM(CSTAT)
		 WRITE(*,*)
     + 'Frequency = '//CFREQ(1:CFREQ_LEN)//
     + ', normalized power = '//CSTAT(1:CSTAT_LEN)
		 LOOP = LOOP + 1
	      END IF
	   END IF
	   COUNTER = COUNTER + 1
	END DO
        NOUT = COUNTER - 1
	
	RETURN
	END
