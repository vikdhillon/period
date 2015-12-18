
	SUBROUTINE PERIOD_PDM (XDATA, YDATA, NDATA, NBIN, WBIN, VARI, 
     +                         PDM, MXCOL, IFAIL)

C===========================================================================
C This routine calculates the PDM statistic of XDATA(NDATA), YDATA(NDATA),
C following the method described by Stellingwerf (1978, APJ, 224, 953).
C Basically, the data is folded on a trial frequency and split up into 
C NBIN samples, each of width WBIN. The PDM statistic is then calculated
C by estimating the variance of each sample (i.e. bin), calculating the 
C overall variance for all of the samples and then dividing this by the
C variance of the whole dataset. If there is an error IFAIL = 1, otherwise
C IFAIL = 0.
C
C Written by Vikram Singh Dhillon @LPO 3-March-1993.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 16-April-2004.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_PDM declarations.
C-----------------------------------------------------------------------------

	INTEGER NDATA, NBIN, IFAIL, MXCOL, I, J, K
	INTEGER NPTS(MXCOL)
	DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
	DOUBLE PRECISION WBIN, VARI, PDM
	DOUBLE PRECISION BINWID, CENBIN, MINBIN, MAXBIN
	DOUBLE PRECISION NOM, DENOM
	DOUBLE PRECISION AVE, ADEV, SDEV
	DOUBLE PRECISION SAMPLE(MXCOL), SVAR(MXCOL)

C-----------------------------------------------------------------------------
C Loop through the NBIN samples.
C-----------------------------------------------------------------------------

	DO I = 1, NBIN

C-----------------------------------------------------------------------------
C Determine the limits of the bin.
C-----------------------------------------------------------------------------

		BINWID = 1.0D0 / DBLE(NBIN)
		CENBIN = (DBLE(I) * BINWID) - (0.5D0 * BINWID)
		MINBIN = CENBIN - (0.5D0 * WBIN)
		MAXBIN = CENBIN + (0.5D0 * WBIN)

C-----------------------------------------------------------------------------
C Loop through the phases and store those that fall in the bin.
C-----------------------------------------------------------------------------

		NPTS(I) = 0
		DO K = 1, 3
		DO J = 1, NDATA
			IF (K .EQ. 1) THEN
				IF (XDATA(J)-1.0D0 .LE. MAXBIN) THEN
				IF (XDATA(J)-1.0D0 .GT. MINBIN) THEN
				NPTS(I) = NPTS(I) + 1
				SAMPLE(NPTS(I)) = YDATA(J)
				END IF
				END IF
			ELSE IF (K .EQ. 2) THEN
				IF (XDATA(J) .LE. MAXBIN) THEN
				IF (XDATA(J) .GT. MINBIN) THEN
				NPTS(I) = NPTS(I) + 1
				SAMPLE(NPTS(I)) = YDATA(J)
				END IF
				END IF
			ELSE IF (K .EQ. 3) THEN
				IF (XDATA(J)+1.0D0 .LE. MAXBIN) THEN
				IF (XDATA(J)+1.0D0 .GT. MINBIN) THEN
				NPTS(I) = NPTS(I) + 1
				SAMPLE(NPTS(I)) = YDATA(J)
				END IF
				END IF
			END IF
		END DO
		END DO
	
C-----------------------------------------------------------------------------
C Calculate the variance of the sample and store it.
C-----------------------------------------------------------------------------
		
		IF (NPTS(I) .GT. 1) THEN
			CALL PERIOD_MOMENT (SAMPLE, NPTS(I), AVE, 
     +                                      ADEV, SDEV, SVAR(I))
		ELSE
			SVAR(I) = 0.0D0
		END IF
	END DO

C-----------------------------------------------------------------------------
C Calculate the overall variance for all of the samples and divide it by the
C variance of the whole dataset. The result is the PDM statistic.
C-----------------------------------------------------------------------------

	NOM = 0.0D0
	DENOM = 0.0D0
	DO I = 1, NBIN
		NOM = NOM + (DBLE(NPTS(I) - 1.0D0) * SVAR(I))
		DENOM = DENOM + DBLE(NPTS(I))
	END DO
	IF (DENOM .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR:  No points in bin in PERIOD_PDM.'
		IFAIL = 1
		GOTO 99
	ELSE IF (VARI .EQ. 0.) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR:  Zero variance in PERIOD_PDM.'
		IFAIL = 1
		GOTO 99
	END IF
	DENOM = DENOM - NBIN
	PDM = (NOM / DENOM) / VARI

C-----------------------------------------------------------------------------
C And return.
C-----------------------------------------------------------------------------

	IFAIL = 0
99	RETURN
	END
