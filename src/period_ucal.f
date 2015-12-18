        
	SUBROUTINE PERIOD_UCAL (Y, MXROW, MXSLOT,
     +			        NPTSARRAY, YERRORARRAY, 
     +			        INFILEARRAY, DETRENDARRAY) 

C===========================================================================
C Routine to convert the ratio of target to comparison star counts into
C calibrated SDSS u'g'r'i'z' fluxes or magnitudes, given an input
C calibrated comparison star magnitude. Typical ULTRACAM input data are
C assumed, i.e.:
C 1. y-axis is target/comparison star counts.'
C 2. Calibrated comparison star magnitude is known.'
C 3. The data are taken with an SDSS filter (ugriz).'
C NOTE: It doesn't matter what filter the data have been recorded in,
C as long as it is an SDSS u'g'r'i'z' filter.
C Conversion uses the formula given, e.g. in Eqn.1 of Fukugita et al. 
C (1996, AJ, 111, 1748). Output units of flux are ergs/s/cm2/Hz.
C 
C Written by Vikram Singh Dhillon @Sheffield 07-March-2015.
C===========================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_UCAL declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION COMPMAG
	INTEGER SLOT, FIRSTSLOT, LASTSLOT, I
	INTEGER FIRSTOUT, IFAIL, SLOTOUT, COUNTER
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        CHARACTER*1 OPTION
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Select input and output data slots.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
		GOTO 99
	END IF

C-----------------------------------------------------------------------------
C Prompt for comparison star magnitude and flux units.
C-----------------------------------------------------------------------------

        WRITE(*,*)' '
        WRITE(*,*)'** WARNING: This routine assumes:'
        WRITE(*,*)
     + '** WARNING: 1. y-axis is target/comparison star counts.'
        WRITE(*,*)
     + '** WARNING: 2. Calibrated comparison star magnitude is known.'
        WRITE(*,*)
     + '** WARNING: 3. The data are taken with an SDSS filter (ugriz).'
        WRITE(*,*)' ' 
10      WRITE(*,'(A,$)')'Enter calibrated comparison star magnitude : '
        READ(*,*,ERR=10)COMPMAG
20      WRITE(*,'(A,$)')
     + 'Output target flux in [e]rgs/s/cm2/Hz, [m]Jy or '//
     + 'm[a]gnitude ? [e] : '
        READ(*,'(A)',ERR=20)OPTION
        IF (OPTION .EQ. ' ') OPTION = 'E'
        CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .NE. 'E' .AND. OPTION .NE. 'M' .AND.
     +       OPTION .NE. 'A') GOTO 20
       
C-----------------------------------------------------------------------------
C Convert to fluxes.
C-----------------------------------------------------------------------------

	COUNTER = 0
	DO SLOT = FIRSTSLOT, LASTSLOT
	IF (NPTSARRAY(SLOT) .EQ. 0) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** ERROR: Slot empty =',SLOT
		GOTO 99
	END IF
	SLOTOUT = FIRSTOUT + COUNTER
	COUNTER = COUNTER + 1
	DO I = 1, NPTSARRAY(SLOT)
		Y(I,1,SLOTOUT) = Y(I,1,SLOT)

                IF (OPTION .EQ. 'E') THEN
                   Y(I,2,SLOTOUT) = 
     + Y(I,2,SLOT)*(10.D0**(-0.4D0*(COMPMAG+48.60D0)))
                   IF (YERRORARRAY(SLOT)) THEN
                      Y(I,3,SLOTOUT) = 
     + Y(I,3,SLOT)*(10.D0**(-0.4D0*(COMPMAG+48.60D0)))
                      Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                   END IF

                ELSE IF (OPTION .EQ. 'M') THEN
                   Y(I,2,SLOTOUT) = 
     + Y(I,2,SLOT)*(10.D0**(-0.4D0*(COMPMAG+48.60D0)))
     + /(1.D-26)
                   IF (YERRORARRAY(SLOT)) THEN
                      Y(I,3,SLOTOUT) = 
     + Y(I,3,SLOT)*(10.D0**(-0.4D0*(COMPMAG+48.60D0)))
     + /(1.D-26)
                      Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                   END IF

                ELSE IF (OPTION .EQ. 'A') THEN
                   Y(I,2,SLOTOUT) = 
     + (-2.5*(DLOG10(Y(I,2,SLOT))))+COMPMAG
                   IF (YERRORARRAY(SLOT)) THEN
                      Y(I,3,SLOTOUT) = 
     + (2.5*Y(I,3,SLOT))/(DLOG(10.0D0)*Y(I,2,SLOT))
                      Y(I,3,SLOTOUT) = DSQRT(Y(I,3,SLOTOUT)**2.0D0)
                   END IF
                END IF

	END DO
	YERRORARRAY(SLOTOUT) = 	YERRORARRAY(SLOT) 
	DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
	NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
	INFILEARRAY(SLOTOUT) = 'calibrated, '//INFILEARRAY(SLOT)
	WRITE(*,*)' '
	WRITE(*,*)'** OK: Calibrated slot = ',SLOTOUT
	END DO

99	RETURN
	END
