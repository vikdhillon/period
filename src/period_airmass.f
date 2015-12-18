        
	SUBROUTINE PERIOD_AIRMASS (Y, MXROW, MXSLOT, 
     +				   NPTSARRAY, YERRORARRAY, 
     +				   INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Calculates airmass, zenith distance or parallactic angle of a list of MJDs 
C (as output by ULTRACAM, for example). Also outputs UTC date, azimuth and hour 
C angle to the screen, which might be of use for some applications. This 
C routine uses the SLALIB library.
C
C Written by Vikram Singh Dhillon @Lisbon 15-May-2011.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_AIRMASS declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION PI, MJD2, MJD2_DATE
        DOUBLE PRECISION SLA_DTT, SLA_PA, DELTATT
        DOUBLE PRECISION RAS, DECS, RA, DEC
	DOUBLE PRECISION LONG, LATITUDE, ALTITUDE
        DOUBLE PRECISION AIRMASS, AZ, ZD, HA, PA
        DOUBLE PRECISION OBSDEC, OBSRA, P, UTC_DAY
	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
        INTEGER RAH, RAM, DECD, DECM
        INTEGER DATE(4), NDP
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LNEG
	CHARACTER*1 OPTION
	CHARACTER*10 OBS_IDEN
	CHARACTER*40 OBS_NAME
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Set constants.
C-----------------------------------------------------------------------------

        DATA PI / 3.141592653589D0 /  
        
C-----------------------------------------------------------------------------
C Select input and output data slots and prompt for calculation choice.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
           GOTO 99
	END IF
        WRITE(*,*)' '
 10     WRITE(*,'(A,$)')'Calculate [A]irmass, '// 
     +       ' [Z]enith distance or [P]arallactic angle ? [Z] : '
        READ(*,'(A)',ERR=10)OPTION
        IF (OPTION .EQ. ' ') OPTION = 'z'
        CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .NE. 'A' .AND. OPTION .NE. 'Z'
     + .AND. OPTION .NE. 'P') GOTO 10
        
C-----------------------------------------------------------------------------
C Prompt for object RA and Dec. Strictly speaking, this should be the 
C apparent RA and Dec, i.e. including space motion, parallax, precession, 
C nutation, annual aberration, and the Sun's gravitational lens effect. 
C For the purposes of this routine, which need not be accurate to less than
C arcminutes, the distinction is unimportant and the mean (i.e. catalogue) RA 
C and Dec can be entered.
C----------------------------------------------------------------------------- 

 20     LNEG = .FALSE.
        WRITE(*,'(A,$)')'Enter RA and Dec'// 
     +       ' (J2000; e.g. 12 34 56.7 -12 34 56.7) : '
        READ(*,*,ERR=20)RAH, RAM, RAS, DECD, DECM, DECS
        IF (DECD .LT. 0.D0) THEN
           DECD = ABS(DECD)
           LNEG = .TRUE.
        END IF

C-----------------------------------------------------------------------------
C Convert RA and Dec to radians.
C----------------------------------------------------------------------------- 

        CALL SLA_DTF2R (RAH, RAM, RAS, RA, IFAIL)
        IF (IFAIL .EQ. 1) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: RA hours outside range 0-23'
           GOTO 20
        ELSE IF (IFAIL .EQ. 2) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: RA minutes outside range 0-59'
           GOTO 20
        ELSE IF (IFAIL .EQ. 3) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)'** ERROR: RA seconds outside range 0-59.999'
           GOTO 20
        END IF

        CALL SLA_DAF2R (DECD, DECM, DECS, DEC, IFAIL)
        IF (IFAIL .EQ. 1) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)
     +          '** ERROR: Declination degrees outside range 0-359'
           GOTO 20
        ELSE IF (IFAIL .EQ. 2) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)
     +          '** ERROR: Declination arcminutes outside range 0-59'
           GOTO 20
        ELSE IF (IFAIL .EQ. 3) THEN
           WRITE(*,*)ACHAR(7)
           WRITE(*,*)
     +         '** ERROR: Declination arcseconds outside range 0-59.999'
           GOTO 20
        END IF
        IF (LNEG) THEN
           DEC = -1.D0 * DEC
        END IF

C-----------------------------------------------------------------------------
C Prompt for observatory.
C----------------------------------------------------------------------------- 

 30     WRITE(*,'(A,$)')
     +       'Enter observatory identifier (<cr> for list) : ' 
        READ(*,'(A)')OBS_IDEN

C-----------------------------------------------------------------------------
C List observatories.
C----------------------------------------------------------------------------- 

        IF (OBS_IDEN .EQ. ' ') THEN
           WRITE(*,*)' '
           DO I = 1, 100
              CALL SLA_OBS (I, OBS_IDEN, OBS_NAME, LONG, 
     +             LATITUDE, ALTITUDE)
              WRITE(*,*)OBS_IDEN, OBS_NAME
              IF (OBS_NAME .EQ. '?') THEN
                 WRITE(*,*)' '
                 GOTO 30
              END IF
           END DO
        ELSE
           CALL SLA_OBS (0, OBS_IDEN, OBS_NAME, LONG, 
     +          LATITUDE, ALTITUDE)
           IF (OBS_NAME .EQ. '?') THEN
              GOTO 30
           ELSE
              
C-----------------------------------------------------------------------------
C Output results.
C----------------------------------------------------------------------------- 

              WRITE(*,*)' '
              WRITE(*,*)'** OK: Observatory = ',OBS_NAME
              WRITE(*,*)'** OK: Latitude (degrees) = ',
     +             LATITUDE*180.D0/PI
              WRITE(*,*)'** OK: Longitude (degrees, +ve=west) = ',
     +             LONG*180.D0/PI
              WRITE(*,*)'** OK: Altitude (m) = ',ALTITUDE
           END IF
        END IF

C----------------------------------------------------------------------------- 
C Loop over slots. 
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
              MJD2 = Y(I,1,SLOT)
              IF (MJD2 .GT. 6.D4 .OR. MJD2 .LT. 5.D4) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)'** ERROR: x-axis is not MJD.'
                 GOTO 99
              END IF
              MJD2_DATE = AINT(MJD2)
              DELTATT = SLA_DTT (MJD2_DATE)

C----------------------------------------------------------------------------- 
C Calculate zenith distance. I assume the sea-level temperature is 293K, the 
C polar motion is zero, the temperature at the observatory is 283K, the 
C humidity is 50%, the wavelength of observation is 550nm, and the 
C tropospheric lapse rate is 0.0065D0 K/m. See the comments for the sla_AOP 
C routine for further details.
C----------------------------------------------------------------------------- 

              P = 1013.25D0*EXP(-1.D0*ALTITUDE/(29.3D0*293.155D0))
              CALL SLA_AOP (RA, DEC, MJD2, DELTATT, LONG*(-1.D0), 
     +             LATITUDE, ALTITUDE, 0.D0, 0.D0, 283.155D0, P, 0.5D0, 
     +             0.55D0, 0.0065D0, AZ, ZD, HA, OBSDEC, OBSRA)
              AIRMASS = 1. / DCOS(ZD)
              ZD = ZD*180.D0/PI

C----------------------------------------------------------------------------- 
C Calculate parallactic angle. I've changed the output so that the
C PA runs from 0-360 degrees instead of -pi to +pi radians, where 180 is
C when the object transits.
C----------------------------------------------------------------------------- 

              PA = SLA_PA (HA, DEC, LATITUDE)
              IF (PA .LT. 0.) THEN
                 PA = 360.D0+(PA*180.D0/PI)
              ELSE
                 PA = (PA*180.D0/PI)
              END IF
                 
C-----------------------------------------------------------------------------
C For output to the screen, calculate the UTC date.
C----------------------------------------------------------------------------- 

              NDP = 6
              CALL SLA_DJCAL (NDP, MJD2, DATE, IFAIL)
              IF (IFAIL .NE. 0) THEN
                 WRITE(*,*)ACHAR(7)
                 WRITE(*,*)'** ERROR: Invalid MJD.'
                 GOTO 20
              END IF
              UTC_DAY = DBLE(DATE(3))+(DBLE(DATE(4))/(10.D0**DBLE(NDP)))

C-----------------------------------------------------------------------------
C Write output slot.
C----------------------------------------------------------------------------- 

              IF (I .EQ. 1) THEN
                 WRITE(*,*)' '
                 WRITE(*,'(A10,10X,A5,2X,A6,3X,A4,7X,
     +           A8,3X,A3,7X,A3,5X,A8,5X,A3)')
     +           ' MJD (UTC)',' Year',' Month',' Day',
     +           ' Airmass',' ZD', ' HA', ' Azimuth',
     +           ' PA'
              END IF
              WRITE(*,'(f18.11,3x,i4,3x,i2,6x,f9.6,3x,
     + f5.3,5x,f6.3,3x,f7.3,3x,f8.3,3x,f8.3)')
     + MJD2, DATE(1), DATE(2), UTC_DAY, AIRMASS, ZD, 
     + HA*180.D0/PI, AZ*180.D0/PI, PA
              IF (OPTION .EQ. 'A') THEN
                 Y(I,2,SLOTOUT) = AIRMASS
              ELSE IF (OPTION .EQ. 'Z') THEN
                 Y(I,2,SLOTOUT) = ZD
              ELSE
                 Y(I,2,SLOTOUT) = PA
              END IF
              Y(I,1,SLOTOUT) = Y(I,1,SLOT)
              Y(I,3,SLOTOUT) = 0.D0
           END DO
           YERRORARRAY(SLOTOUT) = .FALSE.
           DETRENDARRAY(SLOTOUT) = .FALSE.
           NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           IF (OPTION .EQ. 'A') THEN
              INFILEARRAY(SLOTOUT) = 'airmass, '//INFILEARRAY(SLOT)
           ELSE IF (OPTION .EQ. 'Z') THEN
              INFILEARRAY(SLOTOUT) = 'ZD, '//INFILEARRAY(SLOT)
           ELSE
              INFILEARRAY(SLOTOUT) = 'PA, '//INFILEARRAY(SLOT)
           END IF
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT
           
	END DO
              
 99     RETURN
	END
