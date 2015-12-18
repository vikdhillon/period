        
	SUBROUTINE PERIOD_TIME (Y, MXROW, MXSLOT, 
     +				NPTSARRAY, YERRORARRAY, 
     +				INFILEARRAY, DETRENDARRAY)

C==============================================================================
C Applies heliocentric or barycentric timing correction to a list 
C of MJDs (as output by ULTRACAM, for example). This routine
C uses a mixture of the SOFA and SLALIB libraries. The
C former is much more accurate - SLALIB gives a barycentric
C correction accurate to 23 milliseconds with respect to the JPL
C DE96 ephemeris, whereas SOFA is accurate to 45 microseconds with
C respect to the (newer) JPL DE405 ephemeris. Similarly the
C heliocentric correction given by SLALIB is accurate to only 5
C milliseconds, whereas that of SOFA is accurate to 37 microseconds.
C Therefore, SLALIB has only been used on those occasions when SOFA
C does not have the required functionality, e.g. when computing the
C telescope's position on the earth, which does not affect the
C accuracy of the results.
C
C By comparing the output of this routine with the example calculations
C given in the Jodrell Bank Crab Pulsar Monthly Ephemeris, it appears
C that the barycentric correction given here is accurate to just under 2
C milliseconds, but this discrepancy could be due to the fact that
C Jodrell have used the full JPL DE200 ephemeris in their calculation,
C whereas SOFA uses (a fit to) the JPL DE405 ephemeris. The discrepancy
C could also be due to the fact that we have assumed zero parallax,
C proper motion and radial velocity for the Crab, whereas Jodrell may
C have included appropriate values for these quantities in their
C calculation.
C 
C Note that this routine converts MJD (on the UTC timescale, as output
C by ULTRACAM) to either BMJD (i.e. the MJD at the solar system
C barycentre) on the TDB timescale or HMJD (i.e. the MJD at the centre of
C the Sun) on the UTC timescale.
C
C Written by Vikram Singh Dhillon @Santiago 25-April-2005.
C Checked by Vikram Singh Dhillon @Sheffield 27-April-2006 - period now
C definitely links to versions of slalib and sofa which include the most
C recent leap second.
C==============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

	INTEGER   MXROW, MXSLOT
	DOUBLE PRECISION Y(MXROW, 3, MXSLOT)
	INTEGER   NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_TIME declarations.
C-----------------------------------------------------------------------------

        DOUBLE PRECISION PI, AU, C, MJD2, MJD2_DATE, HMJD
        DOUBLE PRECISION SLA_DTT, SLA_RCC
        DOUBLE PRECISION TT, TDB, DELTATT, DELTATDB, UTC
        DOUBLE PRECISION RAS, DECS, RA, DEC
        DOUBLE PRECISION PMRA, PMDEC, PX, RV 
	DOUBLE PRECISION LONG, LATITUDE, ALTITUDE
        DOUBLE PRECISION DISTSPIN, DISTEQ
        DOUBLE PRECISION GMST, EQEQX, LAST, SLA_GMSTA, SLA_EQEQX 
        DOUBLE PRECISION EPVH(3,2), EPVB(3,2), OBSPV(6), SPV(3,2)
        DOUBLE PRECISION E2PVH(3,2), E2PVB(3,2)
        DOUBLE PRECISION EPH(3), EPB(3), SP(3)
        DOUBLE PRECISION ENORMH(3), ENORMB(3), SNORM(3)
        DOUBLE PRECISION EMODH, EMODB, SMOD, THETAH, THETAB
        DOUBLE PRECISION BARY_CORR, HEL_CORR
	INTEGER I, FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, IFAIL
	INTEGER COUNTER, SLOTOUT
        INTEGER RAH, RAM, DECD, DECM
	LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
        LOGICAL LNEG
	CHARACTER*1 OPTION
	CHARACTER*10 OBS_IDEN
	CHARACTER*40 OBS_NAME
	CHARACTER*72 INFILEARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C Set constants.
C-----------------------------------------------------------------------------

        DATA C  / 299792458.D0 /
        DATA PI / 3.141592653589D0 /  
        DATA AU / 1.49597870691D11 /
        
C-----------------------------------------------------------------------------
C Select input and output data slots and prompt for conversion choice.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	CALL PERIOD_SELECT (FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
	IF (IFAIL .EQ. 1) THEN
           GOTO 99
	END IF
        WRITE(*,*)' '
 10     WRITE(*,'(A,$)')'[B]arycentric or'// 
     +       ' [H]eliocentric correction ? [B] : '
        READ(*,'(A)',ERR=10)OPTION
        IF (OPTION .EQ. ' ') OPTION = 'b'
        CALL PERIOD_CASE (OPTION, .TRUE.)
        IF (OPTION .NE. 'B' .AND. OPTION .NE. 'H') GOTO 10
        
C-----------------------------------------------------------------------------
C Prompt for object RA and Dec. Assume zero proper motion, parallax and 
C radial velocity - for the most accurate calculations, these values should
C be entered for the target.
C----------------------------------------------------------------------------- 

 20     LNEG = .FALSE.
        WRITE(*,'(A,$)')'Enter RA and Dec'// 
     +       ' (J2000; e.g. 12 34 56.7 -12 34 56.7) : '
        READ(*,*,ERR=20)RAH, RAM, RAS, DECD, DECM, DECS
        IF (DECD .LT. 0.D0) THEN
           DECD = ABS(DECD)
           LNEG = .TRUE.
        END IF

        PMRA = 0.D0             ! RA proper motion (radians/year) 
        PMDEC = 0.D0            ! Dec proper motion (radians/year) 
        PX = 0.D0               ! Parallax (arcseconds)
        RV = 0.D0               ! Radial velocity (km/s, +ve = receding)

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
C Hard-wire parameters for comparing with the example calculations in the 
C Jodrell Bank Cab Pulsar Monthly Ephemeris.
C----------------------------------------------------------------------------- 

C        MJD2 = 52179.098368267975D0
C        MJD2 = 52486.298901331030D0 
C        RA = 83.633218D0 * PI / 180.D0
C        DEC = -1.d0 * 22.01446361D0 * PI / 180.D0

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
C Calculate distance north of equatorial plane and from Earth's spin axis.
C----------------------------------------------------------------------------- 
              
              CALL SLA_GEOC (LATITUDE, ALTITUDE, DISTSPIN, DISTEQ)

C-----------------------------------------------------------------------------
C Convert DISTEQ and DISTSPIN to km. 
C----------------------------------------------------------------------------- 

              DISTEQ = DISTEQ*AU/1000.D0
              DISTSPIN = DISTSPIN*AU/1000.D0

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
              WRITE(*,*)'** OK: Distance north of equatorial'//
     +             ' plane (km) = ',DISTEQ
              WRITE(*,*)'** OK: Distance from Earth''s spin axis'//
     +             ' (km) = ',DISTSPIN
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

C----------------------------------------------------------------------------- 
C Calculate offset between UTC (in the form of a MJD2) and Terrestrial Time
C (TT). 
C----------------------------------------------------------------------------- 

              MJD2_DATE = AINT(MJD2)
              DELTATT = SLA_DTT (MJD2_DATE)
C              WRITE(*,*)' '
C              WRITE(*,*)'** OK: TT-UTC (s) = ',DELTATT
              TT = MJD2 + (DELTATT/86400.D0)
        
C-----------------------------------------------------------------------------
C Calculate offset between TT and Barycentric Dynamical Time
C (TDB). The first argument passed to SLA_RCC should, strictly, be
C TDB; however, TT can in practice be used without significant loss
C of accuracy. Similarly, the second argument should, strictly, be
C UT1, but UTC (which is kept within +/- 0.9 s of UT1 by means of
C leap seconds) can be used without significant loss of accuracy.
C-----------------------------------------------------------------------------

              UTC = MJD2 - AINT(MJD2)
              DELTATDB = SLA_RCC (TT, UTC, LONG, DISTSPIN, DISTEQ)
C              WRITE(*,*)'** OK: TDB-TT (s) = ',DELTATDB
              TDB = TT + (DELTATDB/86400.D0)

C-----------------------------------------------------------------------------
C We have just calculated the time when the photon from the pulsar hits
C the telescope on Earth, as measured by a clock at the solar system
C barycentre. We now need to add the time it would have taken for the
C photon to hit the solar system heliocentre or barycentre (rather than the 
C telescope on the Earth), as measured by the clock at the solar system 
C barycentre. This then removes the problem of photons arriving at different 
C times relative to each other due to the Earth moving in its orbit around the 
C Sun.
C
C First, determine the heliocentric and barycentric Earth position/velocity 
C vectors (AU,AU/day).
C-----------------------------------------------------------------------------

              CALL IAU_EPV00 (2400000.5D0, TDB, EPVH, EPVB, IFAIL)
              IF (IFAIL .EQ. 1) THEN
                 WRITE(*,*)' '
                 WRITE(*,*)'** WARNING: Date outside 1900-2100 AD.'
                 WRITE(*,*)' '
              END IF

C-----------------------------------------------------------------------------
C The above is from the centre of the Earth to the solar system heliocentre
C and barycentre. We actually want the heliocentric and barycentric telescope 
C position/velocity vectors. So, determine the geocentric position of the 
C telescope. Note that the arguments passed to SLA_GMSTA should, strictly, be
C UT1, but UTC (which is kept within +/- 0.9 s of UT1 by means of leap 
C seconds) can be used without significant loss of accuracy.
C----------------------------------------------------------------------------- 

              GMST  = SLA_GMSTA (MJD2_DATE, UTC)
              EQEQX = SLA_EQEQX (TDB)
              LAST  = GMST + LONG + EQEQX
              CALL SLA_PVOBS (LATITUDE, ALTITUDE, LAST, OBSPV)

C----------------------------------------------------------------------------- 
C Now add this vector to the barycentric and heliocentric Earth 
C position/velocity vectors.
C----------------------------------------------------------------------------- 

              E2PVH(1,1) = EPVH(1,1) + OBSPV(1)
              E2PVH(2,1) = EPVH(2,1) + OBSPV(2)
              E2PVH(3,1) = EPVH(3,1) + OBSPV(3)
              E2PVH(1,2) = EPVH(1,2) + OBSPV(4)
              E2PVH(2,2) = EPVH(2,2) + OBSPV(5)
              E2PVH(3,2) = EPVH(3,2) + OBSPV(6)
              
              E2PVB(1,1) = EPVB(1,1) + OBSPV(1)
              E2PVB(2,1) = EPVB(2,1) + OBSPV(2)
              E2PVB(3,1) = EPVB(3,1) + OBSPV(3)
              E2PVB(1,2) = EPVB(1,2) + OBSPV(4)
              E2PVB(2,2) = EPVB(2,2) + OBSPV(5)
              E2PVB(3,2) = EPVB(3,2) + OBSPV(6)

C----------------------------------------------------------------------------- 
C Now convert the star catalogue position to position/velocity vector
C (AU, AU/day). 
C----------------------------------------------------------------------------- 

              CALL IAU_STARPV (RA, DEC, PMRA, PMDEC, PX, RV, SPV, IFAIL)
              IF (IFAIL .NE. 0) THEN
                 IF (IFAIL .EQ. 1) THEN
C                   WRITE(*,*)' '
C                   WRITE(*,*)'** WARNING: Distance overridden. Zero parallax?'
C                   WRITE(*,*)' '
                 ELSE IF (IFAIL .EQ. 2) THEN
                    WRITE(*,*)' '
                    WRITE(*,*)'** WARNING: Excessive velocity.'
                    WRITE(*,*)' '
                 ELSE IF (IFAIL .EQ. 4) THEN
                    WRITE(*,*)' '
                    WRITE(*,*)'** WARNING: Solution did not converge.'
                    WRITE(*,*)' '
                 ELSE
                    WRITE(*,*)' '
                    WRITE(*,*)'** WARNING: Error in IAU_STARPV.'
                    WRITE(*,*)' '
                 END IF
              END IF

C----------------------------------------------------------------------------- 
C Remove the velocity component of the position/velocity vectors.
C----------------------------------------------------------------------------- 

              CALL IAU_PV2P (E2PVH, EPH)
              CALL IAU_PV2P (E2PVB, EPB)
              CALL IAU_PV2P (SPV, SP)

C----------------------------------------------------------------------------- 
C Normalise the position vectors, i.e. ensure that they have values ranging 
C from -1 to 1.
C----------------------------------------------------------------------------- 

              CALL IAU_PN (EPH, EMODH, ENORMH)
              CALL IAU_PN (EPB, EMODB, ENORMB)
              CALL IAU_PN (SP, SMOD, SNORM)

C----------------------------------------------------------------------------- 
C Calculate the angle between the Sun-Earth vector and the Sun-star vector.
C----------------------------------------------------------------------------- 

              CALL IAU_SEPP (ENORMH, SNORM, THETAH)
              CALL IAU_SEPP (ENORMB, SNORM, THETAB)
        
C-----------------------------------------------------------------------------
C Finally, calculate how much more or less time it takes the photon to
C travel to the solar system heliocentre and barycentre (compared to 
C the time it takes to travel to the telescope on Earth).
C-----------------------------------------------------------------------------

              HEL_CORR = EMODH * AU * DCOS(THETAH) / C
              BARY_CORR = EMODB * AU * DCOS(THETAB) / C
              IF (OPTION .EQ. 'B') THEN
                 TDB = TDB + (BARY_CORR/86400.D0)
                 IF (I .EQ. 1) THEN
                    WRITE(*,*)' '
                    WRITE(*,'(A10,10X,A11,10X,A9)')
     +                   ' MJD (UTC)','BMJD (TDB)','DELAY (s)'
                 END IF
                 WRITE(*,'(f18.11,2x,f18.11,2x,f12.6)')
     +                MJD2, TDB, BARY_CORR
                 Y(I,1,SLOTOUT) = TDB
              ELSE
                 HMJD = MJD2 + (HEL_CORR/86400.D0)
                 IF (I .EQ. 1) THEN
                    WRITE(*,*)' '
                    WRITE(*,'(A10,10X,A10,11X,A9)')
     +                   ' MJD (UTC)','HMJD (UTC)','DELAY (s)'
                 END IF
                 WRITE(*,'(f18.11,2x,f18.11,2x,f12.6)')
     +                MJD2, HMJD, HEL_CORR
                 Y(I,1,SLOTOUT) = HMJD
              END IF
              Y(I,2,SLOTOUT) = Y(I,2,SLOT)
              Y(I,3,SLOTOUT) = Y(I,3,SLOT)
           END DO

C-----------------------------------------------------------------------------
C Complete slot.
C----------------------------------------------------------------------------- 

           YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
           DETRENDARRAY(SLOTOUT) = DETRENDARRAY(SLOT)
           NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
           IF (OPTION .EQ. 'B') THEN
              INFILEARRAY(SLOTOUT) = 'barycentric, '//INFILEARRAY(SLOT)
           ELSE
              INFILEARRAY(SLOTOUT) = 'heliocentric, '//INFILEARRAY(SLOT)
           END IF
           WRITE(*,*)' '
           WRITE(*,*)'** OK: Filled slot = ',SLOTOUT

	END DO
              
 99     RETURN
	END
