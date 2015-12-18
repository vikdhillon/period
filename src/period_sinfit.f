
	SUBROUTINE PERIOD_SINFIT (XDATA, YDATA, YERR, NUM, PERIOD,
     +				  GAM, KVEL, PHI0, COV, NP, F, IFAIL)

C============================================================================
C Subroutine to fit " Y = GAM + KVEL*SIN( 2*PI*(X/PERIOD-PHI0) ) "
C Returning GAM, KVEL, PHI0.
C
C Method fits " Y = A + B*SIN(2*PI/PERIOD*X) + C*COS(2*PI/PERIOD*X) "
c using linear least squares, then converts A, B, C TO GAM, KVEL, PHI0.
C Formal least squares error matrix for A, B, C transformed to the 
C matrix for GAM, KVEL, PHI0 using a linear approximation.
C To ignore any point, YERR should be set to less than or equal 0.
C		
C PASSED:
C
C REAL*8 XDATA(NUM) -- ARRAY OF X VALUES
C REAL*8 YDATA(NUM) -- ARRAY OF Y VALUES
C REAL*8 YERR(NUM)  -- ARRAY OF ERRORS ON Y VALUES 1-SIGMA
C REAL*8 PERIOD     -- ( X VALUE/PERIOD ) REPRESENT A PHASE OF THE SINE CURVE
C INTEGER NUM     -- NUMBER OF VALUES
C
C RETURNED: 
C
C REAL*8 GAM      -- "GAMMA VELOCITY"
C REAL*8 KVEL     -- "K VELOCITY"
C REAL*8 PHI0     -- "PHASE OFFSET" 0 IF RED STAR VELOCITY
C REAL*8 COV(6)   -- VARIANCE MATRIX OF THE ABOVE. YERROR ARRAY MUST BE CORRECT
C		     THESE ARE CALCULATED BY TRANSFORMING THE COVARIANCE MATRIX
C		     FOR A, B, C USING A LINEAR APPROXIMATION
C REAL*8  F     -- VALUE OF CHI-SQUARED, CORRECT IF YERR ARE SCALED CORRECTLY
C INTEGER NP    -- ACTUAL NUMBER OF POINTS USED
C INTEGER IFAIL -- 0 IF NO ERROR, 1 OTHERWISE
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C Modified for native PERIOD by VSD @Sheffield 9-May-2003.
C Converted to DOUBLE PRECISION by Vik Dhillon @Tenerife 14-April-2004.
C============================================================================

	IMPLICIT NONE
	INTEGER NUM, IFAIL, NP, I
	DOUBLE PRECISION XDATA(1), YDATA(1), YERR(1)
	DOUBLE PRECISION A, B, C, PERIOD, COV(6)
	DOUBLE PRECISION KVEL, GAM, PHI0, F
	DOUBLE PRECISION PI
	DOUBLE PRECISION SW, SY, SY2, SS, SS2, SYS, SYC 
	DOUBLE PRECISION SC, SSC, SC2, XX, SN, CN, WW, W1
	DOUBLE PRECISION C1, C2, C3, C4, C5, C6, DET, SQ
	DATA PI /3.1415926535897932384626433832795028841971693993751D0/

C----------------------------------------------------------------------------
C Set dummy variables to 0., to accumulate sums.
C SIND and COSD not available under linux f77, so replaced with 
C SIN and COS and a conversion from degrees to radians.
C----------------------------------------------------------------------------

	IF(PERIOD.LE.0.0D0) THEN
	  IFAIL = 1
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_SINFIT failed.'
	  WRITE(*,*)'** ERROR: Period is less than or equal to zero.'
	  RETURN
	END IF
	IFAIL = 0
	SW  = 0.0D0
	SY  = 0.0D0
	SY2 = 0.0D0
	SS  = 0.0D0
	SS2 = 0.0D0
	SYS = 0.0D0
	SYC = 0.0D0
	SC  = 0.0D0
	SSC = 0.0D0
	SC2 = 0.0D0
	NP = 0
	DO I = 1, NUM
	  IF(YERR(I) .GT. 0.0D0) THEN
	    NP = NP + 1
	    XX = 2.0D0*PI*XDATA(I)/PERIOD
	    SN = DSIN(XX)
	    CN = DCOS(XX)
	    WW = 1.0D0/YERR(I)/YERR(I)
	    W1 = WW*SN

C----------------------------------------------------------------------------
C Accumulate sums.
C----------------------------------------------------------------------------

	    SW  = SW  + WW
	    SY  = SY  + WW*YDATA(I)
	    SY2 = SY2 + WW*YDATA(I)*YDATA(I)
	    SS  = SS  + W1
	    SS2 = SS2 + W1*SN
	    SYS = SYS + W1*YDATA(I)
	    SYC = SYC + WW*CN*YDATA(I)
	    SC  = SC  + WW*CN
	    SSC = SSC + W1*CN
	    SC2 = SC2 + WW*CN*CN
	  END IF
	END DO
	IF(NP.LE.3) THEN
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_SINFIT failed.'
	  WRITE(*,*)'** ERROR: <= 3 valid points for a 3 parameter fit.'
	  IFAIL = 1
	END IF

C----------------------------------------------------------------------------
C Linear least squares to find A, B and C. Need to invert 3x3 (symmetric)
C matrix. Calculate co-factors (only 6).
C----------------------------------------------------------------------------

	C1 = SS2*SC2 - SSC*SSC
	C2 = SC*SSC  - SS*SC2
	C3 = SS*SSC  - SC*SS2
	C4 = SW*SC2  - SC*SC
	C5 = SC*SS   - SW*SSC
	C6 = SW*SS2  - SS*SS

C----------------------------------------------------------------------------
C Calculate determinant.
C----------------------------------------------------------------------------

	DET = SW*C1 + SS*C2 + SC*C3
	IF(DABS(DET) .LT. 1.0D-32) THEN
	  IFAIL = 1
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_SINFIT failed.'
	  WRITE(*,*)'** ERROR: ABS(DETERMINANT) < 1.D-32.'
	  RETURN
	END IF

C----------------------------------------------------------------------------
C Calculate A, B and C.
C----------------------------------------------------------------------------

	A = (C1*SY + C2*SYS + C3*SYC) / DET
	B = (C2*SY + C4*SYS + C5*SYC) / DET
	C = (C3*SY + C5*SYS + C6*SYC) / DET

C----------------------------------------------------------------------------
C Calculate chi-squared.
C----------------------------------------------------------------------------

	F = SY2 + A*A*SW + B*B*SS2 + C*C*SC2 + 2.0D0*B*C*SSC +
     &      2.0D0*A*B*SS + 2.0D0*A*C*SC - 2.0D0*A*SY - 
     &      2.0D0*B*SYS - 2.0D0*C*SYC

C----------------------------------------------------------------------------
C Calculate GAM, KVEL and PHI0, and their errors.
C ATAN2D not available under linux f77, so replaced with 
C ATAN2 and a conversion from degrees to radians.
C----------------------------------------------------------------------------

	SQ  = B*B + C*C
	KVEL  = DSQRT( SQ )
	IF(KVEL .EQ. 0.0D0) THEN
	  IFAIL = 1
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_SINFIT failed.'
	  WRITE(*,*)'** ERROR: KVEL = 0. Windowed data?'
	  RETURN
	END IF
	COV(1) =  C1 / DET
	COV(2) = (C2*B + C3*C) /KVEL /DET
	COV(3) = (C2*C - C3*B) /SQ /PI /DET
	COV(4) = (C4*B*B + 2.0D0*C5*B*C + C6*C*C) /SQ /DET
	COV(5) = ((C4-C6)*B*C + C5*(C*C - B*B)) /SQ /KVEL /PI /DET
	COV(6) = (C4*C*C - 2.0D0*C5*B*C + C6*B*B) /DET /SQ /SQ /PI /PI
	GAM   = A
	PHI0  = DATAN2(-C, B) /2.0D0 /PI

	RETURN
	END
