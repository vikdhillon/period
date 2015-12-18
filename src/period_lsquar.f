
      SUBROUTINE PERIOD_LSQUAR (DATA, NUMBER, N, A, CHISQ, XM, NORM)

C=============================================================================
C PERIOD_LSQUAR provides a polynomial fit of specified degree to a set of
C data points. the fit is done in double precision. Uses the double precision 
C matrix inversion subroutine 'PERIOD_MLSRAR'.
C
C DATA    =  ARRAY CONTAINING THE DATA. THIS SHOULD BE ARRANGED:
C            DATA(1,K): X-VALUE OF DATA POINT.
C            DATA(2,K): Y-VALUE OF DATA POINT.
C            DATA(3,K): SIGMA IN Y-VALUE OF DATA POINT.
C            (DOUBLE PRECISION).
C NUMBER  =  NUMBER OF DATA POINTS TO BE FITTED.
C N       =  NUMBER OF COEFFICIENTS IN POLYNOMIAL TO BE FITTED. THE
C            ORDER OF THE POLYNOMIAL IS (N-1).
C A       =  ARRAY HOLDING COEFFICIENTS OF FIT (DOUBLE PRECISION). 
C            THIS IS ARRANGED: POLY = A(1) + A(2)*X + ... A(N)*X**(N-1).
C CHISQ   =  CONTAINS THE CHI-SQUARE VALUE OF THE FIT ON OUTPUT. IF
C            CHISQ = -1. ON OUTPUT THEN THE MATRIX WAS SINGULAR, IF
C            CHISQ = -2. ON OUTPUT THEN OVERFLOW OR DIVIDE CHECK OCCURED.
C            CHISQ = -3. ON OUTPUT THEN INVALID PARAMETERS INPUT.
C            IF YOU SET CHISQ = 0. ON INPUT ERROR MESSAGES WILL BE PRINTED.
C            (DOUBLE PRECISION).
C XM      =  WORKING STORAGE ARRAY (DOUBLE PRECISION). DIMENSION IN THE MAIN
C            PROGRAM AS XM(NMAX,2*NMAX+3) WHERE 'NMAX' IS THE MAXIMUM
C            VALUE OF 'N' YOU WISH TO CALL.
C NORM    =  SCALING PARAMETER. SINCE LARGE POWERS OF THE INPUT DATA ARE
C            TO BE TAKEN OVERFLOWS OR UNDERFLOWS CAN EASILY OCCUR. IF
C            YOU SET NORM = 1 ON INPUT DATA ARE SCALED TO REDUCE LIKELIHOOD
C            OF THIS EVENT. NORM = 0 INSTRUCTS FOR NO SCALING. INPUT DATA
C            ARE NOT DESTROYED BY THE SCALING, AND COEFFICIENTS ARE
C            AUTOMATICALLY SCALED BACK BEFORE OUTPUT.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C==============================================================================

      IMPLICIT NONE

      INTEGER N, NUMBER, NMAX, ITER, N21, N22, N23, NORM
      INTEGER I, J, M2, K, L, ITEST
      LOGICAL RITE
      DOUBLE PRECISION XM(N,2*N+3),XX,RR,EPS,AVE,SIGMA
      DOUBLE PRECISION DATA(3,NUMBER),A(N),CHISQ,S,R
      DOUBLE PRECISION X1, X2, X3

C------------------------------------------------------------------------------
C EPS    =  MAXIMUM ALLOWED ERROR IN ITERATIONS ON THE POLYNOMIAL
C           COEFFICIENTS. CONVERGENCE TERMINATES WHEN ERROR.LT.EPS
C NMAX   =  MAXIMUM ALLOWED NUMBER OF COEFFICIENTS (DUE TO DIMENSION
C           STATEMENTS).
C------------------------------------------------------------------------------

      DATA EPS,NMAX/1.D-6,50/
      RITE=.FALSE.      

C------------------------------------------------------------------------------
C Test input data.
C------------------------------------------------------------------------------

	IF((NUMBER.LE.0.OR.N.GT.NUMBER).OR.N.LE.0) THEN
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_LSQUAR failed.'
	  WRITE(*,*)'** ERROR: Number of points = ',NUMBER
	  WRITE(*,*)'** ERROR: Number of coefficients = ',N
	  WRITE(*,*)'** ERROR: Invalid values (must be positive and'
	  WRITE(*,*)'** ERROR: points .ge. coefficients).'
	  WRITE(*,*)' '
	  CHISQ=-3.D0
	  RETURN
	ELSE IF (N.GT.NMAX) THEN
	  WRITE(*,*)ACHAR(7)
	  WRITE(*,*)'** ERROR: PERIOD_LSQUAR failed.'
	  WRITE(*,*)'** ERROR: Number of coefficients = ',N
	  WRITE(*,*)'** ERROR: Maximum number of coefficients
     + permitted = ',NMAX
	  WRITE(*,*)' '
	  CHISQ=-3.D0
	  RETURN
	END IF

C------------------------------------------------------------------------------
C If the input value of CHISQ is 0. allow printing of error messages.  
C------------------------------------------------------------------------------

	IF (CHISQ.EQ.0.D0) RITE=.TRUE.    
	ITER=5     
	IF (RITE) ITER=-ITER   
	N21 = 2*N + 1
	N22 = N21 + 1
	N23 = N21 + 2

C------------------------------------------------------------------------------
C Rescale the input data (for NORM .NE. 0).
C------------------------------------------------------------------------------

	IF (NORM.NE.0 .AND. N.NE.1) THEN
	  AVE = 0.D0
	  SIGMA = 0.D0
	  DO I=1,NUMBER
	    X1 = DATA(1,I)
	    AVE   = AVE + X1
	    SIGMA = SIGMA + X1*X1    
	  END DO 
	  AVE = AVE/NUMBER
	  SIGMA = DSQRT(SIGMA/NUMBER-AVE*AVE)
	END IF

C------------------------------------------------------------------------------
C Zero the working array.
C------------------------------------------------------------------------------

	DO I = 1,N     
	  DO J = N21,N23
	    XM(I,J) = 0.D0
	  END DO
	END DO
	X2 = 0.D0
	X3 = 0.D0

C------------------------------------------------------------------------------
C Compute the moments of the data.
C Change by TRM @RGO 7-June-1988. Ignore point if sigma error estimate less 
C than or equal to zero.
C------------------------------------------------------------------------------

	M2 = 2*N 
	DO I = 1, NUMBER
          IF(DATA(3,I).GT.0.D0) THEN
	    RR = (1.D0/DATA(3,I))**2.D0
	    X2 = X2 + RR
	    XX = DATA(2,I)*RR
	    X3 = X3 + XX 
	    IF(N.NE.1) THEN
	      X1 = DATA(1,I)
	      DO J = 3, M2    
	        IF (NORM.EQ.0) THEN
		  RR = RR*X1 
	        ELSE
		  RR = RR*(X1-AVE)/SIGMA
	        END IF
	        IF(J.GT.N) THEN
    		  XM(J-N,N22)=XM(J-N,N22)+RR
	        ELSE
		  XM(J,N21)=XM(J,N21)+RR 
 	        END IF
	      END DO
	      DO J = 2, N
	        IF (NORM.EQ.0) THEN
		  XX = XX*X1 
	        ELSE
		  XX = XX*(X1-AVE)/SIGMA
	        END IF
 	        XM(J,N23) = XM(J,N23) + XX 
	      END DO
	    END IF
          END IF
	END DO
	XM(2,N21) = X2
	XM(1,N23) = X3  

C------------------------------------------------------------------------------
C Compute matrix for inversion.
C------------------------------------------------------------------------------

	DO I = 1, N
	  DO J = 1, N
	    K = I + J    
	    IF(K.GT.N) THEN   
	      XM(I,J) = XM(K-N,N22)
	    ELSE
	      XM(I,J) = XM(K,N21)
	    END IF
	  END DO
	END DO

C------------------------------------------------------------------------------
C Call double precision matrix inversion routine.
C------------------------------------------------------------------------------

	IF(N.NE.1) THEN
	  CALL PERIOD_MLSRAR(N,XM,XM(1,N23),ITER,
     + EPS,A,ITEST,0,XM(1,N+1))  
	  IF(ITEST.GE.5) THEN
	    CHISQ = -2.0D0
	    RETURN
	  END IF
	ELSE
	  XM(1,1) = 1.D0/XM(1,1)
	  A(1) = XM(1,1)*XM(1,N23) 
	END IF

C------------------------------------------------------------------------------
C Compute chi-square for resulting fit.
C------------------------------------------------------------------------------

	CHISQ=0.D0
	DO I=1,NUMBER
          IF(DATA(3,I).GT.0.D0) THEN
	    S = A(1)
	    IF(N.NE.1) THEN     
	      R=1.D0
	      X1 = DATA(1,I)
	      DO J = 2, N  
	        IF (NORM.EQ.0) THEN
		  R=R*X1
	        ELSE
		  R=R*(X1-AVE)/SIGMA
	        END IF
	        S = S + A(J)*R
	      END DO
	    END IF
 	    CHISQ = CHISQ +((S-DATA(2,I))/DATA(3,I))**2.D0
          END IF
	END DO

C------------------------------------------------------------------------------
C Error messages after inversion of the matrix XM (H in the write-up).
C------------------------------------------------------------------------------

	IF (NORM.EQ.0 .OR. N.EQ.1) RETURN

C------------------------------------------------------------------------------
C Rescale coefficients if data scaling was requested.
C------------------------------------------------------------------------------

	SIGMA = 1.D0/SIGMA
	AVE = -AVE*SIGMA
	L = N-1   
	DO I=1,L
	  XM(I,1) = AVE**I
	  XM(I,2) = 0.D0
	END DO
	XM(1,2) = 1.D0
	XM(N,2) = 0.D0
	DO I=1,L
	  K=N-I+1
 	  DO J=2,K     
 	    XM(J,2) = XM(J,2) + XM(J-1,2)
	  END DO
	  K = I+1
	  DO J = K,N
	    A(I) = A(I) + A(J)*XM(J-I+1,2)*XM(J-I,1)
	  END DO
	  A(I) = A(I)*SIGMA**(I-1)
	END DO
	A(N) = A(N)*SIGMA**(N-1)

	RETURN 
	END

C=============================================================================

	SUBROUTINE PERIOD_MLSRAR (N, BDMTX, V, ITER, EPS, F, IT, INEW, A)

C=============================================================================
C Double precision matrix inversion.
C
C N       =  ORDER OF MATRIX.
C BDMTX   =  TWO-DIMENSIONAL ARRAY OF COEFFICIENTS.
C V       =  RIGHT-HAND VECTOR.
C ITER    =  MAXIMUM NUMBER OF ITERATIONS DESIRED.
C EPS     =  TOLERANCE FOR CONVERGENCE.
C F       =  RESULTING VECTOR.
C IT      =  OUTPUT FROM ROUTINE SPECIFYING NUMBER OF ITERATIONS ACTUALLY
C            DONE.
C INEW    =  VARIABLE SET TO VALUE .NE.1 ON FIRST CALL. ON SUBSEQUENT CALLS
C            IT IS SET TO 1 IF THE MATRIX IS UNCHANGED BUT THE COLUMN
C            VECTOR 'B' IS CHANGED.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C=============================================================================

        IMPLICIT NONE
        LOGICAL RITE
        INTEGER N, ITER, IT, I, J, N1, INEW, IM1, JMX
        INTEGER K, II, I2
        DOUBLE PRECISION BDMTX(N,N),V(N),F(N),A(N,N),X(50)
        DOUBLE PRECISION IDX(50),XT(50)
        DOUBLE PRECISION SG1,AMX, XI, CX, SUM, SING, EPS, R
        DOUBLE PRECISION T, ABSA

	RITE=.FALSE.
	IF (ITER.LT.0) RITE=.TRUE.
	ITER=IABS(ITER)
	IT = 0
	DO I=1,N
	  X(I) = V(I)
	  F(I) = 0.D0
	END DO
	N1 = N-1
	IF(INEW.EQ.1) GOTO 181
	DO I=1,N
	  DO J=1,N
	    A(I,J)=BDMTX(I,J)
	  END DO
	END DO
	DO I=1,N
 	  IDX(I)=I 
	END DO
	SG1 = 0.D0
	DO I = 2, N

C------------------------------------------------------------------------------
C Partial pivoting, check for max element in (I-1)st column.
C------------------------------------------------------------------------------

	  IM1=I-1
	  AMX=DABS(A(IM1,IM1))
	  JMX=IM1
	  DO J = I, N
	    ABSA=DABS(A(J,IM1))
	    IF(AMX.LT.ABSA) THEN
	      AMX = ABSA 
	      JMX = J
	    END IF
	  END DO
	  IF(JMX.NE.IM1) THEN

C------------------------------------------------------------------------------
C Move the row with max A(J,IM1) to (IM1)st row.
C------------------------------------------------------------------------------

	    DO K = 1, N
	      T = A(IM1,K)
	      A(IM1,K) = A(JMX,K)
 	      A(JMX,K) = T
	    END DO
	    II = INT(IDX(IM1))
	    IDX(IM1) = IDX(JMX)
	    IDX(JMX) = II
	    XI = X(IM1)
	    X(IM1) = X(JMX)
	    X(JMX) = XI
	    SG1=1.D0
	  END IF
	  IF(A(IM1,IM1).EQ.0.D0) GOTO 200
	  DO J=I,N
	    CX=A(J,IM1)/A(IM1,IM1)
	    DO K=I,N
	      A(J,K)=A(J,K)-CX*A(IM1,K)
	    END DO
	    A(J,IM1) = CX
	  END DO
	END DO

C------------------------------------------------------------------------------
C Forward pass - operate on right hand side as on matrix.
C------------------------------------------------------------------------------

62	CONTINUE
	DO I=2,N
	  DO J=I,N
	    X(J)=X(J)-X(I-1)*A(J,I-1) 
	  END DO
	END DO

C------------------------------------------------------------------------------
C Backward pass - solve for AX = B.
C------------------------------------------------------------------------------

	X(N) = X(N)/A(N,N)
	DO I=1,N1
	  SUM=0.D0
	  I2=N-I+1
	  IM1=I2-1
	  DO J=I2,N
	    SUM=SUM+A(IM1,J)*X(J)
	  END DO
	  X(IM1)=(X(IM1)-SUM)/A(IM1,IM1)
	END DO
	DO I=1,N
	  F(I) = F(I) + X(I)
	END DO
	SING = 0.D0
	IF (IT.EQ.ITER) RETURN 
	IT = IT + 1
	DO I=1,N
	  IF(F(I).EQ.0.D0) THEN
	    SING = 1.D38
	    GOTO 150
	  END IF
	  SING = DMAX1(SING,DABS(X(I)/F(I)))
	END DO
	IF (SING.GT.EPS) GOTO 150

C------------------------------------------------------------------------------
C Finished.
C------------------------------------------------------------------------------

	RETURN

C------------------------------------------------------------------------------
C Double precision matrix multiplication.
C------------------------------------------------------------------------------

150	CONTINUE
	DO I=1,N
	  R=0.D0
	  DO J=1,N
	    R=R+BDMTX(I,J)*F(J)
	  END DO
	  X(I)=V(I)-R
	END DO
181	IF(SG1.EQ.0.D0) GOTO 62

C------------------------------------------------------------------------------
C If SG1 .NE. 0, permute X before performing forward pass.
C------------------------------------------------------------------------------

	DO I=1,N
	  XT(I)=X(I)
	END DO
	DO I=1,N
	  K=INT(IDX(I))
	  X(I)=XT(K)
	END DO
	GOTO 62
200	IF (RITE) THEN
		WRITE(*,*)ACHAR(7)
		WRITE(*,*)'** WARNING: In PERIOD_MLSRAR.'
		WRITE(*,*)'** WARNING: Diagonal term = ',IM1
		WRITE(*,*)'** WARNING: reduced to zero.'
		WRITE(*,*)' '
	END IF

	RETURN
	END
