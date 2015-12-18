
      SUBROUTINE PERIOD_GASDEV(ISET, IDUM, RANDOM, IR, IY, GSET)
 
C=============================================================================
C Returns a normally distributed deviate with zero mean and unit variance,
C using PERIOD_RAN1 as the source of uniform deviates. If ISET is odd this
C routine generates 2 uniform deviates using PERIOD_RAN1 and uses these to
C generate 2 random numbers from a Gaussian distribution. The first of these
C is used on odd calls. The second is used if ISET is even without any further
C calculation. IDUM is set to -1 to initialise PERIOD_RAN1.
C
C Adapted from Numerical Recipes by 
C Chris Watson/Vik Dhillon @Sheffield 12-Feb-2004.
C=============================================================================
 
      IMPLICIT NONE
 
      REAL V1, V2, R, FAC, GSET
      REAL IR(97)
      REAL RANDOM
      INTEGER ISET, IDUM, IY
 
      IF ( MOD(ISET,2).NE.0 ) THEN
1        CALL PERIOD_RAN1(IDUM, IR, RANDOM, IY)
         V1 = 2.* RANDOM - 1.
         CALL PERIOD_RAN1(IDUM, IR, RANDOM, IY)
         V2 = 2.* RANDOM - 1.
         R = (V1**2.) + (V2**2.)
         IF (R .GE. 1.) GO TO 1
         FAC = SQRT( -2. * LOG(R)/R )
         GSET = V1 * FAC
         RANDOM = V2 * FAC
      ELSE
         RANDOM = GSET
      END IF

      RETURN
      END
