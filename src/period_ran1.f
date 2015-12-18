
      SUBROUTINE PERIOD_RAN1(IDUM, IR, RANDOM, IY)

C============================================================================
C Returns a uniform deviate between 0.0 and 1.0. Set IDUM to any negative
C value to initialise or reinitialise the sequence. 
C
C Adapted from Numerical Recipes by 
C Chris Watson/Vik Dhillon @Sheffield 12-Feb-2004.
C============================================================================

      IMPLICIT NONE

      REAL IR(97), RANDOM, RM
      INTEGER M, IA, IC, IY, IFF, IDUM, J
      DATA M, IA, IC /714025, 1366, 150889/
      DATA IFF /0/

      RM = 1./M

      IF (IDUM .LT. 0 .OR. IFF .EQ. 0)THEN
         IFF = 1
         IDUM = MOD(IC-IDUM,M)
         DO J = 1, 97
	    IDUM = MOD(IA*IDUM+IC, M)
	    IR(J) = IDUM
         END DO
         IDUM = MOD(IA*IDUM+IC, M)
         IY = IDUM
      END IF

      J = 1 + (97 * IY)/M
      IF (J .GT. 97 .OR. J .LT. 1) THEN
         WRITE(*,*)ACHAR(7)
         WRITE(*,*) '** ERROR: PERIOD_RAN1 failed.'
         GO TO 99
      END IF

      IY = INT(IR(J))
      RANDOM = IY*RM
      IDUM = MOD(IA*IDUM+IC, M)
      IR(J) = IDUM
      
 99   RETURN
      END
