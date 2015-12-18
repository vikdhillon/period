      
      SUBROUTINE PERIOD_MEDFILT (DATA, DOUT, NPOINT, MEDP, IWORK, 
     +                           WORK, IFAIL)

C=============================================================================
C THIS ROUTINE MEDIAN FILTERS ARRAY DATA(NPOINT). IT FITS AS MANY OF THE 
C SPECIFIED SIZE OF FILTER AS IT CAN GIVEN THE END CONSTRAINTS, AND USES THE 
C MEDIAN AS THE FILTERED VALUE.
C INPUT: 
C       REAL*4 DATA(NPOINT) -- DATA ARRAY TO BE FILTERED
C       INTEGER*4 NPOINT -- NUMBER OF POINTS IN ARRAY
C       INTEGER*4 MEDP   -- WIDTH OF FILTER IN BINS (ODD)
C       INTEGER*4 IWORK -- WORK ARRAY OF AT LEAST MEDP POINTS
C       REAL*4    WORK  -- WORK ARRAY OF AT LEAST MEDP+2 POINTS
C OUTPUT:
C       REAL*4 DOUT(NPOINT) -- FILTERED ARRAY
C       INTEGER*4 IFAIL -- ERROR RETURN = 1 IN CASE OF ERROR
C
C Adapted for PERIOD from a Tom Marsh subroutine by Vik Dhillon @Sheffield
C 7-October-2004.
C=============================================================================

      IMPLICIT NONE

      INTEGER IFAIL, I, NPOINT
      INTEGER MEDP, IWORK(1)
      INTEGER JSTART, JSTOP, NACT, JNEW1, JNEW2, IAD, J, IN
      INTEGER ITEST, PERIOD_IFIND
      DOUBLE PRECISION DATA(1), DOUT(1), WORK(1), TEST

C---------------------------------------------------------------------------

      IFAIL = 0
      IF(MEDP.EQ.1) THEN
         DO I=1,NPOINT
            DOUT(I) = DATA(I)
         END DO
         RETURN
      END IF
      IF(2*(MEDP/2)-MEDP.EQ.0) MEDP = MEDP - 1
      JSTART=1
      JSTOP = MIN(MEDP/2+1,NPOINT)
      NACT = JSTOP
      DO J=1,NACT
         WORK(JSTOP+J)=DATA(J)
      END DO

C---------------------------------------------------------------------------
C SORT FIRST FILTER BLOCK.
C---------------------------------------------------------------------------
     
      CALL PERIOD_SHELLSORT( NACT, WORK(JSTOP+1), IWORK)
      DO I=1,NACT
         WORK(I) = WORK(JSTOP+IWORK(I))
      END DO
      DOUT(1) = WORK(NACT/2+1)
      DO I=2,NPOINT

C---------------------------------------------------------------------------
C FIND START AND STOP POINTS FOR FILTER.
C---------------------------------------------------------------------------

         JNEW1 = MAX(I - MEDP/2,1)
         JNEW2 = MIN(I + MEDP/2,NPOINT)

C---------------------------------------------------------------------------
C UPDATE KEY INDEX ARRAY, IF START HAS CHANGED DELETE FIRST POINT.
C---------------------------------------------------------------------------

         IF(JNEW1 - JSTART .EQ. 1) THEN
            IAD = 0
            DO J=1,NACT
               IF(IWORK(J).EQ.1) IAD = 1
               IN = J + IAD
               IWORK(J) = IWORK(IN) - 1
               WORK(J) = WORK(IN)
            END DO
            NACT = NACT -1
         END IF

C---------------------------------------------------------------------------
C IF STOP HAS CHANGED ADD EXTRA POINT TO THE END.
C---------------------------------------------------------------------------
     
         IF(JNEW2 - JSTOP .EQ.1) THEN
            TEST = DATA(JSTOP)
            NACT = NACT + 1
            ITEST = PERIOD_IFIND(WORK, NACT-1, TEST)
            IF(ITEST.EQ.NACT) THEN
               IWORK(NACT) = NACT
               WORK(NACT) = TEST
               GOTO 10
            END IF
            DO J=NACT,ITEST+1,-1
               IWORK(J) = IWORK(J-1)
               WORK(J)  = WORK(J-1)
            END DO
            WORK(ITEST) = TEST
            IWORK(ITEST) = NACT
         END IF
 10      DOUT(I) = WORK(NACT/2+1)
         JSTART = JNEW1
         JSTOP  = JNEW2
      END DO

      RETURN
      END

C===========================================================================

      FUNCTION PERIOD_IFIND(XDATA, NDATA, TEST)

C===========================================================================
C INTEGER FUNCTION TO FIND THE POSITION OF A VALUE "TEST" IN AN ARRAY OF 
C MONOTONICALLY INCREASING VALUES "XDATA(NDATA)". PERIOD_IFIND RETURNED IS 
C THE POSITION OF THE ELEMENT IN THE ARRAY IF IT WAS PLACED IN THE ARRAY, 
C E.G. PERIOD_IFIND = 0 IF LESS THAN ALL XDATA, = NDATA+1 IF GREATER.
C
C Adapted for PERIOD from a Tom Marsh subroutine by Vik Dhillon, @Sheffield,
C 7-October-2004. 
C===========================================================================

      IMPLICIT NONE 

      INTEGER PERIOD_IFIND, NDATA, ISTEP
      DOUBLE PRECISION XDATA(1), TEST

C---------------------------------------------------------------------------

      IF(TEST.LT.XDATA(1)) THEN
         PERIOD_IFIND = 1
         RETURN
      ELSE IF(TEST.GE.XDATA(NDATA)) THEN
         PERIOD_IFIND = NDATA + 1
         RETURN
      END IF
      IF (NDATA.GE.32) THEN

C---------------------------------------------------------------------------
C BINARY CHOP METHOD.
C---------------------------------------------------------------------------

         PERIOD_IFIND = 1
         ISTEP = NDATA
 10      ISTEP = MAX(1, ISTEP/2)
         DO WHILE(TEST .GE. XDATA(PERIOD_IFIND) .AND. 
     +        PERIOD_IFIND.LE.NDATA)
            PERIOD_IFIND = PERIOD_IFIND + ISTEP
         END DO
         IF(ISTEP.GT.1) THEN
            PERIOD_IFIND = PERIOD_IFIND - ISTEP + ISTEP/2
            GOTO 10
         END IF
      ELSE

C---------------------------------------------------------------------------
C STRAIGHT-FORWARD METHOD.
C---------------------------------------------------------------------------

         DO PERIOD_IFIND = 2, NDATA
            IF(TEST.LT.XDATA(PERIOD_IFIND)) RETURN
         END DO
      END IF
      
      RETURN
      END
