
	SUBROUTINE PERIOD_QUIT

C=======================================================================
C Pathetic little routine to quit from a program.
C
C Written by Vikram Singh Dhillon @Sussex 1-July-1992.
C=======================================================================

	IMPLICIT NONE
	
	CHARACTER*1 REPLY

	WRITE(*,*)' '
10	WRITE(*,'(A,$)')'Are you sure that you want to quit ? [N] : '
	READ(*,'(A)',ERR=10) REPLY
	CALL PERIOD_CASE (REPLY, .TRUE.) 
       	IF(REPLY .EQ. 'Y') THEN
          WRITE(*,*)' '
          STOP '** OK: Goodbye.'
	END IF

	RETURN
	END
