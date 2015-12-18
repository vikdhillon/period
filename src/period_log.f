
	SUBROUTINE PERIOD_LOG (LOGFILE, LOG, LOGUNIT)

C=============================================================================
C Routine to open and close a log file for PERIOD.
C
C Written by Vikram Singh Dhillon @LPO 27-January-1992.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_LOG declarations.
C-----------------------------------------------------------------------------

	INTEGER LOGUNIT, I, NUMENT
	LOGICAL LOG, EXIST
	CHARACTER*72 LOGFILE, JUNK
	CHARACTER*1 REPLY
	DATA NUMENT /1000000000/
	DATA EXIST, JUNK /.FALSE., ' '/


	IF (LOG) THEN
		IF (EXIST) THEN
			WRITE(*,*)ACHAR(7)		
			WRITE(*,*)'** ERROR: Log file has already been
     + opened.'
			GOTO 99
		ELSE
			WRITE(*,*)' '
10			WRITE(*,'(A,$)')'Enter name of log file : '
			READ(*,'(A)',ERR=10)LOGFILE
15			WRITE(*,'(A,$)')'[N]ew or [O]ld file ? [N] : '
			READ(*,'(A)',ERR=15)REPLY
			CALL PERIOD_CASE (REPLY, .TRUE.)
			IF (REPLY .EQ. 'O') THEN
        		OPEN(UNIT=LOGUNIT,FILE=LOGFILE,STATUS='OLD',ERR=10)
			EXIST = .TRUE.
			DO I = 1, NUMENT
				READ(LOGUNIT,'(A)',END=20)JUNK
			END DO
20			WRITE(*,*)' '
			WRITE(*,*)'** OK: Opened old log file = '
     + ,LOGFILE(1:43)
			WRITE(*,*)'** OK: Number of lines skipped
     + over = ',I-1
			ELSE
		        OPEN(UNIT=LOGUNIT,FILE=LOGFILE,STATUS='NEW',ERR=10)	
			EXIST = .TRUE.
			WRITE(*,*)' '
			WRITE(*,*)'** OK: Opened new log file = '
     + ,LOGFILE(1:43)
			END IF
		END IF
	ELSE
			IF (EXIST) THEN
			CLOSE (UNIT=LOGUNIT)
			WRITE(*,*)' '
			WRITE(*,*)'** OK: Log file has been closed = '
     + ,LOGFILE(1:39)
			EXIST = .FALSE.
			ELSE	
			WRITE(*,*)ACHAR(7)		
			WRITE(*,*)'** ERROR: No log file has been opened.'
			GOTO 99
			END IF
	END IF
	
99	RETURN
	END
