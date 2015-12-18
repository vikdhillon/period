
	SUBROUTINE PERIOD_INTRO

C=============================================================================
C Routine to show introductory message for PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 13-June-1992.
C=============================================================================

	IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_INTRO declarations.
C-----------------------------------------------------------------------------

	WRITE(*,*)' '
	WRITE(*,*)'|**************************************************|'
	WRITE(*,*)'|**| PERIOD  :>  A time-series analysis package |**|'
	WRITE(*,*)'|**| Version :>  10.0 (native) for ULTRACAM     |**|'
	WRITE(*,*)'|**| Date    :>  04-July-2014                   |**|'
	WRITE(*,*)'|**************************************************|'
	WRITE(*,*)'|**|'
	WRITE(*,*)'|**| Please report bugs, problems or suggestions to:'
	WRITE(*,*)'|**|'
	WRITE(*,*)'|**| Author  :>  Vik Dhillon'
	WRITE(*,*)'|**| Address :>  University of Sheffield'
	WRITE(*,*)'|**| E-mail  :>  vik.dhillon@sheffield.ac.uk'
	WRITE(*,*)'|**| '
	WRITE(*,*)'|**| Type ''?'' for a list of options.'
	WRITE(*,*)' '

	RETURN
	END
