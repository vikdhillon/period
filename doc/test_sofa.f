
      program test_sofa

c simple program to test if this version of sofa has the new leap
c second on 30 June 2015 added correctly. The MJD entered below is for 
c 1 July 2015, so the result should be 36. If the result is 35, it is
c an old (pre-2015) version of sofa and shouldn't be used.
c link with: 
c gfortran test_sofa.f -o test_sofa f77/src/libsofa.a
c vik dhillon @sheffield 20-nov-2015

      implicit none
      integer j
      real*8 deltat, djm0, djm

      call IAU_DAT ( 2015, 7, 1, 0.5, deltat, j)
      call IAU_CAL2JD (2015, 7, 1, djm0, djm, j)
      write(*,*)'TAI-UTC (s)=',deltat
      write(*,*)'MJD = ',djm

      end
