
         program test_sla

c simple program to test if this version of slalib has the new leap
c second on 30 June 2015 added correctly. The MJD entered below is for 
c 1 July 2015, so the result should be 36. If the result is 35, it is
c an old (pre-2015) version of slalib and shouldn't be used.
c link with:
c gfortran -O -fimplicit-none -fno-second-underscore -fno-backslash test_sla.f -o test_sla lnx/lib/libsla.a
c vik dhillon @sheffield 20-nov-2015

      implicit none
      integer iy, im, id, j
      real*8 sla_dat, fd, mjd

      mjd = 57204.d0
      write(*,*)'TAI-UTC (s)=',sla_DAT(mjd)
      call sla_DJCL (mjd, iy, im, id, fd, j)
      write(*,*)'YYYY/MM/DD = ',iy,im,id
      
      end
 
