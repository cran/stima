      subroutine rs_stdev(s,x,m,n)
      implicit none
!
!     Wrapper function for R and S.
!     Use:
!        sd<-.Fortran("rs_stdev",s=as.double(0),
!                     as.double(x),mean.f(x),as.integer(length(x)))$s
!
!     Parameters
!
      integer n
      real*8  x(n), m, s
!
      include 'rs_calls.h'
!
!     Local variables
!
      integer i
!
!     Code
!
      s=0.0d0
      do i=1,n
         s=s+(x(i)-m)**2
      end do
      if (s/dble(n).le.0.0) then
         s=0.0
      else
         s=sqrt(s/dble(n))
      end if

      return
      end
