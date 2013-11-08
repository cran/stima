      subroutine rs_mean(m,x,n)
      implicit none
!
! Use:
!    mean<-.Fortran("rs_mean",m=as.double(0),
!                   as.double(x),as.integer(length(x)))$m
!
! Parameters
!
      integer n
      real*8  x(n),m
!
      include 'rs_calls.h'
!
! Local variables
!
      integer i
!
! Code
!
      m=0.0d0
      do i=1,n
         m=m+x(i)
      end do
      m=m/dble(n)

      return
      end
