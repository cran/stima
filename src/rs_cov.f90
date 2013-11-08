      subroutine rs_cov(c,x,y,mx,my,n)
      implicit none
!
!     Wrapper function for R and S.
!     Use:
!        cov<-.Fortran("rs_cov",c=as.double(0),
!                      as.double(x),as.double(y),
!                      mean.f(x),mean.f(y),
!                      as.integer(length(x)))$c
!
!     Parameters
!
	   integer n
      real*8  x(n),y(n),mx,my,c
!
      include 'rs_calls.h'
!
!     Local variables
!
      integer i
!
!     Code
!
      c=0.0d0
      do i=1,n
	      c=c+(x(i)-mx)*(y(i)-my)
	   end do
      c=c/dble(n)

	   return
	   end
