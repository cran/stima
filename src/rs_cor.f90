      subroutine rs_cor(c,x,y,n)
      implicit none
!
!     Callable function from S-Plus.
!     Use:
!        corr<-.Fortran("rs_cor",c=as.double(0),
!                       as.double(x),as.double(y),
!                       as.integer(length(x)))$c
!     Note:
!        The length of x and y are assumed to be n without check.
!
!     Parameters
!
      integer n
      double precision x(n),y(n),c
!
!      include 'rs_calls.h'
!
!     Local variables
!
      double precision mx,my,sdx,sdy
!
!     Code
!
      sdx=0.0
      sdy=0.0
!      call rsc_mean(mx,x,n)
!      call rsc_mean(my,y,n)
!      call rsc_stdev(sdx,x,mx,n)
!      call rsc_stdev(sdy,y,my,n)
      call rs_mean(mx,x,n)
      call rs_mean(my,y,n)
      call rs_stdev(sdx,x,mx,n)
      call rs_stdev(sdy,y,my,n)
      call rs_cov(c,x,y,mx,my,n)
      if ((sdx*sdy).eq.0.0) then
         c=1.0
      else
         c=c/(sdx*sdy)
      end if

      return
      end
