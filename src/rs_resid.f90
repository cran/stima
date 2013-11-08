      subroutine rs_resid(r,dat,m,n)
	   implicit none
!
!     Wrapper function for R and S.
!     Use:
!        rc<-.Fortran("rs_resid",r=as.double(0),x=as.matrix(x),
!                     as.integer(m),as.integer(n))$r
!
!     Parameters
!
      integer m,n
      real*8  dat(m,n),r(m)
!
      include 'rs_calls.h'
!
!     Local variables
!
      integer ierr
      integer i,j
      real*8  tiny
      logical qerr
      integer ,allocatable :: indx(:)
      double precision ,allocatable :: z(:)
      double precision ,allocatable :: x1(:,:),x2(:,:),zhat(:,:)
      double precision ,allocatable :: vv(:)
      double precision ,allocatable :: b(:,:),binv(:,:),x(:,:)
!
!     Code
!
      r=0.0
      allocate (indx(n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (x1(m,n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (x2(n,1),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (zhat(m,1),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (vv(n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (z(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (b(n,n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (binv(n,n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (x(m,n),stat=ierr)
      if (ierr.ne.0) goto 99999
      qerr=.false.
      tiny=1.0E-8
      do i=1,m
         z(i)=dat(i,1)
         x(i,1)=1.0
         do j=2,n
            x(i,j)=dat(i,j)
         end do
      end do
!     Calculate b=x'*x
      call rs_mtxmult(b,x,x,n,m,n)
!     Calculate binv=b^-1
      call rs_invers(b,n,n,indx,binv,vv,tiny,qerr)
!     Calculate x1=x*binv
      call rs_mtxmul(x1,x,binv,m,n,n)
!     Calculate x2=x'*z
      call rs_mtxmult(x2,x,z,n,m,1)
!     Calculate yhat=x*b
      call rs_mtxmul(zhat,x1,x2,m,n,1)
      do i=1,m
         r(i)=z(i)-zhat(i,1)
      end do

99999 continue
      deallocate (indx,stat=ierr)
      deallocate (vv,stat=ierr)
      deallocate (z,stat=ierr)
      deallocate (x1,stat=ierr)
      deallocate (x2,stat=ierr)
      deallocate (zhat,stat=ierr)
      deallocate (b,stat=ierr)
      deallocate (binv,stat=ierr)
      deallocate (x,stat=ierr)

      return
      end
