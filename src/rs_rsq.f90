      subroutine rs_rsq(r,dat,m,n)
      implicit none
!
!     Wrapper function for R and S.
!     Use:
!        r<-.Fortran("rs_rsq",rsq=as.double(0),x=as.matrix(x),
!                    as.integer(m),as.integer(n))$rsq
!
!     Parameters
!
       integer m,n
      real(8)  dat(m,n),r
!
!      include 'rs_calls.h'
!
!     Local variables
!
      integer ierr
      integer i,j
      double precision tiny
      logical qerr
      integer ,allocatable :: indx(:)
      double precision ,allocatable :: y(:)
      double precision ,allocatable :: yhat(:),yh(:),vv(:)
      double precision ,allocatable :: b(:,:),binv(:,:),x(:,:)
!
!     Code
!
      r=0.0
      allocate (indx(n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (yh(n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (yhat(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (vv(n),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (y(m),stat=ierr)
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
         y(i)=dat(i,1)
         x(i,1)=1.0
          do j=2,n
             x(i,j)=dat(i,j)
          end do
      end do
!     Calculate b=x'*x
      call rs_mtxmult(b,x,x,n,m,n)
!     Calculate binv=b^-1
      call rs_invers(b,n,n,indx,binv,vv,tiny,qerr)
!     Calculate yh=x'*y
      call rs_mtxmult(yh,x,y,n,m,1)
!     Calculate b=binv*yh
      call rs_mtxmul(b,binv,yh,n,n,1)
!     Calculate yhat=x*b
      call rs_mtxmul(yhat,x,b,m,n,1)
!     Calculate correlation between y and yhat.
      call rs_cor(r,y,yhat,m)
      r=r*r

99999 continue
      deallocate (indx,stat=ierr)
      deallocate (vv,stat=ierr)
      deallocate (y,stat=ierr)
      deallocate (yh,stat=ierr)
      deallocate (yhat,stat=ierr)
      deallocate (b,stat=ierr)
      deallocate (binv,stat=ierr)
      deallocate (x,stat=ierr)

      return
      end
