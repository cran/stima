      subroutine rs_boscat(res,dat,nodvec,m,n,trx,trx2,mm,minbucket,crit)
      implicit none
!
!     Wrapper function for R and S.
!     Use:
!        res<-.Fortran("rs_boscat",res=as.double(res),dat=as.matrix(dat),
!                      nv=as.integer(nv),m=as.integer(m),
!                      n=as.integer(n),
!                      trx=as.double(trx),trx2=as.double(trx2),
!                      mm=as.integer(mm),
!                      mb=as.integer(mb),critn=as.integer(critn))$res
!     Note:
!        All dimensions are assumed to be correct without check.
!
!     Parameters
!
      integer m,n,mm,minbucket,crit
      integer nodvec(m)
      real(8)  dat(m,n),res(2),trx(mm),trx2(m)
!
      include 'rs_calls.h'
!
!     Local variables
!
      double precision ,allocatable :: x(:,:)
      double precision ,allocatable :: z(:),fvec(:)

      integer i,j,nz,nfvec,nn,minpos,maxpos,ierr
      double precision y1rsq,y2rsq
      double precision trunk,mxfvec
!
!     Code
!
      res(1)=0.0D0
      res(2)=0.0D0

      nn=n+1
      allocate (x(m,nn),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (z(mm),stat=ierr)
      if (ierr.ne.0) goto 99999

      do j=1,n
         do i=1,m
            x(i,j)=dat(i,j)
         end do
      end do
      call rs_rsq(y1rsq,x,m,n)
      nz=mm
      do i=1,nz
            z(i)=trx(i)
      end do
      call rs_sort(z,nz)

      nfvec=nz-minbucket+2
      allocate (fvec(nfvec),stat=ierr)
      if (ierr.ne.0) goto 99999

      do j=minbucket-1,nfvec
         do i=1,m
            if ((trx2(i).le.z(j)).and.(nodvec(i).eq.1)) then
               trunk=1.0D0
            else
               trunk=0.0D0
            end if
            x(i,nn)=trunk
         end do
         call rs_rsq(y2rsq,x,m,nn)
         if (crit.eq.1) then
!           R2change
            fvec(j)=y2rsq-y1rsq
         else if (crit.eq.2) then
!           F-value
            fvec(j)=((y2rsq-y1rsq)*(m-nn))/(1-y2rsq)
         else if (crit.eq.0) then
!           f2
            fvec(j)=(y2rsq-y1rsq)/(1-y2rsq)
         else
            fvec(j)=0.0D0
         end if
      end do
      mxfvec=fvec(minbucket)
      do j=minbucket+1,nz-minbucket
         mxfvec=max(mxfvec,fvec(j))
      end do
      minpos=0
      do j=minbucket-1,nfvec
         if (mxfvec.eq.fvec(j))then
            maxpos=j
            if (minpos.eq.0) minpos=j
         end if
      end do
!     Correct for ties in the data
      if ((maxpos.gt.nz-minbucket).and.(minpos.gt.minbucket)) then
         mxfvec=fvec(minbucket)
         do j=minbucket,minpos-1
            mxfvec=max(mxfvec,fvec(j))
         end do
         do j=minbucket-1,nfvec
            if (mxfvec.eq.fvec(j)) maxpos=j
         end do
      end if
      if ((minpos.le.minbucket).and.(maxpos.gt.nz-minbucket)) then
         res(1)=0.0D0
         res(2)=0.0D0
      else
         res(1)=(z(maxpos)+z(maxpos+1))/2
         res(2)=mxfvec
      end if

99999 continue
      deallocate (fvec,stat=ierr)
      deallocate (z,stat=ierr)
      deallocate (x,stat=ierr)

      return
      end
