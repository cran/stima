      subroutine rs_boscats(res,dat,nodvec,col,m,n,minbucket,crit)
      implicit none
!
!     Wrapper function for R and S.
!     Use:
!        res<-.Fortran("rs_boscat",res=as.double(res),dat=as.matrix(dat),
!                      nv=as.integer(nv),as.double(col),m=as.integer(m),
!                      n=as.integer(n),
!                      mb=as.integer(mb),critn=as.integer(critn))$res
!     Note:
!        All dimensions are assumed to be correct without check.
!
!     Parameters
!
      integer m,n,minbucket,crit
      integer nodvec(m)
      real(8)  dat(m,n),res(2),col(m)
!
      include 'rs_calls.h'
!
!     Local variables
!
      double precision ,allocatable :: xdat(:,:)
      double precision ,allocatable :: z(:),fvec(:)
      double precision ,allocatable :: x(:),y(:)
      double precision ,allocatable :: trx(:),trx2(:)
      double precision ,allocatable :: cat(:,:)
      double precision ,allocatable :: catmean(:)

      integer i,j,nz,nfvec,nn,minpos,maxpos,ierr
      double precision y1rsq,y2rsq
      double precision trunk,mxfvec

      integer mm,ncat
      integer nxy,catnr,catnr2
!
!     Code
!
      res(1)=0.0D0
      res(2)=0.0D0

      allocate (trx(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (trx2(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (cat(m,4),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (catmean(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (x(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (y(m),stat=ierr)
      if (ierr.ne.0) goto 99999
      ! init cat(i,j)
      do j=1,4
         do i=1,m
            cat(i,j)=0
         end do
      end do
      ! find the categories in col -> cat(,1) + sort
      do i=1,m
         x(i)=col(i)
      end do
      call rs_sort(x,m)
      catnr=1
      cat(catnr,1)=x(1)
      do i=2,m
         if (x(i).ne.x(i-1)) then
            catnr=catnr+1
            cat(catnr,1)=x(i)
         end if
      end do
      call rs_resid(y,dat,m,n)
      nxy=0
      do i=1,m
         if (nodvec(i).eq.1) then
            nxy=nxy+1
            x(nxy)=col(i)
            y(nxy)=y(i)
         end if
      end do
      ! determine mean per cat
      do i=1,nxy
         do j=1,catnr
            if (x(i).eq.cat(j,1)) then
               cat(j,2)=cat(j,2)+1
               cat(j,3)=cat(j,3)+y(i)
               exit
            end if
         end do
      end do
      ncat=0
      do i=1,catnr
         if (cat(i,2).ne.0) then
            ncat=ncat+1
            cat(i,4)=cat(i,3)/cat(i,2)
            catmean(ncat)=cat(i,4)
         end if
      end do
      call rs_sort(catmean,ncat)
      do i=1,catnr
         cat(i,1)=0
         do j=1,ncat
            if (catmean(j).eq.cat(i,4)) then
               cat(i,1)=j
               exit
            end if
         end do
      end do
      ! transform x sorted by the mean.
      do i=1,nxy
         trx(i)=cat(nint(x(i)),1)
      end do
      do i=1,m
         trx2(i)=cat(nint(col(i)),1)
      end do

      mm=nxy
      nn=n+1
      allocate (xdat(m,nn),stat=ierr)
      if (ierr.ne.0) goto 99999
      allocate (z(mm),stat=ierr)
      if (ierr.ne.0) goto 99999

      do j=1,n
         do i=1,m
            xdat(i,j)=dat(i,j)
         end do
      end do
      call rs_rsq(y1rsq,xdat,m,n)
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
            xdat(i,nn)=trunk
         end do
         call rs_rsq(y2rsq,xdat,m,nn)
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
!      goto 99999
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
      deallocate (xdat,stat=ierr)
      deallocate (x,stat=ierr)
      deallocate (y,stat=ierr)
      deallocate (trx,stat=ierr)
      deallocate (trx2,stat=ierr)
      deallocate (catmean,stat=ierr)
      deallocate (cat,stat=ierr)

      return
      end
