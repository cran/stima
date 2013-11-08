      subroutine rs_splitc(fvec,predtree,nodemat2,index2, &
                           dataRTA,dataNUM,typ,npt,nnm2,mnm2, &
                           ni2,nRTA,mRTA,nNUM,mNUM,nTYP,mb,critn)
      implicit none
!
! Use:
!    fvec<-.Fortran("rs_splitc",fvec=as.double(fvec),
!                   as.integer(predtree),as.integer(nodemat2),
!                   as.double(index2),as.double(dataRTA),
!                   as.double(dataNUM),as.integer(typ),
!                   as.integer(length(predtree)),
!                   as.integer(dim(nodemat2)[1]),
!                   as.integer(dim(nodemat2)[2]),
!                   as.integer(length(index2)),
!                   as.integer(dim(dataRTA)[1]),
!                   as.integer(dim(dataRTA)[2]),
!                   as.integer(dim(dataNUM)[1]),
!                   as.integer(dim(dataNUM)[2]),
!                   as.integer(nTYP),as.integer(mb),
!                   as.integer(critn))$fvec
!
! Parameters
!
      integer npt                   ! length(predtree)
      integer nnm2,mnm2             ! dim(nodemat2)
      integer ni2                   ! length(index2)
      integer nRTA,mRTA             ! dim(dataRTA)
      integer nNUM,mNUM             ! dim(dataNUM)
      integer nTYP                  ! length(typ)
      integer mb                    ! minbucket
      integer critn                 ! numeric crit
      real*8  fvec(4)               ! return vector
      integer predtree(npt)         ! predtree
      integer nodemat2(nnm2,mnm2)   ! nodemat2
      real*8  index2(ni2)           ! index2
      integer typ(nTYP)             ! type
      real*8  dataRTA(nRTA,mRTA)    ! dataRTA
      real*8  dataNUM(nNUM,mNUM)    ! dataNUM
!
      include 'rs_calls.h'
!
! Local variables
!
      integer i,j
      logical first
      real*8  mxres
      real*8  res(2)
!
! Code
!
      first=.true.
      do j=1,mnm2
         do i=1,npt
            if (typ(predtree(i)).eq.1) then
               call rs_boscats(res,dataRTA,nodemat2(1,j), &
                               dataNUM(1,predtree(i)), &
                               nRTA,mRTA,mb,critn)
            else
               call rs_bos(res,dataRTA,nodemat2(1,j), &
                           dataNUM(1,predtree(i)), &
                           nRTA,mRTA,mb,critn)
            end if
            if (first) then
               first=.false.
               mxres=res(2)
            end if
            if (res(2).ge.mxres) then
               mxres=res(2)
               fvec(1)=predtree(i)
               fvec(2)=index2(j)
               fvec(3)=res(1)
               fvec(4)=res(2)
            end if
         end do
      end do

      return
      end
