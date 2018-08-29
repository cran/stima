      subroutine rs_mtxmult(c,x,y,l,m,n)
      implicit none
C
C     Parameters
C
      integer l,m,n
      double precision x(m,l),y(m,n),c(l,n)
C
C     Local variables
C
      integer i,j,k
C
C     Code
C
      do j=1,n
         do i=1,l
         c(i,j)=0.0
            do k=1,m
            c(i,j)=c(i,j)+x(k,i)*y(k,j)
          end do
       end do
      end do

      return
      end

C   call rs_inprod(dataRES,dataRTA,105,8)


      subroutine rs_inprod(c,x,n,m)
      implicit none
C
C     Parameters
C
      integer m,n
      double precision x(n,m),c(m,m)
C
C     Local variables
C
      integer i,j,k
C
C     Code
C
      do j=1,m
         do i=1,m
         c(i,j)=0.0
            do k=1,n
            c(i,j)=c(i,j)+x(k,i)*x(k,j)
          end do
       end do
      end do

      return
      end
