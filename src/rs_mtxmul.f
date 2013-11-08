      subroutine rs_mtxmul(c,x,y,l,m,n)
	implicit none
C
C     Parameters
C
	integer l,m,n
      double precision x(l,m),y(m,n),c(l,n)
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
	         c(i,j)=c(i,j)+x(i,k)*y(k,j)
	      end do
	   end do
	end do

	return
	end
