      subroutine rs_sort(z,n)
      implicit none
!
!     Simple sort routine with simple exchange.
!
!     Parameters
!
      integer n
      real(8)  z(n)
!
      include 'rs_calls.h'
!
!     Local variables
!
      integer i
      logical notdone
      double precision t
!
!     Code
!
      notdone=n.ne.1
      do while(notdone)
         notdone=.false.
         do i=1,n-1
            if(z(i).gt.z(i+1))then
               t=z(i)
               z(i)=z(i+1)
               z(i+1)=t
               notdone=.true.
            end if
         end do
      end do

      return
      end