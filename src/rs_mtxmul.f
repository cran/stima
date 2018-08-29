      subroutine rs_mtxmul(c,x,y,l,m,n)
      IMPLICIT none
C
C     Parameters
C
      INTEGER l,m,n
      DOUBLE PRECISION x(l,m),y(m,n),c(l,n)
C
C     Local variables
C
      INTEGER i,j,k
C
C     Code
C
      DO j=1,n
       DO i=1,l
          c(i,j)=0.0
           DO k=1,m
             c(i,j)=c(i,j)+x(i,k)*y(k,j)
           END DO
       END DO
      END DO

      RETURN
      END
