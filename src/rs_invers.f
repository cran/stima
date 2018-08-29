      SUBROUTINE rs_invers(A,N,NP,INDX,Y,VV,TINY,QERR)
      IMPLICIT none
************************************************************************
*                                                                      *
* Function     : If A(N,N) is its LU decomposition then this routine   *
*                solves the linear equation A X = B. B is replaced by  *
*                X. Is efficient to use for matrix inversion.          *
*                NP is physical dim of A.                              *
* Version      : 1.00                                                  *
* Author       : Numerical Recipes                                     *
* Date         : June 1991                                             *
* Typed by     : Patrick Groenen                                       *
* Calls        : LUDCMP,LUBKSB                                         *
* Note         : All arrays in DOUBLE PRECISION                        *
*                                                                      *
************************************************************************

C-----------------------------------------------------------------------
C Parameter block.
C
      INTEGER          N,NP
      INTEGER          INDX(N)
      LOGICAL          QERR
      DOUBLE PRECISION TINY
      DOUBLE PRECISION A(NP,NP),Y(NP,NP),VV(N)
C-----------------------------------------------------------------------
C Local variables.
C
      DOUBLE PRECISION D
      INTEGER          I,J
C-----------------------------------------------------------------------
C Program start.
C
      DO 12 I=1,N
         DO 11 J=1,N
            Y(I,J) = 0.0
   11    CONTINUE
         Y(I,I) = 1.0
   12 CONTINUE   
      CALL LUDCMP(A,N,NP,INDX,D,VV,TINY,QERR)
      DO 13 J=1,N
         CALL LUBKSB(A,N,NP,INDX,Y(1,J))
   13 CONTINUE
   
      RETURN
      END

C
      SUBROUTINE LUDCMP(A,N,NP,INDX,D,VV,TINY,QERR)
      IMPLICIT none
************************************************************************
*                                                                      *
* Function     : replaces A(N,N) by its LU decomposition of a rowwise  *
*                permutation given in INDX (by partial pivoting).      *
*                D is +1 or -1 depending on number of row interchanges *
*                was even or odd. This routine is used with LUBKSB to  *
*                solve linear equations or to invert a matrix.         *
*                TINY is a small real (eg 1.0E-20) and VV stores       *
*                implicit scaling per row. NP is physical dim of A.    *
* Version      : 1.10                                                  *
* Author       : Numerical Recipes                                     *
* Date         : June 1991                                             *
* Typed by     : Patrick Groenen                                       *
* Calls        : None                                                  *
* Note         : All arrays in DOUBLE PRECISION                        *
*                                                                      *
************************************************************************

C-----------------------------------------------------------------------
C Parameter block.
C
      INTEGER          N,NP
      INTEGER          INDX(N)
      LOGICAL          QERR
      DOUBLE PRECISION D,TINY
      DOUBLE PRECISION A(NP,NP),VV(N)
C-----------------------------------------------------------------------
C Local variables.
C
      INTEGER          I,IMAX,J,K
      DOUBLE PRECISION AAMAX,DUM,SUM
C-----------------------------------------------------------------------
C Program start.
C

      QERR = .FALSE.

C     * No row interchanges yet. *

      D = 1.0

C     * Loop over rows to get implicit scaling information. *

      DO 12 I=1,N
        AAMAX = 0.0
        DO 11 J=1,N
          IF (ABS(A(I,J)) .GT. AAMAX) AAMAX = ABS(A(I,J))
   11   CONTINUE

C       * Check for singularity. *

        IF (AAMAX .EQ. 0) THEN
          QERR = .TRUE.
          RETURN
        END IF
        VV(I) = 1.0/AAMAX
   12 CONTINUE

C     * Loop over columns of Crouts method. *

      DO 19 J=1,N
        DO 14 I=1,J-1
          SUM = A(I,J)
          DO 13 K=1,I-1
            SUM = SUM-A(I,K)*A(K,J)
   13     CONTINUE
          A(I,J) = SUM
   14   CONTINUE

C       * Initialize search for largest pivot element. *

        AAMAX = 0.0
        DO 16 I=J,N
          SUM = A(I,J)
          DO 15 K=1,J-1
            SUM = SUM-A(I,K)*A(K,J)
   15     CONTINUE
          A(I,J) = SUM

C       * Figure merit for the pivot. *

          DUM    = VV(I)*ABS(SUM)
          IF (DUM .GE. AAMAX) THEN
            IMAX  = I
            AAMAX = DUM
          END IF
   16   CONTINUE

C       * Do we need to interchange the rows. *

        IF (J .NE. IMAX) THEN
          DO 17 K=1,N
            DUM       = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K)    = DUM
   17     CONTINUE
          D = -D
          VV(IMAX) = VV(J)
        END IF
        INDX(J) = IMAX
        IF (A(J,J) .EQ. 0) A(J,J) = TINY

C       * Divide by the pivot element. *

        IF (J .NE. N) THEN
          DUM = 1.0/A(J,J)
          DO 18 I=J+1,N
            A(I,J) = A(I,J)*DUM
   18     CONTINUE
        END IF
   19 CONTINUE

      RETURN
      END

C
      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      IMPLICIT none
************************************************************************
*                                                                      *
* Function     : If A(N,N) is its LU decomposition then this routine   *
*                solves the linear equation A x = b. B is replaced by  *
*                x. Is efficient to use for matrix inversion.          *
*                This routine can be used after LUDCMP. NP is physical *
*                dim of A.                                             *
* Version      : 1.10                                                  *
* Author       : Numerical Recipes                                     *
* Date         : June 1991                                             *
* Typed by     : Patrick Groenen                                       *
* Calls        : None                                                  *
* Note         : All arrays in DOUBLE PRECISION                        *
*                                                                      *
************************************************************************

C-----------------------------------------------------------------------
C Parameter block.
C
      INTEGER          N,NP
      INTEGER          INDX(N)
      DOUBLE PRECISION A(NP,NP),B(N)
C-----------------------------------------------------------------------
C Local variables.
C
      INTEGER          I,II,J,LL
      DOUBLE PRECISION SUM
C-----------------------------------------------------------------------
C Program start.
C
C     * No row interchanges yet. *

      II = 0

C     * Loop over rows to get implicit scaling information. *

      DO 12 I=1,N
        LL    = INDX(I)
        SUM   = B(LL)
        B(LL) = B(I)
        IF (II .NE. 0) THEN
          DO 11 J=II,I-1
            SUM = SUM - A(I,J)*B(J)
   11     CONTINUE
        ELSE IF (SUM .NE. 0.0) THEN
          II = I
        END IF
        B(I) = SUM
   12 CONTINUE
      DO 14 I=N,1,-1
        SUM = B(I)
        IF (I .LT. N) THEN
          DO 13 J=I+1,N
            SUM = SUM - A(I,J)*B(J)
   13     CONTINUE
        END IF
        B(I) = SUM/A(I,I)
   14 CONTINUE

      RETURN
      END
