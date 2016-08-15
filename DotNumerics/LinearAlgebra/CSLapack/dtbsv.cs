#region Translated by Jose Antonio De Santiago-Castillo.

//Translated by Jose Antonio De Santiago-Castillo.
//E-mail:JAntonioDeSantiago@gmail.com
//Website: www.DotNumerics.com
//
//Fortran to C# Translation.
//Translated by:
//F2CSharp Version 0.72 (Dicember 7, 2009)
//Code Optimizations: , assignment operator, for-loop: array indexes
//
#endregion

using System;
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// Purpose
    /// =======
    /// 
    /// DTBSV  solves one of the systems of equations
    /// 
    /// A*x = b,   or   A'*x = b,
    /// 
    /// where b and x are n element vectors and A is an n by n unit, or
    /// non-unit, upper or lower triangular band matrix, with ( k + 1 )
    /// diagonals.
    /// 
    /// No test for singularity or near-singularity is included in this
    /// routine. Such tests must be performed before calling this routine.
    /// 
    ///</summary>
    public class DTBSV
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DTBSV(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTBSV()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTBSV  solves one of the systems of equations
        /// 
        /// A*x = b,   or   A'*x = b,
        /// 
        /// where b and x are n element vectors and A is an n by n unit, or
        /// non-unit, upper or lower triangular band matrix, with ( k + 1 )
        /// diagonals.
        /// 
        /// No test for singularity or near-singularity is included in this
        /// routine. Such tests must be performed before calling this routine.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// - CHARACTER*1.
        /// On entry, UPLO specifies whether the matrix is an upper or
        /// lower triangular matrix as follows:
        /// 
        /// UPLO = 'U' or 'u'   A is an upper triangular matrix.
        /// 
        /// UPLO = 'L' or 'l'   A is a lower triangular matrix.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="TRANS">
        /// - CHARACTER*1.
        /// On entry, TRANS specifies the equations to be solved as
        /// follows:
        /// 
        /// TRANS = 'N' or 'n'   A*x = b.
        /// 
        /// TRANS = 'T' or 't'   A'*x = b.
        /// 
        /// TRANS = 'C' or 'c'   A'*x = b.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="DIAG">
        /// - CHARACTER*1.
        /// On entry, DIAG specifies whether or not A is unit
        /// triangular as follows:
        /// 
        /// DIAG = 'U' or 'u'   A is assumed to be unit triangular.
        /// 
        /// DIAG = 'N' or 'n'   A is not assumed to be unit
        /// triangular.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="K">
        /// - INTEGER.
        /// On entry with UPLO = 'U' or 'u', K specifies the number of
        /// super-diagonals of the matrix A.
        /// On entry with UPLO = 'L' or 'l', K specifies the number of
        /// sub-diagonals of the matrix A.
        /// K must satisfy  0 .le. K.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
        /// Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
        /// by n part of the array A must contain the upper triangular
        /// band part of the matrix of coefficients, supplied column by
        /// column, with the leading diagonal of the matrix in row
        /// ( k + 1 ) of the array, the first super-diagonal starting at
        /// position 2 in row k, and so on. The top left k by k triangle
        /// of the array A is not referenced.
        /// The following program segment will transfer an upper
        /// triangular band matrix from conventional full matrix storage
        /// to band storage:
        /// 
        /// DO 20, J = 1, N
        /// M = K + 1 - J
        /// DO 10, I = MAX( 1, J - K ), J
        /// A( M + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
        /// by n part of the array A must contain the lower triangular
        /// band part of the matrix of coefficients, supplied column by
        /// column, with the leading diagonal of the matrix in row 1 of
        /// the array, the first sub-diagonal starting at position 1 in
        /// row 2, and so on. The bottom right k by k triangle of the
        /// array A is not referenced.
        /// The following program segment will transfer a lower
        /// triangular band matrix from conventional full matrix storage
        /// to band storage:
        /// 
        /// DO 20, J = 1, N
        /// M = 1 - J
        /// DO 10, I = J, MIN( N, J + K )
        /// A( M + I, J ) = matrix( I, J )
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// Note that when DIAG = 'U' or 'u' the elements of the array A
        /// corresponding to the diagonal elements of the matrix are not
        /// referenced, but are assumed to be unity.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// ( k + 1 ).
        /// Unchanged on exit.
        ///</param>
        /// <param name="X">
        /// - DOUBLE PRECISION array of dimension at least
        /// ( 1 + ( n - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the n
        /// element right-hand side vector b. On exit, X is overwritten
        /// with the solution vector x.
        ///</param>
        /// <param name="INCX">
        /// - INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string UPLO, string TRANS, string DIAG, int N, int K, double[] A, int offset_a
                         , int LDA, ref double[] X, int offset_x, int INCX)
        {

            #region Variables
            
            double TEMP = 0; int I = 0; int INFO = 0; int IX = 0; int J = 0; int JX = 0; int KPLUS1 = 0; int KX = 0; int L = 0; 
            bool NOUNIT = false;

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_x = -1 + offset_x; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DTBSV  solves one of the systems of equations
            // *
            // *     A*x = b,   or   A'*x = b,
            // *
            // *  where b and x are n element vectors and A is an n by n unit, or
            // *  non-unit, upper or lower triangular band matrix, with ( k + 1 )
            // *  diagonals.
            // *
            // *  No test for singularity or near-singularity is included in this
            // *  routine. Such tests must be performed before calling this routine.
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  UPLO   - CHARACTER*1.
            // *           On entry, UPLO specifies whether the matrix is an upper or
            // *           lower triangular matrix as follows:
            // *
            // *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
            // *
            // *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
            // *
            // *           Unchanged on exit.
            // *
            // *  TRANS  - CHARACTER*1.
            // *           On entry, TRANS specifies the equations to be solved as
            // *           follows:
            // *
            // *              TRANS = 'N' or 'n'   A*x = b.
            // *
            // *              TRANS = 'T' or 't'   A'*x = b.
            // *
            // *              TRANS = 'C' or 'c'   A'*x = b.
            // *
            // *           Unchanged on exit.
            // *
            // *  DIAG   - CHARACTER*1.
            // *           On entry, DIAG specifies whether or not A is unit
            // *           triangular as follows:
            // *
            // *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
            // *
            // *              DIAG = 'N' or 'n'   A is not assumed to be unit
            // *                                  triangular.
            // *
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry, N specifies the order of the matrix A.
            // *           N must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  K      - INTEGER.
            // *           On entry with UPLO = 'U' or 'u', K specifies the number of
            // *           super-diagonals of the matrix A.
            // *           On entry with UPLO = 'L' or 'l', K specifies the number of
            // *           sub-diagonals of the matrix A.
            // *           K must satisfy  0 .le. K.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            // *           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
            // *           by n part of the array A must contain the upper triangular
            // *           band part of the matrix of coefficients, supplied column by
            // *           column, with the leading diagonal of the matrix in row
            // *           ( k + 1 ) of the array, the first super-diagonal starting at
            // *           position 2 in row k, and so on. The top left k by k triangle
            // *           of the array A is not referenced.
            // *           The following program segment will transfer an upper
            // *           triangular band matrix from conventional full matrix storage
            // *           to band storage:
            // *
            // *                 DO 20, J = 1, N
            // *                    M = K + 1 - J
            // *                    DO 10, I = MAX( 1, J - K ), J
            // *                       A( M + I, J ) = matrix( I, J )
            // *              10    CONTINUE
            // *              20 CONTINUE
            // *
            // *           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
            // *           by n part of the array A must contain the lower triangular
            // *           band part of the matrix of coefficients, supplied column by
            // *           column, with the leading diagonal of the matrix in row 1 of
            // *           the array, the first sub-diagonal starting at position 1 in
            // *           row 2, and so on. The bottom right k by k triangle of the
            // *           array A is not referenced.
            // *           The following program segment will transfer a lower
            // *           triangular band matrix from conventional full matrix storage
            // *           to band storage:
            // *
            // *                 DO 20, J = 1, N
            // *                    M = 1 - J
            // *                    DO 10, I = J, MIN( N, J + K )
            // *                       A( M + I, J ) = matrix( I, J )
            // *              10    CONTINUE
            // *              20 CONTINUE
            // *
            // *           Note that when DIAG = 'U' or 'u' the elements of the array A
            // *           corresponding to the diagonal elements of the matrix are not
            // *           referenced, but are assumed to be unity.
            // *           Unchanged on exit.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program. LDA must be at least
            // *           ( k + 1 ).
            // *           Unchanged on exit.
            // *
            // *  X      - DOUBLE PRECISION array of dimension at least
            // *           ( 1 + ( n - 1 )*abs( INCX ) ).
            // *           Before entry, the incremented array X must contain the n
            // *           element right-hand side vector b. On exit, X is overwritten
            // *           with the solution vector x.
            // *
            // *  INCX   - INTEGER.
            // *           On entry, INCX specifies the increment for the elements of
            // *           X. INCX must not be zero.
            // *           Unchanged on exit.
            // *
            // *
            // *  Level 2 Blas routine.
            // *
            // *  -- Written on 22-October-1986.
            // *     Jack Dongarra, Argonne National Lab.
            // *     Jeremy Du Croz, Nag Central Office.
            // *     Sven Hammarling, Nag Central Office.
            // *     Richard Hanson, Sandia National Labs.
            // *
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC MAX,MIN;
            // *     ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (!this._lsame.Run(UPLO, "U") && !this._lsame.Run(UPLO, "L"))
            {
                INFO = 1;
            }
            else
            {
                if (!this._lsame.Run(TRANS, "N") && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
                {
                    INFO = 2;
                }
                else
                {
                    if (!this._lsame.Run(DIAG, "U") && !this._lsame.Run(DIAG, "N"))
                    {
                        INFO = 3;
                    }
                    else
                    {
                        if (N < 0)
                        {
                            INFO = 4;
                        }
                        else
                        {
                            if (K < 0)
                            {
                                INFO = 5;
                            }
                            else
                            {
                                if (LDA < (K + 1))
                                {
                                    INFO = 7;
                                }
                                else
                                {
                                    if (INCX == 0)
                                    {
                                        INFO = 9;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTBSV ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if (N == 0) return;
            // *
            NOUNIT = this._lsame.Run(DIAG, "N");
            // *
            // *     Set up the start point in X if the increment is not unity. This
            // *     will be  ( N - 1 )*INCX  too small for descending loops.
            // *
            if (INCX <= 0)
            {
                KX = 1 - (N - 1) * INCX;
            }
            else
            {
                if (INCX != 1)
                {
                    KX = 1;
                }
            }
            // *
            // *     Start the operations. In this version the elements of A are
            // *     accessed by sequentially with one pass through A.
            // *
            if (this._lsame.Run(TRANS, "N"))
            {
                // *
                // *        Form  x := inv( A )*x.
                // *
                if (this._lsame.Run(UPLO, "U"))
                {
                    KPLUS1 = K + 1;
                    if (INCX == 1)
                    {
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            if (X[J + o_x] != ZERO)
                            {
                                L = KPLUS1 - J;
                                if (NOUNIT) X[J + o_x] /= A[KPLUS1+J * LDA + o_a];
                                TEMP = X[J + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J - 1; I >= Math.Max(1, J - K); I +=  - 1)
                                {
                                    X[I + o_x] +=  - TEMP * A[L + I + A_J];
                                }
                            }
                        }
                    }
                    else
                    {
                        KX += (N - 1) * INCX;
                        JX = KX;
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            KX -= INCX;
                            if (X[JX + o_x] != ZERO)
                            {
                                IX = KX;
                                L = KPLUS1 - J;
                                if (NOUNIT) X[JX + o_x] /= A[KPLUS1+J * LDA + o_a];
                                TEMP = X[JX + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J - 1; I >= Math.Max(1, J - K); I +=  - 1)
                                {
                                    X[IX + o_x] +=  - TEMP * A[L + I + A_J];
                                    IX -= INCX;
                                }
                            }
                            JX -= INCX;
                        }
                    }
                }
                else
                {
                    if (INCX == 1)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            if (X[J + o_x] != ZERO)
                            {
                                L = 1 - J;
                                if (NOUNIT) X[J + o_x] /= A[1+J * LDA + o_a];
                                TEMP = X[J + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J + 1; I <= Math.Min(N, J + K); I++)
                                {
                                    X[I + o_x] +=  - TEMP * A[L + I + A_J];
                                }
                            }
                        }
                    }
                    else
                    {
                        JX = KX;
                        for (J = 1; J <= N; J++)
                        {
                            KX += INCX;
                            if (X[JX + o_x] != ZERO)
                            {
                                IX = KX;
                                L = 1 - J;
                                if (NOUNIT) X[JX + o_x] /= A[1+J * LDA + o_a];
                                TEMP = X[JX + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J + 1; I <= Math.Min(N, J + K); I++)
                                {
                                    X[IX + o_x] +=  - TEMP * A[L + I + A_J];
                                    IX += INCX;
                                }
                            }
                            JX += INCX;
                        }
                    }
                }
            }
            else
            {
                // *
                // *        Form  x := inv( A')*x.
                // *
                if (this._lsame.Run(UPLO, "U"))
                {
                    KPLUS1 = K + 1;
                    if (INCX == 1)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            TEMP = X[J + o_x];
                            L = KPLUS1 - J;
                            A_J = J * LDA + o_a;
                            for (I = Math.Max(1, J - K); I <= J - 1; I++)
                            {
                                TEMP +=  - A[L + I + A_J] * X[I + o_x];
                            }
                            if (NOUNIT) TEMP /= A[KPLUS1+J * LDA + o_a];
                            X[J + o_x] = TEMP;
                        }
                    }
                    else
                    {
                        JX = KX;
                        for (J = 1; J <= N; J++)
                        {
                            TEMP = X[JX + o_x];
                            IX = KX;
                            L = KPLUS1 - J;
                            A_J = J * LDA + o_a;
                            for (I = Math.Max(1, J - K); I <= J - 1; I++)
                            {
                                TEMP +=  - A[L + I + A_J] * X[IX + o_x];
                                IX += INCX;
                            }
                            if (NOUNIT) TEMP /= A[KPLUS1+J * LDA + o_a];
                            X[JX + o_x] = TEMP;
                            JX += INCX;
                            if (J > K) KX += INCX;
                        }
                    }
                }
                else
                {
                    if (INCX == 1)
                    {
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            TEMP = X[J + o_x];
                            L = 1 - J;
                            A_J = J * LDA + o_a;
                            for (I = Math.Min(N, J + K); I >= J + 1; I +=  - 1)
                            {
                                TEMP +=  - A[L + I + A_J] * X[I + o_x];
                            }
                            if (NOUNIT) TEMP /= A[1+J * LDA + o_a];
                            X[J + o_x] = TEMP;
                        }
                    }
                    else
                    {
                        KX += (N - 1) * INCX;
                        JX = KX;
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            TEMP = X[JX + o_x];
                            IX = KX;
                            L = 1 - J;
                            A_J = J * LDA + o_a;
                            for (I = Math.Min(N, J + K); I >= J + 1; I +=  - 1)
                            {
                                TEMP +=  - A[L + I + A_J] * X[IX + o_x];
                                IX -= INCX;
                            }
                            if (NOUNIT) TEMP /= A[1+J * LDA + o_a];
                            X[JX + o_x] = TEMP;
                            JX -= INCX;
                            if ((N - J) >= K) KX -= INCX;
                        }
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DTBSV .
            // *

            #endregion

        }
    }
}
