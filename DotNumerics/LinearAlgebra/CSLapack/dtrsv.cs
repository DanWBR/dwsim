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
    /// DTRSV  solves one of the systems of equations
    /// 
    /// A*x = b,   or   A'*x = b,
    /// 
    /// where b and x are n element vectors and A is an n by n unit, or
    /// non-unit, upper or lower triangular matrix.
    /// 
    /// No test for singularity or near-singularity is included in this
    /// routine. Such tests must be performed before calling this routine.
    /// 
    ///</summary>
    public class DTRSV
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DTRSV(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTRSV()
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
        /// DTRSV  solves one of the systems of equations
        /// 
        /// A*x = b,   or   A'*x = b,
        /// 
        /// where b and x are n element vectors and A is an n by n unit, or
        /// non-unit, upper or lower triangular matrix.
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
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
        /// Before entry with  UPLO = 'U' or 'u', the leading n by n
        /// upper triangular part of the array A must contain the upper
        /// triangular matrix and the strictly lower triangular part of
        /// A is not referenced.
        /// Before entry with UPLO = 'L' or 'l', the leading n by n
        /// lower triangular part of the array A must contain the lower
        /// triangular matrix and the strictly upper triangular part of
        /// A is not referenced.
        /// Note that when  DIAG = 'U' or 'u', the diagonal elements of
        /// A are not referenced either, but are assumed to be unity.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, n ).
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
        public void Run(string UPLO, string TRANS, string DIAG, int N, double[] A, int offset_a, int LDA
                         , ref double[] X, int offset_x, int INCX)
        {

            #region Variables
            
            double TEMP = 0; int I = 0; int INFO = 0; int IX = 0; int J = 0; int JX = 0; int KX = 0; bool NOUNIT = false; 

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
            // *  DTRSV  solves one of the systems of equations
            // *
            // *     A*x = b,   or   A'*x = b,
            // *
            // *  where b and x are n element vectors and A is an n by n unit, or
            // *  non-unit, upper or lower triangular matrix.
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
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            // *           Before entry with  UPLO = 'U' or 'u', the leading n by n
            // *           upper triangular part of the array A must contain the upper
            // *           triangular matrix and the strictly lower triangular part of
            // *           A is not referenced.
            // *           Before entry with UPLO = 'L' or 'l', the leading n by n
            // *           lower triangular part of the array A must contain the lower
            // *           triangular matrix and the strictly upper triangular part of
            // *           A is not referenced.
            // *           Note that when  DIAG = 'U' or 'u', the diagonal elements of
            // *           A are not referenced either, but are assumed to be unity.
            // *           Unchanged on exit.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program. LDA must be at least
            // *           max( 1, n ).
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
            //      INTRINSIC MAX;
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
                            if (LDA < Math.Max(1, N))
                            {
                                INFO = 6;
                            }
                            else
                            {
                                if (INCX == 0)
                                {
                                    INFO = 8;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTRSV ", INFO);
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
            // *     accessed sequentially with one pass through A.
            // *
            if (this._lsame.Run(TRANS, "N"))
            {
                // *
                // *        Form  x := inv( A )*x.
                // *
                if (this._lsame.Run(UPLO, "U"))
                {
                    if (INCX == 1)
                    {
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            if (X[J + o_x] != ZERO)
                            {
                                if (NOUNIT) X[J + o_x] /= A[J+J * LDA + o_a];
                                TEMP = X[J + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J - 1; I >= 1; I +=  - 1)
                                {
                                    X[I + o_x] +=  - TEMP * A[I + A_J];
                                }
                            }
                        }
                    }
                    else
                    {
                        JX = KX + (N - 1) * INCX;
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            if (X[JX + o_x] != ZERO)
                            {
                                if (NOUNIT) X[JX + o_x] /= A[J+J * LDA + o_a];
                                TEMP = X[JX + o_x];
                                IX = JX;
                                A_J = J * LDA + o_a;
                                for (I = J - 1; I >= 1; I +=  - 1)
                                {
                                    IX -= INCX;
                                    X[IX + o_x] +=  - TEMP * A[I + A_J];
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
                                if (NOUNIT) X[J + o_x] /= A[J+J * LDA + o_a];
                                TEMP = X[J + o_x];
                                A_J = J * LDA + o_a;
                                for (I = J + 1; I <= N; I++)
                                {
                                    X[I + o_x] +=  - TEMP * A[I + A_J];
                                }
                            }
                        }
                    }
                    else
                    {
                        JX = KX;
                        for (J = 1; J <= N; J++)
                        {
                            if (X[JX + o_x] != ZERO)
                            {
                                if (NOUNIT) X[JX + o_x] /= A[J+J * LDA + o_a];
                                TEMP = X[JX + o_x];
                                IX = JX;
                                A_J = J * LDA + o_a;
                                for (I = J + 1; I <= N; I++)
                                {
                                    IX += INCX;
                                    X[IX + o_x] +=  - TEMP * A[I + A_J];
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
                // *        Form  x := inv( A' )*x.
                // *
                if (this._lsame.Run(UPLO, "U"))
                {
                    if (INCX == 1)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            TEMP = X[J + o_x];
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= J - 1; I++)
                            {
                                TEMP +=  - A[I + A_J] * X[I + o_x];
                            }
                            if (NOUNIT) TEMP /= A[J+J * LDA + o_a];
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
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= J - 1; I++)
                            {
                                TEMP +=  - A[I + A_J] * X[IX + o_x];
                                IX += INCX;
                            }
                            if (NOUNIT) TEMP /= A[J+J * LDA + o_a];
                            X[JX + o_x] = TEMP;
                            JX += INCX;
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
                            A_J = J * LDA + o_a;
                            for (I = N; I >= J + 1; I +=  - 1)
                            {
                                TEMP +=  - A[I + A_J] * X[I + o_x];
                            }
                            if (NOUNIT) TEMP /= A[J+J * LDA + o_a];
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
                            A_J = J * LDA + o_a;
                            for (I = N; I >= J + 1; I +=  - 1)
                            {
                                TEMP +=  - A[I + A_J] * X[IX + o_x];
                                IX -= INCX;
                            }
                            if (NOUNIT) TEMP /= A[J+J * LDA + o_a];
                            X[JX + o_x] = TEMP;
                            JX -= INCX;
                        }
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DTRSV .
            // *

            #endregion

        }
    }
}
