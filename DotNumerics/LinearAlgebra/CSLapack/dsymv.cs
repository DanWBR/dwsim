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
    /// DSYMV  performs the matrix-vector  operation
    /// 
    /// y := alpha*A*x + beta*y,
    /// 
    /// where alpha and beta are scalars, x and y are n element vectors and
    /// A is an n by n symmetric matrix.
    /// 
    ///</summary>
    public class DSYMV
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DSYMV(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DSYMV()
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
        /// DSYMV  performs the matrix-vector  operation
        /// 
        /// y := alpha*A*x + beta*y,
        /// 
        /// where alpha and beta are scalars, x and y are n element vectors and
        /// A is an n by n symmetric matrix.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// - CHARACTER*1.
        /// On entry, UPLO specifies whether the upper or lower
        /// triangular part of the array A is to be referenced as
        /// follows:
        /// 
        /// UPLO = 'U' or 'u'   Only the upper triangular part of A
        /// is to be referenced.
        /// 
        /// UPLO = 'L' or 'l'   Only the lower triangular part of A
        /// is to be referenced.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry, N specifies the order of the matrix A.
        /// N must be at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="ALPHA">
        /// - DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
        /// Before entry with  UPLO = 'U' or 'u', the leading n by n
        /// upper triangular part of the array A must contain the upper
        /// triangular part of the symmetric matrix and the strictly
        /// lower triangular part of A is not referenced.
        /// Before entry with UPLO = 'L' or 'l', the leading n by n
        /// lower triangular part of the array A must contain the lower
        /// triangular part of the symmetric matrix and the strictly
        /// upper triangular part of A is not referenced.
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
        /// element vector x.
        /// Unchanged on exit.
        ///</param>
        /// <param name="INCX">
        /// - INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="BETA">
        /// - DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta. When BETA is
        /// supplied as zero then Y need not be set on input.
        /// Unchanged on exit.
        ///</param>
        /// <param name="Y">
        /// := alpha*A*x + beta*y,
        ///</param>
        /// <param name="INCY">
        /// - INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string UPLO, int N, double ALPHA, double[] A, int offset_a, int LDA, double[] X, int offset_x
                         , int INCX, double BETA, ref double[] Y, int offset_y, int INCY)
        {

            #region Variables
            
            double TEMP1 = 0; double TEMP2 = 0; int I = 0; int INFO = 0; int IX = 0; int IY = 0; int J = 0; int JX = 0; 
            int JY = 0;int KX = 0; int KY = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_x = -1 + offset_x;  int o_y = -1 + offset_y; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

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
            // *  DSYMV  performs the matrix-vector  operation
            // *
            // *     y := alpha*A*x + beta*y,
            // *
            // *  where alpha and beta are scalars, x and y are n element vectors and
            // *  A is an n by n symmetric matrix.
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  UPLO   - CHARACTER*1.
            // *           On entry, UPLO specifies whether the upper or lower
            // *           triangular part of the array A is to be referenced as
            // *           follows:
            // *
            // *              UPLO = 'U' or 'u'   Only the upper triangular part of A
            // *                                  is to be referenced.
            // *
            // *              UPLO = 'L' or 'l'   Only the lower triangular part of A
            // *                                  is to be referenced.
            // *
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry, N specifies the order of the matrix A.
            // *           N must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  ALPHA  - DOUBLE PRECISION.
            // *           On entry, ALPHA specifies the scalar alpha.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            // *           Before entry with  UPLO = 'U' or 'u', the leading n by n
            // *           upper triangular part of the array A must contain the upper
            // *           triangular part of the symmetric matrix and the strictly
            // *           lower triangular part of A is not referenced.
            // *           Before entry with UPLO = 'L' or 'l', the leading n by n
            // *           lower triangular part of the array A must contain the lower
            // *           triangular part of the symmetric matrix and the strictly
            // *           upper triangular part of A is not referenced.
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
            // *           element vector x.
            // *           Unchanged on exit.
            // *
            // *  INCX   - INTEGER.
            // *           On entry, INCX specifies the increment for the elements of
            // *           X. INCX must not be zero.
            // *           Unchanged on exit.
            // *
            // *  BETA   - DOUBLE PRECISION.
            // *           On entry, BETA specifies the scalar beta. When BETA is
            // *           supplied as zero then Y need not be set on input.
            // *           Unchanged on exit.
            // *
            // *  Y      - DOUBLE PRECISION array of dimension at least
            // *           ( 1 + ( n - 1 )*abs( INCY ) ).
            // *           Before entry, the incremented array Y must contain the n
            // *           element vector y. On exit, Y is overwritten by the updated
            // *           vector y.
            // *
            // *  INCY   - INTEGER.
            // *           On entry, INCY specifies the increment for the elements of
            // *           Y. INCY must not be zero.
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
                if (N < 0)
                {
                    INFO = 2;
                }
                else
                {
                    if (LDA < Math.Max(1, N))
                    {
                        INFO = 5;
                    }
                    else
                    {
                        if (INCX == 0)
                        {
                            INFO = 7;
                        }
                        else
                        {
                            if (INCY == 0)
                            {
                                INFO = 10;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DSYMV ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if ((N == 0) || ((ALPHA == ZERO) && (BETA == ONE))) return;
            // *
            // *     Set up the start points in  X  and  Y.
            // *
            if (INCX > 0)
            {
                KX = 1;
            }
            else
            {
                KX = 1 - (N - 1) * INCX;
            }
            if (INCY > 0)
            {
                KY = 1;
            }
            else
            {
                KY = 1 - (N - 1) * INCY;
            }
            // *
            // *     Start the operations. In this version the elements of A are
            // *     accessed sequentially with one pass through the triangular part
            // *     of A.
            // *
            // *     First form  y := beta*y.
            // *
            if (BETA != ONE)
            {
                if (INCY == 1)
                {
                    if (BETA == ZERO)
                    {
                        for (I = 1; I <= N; I++)
                        {
                            Y[I + o_y] = ZERO;
                        }
                    }
                    else
                    {
                        for (I = 1; I <= N; I++)
                        {
                            Y[I + o_y] *= BETA;
                        }
                    }
                }
                else
                {
                    IY = KY;
                    if (BETA == ZERO)
                    {
                        for (I = 1; I <= N; I++)
                        {
                            Y[IY + o_y] = ZERO;
                            IY += INCY;
                        }
                    }
                    else
                    {
                        for (I = 1; I <= N; I++)
                        {
                            Y[IY + o_y] *= BETA;
                            IY += INCY;
                        }
                    }
                }
            }
            if (ALPHA == ZERO) return;
            if (this._lsame.Run(UPLO, "U"))
            {
                // *
                // *        Form  y  when A is stored in upper triangle.
                // *
                if ((INCX == 1) && (INCY == 1))
                {
                    for (J = 1; J <= N; J++)
                    {
                        TEMP1 = ALPHA * X[J + o_x];
                        TEMP2 = ZERO;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= J - 1; I++)
                        {
                            Y[I + o_y] += TEMP1 * A[I + A_J];
                            TEMP2 += A[I + A_J] * X[I + o_x];
                        }
                        Y[J + o_y] += TEMP1 * A[J+J * LDA + o_a] + ALPHA * TEMP2;
                    }
                }
                else
                {
                    JX = KX;
                    JY = KY;
                    for (J = 1; J <= N; J++)
                    {
                        TEMP1 = ALPHA * X[JX + o_x];
                        TEMP2 = ZERO;
                        IX = KX;
                        IY = KY;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= J - 1; I++)
                        {
                            Y[IY + o_y] += TEMP1 * A[I + A_J];
                            TEMP2 += A[I + A_J] * X[IX + o_x];
                            IX += INCX;
                            IY += INCY;
                        }
                        Y[JY + o_y] += TEMP1 * A[J+J * LDA + o_a] + ALPHA * TEMP2;
                        JX += INCX;
                        JY += INCY;
                    }
                }
            }
            else
            {
                // *
                // *        Form  y  when A is stored in lower triangle.
                // *
                if ((INCX == 1) && (INCY == 1))
                {
                    for (J = 1; J <= N; J++)
                    {
                        TEMP1 = ALPHA * X[J + o_x];
                        TEMP2 = ZERO;
                        Y[J + o_y] += TEMP1 * A[J+J * LDA + o_a];
                        A_J = J * LDA + o_a;
                        for (I = J + 1; I <= N; I++)
                        {
                            Y[I + o_y] += TEMP1 * A[I + A_J];
                            TEMP2 += A[I + A_J] * X[I + o_x];
                        }
                        Y[J + o_y] += ALPHA * TEMP2;
                    }
                }
                else
                {
                    JX = KX;
                    JY = KY;
                    for (J = 1; J <= N; J++)
                    {
                        TEMP1 = ALPHA * X[JX + o_x];
                        TEMP2 = ZERO;
                        Y[JY + o_y] += TEMP1 * A[J+J * LDA + o_a];
                        IX = JX;
                        IY = JY;
                        A_J = J * LDA + o_a;
                        for (I = J + 1; I <= N; I++)
                        {
                            IX += INCX;
                            IY += INCY;
                            Y[IY + o_y] += TEMP1 * A[I + A_J];
                            TEMP2 += A[I + A_J] * X[IX + o_x];
                        }
                        Y[JY + o_y] += ALPHA * TEMP2;
                        JX += INCX;
                        JY += INCY;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DSYMV .
            // *

            #endregion

        }
    }
}
