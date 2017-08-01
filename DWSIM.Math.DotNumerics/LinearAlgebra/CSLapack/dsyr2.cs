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
    /// DSYR2  performs the symmetric rank 2 operation
    /// 
    /// A := alpha*x*y' + alpha*y*x' + A,
    /// 
    /// where alpha is a scalar, x and y are n element vectors and A is an n
    /// by n symmetric matrix.
    /// 
    ///</summary>
    public class DSYR2
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DSYR2(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DSYR2()
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
        /// DSYR2  performs the symmetric rank 2 operation
        /// 
        /// A := alpha*x*y' + alpha*y*x' + A,
        /// 
        /// where alpha is a scalar, x and y are n element vectors and A is an n
        /// by n symmetric matrix.
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
        /// <param name="Y">
        /// - DOUBLE PRECISION array of dimension at least
        /// ( 1 + ( n - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array Y must contain the n
        /// element vector y.
        /// Unchanged on exit.
        ///</param>
        /// <param name="INCY">
        /// - INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// := alpha*x*y' + alpha*y*x' + A,
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, n ).
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string UPLO, int N, double ALPHA, double[] X, int offset_x, int INCX, double[] Y, int offset_y
                         , int INCY, ref double[] A, int offset_a, int LDA)
        {

            #region Variables
            
            double TEMP1 = 0; double TEMP2 = 0; int I = 0; int INFO = 0; int IX = 0; int IY = 0; int J = 0; int JX = 0; 
            int JY = 0;int KX = 0; int KY = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y;  int o_a = -1 - LDA + offset_a; 

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
            // *  DSYR2  performs the symmetric rank 2 operation
            // *
            // *     A := alpha*x*y' + alpha*y*x' + A,
            // *
            // *  where alpha is a scalar, x and y are n element vectors and A is an n
            // *  by n symmetric matrix.
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
            // *  Y      - DOUBLE PRECISION array of dimension at least
            // *           ( 1 + ( n - 1 )*abs( INCY ) ).
            // *           Before entry, the incremented array Y must contain the n
            // *           element vector y.
            // *           Unchanged on exit.
            // *
            // *  INCY   - INTEGER.
            // *           On entry, INCY specifies the increment for the elements of
            // *           Y. INCY must not be zero.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            // *           Before entry with  UPLO = 'U' or 'u', the leading n by n
            // *           upper triangular part of the array A must contain the upper
            // *           triangular part of the symmetric matrix and the strictly
            // *           lower triangular part of A is not referenced. On exit, the
            // *           upper triangular part of the array A is overwritten by the
            // *           upper triangular part of the updated matrix.
            // *           Before entry with UPLO = 'L' or 'l', the leading n by n
            // *           lower triangular part of the array A must contain the lower
            // *           triangular part of the symmetric matrix and the strictly
            // *           upper triangular part of A is not referenced. On exit, the
            // *           lower triangular part of the array A is overwritten by the
            // *           lower triangular part of the updated matrix.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program. LDA must be at least
            // *           max( 1, n ).
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
                    if (INCX == 0)
                    {
                        INFO = 5;
                    }
                    else
                    {
                        if (INCY == 0)
                        {
                            INFO = 7;
                        }
                        else
                        {
                            if (LDA < Math.Max(1, N))
                            {
                                INFO = 9;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DSYR2 ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if ((N == 0) || (ALPHA == ZERO)) return;
            // *
            // *     Set up the start points in X and Y if the increments are not both
            // *     unity.
            // *
            if ((INCX != 1) || (INCY != 1))
            {
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
                JX = KX;
                JY = KY;
            }
            // *
            // *     Start the operations. In this version the elements of A are
            // *     accessed sequentially with one pass through the triangular part
            // *     of A.
            // *
            if (this._lsame.Run(UPLO, "U"))
            {
                // *
                // *        Form  A  when A is stored in the upper triangle.
                // *
                if ((INCX == 1) && (INCY == 1))
                {
                    for (J = 1; J <= N; J++)
                    {
                        if ((X[J + o_x] != ZERO) || (Y[J + o_y] != ZERO))
                        {
                            TEMP1 = ALPHA * Y[J + o_y];
                            TEMP2 = ALPHA * X[J + o_x];
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= J; I++)
                            {
                                A[I + A_J] += X[I + o_x] * TEMP1 + Y[I + o_y] * TEMP2;
                            }
                        }
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        if ((X[JX + o_x] != ZERO) || (Y[JY + o_y] != ZERO))
                        {
                            TEMP1 = ALPHA * Y[JY + o_y];
                            TEMP2 = ALPHA * X[JX + o_x];
                            IX = KX;
                            IY = KY;
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= J; I++)
                            {
                                A[I + A_J] += X[IX + o_x] * TEMP1 + Y[IY + o_y] * TEMP2;
                                IX += INCX;
                                IY += INCY;
                            }
                        }
                        JX += INCX;
                        JY += INCY;
                    }
                }
            }
            else
            {
                // *
                // *        Form  A  when A is stored in the lower triangle.
                // *
                if ((INCX == 1) && (INCY == 1))
                {
                    for (J = 1; J <= N; J++)
                    {
                        if ((X[J + o_x] != ZERO) || (Y[J + o_y] != ZERO))
                        {
                            TEMP1 = ALPHA * Y[J + o_y];
                            TEMP2 = ALPHA * X[J + o_x];
                            A_J = J * LDA + o_a;
                            for (I = J; I <= N; I++)
                            {
                                A[I + A_J] += X[I + o_x] * TEMP1 + Y[I + o_y] * TEMP2;
                            }
                        }
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        if ((X[JX + o_x] != ZERO) || (Y[JY + o_y] != ZERO))
                        {
                            TEMP1 = ALPHA * Y[JY + o_y];
                            TEMP2 = ALPHA * X[JX + o_x];
                            IX = JX;
                            IY = JY;
                            A_J = J * LDA + o_a;
                            for (I = J; I <= N; I++)
                            {
                                A[I + A_J] += X[IX + o_x] * TEMP1 + Y[IY + o_y] * TEMP2;
                                IX += INCX;
                                IY += INCY;
                            }
                        }
                        JX += INCX;
                        JY += INCY;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DSYR2 .
            // *

            #endregion

        }
    }
}
