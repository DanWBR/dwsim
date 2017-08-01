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
    /// DGER   performs the rank 1 operation
    /// 
    /// A := alpha*x*y' + A,
    /// 
    /// where alpha is a scalar, x is an m element vector, y is an n element
    /// vector and A is an m by n matrix.
    /// 
    ///</summary>
    public class DGER
    {
    

        #region Dependencies
        
        XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DGER(XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._xerbla = xerbla; 

            #endregion

        }
    
        public DGER()
        {
    

            #region Dependencies (Initialization)
            
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGER   performs the rank 1 operation
        /// 
        /// A := alpha*x*y' + A,
        /// 
        /// where alpha is a scalar, x is an m element vector, y is an n element
        /// vector and A is an m by n matrix.
        /// 
        ///</summary>
        /// <param name="M">
        /// - INTEGER.
        /// On entry, M specifies the number of rows of the matrix A.
        /// M must be at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry, N specifies the number of columns of the matrix A.
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
        /// ( 1 + ( m - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the m
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
        /// := alpha*x*y' + A,
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, m ).
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(int M, int N, double ALPHA, double[] X, int offset_x, int INCX, double[] Y, int offset_y
                         , int INCY, ref double[] A, int offset_a, int LDA)
        {

            #region Variables
            
            double TEMP = 0; int I = 0; int INFO = 0; int IX = 0; int J = 0; int JY = 0; int KX = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y;  int o_a = -1 - LDA + offset_a; 

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
            // *  DGER   performs the rank 1 operation
            // *
            // *     A := alpha*x*y' + A,
            // *
            // *  where alpha is a scalar, x is an m element vector, y is an n element
            // *  vector and A is an m by n matrix.
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  M      - INTEGER.
            // *           On entry, M specifies the number of rows of the matrix A.
            // *           M must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry, N specifies the number of columns of the matrix A.
            // *           N must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  ALPHA  - DOUBLE PRECISION.
            // *           On entry, ALPHA specifies the scalar alpha.
            // *           Unchanged on exit.
            // *
            // *  X      - DOUBLE PRECISION array of dimension at least
            // *           ( 1 + ( m - 1 )*abs( INCX ) ).
            // *           Before entry, the incremented array X must contain the m
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
            // *           Before entry, the leading m by n part of the array A must
            // *           contain the matrix of coefficients. On exit, A is
            // *           overwritten by the updated matrix.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program. LDA must be at least
            // *           max( 1, m ).
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
            if (M < 0)
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
                            if (LDA < Math.Max(1, M))
                            {
                                INFO = 9;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGER  ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if ((M == 0) || (N == 0) || (ALPHA == ZERO)) return;
            // *
            // *     Start the operations. In this version the elements of A are
            // *     accessed sequentially with one pass through A.
            // *
            if (INCY > 0)
            {
                JY = 1;
            }
            else
            {
                JY = 1 - (N - 1) * INCY;
            }
            if (INCX == 1)
            {
                for (J = 1; J <= N; J++)
                {
                    if (Y[JY + o_y] != ZERO)
                    {
                        TEMP = ALPHA * Y[JY + o_y];
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            A[I + A_J] += X[I + o_x] * TEMP;
                        }
                    }
                    JY += INCY;
                }
            }
            else
            {
                if (INCX > 0)
                {
                    KX = 1;
                }
                else
                {
                    KX = 1 - (M - 1) * INCX;
                }
                for (J = 1; J <= N; J++)
                {
                    if (Y[JY + o_y] != ZERO)
                    {
                        TEMP = ALPHA * Y[JY + o_y];
                        IX = KX;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            A[I + A_J] += X[IX + o_x] * TEMP;
                            IX += INCX;
                        }
                    }
                    JY += INCY;
                }
            }
            // *
            return;
            // *
            // *     End of DGER  .
            // *

            #endregion

        }
    }
}
