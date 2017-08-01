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
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLARGV generates a vector of real plane rotations, determined by
    /// elements of the real vectors x and y. For i = 1,2,...,n
    /// 
    /// (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
    /// ( -s(i)  c(i) ) ( y(i) ) = (   0  )
    /// 
    ///</summary>
    public class DLARGV
    {
    

        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLARGV()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARGV generates a vector of real plane rotations, determined by
        /// elements of the real vectors x and y. For i = 1,2,...,n
        /// 
        /// (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
        /// ( -s(i)  c(i) ) ( y(i) ) = (   0  )
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of plane rotations to be generated.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// On entry, the vector x.
        /// On exit, x(i) is overwritten by a(i), for i = 1,...,n.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between elements of X. INCX .GT. 0.
        ///</param>
        /// <param name="Y">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCY)
        /// On entry, the vector y.
        /// On exit, the sines of the plane rotations.
        ///</param>
        /// <param name="INCY">
        /// (input) INTEGER
        /// The increment between elements of Y. INCY .GT. 0.
        ///</param>
        /// <param name="C">
        /// (output) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
        /// The cosines of the plane rotations.
        ///</param>
        /// <param name="INCC">
        /// (input) INTEGER
        /// The increment between elements of C. INCC .GT. 0.
        ///</param>
        public void Run(int N, ref double[] X, int offset_x, int INCX, ref double[] Y, int offset_y, int INCY, ref double[] C, int offset_c
                         , int INCC)
        {

            #region Variables
            
            int I = 0; int IC = 0; int IX = 0; int IY = 0; double F = 0; double G = 0; double T = 0; double TT = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y;  int o_c = -1 + offset_c; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLARGV generates a vector of real plane rotations, determined by
            // *  elements of the real vectors x and y. For i = 1,2,...,n
            // *
            // *     (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
            // *     ( -s(i)  c(i) ) ( y(i) ) = (   0  )
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of plane rotations to be generated.
            // *
            // *  X       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          On entry, the vector x.
            // *          On exit, x(i) is overwritten by a(i), for i = 1,...,n.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between elements of X. INCX > 0.
            // *
            // *  Y       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCY)
            // *          On entry, the vector y.
            // *          On exit, the sines of the plane rotations.
            // *
            // *  INCY    (input) INTEGER
            // *          The increment between elements of Y. INCY > 0.
            // *
            // *  C       (output) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
            // *          The cosines of the plane rotations.
            // *
            // *  INCC    (input) INTEGER
            // *          The increment between elements of C. INCC > 0.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            IX = 1;
            IY = 1;
            IC = 1;
            for (I = 1; I <= N; I++)
            {
                F = X[IX + o_x];
                G = Y[IY + o_y];
                if (G == ZERO)
                {
                    C[IC + o_c] = ONE;
                }
                else
                {
                    if (F == ZERO)
                    {
                        C[IC + o_c] = ZERO;
                        Y[IY + o_y] = ONE;
                        X[IX + o_x] = G;
                    }
                    else
                    {
                        if (Math.Abs(F) > Math.Abs(G))
                        {
                            T = G / F;
                            TT = Math.Sqrt(ONE + T * T);
                            C[IC + o_c] = ONE / TT;
                            Y[IY + o_y] = T * C[IC + o_c];
                            X[IX + o_x] = F * TT;
                        }
                        else
                        {
                            T = F / G;
                            TT = Math.Sqrt(ONE + T * T);
                            Y[IY + o_y] = ONE / TT;
                            C[IC + o_c] = T * Y[IY + o_y];
                            X[IX + o_x] = G * TT;
                        }
                    }
                }
                IC += INCC;
                IY += INCY;
                IX += INCX;
            }
            return;
            // *
            // *     End of DLARGV
            // *

            #endregion

        }
    }
}
