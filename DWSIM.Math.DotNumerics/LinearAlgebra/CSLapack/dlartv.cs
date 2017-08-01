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
    /// DLARTV applies a vector of real plane rotations to elements of the
    /// real vectors x and y. For i = 1,2,...,n
    /// 
    /// ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
    /// ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
    /// 
    ///</summary>
    public class DLARTV
    {
    
        public DLARTV()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARTV applies a vector of real plane rotations to elements of the
        /// real vectors x and y. For i = 1,2,...,n
        /// 
        /// ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
        /// ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of plane rotations to be applied.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// The vector x.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between elements of X. INCX .GT. 0.
        ///</param>
        /// <param name="Y">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCY)
        /// The vector y.
        ///</param>
        /// <param name="INCY">
        /// (input) INTEGER
        /// The increment between elements of Y. INCY .GT. 0.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
        /// The cosines of the plane rotations.
        ///</param>
        /// <param name="S">
        /// (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
        /// The sines of the plane rotations.
        ///</param>
        /// <param name="INCC">
        /// (input) INTEGER
        /// The increment between elements of C and S. INCC .GT. 0.
        ///</param>
        public void Run(int N, ref double[] X, int offset_x, int INCX, ref double[] Y, int offset_y, int INCY, double[] C, int offset_c
                         , double[] S, int offset_s, int INCC)
        {

            #region Variables
            
            int I = 0; int IC = 0; int IX = 0; int IY = 0; double XI = 0; double YI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y;  int o_c = -1 + offset_c;  int o_s = -1 + offset_s; 

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
            // *  DLARTV applies a vector of real plane rotations to elements of the
            // *  real vectors x and y. For i = 1,2,...,n
            // *
            // *     ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
            // *     ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of plane rotations to be applied.
            // *
            // *  X       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          The vector x.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between elements of X. INCX > 0.
            // *
            // *  Y       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCY)
            // *          The vector y.
            // *
            // *  INCY    (input) INTEGER
            // *          The increment between elements of Y. INCY > 0.
            // *
            // *  C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
            // *          The cosines of the plane rotations.
            // *
            // *  S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
            // *          The sines of the plane rotations.
            // *
            // *  INCC    (input) INTEGER
            // *          The increment between elements of C and S. INCC > 0.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion

            IX = 1;
            IY = 1;
            IC = 1;
            for (I = 1; I <= N; I++)
            {
                XI = X[IX + o_x];
                YI = Y[IY + o_y];
                X[IX + o_x] = C[IC + o_c] * XI + S[IC + o_s] * YI;
                Y[IY + o_y] = C[IC + o_c] * YI - S[IC + o_s] * XI;
                IX += INCX;
                IY += INCY;
                IC += INCC;
            }
            return;
            // *
            // *     End of DLARTV
            // *
        }
    }
}
