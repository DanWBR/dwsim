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
    /// DLASSQ  returns the values  scl  and  smsq  such that
    /// 
    /// ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
    /// 
    /// where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
    /// assumed to be non-negative and  scl  returns the value
    /// 
    /// scl = max( scale, abs( x( i ) ) ).
    /// 
    /// scale and sumsq must be supplied in SCALE and SUMSQ and
    /// scl and smsq are overwritten on SCALE and SUMSQ respectively.
    /// 
    /// The routine makes only one pass through the vector x.
    /// 
    ///</summary>
    public class DLASSQ
    {
    

        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DLASSQ()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASSQ  returns the values  scl  and  smsq  such that
        /// 
        /// ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
        /// 
        /// where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
        /// assumed to be non-negative and  scl  returns the value
        /// 
        /// scl = max( scale, abs( x( i ) ) ).
        /// 
        /// scale and sumsq must be supplied in SCALE and SUMSQ and
        /// scl and smsq are overwritten on SCALE and SUMSQ respectively.
        /// 
        /// The routine makes only one pass through the vector x.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of elements to be used from the vector X.
        ///</param>
        /// <param name="X">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// The vector for which a scaled sum of squares is computed.
        /// x( i )  = X( 1 + ( i - 1 )*INCX ), 1 .LE. i .LE. n.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between successive values of the vector X.
        /// INCX .GT. 0.
        ///</param>
        /// <param name="SCALE">
        /// and sumsq must be supplied in SCALE and SUMSQ and
        ///</param>
        /// <param name="SUMSQ">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the value  sumsq  in the equation above.
        /// On exit, SUMSQ is overwritten with  smsq , the basic sum of
        /// squares from which  scl  has been factored out.
        ///</param>
        public void Run(int N, double[] X, int offset_x, int INCX, ref double SCALE, ref double SUMSQ)
        {

            #region Variables
            
            int IX = 0; double ABSXI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x; 

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
            // *  DLASSQ  returns the values  scl  and  smsq  such that
            // *
            // *     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
            // *
            // *  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
            // *  assumed to be non-negative and  scl  returns the value
            // *
            // *     scl = max( scale, abs( x( i ) ) ).
            // *
            // *  scale and sumsq must be supplied in SCALE and SUMSQ and
            // *  scl and smsq are overwritten on SCALE and SUMSQ respectively.
            // *
            // *  The routine makes only one pass through the vector x.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of elements to be used from the vector X.
            // *
            // *  X       (input) DOUBLE PRECISION array, dimension (N)
            // *          The vector for which a scaled sum of squares is computed.
            // *             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between successive values of the vector X.
            // *          INCX > 0.
            // *
            // *  SCALE   (input/output) DOUBLE PRECISION
            // *          On entry, the value  scale  in the equation above.
            // *          On exit, SCALE is overwritten with  scl , the scaling factor
            // *          for the sum of squares.
            // *
            // *  SUMSQ   (input/output) DOUBLE PRECISION
            // *          On entry, the value  sumsq  in the equation above.
            // *          On exit, SUMSQ is overwritten with  smsq , the basic sum of
            // *          squares from which  scl  has been factored out.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (N > 0)
            {
                for (IX = 1; (INCX >= 0) ? (IX <= 1 + (N - 1) * INCX) : (IX >= 1 + (N - 1) * INCX); IX += INCX)
                {
                    if (X[IX + o_x] != ZERO)
                    {
                        ABSXI = Math.Abs(X[IX + o_x]);
                        if (SCALE < ABSXI)
                        {
                            SUMSQ = 1 + SUMSQ * Math.Pow(SCALE / ABSXI,2);
                            SCALE = ABSXI;
                        }
                        else
                        {
                            SUMSQ += Math.Pow(ABSXI / SCALE,2);
                        }
                    }
                }
            }
            return;
            // *
            // *     End of DLASSQ
            // *

            #endregion

        }
    }
}
