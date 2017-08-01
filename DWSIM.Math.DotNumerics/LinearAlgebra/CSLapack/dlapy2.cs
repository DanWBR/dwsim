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
    /// DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
    /// overflow.
    /// 
    ///</summary>
    public class DLAPY2
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DLAPY2()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
        /// overflow.
        /// 
        ///</summary>
        /// <param name="X">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="Y">
        /// (input) DOUBLE PRECISION
        /// X and Y specify the values x and y.
        ///</param>
        public double Run(double X, double Y)
        {
        double dlapy2 = 0;

            #region Variables
            
            double W = 0; double XABS = 0; double YABS = 0; double Z = 0; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
            // *  overflow.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  X       (input) DOUBLE PRECISION
            // *  Y       (input) DOUBLE PRECISION
            // *          X and Y specify the values x and y.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion

            XABS = Math.Abs(X);
            YABS = Math.Abs(Y);
            W = Math.Max(XABS, YABS);
            Z = Math.Min(XABS, YABS);
            if (Z == ZERO)
            {
                dlapy2 = W;
            }
            else
            {
                dlapy2 = W * Math.Sqrt(ONE + Math.Pow(Z / W,2));
            }
            return dlapy2;
            // *
            // *     End of DLAPY2
            // *
        }
    }
}
