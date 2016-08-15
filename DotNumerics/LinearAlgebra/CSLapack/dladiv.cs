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
    /// DLADIV performs complex division in  real arithmetic
    /// 
    /// a + i*b
    /// p + i*q = ---------
    /// c + i*d
    /// 
    /// The algorithm is due to Robert L. Smith and can be found
    /// in D. Knuth, The art of Computer Programming, Vol.2, p.195
    /// 
    ///</summary>
    public class DLADIV
    {
    
        public DLADIV()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLADIV performs complex division in  real arithmetic
        /// 
        /// a + i*b
        /// p + i*q = ---------
        /// c + i*d
        /// 
        /// The algorithm is due to Robert L. Smith and can be found
        /// in D. Knuth, The art of Computer Programming, Vol.2, p.195
        /// 
        ///</summary>
        /// <param name="A">
        /// + i*b
        ///</param>
        /// <param name="B">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="C">
        /// + i*d
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION
        /// The scalars a, b, c, and d in the above expression.
        ///</param>
        /// <param name="P">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="Q">
        /// (output) DOUBLE PRECISION
        /// The scalars p and q in the above expression.
        ///</param>
        public void Run(double A, double B, double C, double D, ref double P, ref double Q)
        {

            #region Variables
            
            double E = 0; double F = 0; 

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
            // *  DLADIV performs complex division in  real arithmetic
            // *
            // *                        a + i*b
            // *             p + i*q = ---------
            // *                        c + i*d
            // *
            // *  The algorithm is due to Robert L. Smith and can be found
            // *  in D. Knuth, The art of Computer Programming, Vol.2, p.195
            // *
            // *  Arguments
            // *  =========
            // *
            // *  A       (input) DOUBLE PRECISION
            // *  B       (input) DOUBLE PRECISION
            // *  C       (input) DOUBLE PRECISION
            // *  D       (input) DOUBLE PRECISION
            // *          The scalars a, b, c, and d in the above expression.
            // *
            // *  P       (output) DOUBLE PRECISION
            // *  Q       (output) DOUBLE PRECISION
            // *          The scalars p and q in the above expression.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion

            if (Math.Abs(D) < Math.Abs(C))
            {
                E = D / C;
                F = C + D * E;
                P = (A + B * E) / F;
                Q = (B - A * E) / F;
            }
            else
            {
                E = C / D;
                F = D + C * E;
                P = (B + A * E) / F;
                Q = ( - A + B * E) / F;
            }
            // *
            return;
            // *
            // *     End of DLADIV
            // *
        }
    }
}
