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
    /// DLAS2  computes the singular values of the 2-by-2 matrix
    /// [  F   G  ]
    /// [  0   H  ].
    /// On return, SSMIN is the smaller singular value and SSMAX is the
    /// larger singular value.
    /// 
    ///</summary>
    public class DLAS2
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 

        #endregion

        public DLAS2()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAS2  computes the singular values of the 2-by-2 matrix
        /// [  F   G  ]
        /// [  0   H  ].
        /// On return, SSMIN is the smaller singular value and SSMAX is the
        /// larger singular value.
        /// 
        ///</summary>
        /// <param name="F">
        /// (input) DOUBLE PRECISION
        /// The (1,1) element of the 2-by-2 matrix.
        ///</param>
        /// <param name="G">
        /// (input) DOUBLE PRECISION
        /// The (1,2) element of the 2-by-2 matrix.
        ///</param>
        /// <param name="H">
        /// (input) DOUBLE PRECISION
        /// The (2,2) element of the 2-by-2 matrix.
        ///</param>
        /// <param name="SSMIN">
        /// (output) DOUBLE PRECISION
        /// The smaller singular value.
        ///</param>
        /// <param name="SSMAX">
        /// (output) DOUBLE PRECISION
        /// The larger singular value.
        ///</param>
        public void Run(double F, double G, double H, ref double SSMIN, ref double SSMAX)
        {

            #region Variables
            
            double AS = 0; double AT = 0; double AU = 0; double C = 0; double FA = 0; double FHMN = 0; double FHMX = 0; 
            double GA = 0;double HA = 0; 

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
            // *  DLAS2  computes the singular values of the 2-by-2 matrix
            // *     [  F   G  ]
            // *     [  0   H  ].
            // *  On return, SSMIN is the smaller singular value and SSMAX is the
            // *  larger singular value.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  F       (input) DOUBLE PRECISION
            // *          The (1,1) element of the 2-by-2 matrix.
            // *
            // *  G       (input) DOUBLE PRECISION
            // *          The (1,2) element of the 2-by-2 matrix.
            // *
            // *  H       (input) DOUBLE PRECISION
            // *          The (2,2) element of the 2-by-2 matrix.
            // *
            // *  SSMIN   (output) DOUBLE PRECISION
            // *          The smaller singular value.
            // *
            // *  SSMAX   (output) DOUBLE PRECISION
            // *          The larger singular value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Barring over/underflow, all output quantities are correct to within
            // *  a few units in the last place (ulps), even in the absence of a guard
            // *  digit in addition/subtraction.
            // *
            // *  In IEEE arithmetic, the code works correctly if one matrix element is
            // *  infinite.
            // *
            // *  Overflow will not occur unless the largest singular value itself
            // *  overflows, or is within a few ulps of overflow. (On machines with
            // *  partial overflow, like the Cray, overflow may occur if the largest
            // *  singular value is within a factor of 2 of overflow.)
            // *
            // *  Underflow is harmless if underflow is gradual. Otherwise, results
            // *  may correspond to a matrix modified by perturbations of size near
            // *  the underflow threshold.
            // *
            // *  ====================================================================
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


            #region Body
            
            FA = Math.Abs(F);
            GA = Math.Abs(G);
            HA = Math.Abs(H);
            FHMN = Math.Min(FA, HA);
            FHMX = Math.Max(FA, HA);
            if (FHMN == ZERO)
            {
                SSMIN = ZERO;
                if (FHMX == ZERO)
                {
                    SSMAX = GA;
                }
                else
                {
                    SSMAX = Math.Max(FHMX, GA) * Math.Sqrt(ONE + Math.Pow(Math.Min(FHMX, GA) / Math.Max(FHMX, GA),2));
                }
            }
            else
            {
                if (GA < FHMX)
                {
                    AS = ONE + FHMN / FHMX;
                    AT = (FHMX - FHMN) / FHMX;
                    AU = Math.Pow(GA / FHMX,2);
                    C = TWO / (Math.Sqrt(AS * AS + AU) + Math.Sqrt(AT * AT + AU));
                    SSMIN = FHMN * C;
                    SSMAX = FHMX / C;
                }
                else
                {
                    AU = FHMX / GA;
                    if (AU == ZERO)
                    {
                        // *
                        // *              Avoid possible harmful underflow if exponent range
                        // *              asymmetric (true SSMIN may not underflow even if
                        // *              AU underflows)
                        // *
                        SSMIN = (FHMN * FHMX) / GA;
                        SSMAX = GA;
                    }
                    else
                    {
                        AS = ONE + FHMN / FHMX;
                        AT = (FHMX - FHMN) / FHMX;
                        C = ONE / (Math.Sqrt(ONE + Math.Pow(AS * AU,2)) + Math.Sqrt(ONE + Math.Pow(AT * AU,2)));
                        SSMIN = (FHMN * C) * AU;
                        SSMIN += SSMIN;
                        SSMAX = GA / (C + C);
                    }
                }
            }
            return;
            // *
            // *     End of DLAS2
            // *

            #endregion

        }
    }
}
