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
    ///</summary>
    public class DLAQR1
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; 

        #endregion

        public DLAQR1()
        {
    
        }
    
        /// <param name="N">
        /// (input) integer
        /// Order of the matrix H. N must be either 2 or 3.
        ///</param>
        /// <param name="H">
        /// (input) DOUBLE PRECISION array of dimension (LDH,N)
        /// The 2-by-2 or 3-by-3 matrix H in (*).
        ///</param>
        /// <param name="LDH">
        /// (input) integer
        /// The leading dimension of H as declared in
        /// the calling procedure.  LDH.GE.N
        ///</param>
        /// <param name="SR1">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="SI1">
        /// The shifts in (*).
        ///</param>
        /// <param name="V">
        /// (output) DOUBLE PRECISION array of dimension N
        /// A scalar multiple of the first column of the
        /// matrix K in (*).
        ///</param>
        public void Run(int N, double[] H, int offset_h, int LDH, double SR1, double SI1, double SR2
                         , double SI2, ref double[] V, int offset_v)
        {

            #region Variables
            
            double H21S = 0; double H31S = 0; double S = 0; 

            #endregion


            #region Array Index Correction
            
             int o_h = -1 - LDH + offset_h;  int o_v = -1 + offset_v; 

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
            // *       Given a 2-by-2 or 3-by-3 matrix H, DLAQR1 sets v to a
            // *       scalar multiple of the first column of the product
            // *
            // *       (*)  K = (H - (sr1 + i*si1)*I)*(H - (sr2 + i*si2)*I)
            // *
            // *       scaling to avoid overflows and most underflows. It
            // *       is assumed that either
            // *
            // *               1) sr1 = sr2 and si1 = -si2
            // *           or
            // *               2) si1 = si2 = 0.
            // *
            // *       This is useful for starting double implicit shift bulges
            // *       in the QR algorithm.
            // *
            // *
            // *       N      (input) integer
            // *              Order of the matrix H. N must be either 2 or 3.
            // *
            // *       H      (input) DOUBLE PRECISION array of dimension (LDH,N)
            // *              The 2-by-2 or 3-by-3 matrix H in (*).
            // *
            // *       LDH    (input) integer
            // *              The leading dimension of H as declared in
            // *              the calling procedure.  LDH.GE.N
            // *
            // *       SR1    (input) DOUBLE PRECISION
            // *       SI1    The shifts in (*).
            // *       SR2
            // *       SI2
            // *
            // *       V      (output) DOUBLE PRECISION array of dimension N
            // *              A scalar multiple of the first column of the
            // *              matrix K in (*).
            // *
            // *     ================================================================
            // *     Based on contributions by
            // *        Karen Braman and Ralph Byers, Department of Mathematics,
            // *        University of Kansas, USA
            // *
            // *     ================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS;
            // *     ..
            // *     .. Executable Statements ..

            #endregion


            #region Body
            
            if (N == 2)
            {
                S = Math.Abs(H[1+1 * LDH + o_h] - SR2) + Math.Abs(SI2) + Math.Abs(H[2+1 * LDH + o_h]);
                if (S == ZERO)
                {
                    V[1 + o_v] = ZERO;
                    V[2 + o_v] = ZERO;
                }
                else
                {
                    H21S = H[2+1 * LDH + o_h] / S;
                    V[1 + o_v] = H21S * H[1+2 * LDH + o_h] + (H[1+1 * LDH + o_h] - SR1) * ((H[1+1 * LDH + o_h] - SR2) / S) - SI1 * (SI2 / S);
                    V[2 + o_v] = H21S * (H[1+1 * LDH + o_h] + H[2+2 * LDH + o_h] - SR1 - SR2);
                }
            }
            else
            {
                S = Math.Abs(H[1+1 * LDH + o_h] - SR2) + Math.Abs(SI2) + Math.Abs(H[2+1 * LDH + o_h]) + Math.Abs(H[3+1 * LDH + o_h]);
                if (S == ZERO)
                {
                    V[1 + o_v] = ZERO;
                    V[2 + o_v] = ZERO;
                    V[3 + o_v] = ZERO;
                }
                else
                {
                    H21S = H[2+1 * LDH + o_h] / S;
                    H31S = H[3+1 * LDH + o_h] / S;
                    V[1 + o_v] = (H[1+1 * LDH + o_h] - SR1) * ((H[1+1 * LDH + o_h] - SR2) / S) - SI1 * (SI2 / S) + H[1+2 * LDH + o_h] * H21S + H[1+3 * LDH + o_h] * H31S;
                    V[2 + o_v] = H21S * (H[1+1 * LDH + o_h] + H[2+2 * LDH + o_h] - SR1 - SR2) + H[2+3 * LDH + o_h] * H31S;
                    V[3 + o_v] = H31S * (H[1+1 * LDH + o_h] + H[3+3 * LDH + o_h] - SR1 - SR2) + H21S * H[3+2 * LDH + o_h];
                }
            }

            #endregion

        }
    }
}
