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
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// This subroutine computes the I-th eigenvalue of a symmetric rank-one
    /// modification of a 2-by-2 diagonal matrix
    /// 
    /// diag( D )  +  RHO *  Z * transpose(Z) .
    /// 
    /// The diagonal elements in the array D are assumed to satisfy
    /// 
    /// D(i) .LT. D(j)  for  i .LT. j .
    /// 
    /// We also assume RHO .GT. 0 and that the Euclidean norm of the vector
    /// Z is one.
    /// 
    ///</summary>
    public class DLAED5
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; const double FOUR = 4.0E0; 

        #endregion

        public DLAED5()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This subroutine computes the I-th eigenvalue of a symmetric rank-one
        /// modification of a 2-by-2 diagonal matrix
        /// 
        /// diag( D )  +  RHO *  Z * transpose(Z) .
        /// 
        /// The diagonal elements in the array D are assumed to satisfy
        /// 
        /// D(i) .LT. D(j)  for  i .LT. j .
        /// 
        /// We also assume RHO .GT. 0 and that the Euclidean norm of the vector
        /// Z is one.
        /// 
        ///</summary>
        /// <param name="I">
        /// (input) INTEGER
        /// The index of the eigenvalue to be computed.  I = 1 or I = 2.
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION array, dimension (2)
        /// The original eigenvalues.  We assume D(1) .LT. D(2).
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (2)
        /// The components of the updating vector.
        ///</param>
        /// <param name="DELTA">
        /// (output) DOUBLE PRECISION array, dimension (2)
        /// The vector DELTA contains the information necessary
        /// to construct the eigenvectors.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The scalar in the symmetric updating formula.
        ///</param>
        /// <param name="DLAM">
        /// (output) DOUBLE PRECISION
        /// The computed lambda_I, the I-th updated eigenvalue.
        ///</param>
        public void Run(int I, double[] D, int offset_d, double[] Z, int offset_z, ref double[] DELTA, int offset_delta, double RHO, ref double DLAM)
        {

            #region Variables
            
            double B = 0; double C = 0; double DEL = 0; double TAU = 0; double TEMP = 0; double W = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_delta = -1 + offset_delta; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
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
            // *  This subroutine computes the I-th eigenvalue of a symmetric rank-one
            // *  modification of a 2-by-2 diagonal matrix
            // *
            // *             diag( D )  +  RHO *  Z * transpose(Z) .
            // *
            // *  The diagonal elements in the array D are assumed to satisfy
            // *
            // *             D(i) < D(j)  for  i < j .
            // *
            // *  We also assume RHO > 0 and that the Euclidean norm of the vector
            // *  Z is one.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  I      (input) INTEGER
            // *         The index of the eigenvalue to be computed.  I = 1 or I = 2.
            // *
            // *  D      (input) DOUBLE PRECISION array, dimension (2)
            // *         The original eigenvalues.  We assume D(1) < D(2).
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension (2)
            // *         The components of the updating vector.
            // *
            // *  DELTA  (output) DOUBLE PRECISION array, dimension (2)
            // *         The vector DELTA contains the information necessary
            // *         to construct the eigenvectors.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The scalar in the symmetric updating formula.
            // *
            // *  DLAM   (output) DOUBLE PRECISION
            // *         The computed lambda_I, the I-th updated eigenvalue.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ren-Cang Li, Computer Science Division, University of California
            // *     at Berkeley, USA
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
            
            DEL = D[2 + o_d] - D[1 + o_d];
            if (I == 1)
            {
                W = ONE + TWO * RHO * (Z[2 + o_z] * Z[2 + o_z] - Z[1 + o_z] * Z[1 + o_z]) / DEL;
                if (W > ZERO)
                {
                    B = DEL + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                    C = RHO * Z[1 + o_z] * Z[1 + o_z] * DEL;
                    // *
                    // *           B > ZERO, always
                    // *
                    TAU = TWO * C / (B + Math.Sqrt(Math.Abs(B * B - FOUR * C)));
                    DLAM = D[1 + o_d] + TAU;
                    DELTA[1 + o_delta] =  - Z[1 + o_z] / TAU;
                    DELTA[2 + o_delta] = Z[2 + o_z] / (DEL - TAU);
                }
                else
                {
                    B =  - DEL + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                    C = RHO * Z[2 + o_z] * Z[2 + o_z] * DEL;
                    if (B > ZERO)
                    {
                        TAU =  - TWO * C / (B + Math.Sqrt(B * B + FOUR * C));
                    }
                    else
                    {
                        TAU = (B - Math.Sqrt(B * B + FOUR * C)) / TWO;
                    }
                    DLAM = D[2 + o_d] + TAU;
                    DELTA[1 + o_delta] =  - Z[1 + o_z] / (DEL + TAU);
                    DELTA[2 + o_delta] =  - Z[2 + o_z] / TAU;
                }
                TEMP = Math.Sqrt(DELTA[1 + o_delta] * DELTA[1 + o_delta] + DELTA[2 + o_delta] * DELTA[2 + o_delta]);
                DELTA[1 + o_delta] /= TEMP;
                DELTA[2 + o_delta] /= TEMP;
            }
            else
            {
                // *
                // *     Now I=2
                // *
                B =  - DEL + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                C = RHO * Z[2 + o_z] * Z[2 + o_z] * DEL;
                if (B > ZERO)
                {
                    TAU = (B + Math.Sqrt(B * B + FOUR * C)) / TWO;
                }
                else
                {
                    TAU = TWO * C / ( - B + Math.Sqrt(B * B + FOUR * C));
                }
                DLAM = D[2 + o_d] + TAU;
                DELTA[1 + o_delta] =  - Z[1 + o_z] / (DEL + TAU);
                DELTA[2 + o_delta] =  - Z[2 + o_z] / TAU;
                TEMP = Math.Sqrt(DELTA[1 + o_delta] * DELTA[1 + o_delta] + DELTA[2 + o_delta] * DELTA[2 + o_delta]);
                DELTA[1 + o_delta] /= TEMP;
                DELTA[2 + o_delta] /= TEMP;
            }
            return;
            // *
            // *     End OF DLAED5
            // *

            #endregion

        }
    }
}
