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
    /// This subroutine computes the square root of the I-th eigenvalue
    /// of a positive symmetric rank-one modification of a 2-by-2 diagonal
    /// matrix
    /// 
    /// diag( D ) * diag( D ) +  RHO *  Z * transpose(Z) .
    /// 
    /// The diagonal entries in the array D are assumed to satisfy
    /// 
    /// 0 .LE. D(i) .LT. D(j)  for  i .LT. j .
    /// 
    /// We also assume RHO .GT. 0 and that the Euclidean norm of the vector
    /// Z is one.
    /// 
    ///</summary>
    public class DLASD5
    {
    

        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; const double THREE = 3.0E+0; 
        const double FOUR = 4.0E+0;

        #endregion

        public DLASD5()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This subroutine computes the square root of the I-th eigenvalue
        /// of a positive symmetric rank-one modification of a 2-by-2 diagonal
        /// matrix
        /// 
        /// diag( D ) * diag( D ) +  RHO *  Z * transpose(Z) .
        /// 
        /// The diagonal entries in the array D are assumed to satisfy
        /// 
        /// 0 .LE. D(i) .LT. D(j)  for  i .LT. j .
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
        /// (input) DOUBLE PRECISION array, dimension ( 2 )
        /// The original eigenvalues.  We assume 0 .LE. D(1) .LT. D(2).
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( 2 )
        /// The components of the updating vector.
        ///</param>
        /// <param name="DELTA">
        /// (output) DOUBLE PRECISION array, dimension ( 2 )
        /// Contains (D(j) - sigma_I) in its  j-th component.
        /// The vector DELTA contains the information necessary
        /// to construct the eigenvectors.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The scalar in the symmetric updating formula.
        ///</param>
        /// <param name="DSIGMA">
        /// (output) DOUBLE PRECISION
        /// The computed sigma_I, the I-th updated eigenvalue.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension ( 2 )
        /// WORK contains (D(j) + sigma_I) in its  j-th component.
        ///</param>
        public void Run(int I, double[] D, int offset_d, double[] Z, int offset_z, ref double[] DELTA, int offset_delta, double RHO, ref double DSIGMA
                         , ref double[] WORK, int offset_work)
        {

            #region Variables
            
            double B = 0; double C = 0; double DEL = 0; double DELSQ = 0; double TAU = 0; double W = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_delta = -1 + offset_delta;  int o_work = -1 + offset_work; 

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
            // *  This subroutine computes the square root of the I-th eigenvalue
            // *  of a positive symmetric rank-one modification of a 2-by-2 diagonal
            // *  matrix
            // *
            // *             diag( D ) * diag( D ) +  RHO *  Z * transpose(Z) .
            // *
            // *  The diagonal entries in the array D are assumed to satisfy
            // *
            // *             0 <= D(i) < D(j)  for  i < j .
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
            // *  D      (input) DOUBLE PRECISION array, dimension ( 2 )
            // *         The original eigenvalues.  We assume 0 <= D(1) < D(2).
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension ( 2 )
            // *         The components of the updating vector.
            // *
            // *  DELTA  (output) DOUBLE PRECISION array, dimension ( 2 )
            // *         Contains (D(j) - sigma_I) in its  j-th component.
            // *         The vector DELTA contains the information necessary
            // *         to construct the eigenvectors.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The scalar in the symmetric updating formula.
            // *
            // *  DSIGMA (output) DOUBLE PRECISION
            // *         The computed sigma_I, the I-th updated eigenvalue.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension ( 2 )
            // *         WORK contains (D(j) + sigma_I) in its  j-th component.
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
            DELSQ = DEL * (D[2 + o_d] + D[1 + o_d]);
            if (I == 1)
            {
                W = ONE + FOUR * RHO * (Z[2 + o_z] * Z[2 + o_z] / (D[1 + o_d] + THREE * D[2 + o_d]) - Z[1 + o_z] * Z[1 + o_z] / (THREE * D[1 + o_d] + D[2 + o_d])) / DEL;
                if (W > ZERO)
                {
                    B = DELSQ + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                    C = RHO * Z[1 + o_z] * Z[1 + o_z] * DELSQ;
                    // *
                    // *           B > ZERO, always
                    // *
                    // *           The following TAU is DSIGMA * DSIGMA - D( 1 ) * D( 1 )
                    // *
                    TAU = TWO * C / (B + Math.Sqrt(Math.Abs(B * B - FOUR * C)));
                    // *
                    // *           The following TAU is DSIGMA - D( 1 )
                    // *
                    TAU = TAU / (D[1 + o_d] + Math.Sqrt(D[1 + o_d] * D[1 + o_d] + TAU));
                    DSIGMA = D[1 + o_d] + TAU;
                    DELTA[1 + o_delta] =  - TAU;
                    DELTA[2 + o_delta] = DEL - TAU;
                    WORK[1 + o_work] = TWO * D[1 + o_d] + TAU;
                    WORK[2 + o_work] = (D[1 + o_d] + TAU) + D[2 + o_d];
                    // *           DELTA( 1 ) = -Z( 1 ) / TAU
                    // *           DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
                }
                else
                {
                    B =  - DELSQ + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                    C = RHO * Z[2 + o_z] * Z[2 + o_z] * DELSQ;
                    // *
                    // *           The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
                    // *
                    if (B > ZERO)
                    {
                        TAU =  - TWO * C / (B + Math.Sqrt(B * B + FOUR * C));
                    }
                    else
                    {
                        TAU = (B - Math.Sqrt(B * B + FOUR * C)) / TWO;
                    }
                    // *
                    // *           The following TAU is DSIGMA - D( 2 )
                    // *
                    TAU = TAU / (D[2 + o_d] + Math.Sqrt(Math.Abs(D[2 + o_d] * D[2 + o_d] + TAU)));
                    DSIGMA = D[2 + o_d] + TAU;
                    DELTA[1 + o_delta] =  - (DEL + TAU);
                    DELTA[2 + o_delta] =  - TAU;
                    WORK[1 + o_work] = D[1 + o_d] + TAU + D[2 + o_d];
                    WORK[2 + o_work] = TWO * D[2 + o_d] + TAU;
                    // *           DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
                    // *           DELTA( 2 ) = -Z( 2 ) / TAU
                }
                // *        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
                // *        DELTA( 1 ) = DELTA( 1 ) / TEMP
                // *        DELTA( 2 ) = DELTA( 2 ) / TEMP
            }
            else
            {
                // *
                // *        Now I=2
                // *
                B =  - DELSQ + RHO * (Z[1 + o_z] * Z[1 + o_z] + Z[2 + o_z] * Z[2 + o_z]);
                C = RHO * Z[2 + o_z] * Z[2 + o_z] * DELSQ;
                // *
                // *        The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
                // *
                if (B > ZERO)
                {
                    TAU = (B + Math.Sqrt(B * B + FOUR * C)) / TWO;
                }
                else
                {
                    TAU = TWO * C / ( - B + Math.Sqrt(B * B + FOUR * C));
                }
                // *
                // *        The following TAU is DSIGMA - D( 2 )
                // *
                TAU = TAU / (D[2 + o_d] + Math.Sqrt(D[2 + o_d] * D[2 + o_d] + TAU));
                DSIGMA = D[2 + o_d] + TAU;
                DELTA[1 + o_delta] =  - (DEL + TAU);
                DELTA[2 + o_delta] =  - TAU;
                WORK[1 + o_work] = D[1 + o_d] + TAU + D[2 + o_d];
                WORK[2 + o_work] = TWO * D[2 + o_d] + TAU;
                // *        DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
                // *        DELTA( 2 ) = -Z( 2 ) / TAU
                // *        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
                // *        DELTA( 1 ) = DELTA( 1 ) / TEMP
                // *        DELTA( 2 ) = DELTA( 2 ) / TEMP
            }
            return;
            // *
            // *     End of DLASD5
            // *

            #endregion

        }
    }
}
