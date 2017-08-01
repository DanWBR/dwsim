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
    /// DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
    /// In case of failure it changes shifts, and tries again until output
    /// is positive.
    /// 
    ///</summary>
    public class DLAZQ3
    {
    

        #region Dependencies
        
        DLASQ5 _dlasq5; DLASQ6 _dlasq6; DLAZQ4 _dlazq4; DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const double CBIAS = 1.50E0; const double ZERO = 0.0E0; const double QURTR = 0.250E0; const double HALF = 0.5E0; 
        const double ONE = 1.0E0;const double TWO = 2.0E0; const double HUNDRD = 100.0E0; 

        #endregion

        public DLAZQ3(DLASQ5 dlasq5, DLASQ6 dlasq6, DLAZQ4 dlazq4, DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlasq5 = dlasq5; this._dlasq6 = dlasq6; this._dlazq4 = dlazq4; this._dlamch = dlamch; 

            #endregion

        }
    
        public DLAZQ3()
        {
    

            #region Dependencies (Initialization)
            
            DLASQ5 dlasq5 = new DLASQ5();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAZQ4 dlazq4 = new DLAZQ4();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASQ6 dlasq6 = new DLASQ6(dlamch);

            #endregion


            #region Set Dependencies
            
            this._dlasq5 = dlasq5; this._dlasq6 = dlasq6; this._dlazq4 = dlazq4; this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
        /// In case of failure it changes shifts, and tries again until output
        /// is positive.
        /// 
        ///</summary>
        /// <param name="I0">
        /// (input) INTEGER
        /// First index.
        ///</param>
        /// <param name="N0">
        /// (input) INTEGER
        /// Last index.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( 4*N )
        /// Z holds the qd array.
        ///</param>
        /// <param name="PP">
        /// (input) INTEGER
        /// PP=0 for ping, PP=1 for pong.
        ///</param>
        /// <param name="DMIN">
        /// (output) DOUBLE PRECISION
        /// Minimum value of d.
        ///</param>
        /// <param name="SIGMA">
        /// (output) DOUBLE PRECISION
        /// Sum of shifts used in current segment.
        ///</param>
        /// <param name="DESIG">
        /// (input/output) DOUBLE PRECISION
        /// Lower order part of SIGMA
        ///</param>
        /// <param name="QMAX">
        /// (input) DOUBLE PRECISION
        /// Maximum value of q.
        ///</param>
        /// <param name="NFAIL">
        /// (output) INTEGER
        /// Number of times shift was too big.
        ///</param>
        /// <param name="ITER">
        /// (output) INTEGER
        /// Number of iterations.
        ///</param>
        /// <param name="NDIV">
        /// (output) INTEGER
        /// Number of divisions.
        ///</param>
        /// <param name="IEEE">
        /// (input) LOGICAL
        /// Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
        ///</param>
        /// <param name="TTYPE">
        /// (input/output) INTEGER
        /// Shift type.  TTYPE is passed as an argument in order to save
        /// its value between calls to DLAZQ3
        ///</param>
        /// <param name="DMIN1">
        /// (input/output) REAL
        ///</param>
        /// <param name="DMIN2">
        /// (input/output) REAL
        ///</param>
        /// <param name="DN">
        /// (input/output) REAL
        ///</param>
        /// <param name="DN1">
        /// (input/output) REAL
        ///</param>
        /// <param name="DN2">
        /// (input/output) REAL
        ///</param>
        /// <param name="TAU">
        /// (input/output) REAL
        /// These are passed as arguments in order to save their values
        /// between calls to DLAZQ3
        ///</param>
        public void Run(int I0, ref int N0, ref double[] Z, int offset_z, int PP, ref double DMIN, ref double SIGMA
                         , ref double DESIG, ref double QMAX, ref int NFAIL, ref int ITER, ref int NDIV, bool IEEE
                         , ref int TTYPE, ref double DMIN1, ref double DMIN2, ref double DN, ref double DN1, ref double DN2
                         , ref double TAU)
        {

            #region Variables
            
            int IPN4 = 0; int J4 = 0; int N0IN = 0; int NN = 0; double EPS = 0; double G = 0; double S = 0; double SAFMIN = 0; 
            double T = 0;double TEMP = 0; double TOL = 0; double TOL2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_z = -1 + offset_z; 

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
            // *  DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
            // *  In case of failure it changes shifts, and tries again until output
            // *  is positive.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  I0     (input) INTEGER
            // *         First index.
            // *
            // *  N0     (input) INTEGER
            // *         Last index.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
            // *         Z holds the qd array.
            // *
            // *  PP     (input) INTEGER
            // *         PP=0 for ping, PP=1 for pong.
            // *
            // *  DMIN   (output) DOUBLE PRECISION
            // *         Minimum value of d.
            // *
            // *  SIGMA  (output) DOUBLE PRECISION
            // *         Sum of shifts used in current segment.
            // *
            // *  DESIG  (input/output) DOUBLE PRECISION
            // *         Lower order part of SIGMA
            // *
            // *  QMAX   (input) DOUBLE PRECISION
            // *         Maximum value of q.
            // *
            // *  NFAIL  (output) INTEGER
            // *         Number of times shift was too big.
            // *
            // *  ITER   (output) INTEGER
            // *         Number of iterations.
            // *
            // *  NDIV   (output) INTEGER
            // *         Number of divisions.
            // *
            // *  IEEE   (input) LOGICAL
            // *         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
            // *
            // *  TTYPE  (input/output) INTEGER
            // *         Shift type.  TTYPE is passed as an argument in order to save
            // *         its value between calls to DLAZQ3
            // *
            // *  DMIN1  (input/output) REAL
            // *  DMIN2  (input/output) REAL
            // *  DN     (input/output) REAL
            // *  DN1    (input/output) REAL
            // *  DN2    (input/output) REAL
            // *  TAU    (input/output) REAL
            // *         These are passed as arguments in order to save their values
            // *         between calls to DLAZQ3
            // *
            // *  This is a thread safe version of DLASQ3, which passes TTYPE, DMIN1,
            // *  DMIN2, DN, DN1. DN2 and TAU through the argument list in place of
            // *  declaring them in a SAVE statment.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Function ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            N0IN = N0;
            EPS = this._dlamch.Run("Precision");
            SAFMIN = this._dlamch.Run("Safe minimum");
            TOL = EPS * HUNDRD;
            TOL2 = Math.Pow(TOL,2);
            G = ZERO;
            // *
            // *     Check for deflation.
            // *
        LABEL10:;
            // *
            if (N0 < I0) return;
            if (N0 == I0) goto LABEL20;
            NN = 4 * N0 + PP;
            if (N0 == (I0 + 1)) goto LABEL40;
            // *
            // *     Check whether E(N0-1) is negligible, 1 eigenvalue.
            // *
            if (Z[NN - 5 + o_z] > TOL2 * (SIGMA + Z[NN - 3 + o_z]) && Z[NN - 2 * PP - 4 + o_z] > TOL2 * Z[NN - 7 + o_z]) goto LABEL30;
            // *
        LABEL20:;
            // *
            Z[4 * N0 - 3 + o_z] = Z[4 * N0 + PP - 3 + o_z] + SIGMA;
            N0 -= 1;
            goto LABEL10;
            // *
            // *     Check  whether E(N0-2) is negligible, 2 eigenvalues.
            // *
        LABEL30:;
            // *
            if (Z[NN - 9 + o_z] > TOL2 * SIGMA && Z[NN - 2 * PP - 8 + o_z] > TOL2 * Z[NN - 11 + o_z]) goto LABEL50;
            // *
        LABEL40:;
            // *
            if (Z[NN - 3 + o_z] > Z[NN - 7 + o_z])
            {
                S = Z[NN - 3 + o_z];
                Z[NN - 3 + o_z] = Z[NN - 7 + o_z];
                Z[NN - 7 + o_z] = S;
            }
            if (Z[NN - 5 + o_z] > Z[NN - 3 + o_z] * TOL2)
            {
                T = HALF * ((Z[NN - 7 + o_z] - Z[NN - 3 + o_z]) + Z[NN - 5 + o_z]);
                S = Z[NN - 3 + o_z] * (Z[NN - 5 + o_z] / T);
                if (S <= T)
                {
                    S = Z[NN - 3 + o_z] * (Z[NN - 5 + o_z] / (T * (ONE + Math.Sqrt(ONE + S / T))));
                }
                else
                {
                    S = Z[NN - 3 + o_z] * (Z[NN - 5 + o_z] / (T + Math.Sqrt(T) * Math.Sqrt(T + S)));
                }
                T = Z[NN - 7 + o_z] + (S + Z[NN - 5 + o_z]);
                Z[NN - 3 + o_z] = Z[NN - 3 + o_z] * (Z[NN - 7 + o_z] / T);
                Z[NN - 7 + o_z] = T;
            }
            Z[4 * N0 - 7 + o_z] = Z[NN - 7 + o_z] + SIGMA;
            Z[4 * N0 - 3 + o_z] = Z[NN - 3 + o_z] + SIGMA;
            N0 -= 2;
            goto LABEL10;
            // *
        LABEL50:;
            // *
            // *     Reverse the qd-array, if warranted.
            // *
            if (DMIN <= ZERO || N0 < N0IN)
            {
                if (CBIAS * Z[4 * I0 + PP - 3 + o_z] < Z[4 * N0 + PP - 3 + o_z])
                {
                    IPN4 = 4 * (I0 + N0);
                    for (J4 = 4 * I0; J4 <= 2 * (I0 + N0 - 1); J4 += 4)
                    {
                        TEMP = Z[J4 - 3 + o_z];
                        Z[J4 - 3 + o_z] = Z[IPN4 - J4 - 3 + o_z];
                        Z[IPN4 - J4 - 3 + o_z] = TEMP;
                        TEMP = Z[J4 - 2 + o_z];
                        Z[J4 - 2 + o_z] = Z[IPN4 - J4 - 2 + o_z];
                        Z[IPN4 - J4 - 2 + o_z] = TEMP;
                        TEMP = Z[J4 - 1 + o_z];
                        Z[J4 - 1 + o_z] = Z[IPN4 - J4 - 5 + o_z];
                        Z[IPN4 - J4 - 5 + o_z] = TEMP;
                        TEMP = Z[J4 + o_z];
                        Z[J4 + o_z] = Z[IPN4 - J4 - 4 + o_z];
                        Z[IPN4 - J4 - 4 + o_z] = TEMP;
                    }
                    if (N0 - I0 <= 4)
                    {
                        Z[4 * N0 + PP - 1 + o_z] = Z[4 * I0 + PP - 1 + o_z];
                        Z[4 * N0 - PP + o_z] = Z[4 * I0 - PP + o_z];
                    }
                    DMIN2 = Math.Min(DMIN2, Z[4 * N0 + PP - 1 + o_z]);
                    Z[4 * N0 + PP - 1 + o_z] = Math.Min(Z[4 * N0 + PP - 1 + o_z], Math.Min(Z[4 * I0 + PP - 1 + o_z], Z[4 * I0 + PP + 3 + o_z]));
                    Z[4 * N0 - PP + o_z] = Math.Min(Z[4 * N0 - PP + o_z], Math.Min(Z[4 * I0 - PP + o_z], Z[4 * I0 - PP + 4 + o_z]));
                    QMAX = Math.Max(QMAX, Math.Max(Z[4 * I0 + PP - 3 + o_z], Z[4 * I0 + PP + 1 + o_z]));
                    DMIN =  - ZERO;
                }
            }
            // *
            if (DMIN < ZERO || SAFMIN * QMAX < Math.Min(Z[4 * N0 + PP - 1 + o_z], Math.Min(Z[4 * N0 + PP - 9 + o_z], DMIN2 + Z[4 * N0 - PP + o_z])))
            {
                // *
                // *        Choose a shift.
                // *
                this._dlazq4.Run(I0, N0, Z, offset_z, PP, N0IN, DMIN
                                 , DMIN1, DMIN2, DN, DN1, DN2, ref TAU
                                 , ref TTYPE, ref G);
                // *
                // *        Call dqds until DMIN > 0.
                // *
            LABEL80:;
                // *
                this._dlasq5.Run(I0, N0, ref Z, offset_z, PP, TAU, ref DMIN
                                 , ref DMIN1, ref DMIN2, ref DN, ref DN1, ref DN2, IEEE);
                // *
                NDIV += (N0 - I0 + 2);
                ITER += 1;
                // *
                // *        Check status.
                // *
                if (DMIN >= ZERO && DMIN1 > ZERO)
                {
                    // *
                    // *           Success.
                    // *
                    goto LABEL100;
                    // *
                }
                else
                {
                    if (DMIN < ZERO && DMIN1 > ZERO && Z[4 * (N0 - 1) - PP + o_z] < TOL * (SIGMA + DN1) && Math.Abs(DN) < TOL * SIGMA)
                    {
                        // *
                        // *           Convergence hidden by negative DN.
                        // *
                        Z[4 * (N0 - 1) - PP + 2 + o_z] = ZERO;
                        DMIN = ZERO;
                        goto LABEL100;
                    }
                    else
                    {
                        if (DMIN < ZERO)
                        {
                            // *
                            // *           TAU too big. Select new TAU and try again.
                            // *
                            NFAIL += 1;
                            if (TTYPE <  - 22)
                            {
                                // *
                                // *              Failed twice. Play it safe.
                                // *
                                TAU = ZERO;
                            }
                            else
                            {
                                if (DMIN1 > ZERO)
                                {
                                    // *
                                    // *              Late failure. Gives excellent shift.
                                    // *
                                    TAU = (TAU + DMIN) * (ONE - TWO * EPS);
                                    TTYPE -= 11;
                                }
                                else
                                {
                                    // *
                                    // *              Early failure. Divide by 4.
                                    // *
                                    TAU *= QURTR;
                                    TTYPE -= 12;
                                }
                            }
                            goto LABEL80;
                        }
                        else
                        {
                            if (DMIN != DMIN)
                            {
                                // *
                                // *           NaN.
                                // *
                                TAU = ZERO;
                                goto LABEL80;
                            }
                            else
                            {
                                // *
                                // *           Possible underflow. Play it safe.
                                // *
                                goto LABEL90;
                            }
                        }
                    }
                }
            }
            // *
            // *     Risk of underflow.
            // *
        LABEL90:;
            this._dlasq6.Run(I0, N0, ref Z, offset_z, PP, ref DMIN, ref DMIN1
                             , ref DMIN2, ref DN, ref DN1, ref DN2);
            NDIV += (N0 - I0 + 2);
            ITER += 1;
            TAU = ZERO;
            // *
        LABEL100:;
            if (TAU < SIGMA)
            {
                DESIG += TAU;
                T = SIGMA + DESIG;
                DESIG +=  - (T - SIGMA);
            }
            else
            {
                T = SIGMA + TAU;
                DESIG = SIGMA - (T - TAU) + DESIG;
            }
            SIGMA = T;
            // *
            return;
            // *
            // *     End of DLAZQ3
            // *

            #endregion

        }
    }
}
