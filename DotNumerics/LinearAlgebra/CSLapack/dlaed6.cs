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
    /// -- LAPACK routine (version 3.1.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// February 2007
    /// Purpose
    /// =======
    /// 
    /// DLAED6 computes the positive or negative root (closest to the origin)
    /// of
    /// z(1)        z(2)        z(3)
    /// f(x) =   rho + --------- + ---------- + ---------
    /// d(1)-x      d(2)-x      d(3)-x
    /// 
    /// It is assumed that
    /// 
    /// if ORGATI = .true. the root is between d(2) and d(3);
    /// otherwise it is between d(1) and d(2)
    /// 
    /// This routine will be called by DLAED4 when necessary. In most cases,
    /// the root sought is the smallest in magnitude, though it might not be
    /// in some extremely rare situations.
    /// 
    ///</summary>
    public class DLAED6
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const int MAXIT = 40; const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double THREE = 3.0E0;const double FOUR = 4.0E0; const double EIGHT = 8.0E0; double[] DSCALE = new double[3]; 
        double[] ZSCALE = new double[3];

        #endregion

        public DLAED6(DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
    
        public DLAED6()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED6 computes the positive or negative root (closest to the origin)
        /// of
        /// z(1)        z(2)        z(3)
        /// f(x) =   rho + --------- + ---------- + ---------
        /// d(1)-x      d(2)-x      d(3)-x
        /// 
        /// It is assumed that
        /// 
        /// if ORGATI = .true. the root is between d(2) and d(3);
        /// otherwise it is between d(1) and d(2)
        /// 
        /// This routine will be called by DLAED4 when necessary. In most cases,
        /// the root sought is the smallest in magnitude, though it might not be
        /// in some extremely rare situations.
        /// 
        ///</summary>
        /// <param name="KNITER">
        /// (input) INTEGER
        /// Refer to DLAED4 for its significance.
        ///</param>
        /// <param name="ORGATI">
        /// (input) LOGICAL
        /// If ORGATI is true, the needed root is between d(2) and
        /// d(3); otherwise it is between d(1) and d(2).  See
        /// DLAED4 for further details.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// Refer to the equation f(x) above.
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION array, dimension (3)
        /// D satisfies d(1) .LT. d(2) .LT. d(3).
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (3)
        /// Each of the elements in z must be positive.
        ///</param>
        /// <param name="FINIT">
        /// (input) DOUBLE PRECISION
        /// The value of f at 0. It is more accurate than the one
        /// evaluated inside this routine (if someone wants to do
        /// so).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION
        /// The root of the equation f(x).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .GT. 0: if INFO = 1, failure to converge
        ///</param>
        public void Run(int KNITER, bool ORGATI, double RHO, double[] D, int offset_d, double[] Z, int offset_z, double FINIT
                         , ref double TAU, ref int INFO)
        {

            #region Variables
            
            int o_dscale = -1; int o_zscale = -1; bool SCALE = false; int I = 0; 
            int ITER = 0;int NITER = 0; double A = 0; double B = 0; double BASE = 0; double C = 0; double DDF = 0; double DF = 0; 
            double EPS = 0;double ERRETM = 0; double ETA = 0; double F = 0; double FC = 0; double SCLFAC = 0; double SCLINV = 0; 
            double SMALL1 = 0;double SMALL2 = 0; double SMINV1 = 0; double SMINV2 = 0; double TEMP = 0; double TEMP1 = 0; 
            double TEMP2 = 0;double TEMP3 = 0; double TEMP4 = 0; double LBD = 0; double UBD = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     February 2007
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAED6 computes the positive or negative root (closest to the origin)
            // *  of
            // *                   z(1)        z(2)        z(3)
            // *  f(x) =   rho + --------- + ---------- + ---------
            // *                  d(1)-x      d(2)-x      d(3)-x
            // *
            // *  It is assumed that
            // *
            // *        if ORGATI = .true. the root is between d(2) and d(3);
            // *        otherwise it is between d(1) and d(2)
            // *
            // *  This routine will be called by DLAED4 when necessary. In most cases,
            // *  the root sought is the smallest in magnitude, though it might not be
            // *  in some extremely rare situations.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  KNITER       (input) INTEGER
            // *               Refer to DLAED4 for its significance.
            // *
            // *  ORGATI       (input) LOGICAL
            // *               If ORGATI is true, the needed root is between d(2) and
            // *               d(3); otherwise it is between d(1) and d(2).  See
            // *               DLAED4 for further details.
            // *
            // *  RHO          (input) DOUBLE PRECISION
            // *               Refer to the equation f(x) above.
            // *
            // *  D            (input) DOUBLE PRECISION array, dimension (3)
            // *               D satisfies d(1) < d(2) < d(3).
            // *
            // *  Z            (input) DOUBLE PRECISION array, dimension (3)
            // *               Each of the elements in z must be positive.
            // *
            // *  FINIT        (input) DOUBLE PRECISION
            // *               The value of f at 0. It is more accurate than the one
            // *               evaluated inside this routine (if someone wants to do
            // *               so).
            // *
            // *  TAU          (output) DOUBLE PRECISION
            // *               The root of the equation f(x).
            // *
            // *  INFO         (output) INTEGER
            // *               = 0: successful exit
            // *               > 0: if INFO = 1, failure to converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  30/06/99: Based on contributions by
            // *     Ren-Cang Li, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *
            // *  10/02/03: This version has a few statements commented out for thread
            // *  safety (machine parameters are computed on each entry). SJH.
            // *
            // *  05/10/06: Modified from a new version of Ren-Cang Li, use
            // *     Gragg-Thornton-Warner cubic convergent scheme for better stability.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (ORGATI)
            {
                LBD = D[2 + o_d];
                UBD = D[3 + o_d];
            }
            else
            {
                LBD = D[1 + o_d];
                UBD = D[2 + o_d];
            }
            if (FINIT < ZERO)
            {
                LBD = ZERO;
            }
            else
            {
                UBD = ZERO;
            }
            // *
            NITER = 1;
            TAU = ZERO;
            if (KNITER == 2)
            {
                if (ORGATI)
                {
                    TEMP = (D[3 + o_d] - D[2 + o_d]) / TWO;
                    C = RHO + Z[1 + o_z] / ((D[1 + o_d] - D[2 + o_d]) - TEMP);
                    A = C * (D[2 + o_d] + D[3 + o_d]) + Z[2 + o_z] + Z[3 + o_z];
                    B = C * D[2 + o_d] * D[3 + o_d] + Z[2 + o_z] * D[3 + o_d] + Z[3 + o_z] * D[2 + o_d];
                }
                else
                {
                    TEMP = (D[1 + o_d] - D[2 + o_d]) / TWO;
                    C = RHO + Z[3 + o_z] / ((D[3 + o_d] - D[2 + o_d]) - TEMP);
                    A = C * (D[1 + o_d] + D[2 + o_d]) + Z[1 + o_z] + Z[2 + o_z];
                    B = C * D[1 + o_d] * D[2 + o_d] + Z[1 + o_z] * D[2 + o_d] + Z[2 + o_z] * D[1 + o_d];
                }
                TEMP = Math.Max(Math.Abs(A), Math.Max(Math.Abs(B), Math.Abs(C)));
                A /= TEMP;
                B /= TEMP;
                C /= TEMP;
                if (C == ZERO)
                {
                    TAU = B / A;
                }
                else
                {
                    if (A <= ZERO)
                    {
                        TAU = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    else
                    {
                        TAU = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                }
                if (TAU < LBD || TAU > UBD) TAU = (LBD + UBD) / TWO;
                if (D[1 + o_d] == TAU || D[2 + o_d] == TAU || D[3 + o_d] == TAU)
                {
                    TAU = ZERO;
                }
                else
                {
                    TEMP = FINIT + TAU * Z[1 + o_z] / (D[1 + o_d] * (D[1 + o_d] - TAU)) + TAU * Z[2 + o_z] / (D[2 + o_d] * (D[2 + o_d] - TAU)) + TAU * Z[3 + o_z] / (D[3 + o_d] * (D[3 + o_d] - TAU));
                    if (TEMP <= ZERO)
                    {
                        LBD = TAU;
                    }
                    else
                    {
                        UBD = TAU;
                    }
                    if (Math.Abs(FINIT) <= Math.Abs(TEMP)) TAU = ZERO;
                }
            }
            // *
            // *     get machine parameters for possible scaling to avoid overflow
            // *
            // *     modified by Sven: parameters SMALL1, SMINV1, SMALL2,
            // *     SMINV2, EPS are not SAVEd anymore between one call to the
            // *     others but recomputed at each call
            // *
            EPS = this._dlamch.Run("Epsilon");
            BASE = this._dlamch.Run("Base");
            SMALL1 = Math.Pow(BASE,Convert.ToInt32(Math.Truncate(Math.Log(this._dlamch.Run("SafMin")) / Math.Log(BASE) / THREE)));
            SMINV1 = ONE / SMALL1;
            SMALL2 = SMALL1 * SMALL1;
            SMINV2 = SMINV1 * SMINV1;
            // *
            // *     Determine if scaling of inputs necessary to avoid overflow
            // *     when computing 1/TEMP**3
            // *
            if (ORGATI)
            {
                TEMP = Math.Min(Math.Abs(D[2 + o_d] - TAU), Math.Abs(D[3 + o_d] - TAU));
            }
            else
            {
                TEMP = Math.Min(Math.Abs(D[1 + o_d] - TAU), Math.Abs(D[2 + o_d] - TAU));
            }
            SCALE = false;
            if (TEMP <= SMALL1)
            {
                SCALE = true;
                if (TEMP <= SMALL2)
                {
                    // *
                    // *        Scale up by power of radix nearest 1/SAFMIN**(2/3)
                    // *
                    SCLFAC = SMINV2;
                    SCLINV = SMALL2;
                }
                else
                {
                    // *
                    // *        Scale up by power of radix nearest 1/SAFMIN**(1/3)
                    // *
                    SCLFAC = SMINV1;
                    SCLINV = SMALL1;
                }
                // *
                // *        Scaling up safe because D, Z, TAU scaled elsewhere to be O(1)
                // *
                for (I = 1; I <= 3; I++)
                {
                    DSCALE[I + o_dscale] = D[I + o_d] * SCLFAC;
                    ZSCALE[I + o_zscale] = Z[I + o_z] * SCLFAC;
                }
                TAU *= SCLFAC;
                LBD *= SCLFAC;
                UBD *= SCLFAC;
            }
            else
            {
                // *
                // *        Copy D and Z to DSCALE and ZSCALE
                // *
                for (I = 1; I <= 3; I++)
                {
                    DSCALE[I + o_dscale] = D[I + o_d];
                    ZSCALE[I + o_zscale] = Z[I + o_z];
                }
            }
            // *
            FC = ZERO;
            DF = ZERO;
            DDF = ZERO;
            for (I = 1; I <= 3; I++)
            {
                TEMP = ONE / (DSCALE[I + o_dscale] - TAU);
                TEMP1 = ZSCALE[I + o_zscale] * TEMP;
                TEMP2 = TEMP1 * TEMP;
                TEMP3 = TEMP2 * TEMP;
                FC += TEMP1 / DSCALE[I + o_dscale];
                DF += TEMP2;
                DDF += TEMP3;
            }
            F = FINIT + TAU * FC;
            // *
            if (Math.Abs(F) <= ZERO) goto LABEL60;
            if (F <= ZERO)
            {
                LBD = TAU;
            }
            else
            {
                UBD = TAU;
            }
            // *
            // *        Iteration begins -- Use Gragg-Thornton-Warner cubic convergent
            // *                            scheme
            // *
            // *     It is not hard to see that
            // *
            // *           1) Iterations will go up monotonically
            // *              if FINIT < 0;
            // *
            // *           2) Iterations will go down monotonically
            // *              if FINIT > 0.
            // *
            ITER = NITER + 1;
            // *
            for (NITER = ITER; NITER <= MAXIT; NITER++)
            {
                // *
                if (ORGATI)
                {
                    TEMP1 = DSCALE[2 + o_dscale] - TAU;
                    TEMP2 = DSCALE[3 + o_dscale] - TAU;
                }
                else
                {
                    TEMP1 = DSCALE[1 + o_dscale] - TAU;
                    TEMP2 = DSCALE[2 + o_dscale] - TAU;
                }
                A = (TEMP1 + TEMP2) * F - TEMP1 * TEMP2 * DF;
                B = TEMP1 * TEMP2 * F;
                C = F - (TEMP1 + TEMP2) * DF + TEMP1 * TEMP2 * DDF;
                TEMP = Math.Max(Math.Abs(A), Math.Max(Math.Abs(B), Math.Abs(C)));
                A /= TEMP;
                B /= TEMP;
                C /= TEMP;
                if (C == ZERO)
                {
                    ETA = B / A;
                }
                else
                {
                    if (A <= ZERO)
                    {
                        ETA = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    else
                    {
                        ETA = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                }
                if (F * ETA >= ZERO)
                {
                    ETA =  - F / DF;
                }
                // *
                TAU += ETA;
                if (TAU < LBD || TAU > UBD) TAU = (LBD + UBD) / TWO;
                // *
                FC = ZERO;
                ERRETM = ZERO;
                DF = ZERO;
                DDF = ZERO;
                for (I = 1; I <= 3; I++)
                {
                    TEMP = ONE / (DSCALE[I + o_dscale] - TAU);
                    TEMP1 = ZSCALE[I + o_zscale] * TEMP;
                    TEMP2 = TEMP1 * TEMP;
                    TEMP3 = TEMP2 * TEMP;
                    TEMP4 = TEMP1 / DSCALE[I + o_dscale];
                    FC += TEMP4;
                    ERRETM += Math.Abs(TEMP4);
                    DF += TEMP2;
                    DDF += TEMP3;
                }
                F = FINIT + TAU * FC;
                ERRETM = EIGHT * (Math.Abs(FINIT) + Math.Abs(TAU) * ERRETM) + Math.Abs(TAU) * DF;
                if (Math.Abs(F) <= EPS * ERRETM) goto LABEL60;
                if (F <= ZERO)
                {
                    LBD = TAU;
                }
                else
                {
                    UBD = TAU;
                }
            }
            INFO = 1;
        LABEL60:;
            // *
            // *     Undo scaling
            // *
            if (SCALE) TAU *= SCLINV;
            return;
            // *
            // *     End of DLAED6
            // *

            #endregion

        }
    }
}
