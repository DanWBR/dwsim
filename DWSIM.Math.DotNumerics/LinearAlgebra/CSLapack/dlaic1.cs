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
    /// DLAIC1 applies one step of incremental condition estimation in
    /// its simplest version:
    /// 
    /// Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
    /// lower triangular matrix L, such that
    /// twonorm(L*x) = sest
    /// Then DLAIC1 computes sestpr, s, c such that
    /// the vector
    /// [ s*x ]
    /// xhat = [  c  ]
    /// is an approximate singular vector of
    /// [ L     0  ]
    /// Lhat = [ w' gamma ]
    /// in the sense that
    /// twonorm(Lhat*xhat) = sestpr.
    /// 
    /// Depending on JOB, an estimate for the largest or smallest singular
    /// value is computed.
    /// 
    /// Note that [s c]' and sestpr**2 is an eigenpair of the system
    /// 
    /// diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
    /// [ gamma ]
    /// 
    /// where  alpha =  x'*w.
    /// 
    ///</summary>
    public class DLAIC1
    {
    

        #region Dependencies
        
        DDOT _ddot; DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; const double HALF = 0.5E0; 
        const double FOUR = 4.0E0;

        #endregion

        public DLAIC1(DDOT ddot, DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._ddot = ddot; this._dlamch = dlamch; 

            #endregion

        }
    
        public DLAIC1()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._ddot = ddot; this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAIC1 applies one step of incremental condition estimation in
        /// its simplest version:
        /// 
        /// Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
        /// lower triangular matrix L, such that
        /// twonorm(L*x) = sest
        /// Then DLAIC1 computes sestpr, s, c such that
        /// the vector
        /// [ s*x ]
        /// xhat = [  c  ]
        /// is an approximate singular vector of
        /// [ L     0  ]
        /// Lhat = [ w' gamma ]
        /// in the sense that
        /// twonorm(Lhat*xhat) = sestpr.
        /// 
        /// Depending on JOB, an estimate for the largest or smallest singular
        /// value is computed.
        /// 
        /// Note that [s c]' and sestpr**2 is an eigenpair of the system
        /// 
        /// diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
        /// [ gamma ]
        /// 
        /// where  alpha =  x'*w.
        /// 
        ///</summary>
        /// <param name="JOB">
        /// (input) INTEGER
        /// = 1: an estimate for the largest singular value is computed.
        /// = 2: an estimate for the smallest singular value is computed.
        ///</param>
        /// <param name="J">
        /// (input) INTEGER
        /// Length of X and W
        ///</param>
        /// <param name="X">
        /// (input) DOUBLE PRECISION array, dimension (J)
        /// The j-vector x.
        ///</param>
        /// <param name="SEST">
        /// (input) DOUBLE PRECISION
        /// Estimated singular value of j by j matrix L
        ///</param>
        /// <param name="W">
        /// (input) DOUBLE PRECISION array, dimension (J)
        /// The j-vector w.
        ///</param>
        /// <param name="GAMMA">
        /// (input) DOUBLE PRECISION
        /// The diagonal element gamma.
        ///</param>
        /// <param name="SESTPR">
        /// (output) DOUBLE PRECISION
        /// Estimated singular value of (j+1) by (j+1) matrix Lhat.
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION
        /// Sine needed in forming xhat.
        ///</param>
        /// <param name="C">
        /// (output) DOUBLE PRECISION
        /// Cosine needed in forming xhat.
        ///</param>
        public void Run(int JOB, int J, double[] X, int offset_x, double SEST, double[] W, int offset_w, double GAMMA
                         , ref double SESTPR, ref double S, ref double C)
        {

            #region Variables
            
            double ABSALP = 0; double ABSEST = 0; double ABSGAM = 0; double ALPHA = 0; double B = 0; double COSINE = 0; 
            double EPS = 0;double NORMA = 0; double S1 = 0; double S2 = 0; double SINE = 0; double T = 0; double TEST = 0; 
            double TMP = 0;double ZETA1 = 0; double ZETA2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_w = -1 + offset_w; 

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
            // *  DLAIC1 applies one step of incremental condition estimation in
            // *  its simplest version:
            // *
            // *  Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
            // *  lower triangular matrix L, such that
            // *           twonorm(L*x) = sest
            // *  Then DLAIC1 computes sestpr, s, c such that
            // *  the vector
            // *                  [ s*x ]
            // *           xhat = [  c  ]
            // *  is an approximate singular vector of
            // *                  [ L     0  ]
            // *           Lhat = [ w' gamma ]
            // *  in the sense that
            // *           twonorm(Lhat*xhat) = sestpr.
            // *
            // *  Depending on JOB, an estimate for the largest or smallest singular
            // *  value is computed.
            // *
            // *  Note that [s c]' and sestpr**2 is an eigenpair of the system
            // *
            // *      diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
            // *                                            [ gamma ]
            // *
            // *  where  alpha =  x'*w.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOB     (input) INTEGER
            // *          = 1: an estimate for the largest singular value is computed.
            // *          = 2: an estimate for the smallest singular value is computed.
            // *
            // *  J       (input) INTEGER
            // *          Length of X and W
            // *
            // *  X       (input) DOUBLE PRECISION array, dimension (J)
            // *          The j-vector x.
            // *
            // *  SEST    (input) DOUBLE PRECISION
            // *          Estimated singular value of j by j matrix L
            // *
            // *  W       (input) DOUBLE PRECISION array, dimension (J)
            // *          The j-vector w.
            // *
            // *  GAMMA   (input) DOUBLE PRECISION
            // *          The diagonal element gamma.
            // *
            // *  SESTPR  (output) DOUBLE PRECISION
            // *          Estimated singular value of (j+1) by (j+1) matrix Lhat.
            // *
            // *  S       (output) DOUBLE PRECISION
            // *          Sine needed in forming xhat.
            // *
            // *  C       (output) DOUBLE PRECISION
            // *          Cosine needed in forming xhat.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, SIGN, SQRT;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            EPS = this._dlamch.Run("Epsilon");
            ALPHA = this._ddot.Run(J, X, offset_x, 1, W, offset_w, 1);
            // *
            ABSALP = Math.Abs(ALPHA);
            ABSGAM = Math.Abs(GAMMA);
            ABSEST = Math.Abs(SEST);
            // *
            if (JOB == 1)
            {
                // *
                // *        Estimating largest singular value
                // *
                // *        special cases
                // *
                if (SEST == ZERO)
                {
                    S1 = Math.Max(ABSGAM, ABSALP);
                    if (S1 == ZERO)
                    {
                        S = ZERO;
                        C = ONE;
                        SESTPR = ZERO;
                    }
                    else
                    {
                        S = ALPHA / S1;
                        C = GAMMA / S1;
                        TMP = Math.Sqrt(S * S + C * C);
                        S /= TMP;
                        C /= TMP;
                        SESTPR = S1 * TMP;
                    }
                    return;
                }
                else
                {
                    if (ABSGAM <= EPS * ABSEST)
                    {
                        S = ONE;
                        C = ZERO;
                        TMP = Math.Max(ABSEST, ABSALP);
                        S1 = ABSEST / TMP;
                        S2 = ABSALP / TMP;
                        SESTPR = TMP * Math.Sqrt(S1 * S1 + S2 * S2);
                        return;
                    }
                    else
                    {
                        if (ABSALP <= EPS * ABSEST)
                        {
                            S1 = ABSGAM;
                            S2 = ABSEST;
                            if (S1 <= S2)
                            {
                                S = ONE;
                                C = ZERO;
                                SESTPR = S2;
                            }
                            else
                            {
                                S = ZERO;
                                C = ONE;
                                SESTPR = S1;
                            }
                            return;
                        }
                        else
                        {
                            if (ABSEST <= EPS * ABSALP || ABSEST <= EPS * ABSGAM)
                            {
                                S1 = ABSGAM;
                                S2 = ABSALP;
                                if (S1 <= S2)
                                {
                                    TMP = S1 / S2;
                                    S = Math.Sqrt(ONE + TMP * TMP);
                                    SESTPR = S2 * S;
                                    C = (GAMMA / S2) / S;
                                    S = FortranLib.Sign(ONE,ALPHA) / S;
                                }
                                else
                                {
                                    TMP = S2 / S1;
                                    C = Math.Sqrt(ONE + TMP * TMP);
                                    SESTPR = S1 * C;
                                    S = (ALPHA / S1) / C;
                                    C = FortranLib.Sign(ONE,GAMMA) / C;
                                }
                                return;
                            }
                            else
                            {
                                // *
                                // *           normal case
                                // *
                                ZETA1 = ALPHA / ABSEST;
                                ZETA2 = GAMMA / ABSEST;
                                // *
                                B = (ONE - ZETA1 * ZETA1 - ZETA2 * ZETA2) * HALF;
                                C = ZETA1 * ZETA1;
                                if (B > ZERO)
                                {
                                    T = C / (B + Math.Sqrt(B * B + C));
                                }
                                else
                                {
                                    T = Math.Sqrt(B * B + C) - B;
                                }
                                // *
                                SINE =  - ZETA1 / T;
                                COSINE =  - ZETA2 / (ONE + T);
                                TMP = Math.Sqrt(SINE * SINE + COSINE * COSINE);
                                S = SINE / TMP;
                                C = COSINE / TMP;
                                SESTPR = Math.Sqrt(T + ONE) * ABSEST;
                                return;
                            }
                        }
                    }
                }
                // *
            }
            else
            {
                if (JOB == 2)
                {
                    // *
                    // *        Estimating smallest singular value
                    // *
                    // *        special cases
                    // *
                    if (SEST == ZERO)
                    {
                        SESTPR = ZERO;
                        if (Math.Max(ABSGAM, ABSALP) == ZERO)
                        {
                            SINE = ONE;
                            COSINE = ZERO;
                        }
                        else
                        {
                            SINE =  - GAMMA;
                            COSINE = ALPHA;
                        }
                        S1 = Math.Max(Math.Abs(SINE), Math.Abs(COSINE));
                        S = SINE / S1;
                        C = COSINE / S1;
                        TMP = Math.Sqrt(S * S + C * C);
                        S /= TMP;
                        C /= TMP;
                        return;
                    }
                    else
                    {
                        if (ABSGAM <= EPS * ABSEST)
                        {
                            S = ZERO;
                            C = ONE;
                            SESTPR = ABSGAM;
                            return;
                        }
                        else
                        {
                            if (ABSALP <= EPS * ABSEST)
                            {
                                S1 = ABSGAM;
                                S2 = ABSEST;
                                if (S1 <= S2)
                                {
                                    S = ZERO;
                                    C = ONE;
                                    SESTPR = S1;
                                }
                                else
                                {
                                    S = ONE;
                                    C = ZERO;
                                    SESTPR = S2;
                                }
                                return;
                            }
                            else
                            {
                                if (ABSEST <= EPS * ABSALP || ABSEST <= EPS * ABSGAM)
                                {
                                    S1 = ABSGAM;
                                    S2 = ABSALP;
                                    if (S1 <= S2)
                                    {
                                        TMP = S1 / S2;
                                        C = Math.Sqrt(ONE + TMP * TMP);
                                        SESTPR = ABSEST * (TMP / C);
                                        S =  - (GAMMA / S2) / C;
                                        C = FortranLib.Sign(ONE,ALPHA) / C;
                                    }
                                    else
                                    {
                                        TMP = S2 / S1;
                                        S = Math.Sqrt(ONE + TMP * TMP);
                                        SESTPR = ABSEST / S;
                                        C = (ALPHA / S1) / S;
                                        S =  - FortranLib.Sign(ONE,GAMMA) / S;
                                    }
                                    return;
                                }
                                else
                                {
                                    // *
                                    // *           normal case
                                    // *
                                    ZETA1 = ALPHA / ABSEST;
                                    ZETA2 = GAMMA / ABSEST;
                                    // *
                                    NORMA = Math.Max(ONE + ZETA1 * ZETA1 + Math.Abs(ZETA1 * ZETA2), Math.Abs(ZETA1 * ZETA2) + ZETA2 * ZETA2);
                                    // *
                                    // *           See if root is closer to zero or to ONE
                                    // *
                                    TEST = ONE + TWO * (ZETA1 - ZETA2) * (ZETA1 + ZETA2);
                                    if (TEST >= ZERO)
                                    {
                                        // *
                                        // *              root is close to zero, compute directly
                                        // *
                                        B = (ZETA1 * ZETA1 + ZETA2 * ZETA2 + ONE) * HALF;
                                        C = ZETA2 * ZETA2;
                                        T = C / (B + Math.Sqrt(Math.Abs(B * B - C)));
                                        SINE = ZETA1 / (ONE - T);
                                        COSINE =  - ZETA2 / T;
                                        SESTPR = Math.Sqrt(T + FOUR * EPS * EPS * NORMA) * ABSEST;
                                    }
                                    else
                                    {
                                        // *
                                        // *              root is closer to ONE, shift by that amount
                                        // *
                                        B = (ZETA2 * ZETA2 + ZETA1 * ZETA1 - ONE) * HALF;
                                        C = ZETA1 * ZETA1;
                                        if (B >= ZERO)
                                        {
                                            T =  - C / (B + Math.Sqrt(B * B + C));
                                        }
                                        else
                                        {
                                            T = B - Math.Sqrt(B * B + C);
                                        }
                                        SINE =  - ZETA1 / T;
                                        COSINE =  - ZETA2 / (ONE + T);
                                        SESTPR = Math.Sqrt(ONE + T + FOUR * EPS * EPS * NORMA) * ABSEST;
                                    }
                                    TMP = Math.Sqrt(SINE * SINE + COSINE * COSINE);
                                    S = SINE / TMP;
                                    C = COSINE / TMP;
                                    return;
                                    // *
                                }
                            }
                        }
                    }
                }
            }
            return;
            // *
            // *     End of DLAIC1
            // *

            #endregion

        }
    }
}
