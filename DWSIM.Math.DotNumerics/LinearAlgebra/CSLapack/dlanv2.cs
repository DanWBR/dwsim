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
    /// -- LAPACK driver routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
    /// matrix in standard form:
    /// 
    /// [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
    /// [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
    /// 
    /// where either
    /// 1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
    /// 2) AA = DD and BB*CC .LT. 0, so that AA + or - sqrt(BB*CC) are complex
    /// conjugate eigenvalues.
    /// 
    ///</summary>
    public class DLANV2
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLAPY2 _dlapy2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double HALF = 0.5E+0; const double ONE = 1.0E+0; const double MULTPL = 4.0E+0; 

        #endregion

        public DLANV2(DLAMCH dlamch, DLAPY2 dlapy2)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; 

            #endregion

        }
    
        public DLANV2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
        /// matrix in standard form:
        /// 
        /// [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
        /// [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
        /// 
        /// where either
        /// 1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
        /// 2) AA = DD and BB*CC .LT. 0, so that AA + or - sqrt(BB*CC) are complex
        /// conjugate eigenvalues.
        /// 
        ///</summary>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the elements of the input matrix.
        /// On exit, they are overwritten by the elements of the
        /// standardised Schur form.
        ///</param>
        /// <param name="RT1R">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="RT1I">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="RT2R">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="RT2I">
        /// (output) DOUBLE PRECISION
        /// The real and imaginary parts of the eigenvalues. If the
        /// eigenvalues are a complex conjugate pair, RT1I .GT. 0.
        ///</param>
        /// <param name="CS">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="SN">
        /// (output) DOUBLE PRECISION
        /// Parameters of the rotation matrix.
        ///</param>
        public void Run(ref double A, ref double B, ref double C, ref double D, ref double RT1R, ref double RT1I
                         , ref double RT2R, ref double RT2I, ref double CS, ref double SN)
        {

            #region Variables
            
            double AA = 0; double BB = 0; double BCMAX = 0; double BCMIS = 0; double CC = 0; double CS1 = 0; double DD = 0; 
            double EPS = 0;double P = 0; double SAB = 0; double SAC = 0; double SCALE = 0; double SIGMA = 0; double SN1 = 0; 
            double TAU = 0;double TEMP = 0; double Z = 0; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK driver routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
            // *  matrix in standard form:
            // *
            // *       [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
            // *       [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
            // *
            // *  where either
            // *  1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
            // *  2) AA = DD and BB*CC < 0, so that AA + or - sqrt(BB*CC) are complex
            // *  conjugate eigenvalues.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  A       (input/output) DOUBLE PRECISION
            // *  B       (input/output) DOUBLE PRECISION
            // *  C       (input/output) DOUBLE PRECISION
            // *  D       (input/output) DOUBLE PRECISION
            // *          On entry, the elements of the input matrix.
            // *          On exit, they are overwritten by the elements of the
            // *          standardised Schur form.
            // *
            // *  RT1R    (output) DOUBLE PRECISION
            // *  RT1I    (output) DOUBLE PRECISION
            // *  RT2R    (output) DOUBLE PRECISION
            // *  RT2I    (output) DOUBLE PRECISION
            // *          The real and imaginary parts of the eigenvalues. If the
            // *          eigenvalues are a complex conjugate pair, RT1I > 0.
            // *
            // *  CS      (output) DOUBLE PRECISION
            // *  SN      (output) DOUBLE PRECISION
            // *          Parameters of the rotation matrix.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Modified by V. Sima, Research Institute for Informatics, Bucharest,
            // *  Romania, to reduce the risk of cancellation errors,
            // *  when computing real eigenvalues, and to ensure, if possible, that
            // *  abs(RT1R) >= abs(RT2R).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            EPS = this._dlamch.Run("P");
            if (C == ZERO)
            {
                CS = ONE;
                SN = ZERO;
                goto LABEL10;
                // *
            }
            else
            {
                if (B == ZERO)
                {
                    // *
                    // *        Swap rows and columns
                    // *
                    CS = ZERO;
                    SN = ONE;
                    TEMP = D;
                    D = A;
                    A = TEMP;
                    B =  - C;
                    C = ZERO;
                    goto LABEL10;
                }
                else
                {
                    if ((A - D) == ZERO && FortranLib.Sign(ONE,B) != FortranLib.Sign(ONE,C))
                    {
                        CS = ONE;
                        SN = ZERO;
                        goto LABEL10;
                    }
                    else
                    {
                        // *
                        TEMP = A - D;
                        P = HALF * TEMP;
                        BCMAX = Math.Max(Math.Abs(B), Math.Abs(C));
                        BCMIS = Math.Min(Math.Abs(B), Math.Abs(C)) * FortranLib.Sign(ONE,B) * FortranLib.Sign(ONE,C);
                        SCALE = Math.Max(Math.Abs(P), BCMAX);
                        Z = (P / SCALE) * P + (BCMAX / SCALE) * BCMIS;
                        // *
                        // *        If Z is of the order of the machine accuracy, postpone the
                        // *        decision on the nature of eigenvalues
                        // *
                        if (Z >= MULTPL * EPS)
                        {
                            // *
                            // *           Real eigenvalues. Compute A and D.
                            // *
                            Z = P + FortranLib.Sign(Math.Sqrt(SCALE) * Math.Sqrt(Z),P);
                            A = D + Z;
                            D +=  - (BCMAX / Z) * BCMIS;
                            // *
                            // *           Compute B and the rotation matrix
                            // *
                            TAU = this._dlapy2.Run(C, Z);
                            CS = Z / TAU;
                            SN = C / TAU;
                            B -= C;
                            C = ZERO;
                        }
                        else
                        {
                            // *
                            // *           Complex eigenvalues, or real (almost) equal eigenvalues.
                            // *           Make diagonal elements equal.
                            // *
                            SIGMA = B + C;
                            TAU = this._dlapy2.Run(SIGMA, TEMP);
                            CS = Math.Sqrt(HALF * (ONE + Math.Abs(SIGMA) / TAU));
                            SN =  - (P / (TAU * CS)) * FortranLib.Sign(ONE,SIGMA);
                            // *
                            // *           Compute [ AA  BB ] = [ A  B ] [ CS -SN ]
                            // *                   [ CC  DD ]   [ C  D ] [ SN  CS ]
                            // *
                            AA = A * CS + B * SN;
                            BB =  - A * SN + B * CS;
                            CC = C * CS + D * SN;
                            DD =  - C * SN + D * CS;
                            // *
                            // *           Compute [ A  B ] = [ CS  SN ] [ AA  BB ]
                            // *                   [ C  D ]   [-SN  CS ] [ CC  DD ]
                            // *
                            A = AA * CS + CC * SN;
                            B = BB * CS + DD * SN;
                            C =  - AA * SN + CC * CS;
                            D =  - BB * SN + DD * CS;
                            // *
                            TEMP = HALF * (A + D);
                            A = TEMP;
                            D = TEMP;
                            // *
                            if (C != ZERO)
                            {
                                if (B != ZERO)
                                {
                                    if (FortranLib.Sign(ONE,B) == FortranLib.Sign(ONE,C))
                                    {
                                        // *
                                        // *                    Real eigenvalues: reduce to upper triangular form
                                        // *
                                        SAB = Math.Sqrt(Math.Abs(B));
                                        SAC = Math.Sqrt(Math.Abs(C));
                                        P = FortranLib.Sign(SAB * SAC,C);
                                        TAU = ONE / Math.Sqrt(Math.Abs(B + C));
                                        A = TEMP + P;
                                        D = TEMP - P;
                                        B -= C;
                                        C = ZERO;
                                        CS1 = SAB * TAU;
                                        SN1 = SAC * TAU;
                                        TEMP = CS * CS1 - SN * SN1;
                                        SN = CS * SN1 + SN * CS1;
                                        CS = TEMP;
                                    }
                                }
                                else
                                {
                                    B =  - C;
                                    C = ZERO;
                                    TEMP = CS;
                                    CS =  - SN;
                                    SN = TEMP;
                                }
                            }
                        }
                        // *
                    }
                }
            }
            // *
        LABEL10:;
            // *
            // *     Store eigenvalues in (RT1R,RT1I) and (RT2R,RT2I).
            // *
            RT1R = A;
            RT2R = D;
            if (C == ZERO)
            {
                RT1I = ZERO;
                RT2I = ZERO;
            }
            else
            {
                RT1I = Math.Sqrt(Math.Abs(B)) * Math.Sqrt(Math.Abs(C));
                RT2I =  - RT1I;
            }
            return;
            // *
            // *     End of DLANV2
            // *

            #endregion

        }
    }
}
