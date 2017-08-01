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
    /// DLASV2 computes the singular value decomposition of a 2-by-2
    /// triangular matrix
    /// [  F   G  ]
    /// [  0   H  ].
    /// On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
    /// smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
    /// right singular vectors for abs(SSMAX), giving the decomposition
    /// 
    /// [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
    /// [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
    /// 
    ///</summary>
    public class DLASV2
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double HALF = 0.5E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double FOUR = 4.0E0;

        #endregion

        public DLASV2(DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
    
        public DLASV2()
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
        /// DLASV2 computes the singular value decomposition of a 2-by-2
        /// triangular matrix
        /// [  F   G  ]
        /// [  0   H  ].
        /// On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
        /// smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
        /// right singular vectors for abs(SSMAX), giving the decomposition
        /// 
        /// [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
        /// [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
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
        /// abs(SSMIN) is the smaller singular value.
        ///</param>
        /// <param name="SSMAX">
        /// (output) DOUBLE PRECISION
        /// abs(SSMAX) is the larger singular value.
        ///</param>
        /// <param name="SNR">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="CSR">
        /// (output) DOUBLE PRECISION
        /// The vector (CSR, SNR) is a unit right singular vector for the
        /// singular value abs(SSMAX).
        ///</param>
        /// <param name="SNL">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="CSL">
        /// (output) DOUBLE PRECISION
        /// The vector (CSL, SNL) is a unit left singular vector for the
        /// singular value abs(SSMAX).
        ///</param>
        public void Run(double F, double G, double H, ref double SSMIN, ref double SSMAX, ref double SNR
                         , ref double CSR, ref double SNL, ref double CSL)
        {

            #region Variables
            
            bool GASMAL = false; bool SWAP = false; int PMAX = 0; double A = 0; double CLT = 0; double CRT = 0; double D = 0; 
            double FA = 0;double FT = 0; double GA = 0; double GT = 0; double HA = 0; double HT = 0; double L = 0; double M = 0; 
            double MM = 0;double R = 0; double S = 0; double SLT = 0; double SRT = 0; double T = 0; double TEMP = 0; 
            double TSIGN = 0;double TT = 0; 

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
            // *  DLASV2 computes the singular value decomposition of a 2-by-2
            // *  triangular matrix
            // *     [  F   G  ]
            // *     [  0   H  ].
            // *  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
            // *  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
            // *  right singular vectors for abs(SSMAX), giving the decomposition
            // *
            // *     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
            // *     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
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
            // *          abs(SSMIN) is the smaller singular value.
            // *
            // *  SSMAX   (output) DOUBLE PRECISION
            // *          abs(SSMAX) is the larger singular value.
            // *
            // *  SNL     (output) DOUBLE PRECISION
            // *  CSL     (output) DOUBLE PRECISION
            // *          The vector (CSL, SNL) is a unit left singular vector for the
            // *          singular value abs(SSMAX).
            // *
            // *  SNR     (output) DOUBLE PRECISION
            // *  CSR     (output) DOUBLE PRECISION
            // *          The vector (CSR, SNR) is a unit right singular vector for the
            // *          singular value abs(SSMAX).
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Any input parameter may be aliased with any output parameter.
            // *
            // *  Barring over/underflow and assuming a guard digit in subtraction, all
            // *  output quantities are correct to within a few units in the last
            // *  place (ulps).
            // *
            // *  In IEEE arithmetic, the code works correctly if one matrix element is
            // *  infinite.
            // *
            // *  Overflow will not occur unless the largest singular value itself
            // *  overflows or is within a few ulps of overflow. (On machines with
            // *  partial overflow, like the Cray, overflow may occur if the largest
            // *  singular value is within a factor of 2 of overflow.)
            // *
            // *  Underflow is harmless if underflow is gradual. Otherwise, results
            // *  may correspond to a matrix modified by perturbations of size near
            // *  the underflow threshold.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SIGN, SQRT;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            FT = F;
            FA = Math.Abs(FT);
            HT = H;
            HA = Math.Abs(H);
            // *
            // *     PMAX points to the maximum absolute element of matrix
            // *       PMAX = 1 if F largest in absolute values
            // *       PMAX = 2 if G largest in absolute values
            // *       PMAX = 3 if H largest in absolute values
            // *
            PMAX = 1;
            SWAP = (HA > FA);
            if (SWAP)
            {
                PMAX = 3;
                TEMP = FT;
                FT = HT;
                HT = TEMP;
                TEMP = FA;
                FA = HA;
                HA = TEMP;
                // *
                // *        Now FA .ge. HA
                // *
            }
            GT = G;
            GA = Math.Abs(GT);
            if (GA == ZERO)
            {
                // *
                // *        Diagonal matrix
                // *
                SSMIN = HA;
                SSMAX = FA;
                CLT = ONE;
                CRT = ONE;
                SLT = ZERO;
                SRT = ZERO;
            }
            else
            {
                GASMAL = true;
                if (GA > FA)
                {
                    PMAX = 2;
                    if ((FA / GA) < this._dlamch.Run("EPS"))
                    {
                        // *
                        // *              Case of very large GA
                        // *
                        GASMAL = false;
                        SSMAX = GA;
                        if (HA > ONE)
                        {
                            SSMIN = FA / (GA / HA);
                        }
                        else
                        {
                            SSMIN = (FA / GA) * HA;
                        }
                        CLT = ONE;
                        SLT = HT / GT;
                        SRT = ONE;
                        CRT = FT / GT;
                    }
                }
                if (GASMAL)
                {
                    // *
                    // *           Normal case
                    // *
                    D = FA - HA;
                    if (D == FA)
                    {
                        // *
                        // *              Copes with infinite F or H
                        // *
                        L = ONE;
                    }
                    else
                    {
                        L = D / FA;
                    }
                    // *
                    // *           Note that 0 .le. L .le. 1
                    // *
                    M = GT / FT;
                    // *
                    // *           Note that abs(M) .le. 1/macheps
                    // *
                    T = TWO - L;
                    // *
                    // *           Note that T .ge. 1
                    // *
                    MM = M * M;
                    TT = T * T;
                    S = Math.Sqrt(TT + MM);
                    // *
                    // *           Note that 1 .le. S .le. 1 + 1/macheps
                    // *
                    if (L == ZERO)
                    {
                        R = Math.Abs(M);
                    }
                    else
                    {
                        R = Math.Sqrt(L * L + MM);
                    }
                    // *
                    // *           Note that 0 .le. R .le. 1 + 1/macheps
                    // *
                    A = HALF * (S + R);
                    // *
                    // *           Note that 1 .le. A .le. 1 + abs(M)
                    // *
                    SSMIN = HA / A;
                    SSMAX = FA * A;
                    if (MM == ZERO)
                    {
                        // *
                        // *              Note that M is very tiny
                        // *
                        if (L == ZERO)
                        {
                            T = FortranLib.Sign(TWO,FT) * FortranLib.Sign(ONE,GT);
                        }
                        else
                        {
                            T = GT / FortranLib.Sign(D,FT) + M / T;
                        }
                    }
                    else
                    {
                        T = (M / (S + T) + M / (R + L)) * (ONE + A);
                    }
                    L = Math.Sqrt(T * T + FOUR);
                    CRT = TWO / L;
                    SRT = T / L;
                    CLT = (CRT + SRT * M) / A;
                    SLT = (HT / FT) * SRT / A;
                }
            }
            if (SWAP)
            {
                CSL = SRT;
                SNL = CRT;
                CSR = SLT;
                SNR = CLT;
            }
            else
            {
                CSL = CLT;
                SNL = SLT;
                CSR = CRT;
                SNR = SRT;
            }
            // *
            // *     Correct signs of SSMAX and SSMIN
            // *
            if (PMAX == 1) TSIGN = FortranLib.Sign(ONE,CSR) * FortranLib.Sign(ONE,CSL) * FortranLib.Sign(ONE,F);
            if (PMAX == 2) TSIGN = FortranLib.Sign(ONE,SNR) * FortranLib.Sign(ONE,CSL) * FortranLib.Sign(ONE,G);
            if (PMAX == 3) TSIGN = FortranLib.Sign(ONE,SNR) * FortranLib.Sign(ONE,SNL) * FortranLib.Sign(ONE,H);
            SSMAX = FortranLib.Sign(SSMAX,TSIGN);
            SSMIN = FortranLib.Sign(SSMIN,TSIGN * FortranLib.Sign(ONE,F) * FortranLib.Sign(ONE,H));
            return;
            // *
            // *     End of DLASV2
            // *

            #endregion

        }
    }
}
