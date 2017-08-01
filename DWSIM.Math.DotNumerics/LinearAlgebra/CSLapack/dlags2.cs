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
    /// DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
    /// that if ( UPPER ) then
    /// 
    /// U'*A*Q = U'*( A1 A2 )*Q = ( x  0  )
    /// ( 0  A3 )     ( x  x  )
    /// and
    /// V'*B*Q = V'*( B1 B2 )*Q = ( x  0  )
    /// ( 0  B3 )     ( x  x  )
    /// 
    /// or if ( .NOT.UPPER ) then
    /// 
    /// U'*A*Q = U'*( A1 0  )*Q = ( x  x  )
    /// ( A2 A3 )     ( 0  x  )
    /// and
    /// V'*B*Q = V'*( B1 0  )*Q = ( x  x  )
    /// ( B2 B3 )     ( 0  x  )
    /// 
    /// The rows of the transformed A and B are parallel, where
    /// 
    /// U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
    /// ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
    /// 
    /// Z' denotes the transpose of Z.
    ///</summary>
    public class DLAGS2
    {
    

        #region Dependencies
        
        DLARTG _dlartg; DLASV2 _dlasv2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DLAGS2(DLARTG dlartg, DLASV2 dlasv2)
        {
    

            #region Set Dependencies
            
            this._dlartg = dlartg; this._dlasv2 = dlasv2; 

            #endregion

        }
    
        public DLAGS2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASV2 dlasv2 = new DLASV2(dlamch);

            #endregion


            #region Set Dependencies
            
            this._dlartg = dlartg; this._dlasv2 = dlasv2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
        /// that if ( UPPER ) then
        /// 
        /// U'*A*Q = U'*( A1 A2 )*Q = ( x  0  )
        /// ( 0  A3 )     ( x  x  )
        /// and
        /// V'*B*Q = V'*( B1 B2 )*Q = ( x  0  )
        /// ( 0  B3 )     ( x  x  )
        /// 
        /// or if ( .NOT.UPPER ) then
        /// 
        /// U'*A*Q = U'*( A1 0  )*Q = ( x  x  )
        /// ( A2 A3 )     ( 0  x  )
        /// and
        /// V'*B*Q = V'*( B1 0  )*Q = ( x  x  )
        /// ( B2 B3 )     ( 0  x  )
        /// 
        /// The rows of the transformed A and B are parallel, where
        /// 
        /// U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
        /// ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
        /// 
        /// Z' denotes the transpose of Z.
        ///</summary>
        /// <param name="UPPER">
        /// (input) LOGICAL
        /// = .TRUE.: the input matrices A and B are upper triangular.
        /// = .FALSE.: the input matrices A and B are lower triangular.
        ///</param>
        /// <param name="A1">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="A2">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="A3">
        /// (input) DOUBLE PRECISION
        /// On entry, A1, A2 and A3 are elements of the input 2-by-2
        /// upper (lower) triangular matrix A.
        ///</param>
        /// <param name="B1">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="B2">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="B3">
        /// (input) DOUBLE PRECISION
        /// On entry, B1, B2 and B3 are elements of the input 2-by-2
        /// upper (lower) triangular matrix B.
        ///</param>
        /// <param name="CSU">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="SNU">
        /// (output) DOUBLE PRECISION
        /// The desired orthogonal matrix U.
        ///</param>
        /// <param name="CSV">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="SNV">
        /// (output) DOUBLE PRECISION
        /// The desired orthogonal matrix V.
        ///</param>
        /// <param name="CSQ">
        /// (output) DOUBLE PRECISION
        ///</param>
        /// <param name="SNQ">
        /// (output) DOUBLE PRECISION
        /// The desired orthogonal matrix Q.
        ///</param>
        public void Run(bool UPPER, double A1, double A2, double A3, double B1, double B2
                         , double B3, ref double CSU, ref double SNU, ref double CSV, ref double SNV, ref double CSQ
                         , ref double SNQ)
        {

            #region Variables
            
            double A = 0; double AUA11 = 0; double AUA12 = 0; double AUA21 = 0; double AUA22 = 0; double AVB11 = 0; 
            double AVB12 = 0;double AVB21 = 0; double AVB22 = 0; double B = 0; double C = 0; double CSL = 0; double CSR = 0; 
            double D = 0;double R = 0; double S1 = 0; double S2 = 0; double SNL = 0; double SNR = 0; double UA11 = 0; 
            double UA11R = 0;double UA12 = 0; double UA21 = 0; double UA22 = 0; double UA22R = 0; double VB11 = 0; 
            double VB11R = 0;double VB12 = 0; double VB21 = 0; double VB22 = 0; double VB22R = 0; 

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
            // *  DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
            // *  that if ( UPPER ) then
            // *
            // *            U'*A*Q = U'*( A1 A2 )*Q = ( x  0  )
            // *                        ( 0  A3 )     ( x  x  )
            // *  and
            // *            V'*B*Q = V'*( B1 B2 )*Q = ( x  0  )
            // *                        ( 0  B3 )     ( x  x  )
            // *
            // *  or if ( .NOT.UPPER ) then
            // *
            // *            U'*A*Q = U'*( A1 0  )*Q = ( x  x  )
            // *                        ( A2 A3 )     ( 0  x  )
            // *  and
            // *            V'*B*Q = V'*( B1 0  )*Q = ( x  x  )
            // *                        ( B2 B3 )     ( 0  x  )
            // *
            // *  The rows of the transformed A and B are parallel, where
            // *
            // *    U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
            // *        ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
            // *
            // *  Z' denotes the transpose of Z.
            // *
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPPER   (input) LOGICAL
            // *          = .TRUE.: the input matrices A and B are upper triangular.
            // *          = .FALSE.: the input matrices A and B are lower triangular.
            // *
            // *  A1      (input) DOUBLE PRECISION
            // *  A2      (input) DOUBLE PRECISION
            // *  A3      (input) DOUBLE PRECISION
            // *          On entry, A1, A2 and A3 are elements of the input 2-by-2
            // *          upper (lower) triangular matrix A.
            // *
            // *  B1      (input) DOUBLE PRECISION
            // *  B2      (input) DOUBLE PRECISION
            // *  B3      (input) DOUBLE PRECISION
            // *          On entry, B1, B2 and B3 are elements of the input 2-by-2
            // *          upper (lower) triangular matrix B.
            // *
            // *  CSU     (output) DOUBLE PRECISION
            // *  SNU     (output) DOUBLE PRECISION
            // *          The desired orthogonal matrix U.
            // *
            // *  CSV     (output) DOUBLE PRECISION
            // *  SNV     (output) DOUBLE PRECISION
            // *          The desired orthogonal matrix V.
            // *
            // *  CSQ     (output) DOUBLE PRECISION
            // *  SNQ     (output) DOUBLE PRECISION
            // *          The desired orthogonal matrix Q.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (UPPER)
            {
                // *
                // *        Input matrices A and B are upper triangular matrices
                // *
                // *        Form matrix C = A*adj(B) = ( a b )
                // *                                   ( 0 d )
                // *
                A = A1 * B3;
                D = A3 * B1;
                B = A2 * B1 - A1 * B2;
                // *
                // *        The SVD of real 2-by-2 triangular C
                // *
                // *         ( CSL -SNL )*( A B )*(  CSR  SNR ) = ( R 0 )
                // *         ( SNL  CSL ) ( 0 D ) ( -SNR  CSR )   ( 0 T )
                // *
                this._dlasv2.Run(A, B, D, ref S1, ref S2, ref SNR
                                 , ref CSR, ref SNL, ref CSL);
                // *
                if (Math.Abs(CSL) >= Math.Abs(SNL) || Math.Abs(CSR) >= Math.Abs(SNR))
                {
                    // *
                    // *           Compute the (1,1) and (1,2) elements of U'*A and V'*B,
                    // *           and (1,2) element of |U|'*|A| and |V|'*|B|.
                    // *
                    UA11R = CSL * A1;
                    UA12 = CSL * A2 + SNL * A3;
                    // *
                    VB11R = CSR * B1;
                    VB12 = CSR * B2 + SNR * B3;
                    // *
                    AUA12 = Math.Abs(CSL) * Math.Abs(A2) + Math.Abs(SNL) * Math.Abs(A3);
                    AVB12 = Math.Abs(CSR) * Math.Abs(B2) + Math.Abs(SNR) * Math.Abs(B3);
                    // *
                    // *           zero (1,2) elements of U'*A and V'*B
                    // *
                    if ((Math.Abs(UA11R) + Math.Abs(UA12)) != ZERO)
                    {
                        if (AUA12 / (Math.Abs(UA11R) + Math.Abs(UA12)) <= AVB12 / (Math.Abs(VB11R) + Math.Abs(VB12)))
                        {
                            this._dlartg.Run( - UA11R, UA12, ref CSQ, ref SNQ, ref R);
                        }
                        else
                        {
                            this._dlartg.Run( - VB11R, VB12, ref CSQ, ref SNQ, ref R);
                        }
                    }
                    else
                    {
                        this._dlartg.Run( - VB11R, VB12, ref CSQ, ref SNQ, ref R);
                    }
                    // *
                    CSU = CSL;
                    SNU =  - SNL;
                    CSV = CSR;
                    SNV =  - SNR;
                    // *
                }
                else
                {
                    // *
                    // *           Compute the (2,1) and (2,2) elements of U'*A and V'*B,
                    // *           and (2,2) element of |U|'*|A| and |V|'*|B|.
                    // *
                    UA21 =  - SNL * A1;
                    UA22 =  - SNL * A2 + CSL * A3;
                    // *
                    VB21 =  - SNR * B1;
                    VB22 =  - SNR * B2 + CSR * B3;
                    // *
                    AUA22 = Math.Abs(SNL) * Math.Abs(A2) + Math.Abs(CSL) * Math.Abs(A3);
                    AVB22 = Math.Abs(SNR) * Math.Abs(B2) + Math.Abs(CSR) * Math.Abs(B3);
                    // *
                    // *           zero (2,2) elements of U'*A and V'*B, and then swap.
                    // *
                    if ((Math.Abs(UA21) + Math.Abs(UA22)) != ZERO)
                    {
                        if (AUA22 / (Math.Abs(UA21) + Math.Abs(UA22)) <= AVB22 / (Math.Abs(VB21) + Math.Abs(VB22)))
                        {
                            this._dlartg.Run( - UA21, UA22, ref CSQ, ref SNQ, ref R);
                        }
                        else
                        {
                            this._dlartg.Run( - VB21, VB22, ref CSQ, ref SNQ, ref R);
                        }
                    }
                    else
                    {
                        this._dlartg.Run( - VB21, VB22, ref CSQ, ref SNQ, ref R);
                    }
                    // *
                    CSU = SNL;
                    SNU = CSL;
                    CSV = SNR;
                    SNV = CSR;
                    // *
                }
                // *
            }
            else
            {
                // *
                // *        Input matrices A and B are lower triangular matrices
                // *
                // *        Form matrix C = A*adj(B) = ( a 0 )
                // *                                   ( c d )
                // *
                A = A1 * B3;
                D = A3 * B1;
                C = A2 * B3 - A3 * B2;
                // *
                // *        The SVD of real 2-by-2 triangular C
                // *
                // *         ( CSL -SNL )*( A 0 )*(  CSR  SNR ) = ( R 0 )
                // *         ( SNL  CSL ) ( C D ) ( -SNR  CSR )   ( 0 T )
                // *
                this._dlasv2.Run(A, C, D, ref S1, ref S2, ref SNR
                                 , ref CSR, ref SNL, ref CSL);
                // *
                if (Math.Abs(CSR) >= Math.Abs(SNR) || Math.Abs(CSL) >= Math.Abs(SNL))
                {
                    // *
                    // *           Compute the (2,1) and (2,2) elements of U'*A and V'*B,
                    // *           and (2,1) element of |U|'*|A| and |V|'*|B|.
                    // *
                    UA21 =  - SNR * A1 + CSR * A2;
                    UA22R = CSR * A3;
                    // *
                    VB21 =  - SNL * B1 + CSL * B2;
                    VB22R = CSL * B3;
                    // *
                    AUA21 = Math.Abs(SNR) * Math.Abs(A1) + Math.Abs(CSR) * Math.Abs(A2);
                    AVB21 = Math.Abs(SNL) * Math.Abs(B1) + Math.Abs(CSL) * Math.Abs(B2);
                    // *
                    // *           zero (2,1) elements of U'*A and V'*B.
                    // *
                    if ((Math.Abs(UA21) + Math.Abs(UA22R)) != ZERO)
                    {
                        if (AUA21 / (Math.Abs(UA21) + Math.Abs(UA22R)) <= AVB21 / (Math.Abs(VB21) + Math.Abs(VB22R)))
                        {
                            this._dlartg.Run(UA22R, UA21, ref CSQ, ref SNQ, ref R);
                        }
                        else
                        {
                            this._dlartg.Run(VB22R, VB21, ref CSQ, ref SNQ, ref R);
                        }
                    }
                    else
                    {
                        this._dlartg.Run(VB22R, VB21, ref CSQ, ref SNQ, ref R);
                    }
                    // *
                    CSU = CSR;
                    SNU =  - SNR;
                    CSV = CSL;
                    SNV =  - SNL;
                    // *
                }
                else
                {
                    // *
                    // *           Compute the (1,1) and (1,2) elements of U'*A and V'*B,
                    // *           and (1,1) element of |U|'*|A| and |V|'*|B|.
                    // *
                    UA11 = CSR * A1 + SNR * A2;
                    UA12 = SNR * A3;
                    // *
                    VB11 = CSL * B1 + SNL * B2;
                    VB12 = SNL * B3;
                    // *
                    AUA11 = Math.Abs(CSR) * Math.Abs(A1) + Math.Abs(SNR) * Math.Abs(A2);
                    AVB11 = Math.Abs(CSL) * Math.Abs(B1) + Math.Abs(SNL) * Math.Abs(B2);
                    // *
                    // *           zero (1,1) elements of U'*A and V'*B, and then swap.
                    // *
                    if ((Math.Abs(UA11) + Math.Abs(UA12)) != ZERO)
                    {
                        if (AUA11 / (Math.Abs(UA11) + Math.Abs(UA12)) <= AVB11 / (Math.Abs(VB11) + Math.Abs(VB12)))
                        {
                            this._dlartg.Run(UA12, UA11, ref CSQ, ref SNQ, ref R);
                        }
                        else
                        {
                            this._dlartg.Run(VB12, VB11, ref CSQ, ref SNQ, ref R);
                        }
                    }
                    else
                    {
                        this._dlartg.Run(VB12, VB11, ref CSQ, ref SNQ, ref R);
                    }
                    // *
                    CSU = SNR;
                    SNU = CSR;
                    CSV = SNL;
                    SNV = CSL;
                    // *
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DLAGS2
            // *

            #endregion

        }
    }
}
