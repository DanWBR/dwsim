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
    /// This program sets problem and machine dependent parameters
    /// useful for xHSEQR and its subroutines. It is called whenever 
    /// ILAENV is called with 12 .LE. ISPEC .LE. 16
    /// 
    ///</summary>
    public class IPARMQ
    {
    

        #region Variables
        
        const int INMIN = 12; const int INWIN = 13; const int INIBL = 14; const int ISHFTS = 15; const int IACC22 = 16; 
        const int NMIN = 75;const int K22MIN = 14; const int KACMIN = 14; const int NIBBLE = 14; const int KNWSWP = 500; 
        const double TWO = 2.0;

        #endregion

        public IPARMQ()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This program sets problem and machine dependent parameters
        /// useful for xHSEQR and its subroutines. It is called whenever 
        /// ILAENV is called with 12 .LE. ISPEC .LE. 16
        /// 
        ///</summary>
        /// <param name="ISPEC">
        /// (input) integer scalar
        /// ISPEC specifies which tunable parameter IPARMQ should
        /// return.
        /// 
        /// ISPEC=12: (INMIN)  Matrices of order nmin or less
        /// are sent directly to xLAHQR, the implicit
        /// double shift QR algorithm.  NMIN must be
        /// at least 11.
        /// 
        /// ISPEC=13: (INWIN)  Size of the deflation window.
        /// This is best set greater than or equal to
        /// the number of simultaneous shifts NS.
        /// Larger matrices benefit from larger deflation
        /// windows.
        /// 
        /// ISPEC=14: (INIBL) Determines when to stop nibbling and
        /// invest in an (expensive) multi-shift QR sweep.
        /// If the aggressive early deflation subroutine
        /// finds LD converged eigenvalues from an order
        /// NW deflation window and LD.GT.(NW*NIBBLE)/100,
        /// then the next QR sweep is skipped and early
        /// deflation is applied immediately to the
        /// remaining active diagonal block.  Setting
        /// IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
        /// multi-shift QR sweep whenever early deflation
        /// finds a converged eigenvalue.  Setting
        /// IPARMQ(ISPEC=14) greater than or equal to 100
        /// prevents TTQRE from skipping a multi-shift
        /// QR sweep.
        /// 
        /// ISPEC=15: (NSHFTS) The number of simultaneous shifts in
        /// a multi-shift QR iteration.
        /// 
        /// ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
        /// following meanings.
        /// 0:  During the multi-shift QR sweep,
        /// xLAQR5 does not accumulate reflections and
        /// does not use matrix-matrix multiply to
        /// update the far-from-diagonal matrix
        /// entries.
        /// 1:  During the multi-shift QR sweep,
        /// xLAQR5 and/or xLAQRaccumulates reflections and uses
        /// matrix-matrix multiply to update the
        /// far-from-diagonal matrix entries.
        /// 2:  During the multi-shift QR sweep.
        /// xLAQR5 accumulates reflections and takes
        /// advantage of 2-by-2 block structure during
        /// matrix-matrix multiplies.
        /// (If xTRMM is slower than xGEMM, then
        /// IPARMQ(ISPEC=16)=1 may be more efficient than
        /// IPARMQ(ISPEC=16)=2 despite the greater level of
        /// arithmetic work implied by the latter choice.)
        ///</param>
        /// <param name="NAME">
        /// (input) character string
        /// Name of the calling subroutine
        ///</param>
        /// <param name="OPTS">
        /// (input) character string
        /// This is a concatenation of the string arguments to
        /// TTQRE.
        ///</param>
        /// <param name="N">
        /// (input) integer scalar
        /// N is the order of the Hessenberg matrix H.
        ///</param>
        /// <param name="ILO">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHI">
        /// (input) INTEGER
        /// It is assumed that H is already upper triangular
        /// in rows and columns 1:ILO-1 and IHI+1:N.
        ///</param>
        /// <param name="LWORK">
        /// (input) integer scalar
        /// The amount of workspace available.
        ///</param>
        public int Run(int ISPEC, string NAME, string OPTS, int N, int ILO, int IHI
                        , int LWORK)
        {
        int iparmq = 0;

            #region Variables
            
            int NH = 0; int NS = 0; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *     
            // *     .. Scalar Arguments ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *       This program sets problem and machine dependent parameters
            // *       useful for xHSEQR and its subroutines. It is called whenever 
            // *       ILAENV is called with 12 <= ISPEC <= 16
            // *
            // *  Arguments
            // *  =========
            // *
            // *       ISPEC  (input) integer scalar
            // *              ISPEC specifies which tunable parameter IPARMQ should
            // *              return.
            // *
            // *              ISPEC=12: (INMIN)  Matrices of order nmin or less
            // *                        are sent directly to xLAHQR, the implicit
            // *                        double shift QR algorithm.  NMIN must be
            // *                        at least 11.
            // *
            // *              ISPEC=13: (INWIN)  Size of the deflation window.
            // *                        This is best set greater than or equal to
            // *                        the number of simultaneous shifts NS.
            // *                        Larger matrices benefit from larger deflation
            // *                        windows.
            // *
            // *              ISPEC=14: (INIBL) Determines when to stop nibbling and
            // *                        invest in an (expensive) multi-shift QR sweep.
            // *                        If the aggressive early deflation subroutine
            // *                        finds LD converged eigenvalues from an order
            // *                        NW deflation window and LD.GT.(NW*NIBBLE)/100,
            // *                        then the next QR sweep is skipped and early
            // *                        deflation is applied immediately to the
            // *                        remaining active diagonal block.  Setting
            // *                        IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
            // *                        multi-shift QR sweep whenever early deflation
            // *                        finds a converged eigenvalue.  Setting
            // *                        IPARMQ(ISPEC=14) greater than or equal to 100
            // *                        prevents TTQRE from skipping a multi-shift
            // *                        QR sweep.
            // *
            // *              ISPEC=15: (NSHFTS) The number of simultaneous shifts in
            // *                        a multi-shift QR iteration.
            // *
            // *              ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
            // *                        following meanings.
            // *                        0:  During the multi-shift QR sweep,
            // *                            xLAQR5 does not accumulate reflections and
            // *                            does not use matrix-matrix multiply to
            // *                            update the far-from-diagonal matrix
            // *                            entries.
            // *                        1:  During the multi-shift QR sweep,
            // *                            xLAQR5 and/or xLAQRaccumulates reflections and uses
            // *                            matrix-matrix multiply to update the
            // *                            far-from-diagonal matrix entries.
            // *                        2:  During the multi-shift QR sweep.
            // *                            xLAQR5 accumulates reflections and takes
            // *                            advantage of 2-by-2 block structure during
            // *                            matrix-matrix multiplies.
            // *                        (If xTRMM is slower than xGEMM, then
            // *                        IPARMQ(ISPEC=16)=1 may be more efficient than
            // *                        IPARMQ(ISPEC=16)=2 despite the greater level of
            // *                        arithmetic work implied by the latter choice.)
            // *
            // *       NAME    (input) character string
            // *               Name of the calling subroutine
            // *
            // *       OPTS    (input) character string
            // *               This is a concatenation of the string arguments to
            // *               TTQRE.
            // *
            // *       N       (input) integer scalar
            // *               N is the order of the Hessenberg matrix H.
            // *
            // *       ILO     (input) INTEGER
            // *       IHI     (input) INTEGER
            // *               It is assumed that H is already upper triangular
            // *               in rows and columns 1:ILO-1 and IHI+1:N.
            // *
            // *       LWORK   (input) integer scalar
            // *               The amount of workspace available.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *       Little is known about how best to choose these parameters.
            // *       It is possible to use different values of the parameters
            // *       for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.
            // *
            // *       It is probably best to choose different parameters for
            // *       different matrices and different parameters at different
            // *       times during the iteration, but this has not been
            // *       implemented --- yet.
            // *
            // *
            // *       The best choices of most of the parameters depend
            // *       in an ill-understood way on the relative execution
            // *       rate of xLAQR3 and xLAQR5 and on the nature of each
            // *       particular eigenvalue problem.  Experiment may be the
            // *       only practical way to determine which choices are most
            // *       effective.
            // *
            // *       Following is a list of default values supplied by IPARMQ.
            // *       These defaults may be adjusted in order to attain better
            // *       performance in any particular computational environment.
            // *
            // *       IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.
            // *                        Default: 75. (Must be at least 11.)
            // *
            // *       IPARMQ(ISPEC=13) Recommended deflation window size.
            // *                        This depends on ILO, IHI and NS, the
            // *                        number of simultaneous shifts returned
            // *                        by IPARMQ(ISPEC=15).  The default for
            // *                        (IHI-ILO+1).LE.500 is NS.  The default
            // *                        for (IHI-ILO+1).GT.500 is 3*NS/2.
            // *
            // *       IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.
            // *
            // *       IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.
            // *                        a multi-shift QR iteration.
            // *
            // *                        If IHI-ILO+1 is ...
            // *
            // *                        greater than      ...but less    ... the
            // *                        or equal to ...      than        default is
            // *
            // *                                0               30       NS =   2+
            // *                               30               60       NS =   4+
            // *                               60              150       NS =  10
            // *                              150              590       NS =  **
            // *                              590             3000       NS =  64
            // *                             3000             6000       NS = 128
            // *                             6000             infinity   NS = 256
            // *
            // *                    (+)  By default matrices of this order are
            // *                         passed to the implicit double shift routine
            // *                         xLAHQR.  See IPARMQ(ISPEC=12) above.   These
            // *                         values of NS are used only in case of a rare
            // *                         xLAHQR failure.
            // *
            // *                    (**) The asterisks (**) indicate an ad-hoc
            // *                         function increasing from 10 to 64.
            // *
            // *       IPARMQ(ISPEC=16) Select structured matrix multiply.
            // *                        (See ISPEC=16 above for details.)
            // *                        Default: 3.
            // *
            // *     ================================================================
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          LOG, MAX, MOD, NINT, REAL;
            // *     ..
            // *     .. Executable Statements ..

            #endregion


            #region Body
            
            if ((ISPEC == ISHFTS) || (ISPEC == INWIN) || (ISPEC == IACC22))
            {
                // *
                // *        ==== Set the number simultaneous shifts ====
                // *
                NH = IHI - ILO + 1;
                NS = 2;
                if (NH >= 30) NS = 4;
                if (NH >= 60) NS = 10;
                if (NH >= 150) NS = (int)Math.Max(10, NH / Math.Round(Math.Log(Convert.ToSingle(NH)) / Math.Log(TWO)));
                if (NH >= 590) NS = 64;
                if (NH >= 3000) NS = 128;
                if (NH >= 6000) NS = 256;
                NS = Math.Max(2, NS - FortranLib.Mod(NS,2));
            }
            // *
            if (ISPEC == INMIN)
            {
                // *
                // *
                // *        ===== Matrices of order smaller than NMIN get sent
                // *        .     to xLAHQR, the classic double shift algorithm.
                // *        .     This must be at least 11. ====
                // *
                iparmq = NMIN;
                // *
            }
            else
            {
                if (ISPEC == INIBL)
                {
                    // *
                    // *        ==== INIBL: skip a multi-shift qr iteration and
                    // *        .    whenever aggressive early deflation finds
                    // *        .    at least (NIBBLE*(window size)/100) deflations. ====
                    // *
                    iparmq = NIBBLE;
                    // *
                }
                else
                {
                    if (ISPEC == ISHFTS)
                    {
                        // *
                        // *        ==== NSHFTS: The number of simultaneous shifts =====
                        // *
                        iparmq = NS;
                        // *
                    }
                    else
                    {
                        if (ISPEC == INWIN)
                        {
                            // *
                            // *        ==== NW: deflation window size.  ====
                            // *
                            if (NH <= KNWSWP)
                            {
                                iparmq = NS;
                            }
                            else
                            {
                                iparmq = 3 * NS / 2;
                            }
                            // *
                        }
                        else
                        {
                            if (ISPEC == IACC22)
                            {
                                // *
                                // *        ==== IACC22: Whether to accumulate reflections
                                // *        .     before updating the far-from-diagonal elements
                                // *        .     and whether to use 2-by-2 block structure while
                                // *        .     doing it.  A small amount of work could be saved
                                // *        .     by making this choice dependent also upon the
                                // *        .     NH=IHI-ILO+1.
                                // *
                                iparmq = 0;
                                if (NS >= KACMIN) iparmq = 1;
                                if (NS >= K22MIN) iparmq = 2;
                                // *
                            }
                            else
                            {
                                // *        ===== invalid value of ispec =====
                                iparmq =  - 1;
                                // *
                            }
                        }
                    }
                }
            }
            // *
            // *     ==== End of IPARMQ ====
            // *
        return iparmq;

            #endregion

        }
    }
}
