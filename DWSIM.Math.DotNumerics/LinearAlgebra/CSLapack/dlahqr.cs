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
    /// DLAHQR is an auxiliary routine called by DHSEQR to update the
    /// eigenvalues and Schur decomposition already computed by DHSEQR, by
    /// dealing with the Hessenberg submatrix in rows and columns ILO to
    /// IHI.
    /// 
    ///</summary>
    public class DLAHQR
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DCOPY _dcopy; DLABAD _dlabad; DLANV2 _dlanv2; DLARFG _dlarfg; DROT _drot; 

        #endregion


        #region Variables
        
        const int ITMAX = 30; const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double DAT1 = 3.0E0 / 4.0E0;const double DAT2 =  - 0.4375E0; double[] V = new double[3]; 

        #endregion

        public DLAHQR(DLAMCH dlamch, DCOPY dcopy, DLABAD dlabad, DLANV2 dlanv2, DLARFG dlarfg, DROT drot)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dcopy = dcopy; this._dlabad = dlabad; this._dlanv2 = dlanv2; this._dlarfg = dlarfg; 
            this._drot = drot;

            #endregion

        }
    
        public DLAHQR()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DCOPY dcopy = new DCOPY();
            DLABAD dlabad = new DLABAD();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DROT drot = new DROT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANV2 dlanv2 = new DLANV2(dlamch, dlapy2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dcopy = dcopy; this._dlabad = dlabad; this._dlanv2 = dlanv2; this._dlarfg = dlarfg; 
            this._drot = drot;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAHQR is an auxiliary routine called by DHSEQR to update the
        /// eigenvalues and Schur decomposition already computed by DHSEQR, by
        /// dealing with the Hessenberg submatrix in rows and columns ILO to
        /// IHI.
        /// 
        ///</summary>
        /// <param name="WANTT">
        /// (input) LOGICAL
        /// = .TRUE. : the full Schur form T is required;
        /// = .FALSE.: only eigenvalues are required.
        ///</param>
        /// <param name="WANTZ">
        /// (input) LOGICAL
        /// = .TRUE. : the matrix of Schur vectors Z is required;
        /// = .FALSE.: Schur vectors are not required.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix H.  N .GE. 0.
        ///</param>
        /// <param name="ILO">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHI">
        /// (input) INTEGER
        /// It is assumed that H is already upper quasi-triangular in
        /// rows and columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless
        /// ILO = 1). DLAHQR works primarily with the Hessenberg
        /// submatrix in rows and columns ILO to IHI, but applies
        /// transformations to all of H if WANTT is .TRUE..
        /// 1 .LE. ILO .LE. max(1,IHI); IHI .LE. N.
        ///</param>
        /// <param name="H">
        /// (input/output) DOUBLE PRECISION array, dimension (LDH,N)
        /// On entry, the upper Hessenberg matrix H.
        /// On exit, if INFO is zero and if WANTT is .TRUE., H is upper
        /// quasi-triangular in rows and columns ILO:IHI, with any
        /// 2-by-2 diagonal blocks in standard form. If INFO is zero
        /// and WANTT is .FALSE., the contents of H are unspecified on
        /// exit.  The output state of H if INFO is nonzero is given
        /// below under the description of INFO.
        ///</param>
        /// <param name="LDH">
        /// (input) INTEGER
        /// The leading dimension of the array H. LDH .GE. max(1,N).
        ///</param>
        /// <param name="WR">
        /// (output) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="WI">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The real and imaginary parts, respectively, of the computed
        /// eigenvalues ILO to IHI are stored in the corresponding
        /// elements of WR and WI. If two eigenvalues are computed as a
        /// complex conjugate pair, they are stored in consecutive
        /// elements of WR and WI, say the i-th and (i+1)th, with
        /// WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., the
        /// eigenvalues are stored in the same order as on the diagonal
        /// of the Schur form returned in H, with WR(i) = H(i,i), and, if
        /// H(i:i+1,i:i+1) is a 2-by-2 diagonal block,
        /// WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and WI(i+1) = -WI(i).
        ///</param>
        /// <param name="ILOZ">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHIZ">
        /// (input) INTEGER
        /// Specify the rows of Z to which transformations must be
        /// applied if WANTZ is .TRUE..
        /// 1 .LE. ILOZ .LE. ILO; IHI .LE. IHIZ .LE. N.
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
        /// If WANTZ is .TRUE., on entry Z must contain the current
        /// matrix Z of transformations accumulated by DHSEQR, and on
        /// exit Z has been updated; transformations are applied only to
        /// the submatrix Z(ILOZ:IHIZ,ILO:IHI).
        /// If WANTZ is .FALSE., Z is not referenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) INTEGER
        /// The leading dimension of the array Z. LDZ .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// =   0: successful exit
        /// .GT. 0: If INFO = i, DLAHQR failed to compute all the
        /// eigenvalues ILO to IHI in a total of 30 iterations
        /// per eigenvalue; elements i+1:ihi of WR and WI
        /// contain those eigenvalues which have been
        /// successfully computed.
        /// 
        /// If INFO .GT. 0 and WANTT is .FALSE., then on exit,
        /// the remaining unconverged eigenvalues are the
        /// eigenvalues of the upper Hessenberg matrix rows
        /// and columns ILO thorugh INFO of the final, output
        /// value of H.
        /// 
        /// If INFO .GT. 0 and WANTT is .TRUE., then on exit
        /// (*)       (initial value of H)*U  = U*(final value of H)
        /// where U is an orthognal matrix.    The final
        /// value of H is upper Hessenberg and triangular in
        /// rows and columns INFO+1 through IHI.
        /// 
        /// If INFO .GT. 0 and WANTZ is .TRUE., then on exit
        /// (final value of Z)  = (initial value of Z)*U
        /// where U is the orthogonal matrix in (*)
        /// (regardless of the value of WANTT.)
        ///</param>
        public void Run(bool WANTT, bool WANTZ, int N, int ILO, int IHI, ref double[] H, int offset_h
                         , int LDH, ref double[] WR, int offset_wr, ref double[] WI, int offset_wi, int ILOZ, int IHIZ, ref double[] Z, int offset_z
                         , int LDZ, ref int INFO)
        {

            #region Variables
            
            double AA = 0; double AB = 0; double BA = 0; double BB = 0; double CS = 0; double DET = 0; double H11 = 0; 
            double H12 = 0;double H21 = 0; double H21S = 0; double H22 = 0; double RT1I = 0; double RT1R = 0; double RT2I = 0; 
            double RT2R = 0;double RTDISC = 0; double S = 0; double SAFMAX = 0; double SAFMIN = 0; double SMLNUM = 0; 
            double SN = 0;double SUM = 0; double T1 = 0; double T2 = 0; double T3 = 0; double TR = 0; double TST = 0; 
            double ULP = 0;double V2 = 0; double V3 = 0; int I = 0; int I1 = 0; int I2 = 0; int ITS = 0; int J = 0; int K = 0; 
            int L = 0;int M = 0; int NH = 0; int NR = 0; int NZ = 0; int offset_v = 0; int o_v = -1; 

            #endregion


            #region Implicit Variables
            
            int H_K = 0; int H_0 = 0; int H_1 = 0; int H_2 = 0; int H_3 = 0; int Z_K = 0; int Z_0 = 0; int Z_1 = 0; int Z_2 = 0; 
            int Z_3 = 0;int H_4 = 0; int H_5 = 0; int Z_4 = 0; int Z_5 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_h = -1 - LDH + offset_h;  int o_wr = -1 + offset_wr;  int o_wi = -1 + offset_wi; 
             int o_z = -1 - LDZ + offset_z;

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
            // *     Purpose
            // *     =======
            // *
            // *     DLAHQR is an auxiliary routine called by DHSEQR to update the
            // *     eigenvalues and Schur decomposition already computed by DHSEQR, by
            // *     dealing with the Hessenberg submatrix in rows and columns ILO to
            // *     IHI.
            // *
            // *     Arguments
            // *     =========
            // *
            // *     WANTT   (input) LOGICAL
            // *          = .TRUE. : the full Schur form T is required;
            // *          = .FALSE.: only eigenvalues are required.
            // *
            // *     WANTZ   (input) LOGICAL
            // *          = .TRUE. : the matrix of Schur vectors Z is required;
            // *          = .FALSE.: Schur vectors are not required.
            // *
            // *     N       (input) INTEGER
            // *          The order of the matrix H.  N >= 0.
            // *
            // *     ILO     (input) INTEGER
            // *     IHI     (input) INTEGER
            // *          It is assumed that H is already upper quasi-triangular in
            // *          rows and columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless
            // *          ILO = 1). DLAHQR works primarily with the Hessenberg
            // *          submatrix in rows and columns ILO to IHI, but applies
            // *          transformations to all of H if WANTT is .TRUE..
            // *          1 <= ILO <= max(1,IHI); IHI <= N.
            // *
            // *     H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
            // *          On entry, the upper Hessenberg matrix H.
            // *          On exit, if INFO is zero and if WANTT is .TRUE., H is upper
            // *          quasi-triangular in rows and columns ILO:IHI, with any
            // *          2-by-2 diagonal blocks in standard form. If INFO is zero
            // *          and WANTT is .FALSE., the contents of H are unspecified on
            // *          exit.  The output state of H if INFO is nonzero is given
            // *          below under the description of INFO.
            // *
            // *     LDH     (input) INTEGER
            // *          The leading dimension of the array H. LDH >= max(1,N).
            // *
            // *     WR      (output) DOUBLE PRECISION array, dimension (N)
            // *     WI      (output) DOUBLE PRECISION array, dimension (N)
            // *          The real and imaginary parts, respectively, of the computed
            // *          eigenvalues ILO to IHI are stored in the corresponding
            // *          elements of WR and WI. If two eigenvalues are computed as a
            // *          complex conjugate pair, they are stored in consecutive
            // *          elements of WR and WI, say the i-th and (i+1)th, with
            // *          WI(i) > 0 and WI(i+1) < 0. If WANTT is .TRUE., the
            // *          eigenvalues are stored in the same order as on the diagonal
            // *          of the Schur form returned in H, with WR(i) = H(i,i), and, if
            // *          H(i:i+1,i:i+1) is a 2-by-2 diagonal block,
            // *          WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and WI(i+1) = -WI(i).
            // *
            // *     ILOZ    (input) INTEGER
            // *     IHIZ    (input) INTEGER
            // *          Specify the rows of Z to which transformations must be
            // *          applied if WANTZ is .TRUE..
            // *          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
            // *
            // *     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
            // *          If WANTZ is .TRUE., on entry Z must contain the current
            // *          matrix Z of transformations accumulated by DHSEQR, and on
            // *          exit Z has been updated; transformations are applied only to
            // *          the submatrix Z(ILOZ:IHIZ,ILO:IHI).
            // *          If WANTZ is .FALSE., Z is not referenced.
            // *
            // *     LDZ     (input) INTEGER
            // *          The leading dimension of the array Z. LDZ >= max(1,N).
            // *
            // *     INFO    (output) INTEGER
            // *           =   0: successful exit
            // *          .GT. 0: If INFO = i, DLAHQR failed to compute all the
            // *                  eigenvalues ILO to IHI in a total of 30 iterations
            // *                  per eigenvalue; elements i+1:ihi of WR and WI
            // *                  contain those eigenvalues which have been
            // *                  successfully computed.
            // *
            // *                  If INFO .GT. 0 and WANTT is .FALSE., then on exit,
            // *                  the remaining unconverged eigenvalues are the
            // *                  eigenvalues of the upper Hessenberg matrix rows
            // *                  and columns ILO thorugh INFO of the final, output
            // *                  value of H.
            // *
            // *                  If INFO .GT. 0 and WANTT is .TRUE., then on exit
            // *          (*)       (initial value of H)*U  = U*(final value of H)
            // *                  where U is an orthognal matrix.    The final
            // *                  value of H is upper Hessenberg and triangular in
            // *                  rows and columns INFO+1 through IHI.
            // *
            // *                  If INFO .GT. 0 and WANTZ is .TRUE., then on exit
            // *                      (final value of Z)  = (initial value of Z)*U
            // *                  where U is the orthogonal matrix in (*)
            // *                  (regardless of the value of WANTT.)
            // *
            // *     Further Details
            // *     ===============
            // *
            // *     02-96 Based on modifications by
            // *     David Day, Sandia National Laboratory, USA
            // *
            // *     12-04 Further modifications by
            // *     Ralph Byers, University of Kansas, USA
            // *
            // *       This is a modified version of DLAHQR from LAPACK version 3.0.
            // *       It is (1) more robust against overflow and underflow and
            // *       (2) adopts the more conservative Ahues & Tisseur stopping
            // *       criterion (LAWN 122, 1997).
            // *
            // *     =========================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            if (ILO == IHI)
            {
                WR[ILO + o_wr] = H[ILO+ILO * LDH + o_h];
                WI[ILO + o_wi] = ZERO;
                return;
            }
            // *
            // *     ==== clear out the trash ====
            for (J = ILO; J <= IHI - 3; J++)
            {
                H[J + 2+J * LDH + o_h] = ZERO;
                H[J + 3+J * LDH + o_h] = ZERO;
            }
            if (ILO <= IHI - 2) H[IHI+(IHI - 2) * LDH + o_h] = ZERO;
            // *
            NH = IHI - ILO + 1;
            NZ = IHIZ - ILOZ + 1;
            // *
            // *     Set machine-dependent constants for the stopping criterion.
            // *
            SAFMIN = this._dlamch.Run("SAFE MINIMUM");
            SAFMAX = ONE / SAFMIN;
            this._dlabad.Run(ref SAFMIN, ref SAFMAX);
            ULP = this._dlamch.Run("PRECISION");
            SMLNUM = SAFMIN * (Convert.ToDouble(NH) / ULP);
            // *
            // *     I1 and I2 are the indices of the first row and last column of H
            // *     to which transformations must be applied. If eigenvalues only are
            // *     being computed, I1 and I2 are set inside the main loop.
            // *
            if (WANTT)
            {
                I1 = 1;
                I2 = N;
            }
            // *
            // *     The main loop begins here. I is the loop index and decreases from
            // *     IHI to ILO in steps of 1 or 2. Each iteration of the loop works
            // *     with the active submatrix in rows and columns L to I.
            // *     Eigenvalues I+1 to IHI have already converged. Either L = ILO or
            // *     H(L,L-1) is negligible so that the matrix splits.
            // *
            I = IHI;
        LABEL20:;
            L = ILO;
            if (I < ILO) goto LABEL160;
            // *
            // *     Perform QR iterations on rows and columns ILO to I until a
            // *     submatrix of order 1 or 2 splits off at the bottom because a
            // *     subdiagonal element has become negligible.
            // *
            for (ITS = 0; ITS <= ITMAX; ITS++)
            {
                // *
                // *        Look for a single small subdiagonal element.
                // *
                for (K = I; K >= L + 1; K +=  - 1)
                {
                    if (Math.Abs(H[K+(K - 1) * LDH + o_h]) <= SMLNUM) goto LABEL40;
                    TST = Math.Abs(H[K - 1+(K - 1) * LDH + o_h]) + Math.Abs(H[K+K * LDH + o_h]);
                    if (TST == ZERO)
                    {
                        if (K - 2 >= ILO) TST += Math.Abs(H[K - 1+(K - 2) * LDH + o_h]);
                        if (K + 1 <= IHI) TST += Math.Abs(H[K + 1+K * LDH + o_h]);
                    }
                    // *           ==== The following is a conservative small subdiagonal
                    // *           .    deflation  criterion due to Ahues & Tisseur (LAWN 122,
                    // *           .    1997). It has better mathematical foundation and
                    // *           .    improves accuracy in some cases.  ====
                    if (Math.Abs(H[K+(K - 1) * LDH + o_h]) <= ULP * TST)
                    {
                        AB = Math.Max(Math.Abs(H[K+(K - 1) * LDH + o_h]), Math.Abs(H[K - 1+K * LDH + o_h]));
                        BA = Math.Min(Math.Abs(H[K+(K - 1) * LDH + o_h]), Math.Abs(H[K - 1+K * LDH + o_h]));
                        AA = Math.Max(Math.Abs(H[K+K * LDH + o_h]), Math.Abs(H[K - 1+(K - 1) * LDH + o_h] - H[K+K * LDH + o_h]));
                        BB = Math.Min(Math.Abs(H[K+K * LDH + o_h]), Math.Abs(H[K - 1+(K - 1) * LDH + o_h] - H[K+K * LDH + o_h]));
                        S = AA + AB;
                        if (BA * (AB / S) <= Math.Max(SMLNUM, ULP * (BB * (AA / S)))) goto LABEL40;
                    }
                }
            LABEL40:;
                L = K;
                if (L > ILO)
                {
                    // *
                    // *           H(L,L-1) is negligible
                    // *
                    H[L+(L - 1) * LDH + o_h] = ZERO;
                }
                // *
                // *        Exit from loop if a submatrix of order 1 or 2 has split off.
                // *
                if (L >= I - 1) goto LABEL150;
                // *
                // *        Now the active submatrix is in rows and columns L to I. If
                // *        eigenvalues only are being computed, only the active submatrix
                // *        need be transformed.
                // *
                if (!WANTT)
                {
                    I1 = L;
                    I2 = I;
                }
                // *
                if (ITS == 10 || ITS == 20)
                {
                    // *
                    // *           Exceptional shift.
                    // *
                    H11 = DAT1 * S + H[I+I * LDH + o_h];
                    H12 = DAT2 * S;
                    H21 = S;
                    H22 = H11;
                }
                else
                {
                    // *
                    // *           Prepare to use Francis' double shift
                    // *           (i.e. 2nd degree generalized Rayleigh quotient)
                    // *
                    H11 = H[I - 1+(I - 1) * LDH + o_h];
                    H21 = H[I+(I - 1) * LDH + o_h];
                    H12 = H[I - 1+I * LDH + o_h];
                    H22 = H[I+I * LDH + o_h];
                }
                S = Math.Abs(H11) + Math.Abs(H12) + Math.Abs(H21) + Math.Abs(H22);
                if (S == ZERO)
                {
                    RT1R = ZERO;
                    RT1I = ZERO;
                    RT2R = ZERO;
                    RT2I = ZERO;
                }
                else
                {
                    H11 /= S;
                    H21 /= S;
                    H12 /= S;
                    H22 /= S;
                    TR = (H11 + H22) / TWO;
                    DET = (H11 - TR) * (H22 - TR) - H12 * H21;
                    RTDISC = Math.Sqrt(Math.Abs(DET));
                    if (DET >= ZERO)
                    {
                        // *
                        // *              ==== complex conjugate shifts ====
                        // *
                        RT1R = TR * S;
                        RT2R = RT1R;
                        RT1I = RTDISC * S;
                        RT2I =  - RT1I;
                    }
                    else
                    {
                        // *
                        // *              ==== real shifts (use only one of them)  ====
                        // *
                        RT1R = TR + RTDISC;
                        RT2R = TR - RTDISC;
                        if (Math.Abs(RT1R - H22) <= Math.Abs(RT2R - H22))
                        {
                            RT1R *= S;
                            RT2R = RT1R;
                        }
                        else
                        {
                            RT2R *= S;
                            RT1R = RT2R;
                        }
                        RT1I = ZERO;
                        RT2I = ZERO;
                    }
                }
                // *
                // *        Look for two consecutive small subdiagonal elements.
                // *
                for (M = I - 2; M >= L; M +=  - 1)
                {
                    // *           Determine the effect of starting the double-shift QR
                    // *           iteration at row M, and see if this would make H(M,M-1)
                    // *           negligible.  (The following uses scaling to avoid
                    // *           overflows and most underflows.)
                    // *
                    H21S = H[M + 1+M * LDH + o_h];
                    S = Math.Abs(H[M+M * LDH + o_h] - RT2R) + Math.Abs(RT2I) + Math.Abs(H21S);
                    H21S = H[M + 1+M * LDH + o_h] / S;
                    V[1 + o_v] = H21S * H[M+(M + 1) * LDH + o_h] + (H[M+M * LDH + o_h] - RT1R) * ((H[M+M * LDH + o_h] - RT2R) / S) - RT1I * (RT2I / S);
                    V[2 + o_v] = H21S * (H[M+M * LDH + o_h] + H[M + 1+(M + 1) * LDH + o_h] - RT1R - RT2R);
                    V[3 + o_v] = H21S * H[M + 2+(M + 1) * LDH + o_h];
                    S = Math.Abs(V[1 + o_v]) + Math.Abs(V[2 + o_v]) + Math.Abs(V[3 + o_v]);
                    V[1 + o_v] /= S;
                    V[2 + o_v] /= S;
                    V[3 + o_v] /= S;
                    if (M == L) goto LABEL60;
                    if (Math.Abs(H[M+(M - 1) * LDH + o_h]) * (Math.Abs(V[2 + o_v]) + Math.Abs(V[3 + o_v])) <= ULP * Math.Abs(V[1 + o_v]) * (Math.Abs(H[M - 1+(M - 1) * LDH + o_h]) + Math.Abs(H[M+M * LDH + o_h]) + Math.Abs(H[M + 1+(M + 1) * LDH + o_h]))) goto LABEL60;
                }
            LABEL60:;
                // *
                // *        Double-shift QR step
                // *
                for (K = M; K <= I - 1; K++)
                {
                    // *
                    // *           The first iteration of this loop determines a reflection G
                    // *           from the vector V and applies it from left and right to H,
                    // *           thus creating a nonzero bulge below the subdiagonal.
                    // *
                    // *           Each subsequent iteration determines a reflection G to
                    // *           restore the Hessenberg form in the (K-1)th column, and thus
                    // *           chases the bulge one step toward the bottom of the active
                    // *           submatrix. NR is the order of G.
                    // *
                    NR = Math.Min(3, I - K + 1);
                    if (K > M) this._dcopy.Run(NR, H, K+(K - 1) * LDH + o_h, 1, ref V, offset_v, 1);
                    this._dlarfg.Run(NR, ref V[1 + o_v], ref V, 2 + o_v, 1, ref T1);
                    if (K > M)
                    {
                        H[K+(K - 1) * LDH + o_h] = V[1 + o_v];
                        H[K + 1+(K - 1) * LDH + o_h] = ZERO;
                        if (K < I - 1) H[K + 2+(K - 1) * LDH + o_h] = ZERO;
                    }
                    else
                    {
                        if (M > L)
                        {
                            H[K+(K - 1) * LDH + o_h] =  - H[K+(K - 1) * LDH + o_h];
                        }
                    }
                    V2 = V[2 + o_v];
                    T2 = T1 * V2;
                    if (NR == 3)
                    {
                        V3 = V[3 + o_v];
                        T3 = T1 * V3;
                        // *
                        // *              Apply G from the left to transform the rows of the matrix
                        // *              in columns K to I2.
                        // *
                        for (J = K; J <= I2; J++)
                        {
                            SUM = H[K+J * LDH + o_h] + V2 * H[K + 1+J * LDH + o_h] + V3 * H[K + 2+J * LDH + o_h];
                            H[K+J * LDH + o_h] +=  - SUM * T1;
                            H[K + 1+J * LDH + o_h] +=  - SUM * T2;
                            H[K + 2+J * LDH + o_h] +=  - SUM * T3;
                        }
                        // *
                        // *              Apply G from the right to transform the columns of the
                        // *              matrix in rows I1 to min(K+3,I).
                        // *
                        H_K = K * LDH + o_h;
                        H_0 = (K + 1) * LDH + o_h;
                        H_1 = (K + 2) * LDH + o_h;
                        H_2 = (K + 1) * LDH + o_h;
                        H_3 = (K + 2) * LDH + o_h;
                        for (J = I1; J <= Math.Min(K + 3, I); J++)
                        {
                            SUM = H[J + H_K] + V2 * H[J + H_0] + V3 * H[J + H_1];
                            H[J + H_K] +=  - SUM * T1;
                            H[J + H_2] +=  - SUM * T2;
                            H[J + H_3] +=  - SUM * T3;
                        }
                        // *
                        if (WANTZ)
                        {
                            // *
                            // *                 Accumulate transformations in the matrix Z
                            // *
                            Z_K = K * LDZ + o_z;
                            Z_0 = (K + 1) * LDZ + o_z;
                            Z_1 = (K + 2) * LDZ + o_z;
                            Z_2 = (K + 1) * LDZ + o_z;
                            Z_3 = (K + 2) * LDZ + o_z;
                            for (J = ILOZ; J <= IHIZ; J++)
                            {
                                SUM = Z[J + Z_K] + V2 * Z[J + Z_0] + V3 * Z[J + Z_1];
                                Z[J + Z_K] +=  - SUM * T1;
                                Z[J + Z_2] +=  - SUM * T2;
                                Z[J + Z_3] +=  - SUM * T3;
                            }
                        }
                    }
                    else
                    {
                        if (NR == 2)
                        {
                            // *
                            // *              Apply G from the left to transform the rows of the matrix
                            // *              in columns K to I2.
                            // *
                            for (J = K; J <= I2; J++)
                            {
                                SUM = H[K+J * LDH + o_h] + V2 * H[K + 1+J * LDH + o_h];
                                H[K+J * LDH + o_h] +=  - SUM * T1;
                                H[K + 1+J * LDH + o_h] +=  - SUM * T2;
                            }
                            // *
                            // *              Apply G from the right to transform the columns of the
                            // *              matrix in rows I1 to min(K+3,I).
                            // *
                            H_K = K * LDH + o_h;
                            H_4 = (K + 1) * LDH + o_h;
                            H_5 = (K + 1) * LDH + o_h;
                            for (J = I1; J <= I; J++)
                            {
                                SUM = H[J + H_K] + V2 * H[J + H_4];
                                H[J + H_K] +=  - SUM * T1;
                                H[J + H_5] +=  - SUM * T2;
                            }
                            // *
                            if (WANTZ)
                            {
                                // *
                                // *                 Accumulate transformations in the matrix Z
                                // *
                                Z_K = K * LDZ + o_z;
                                Z_4 = (K + 1) * LDZ + o_z;
                                Z_5 = (K + 1) * LDZ + o_z;
                                for (J = ILOZ; J <= IHIZ; J++)
                                {
                                    SUM = Z[J + Z_K] + V2 * Z[J + Z_4];
                                    Z[J + Z_K] +=  - SUM * T1;
                                    Z[J + Z_5] +=  - SUM * T2;
                                }
                            }
                        }
                    }
                }
                // *
            }
            // *
            // *     Failure to converge in remaining number of iterations
            // *
            INFO = I;
            return;
            // *
        LABEL150:;
            // *
            if (L == I)
            {
                // *
                // *        H(I,I-1) is negligible: one eigenvalue has converged.
                // *
                WR[I + o_wr] = H[I+I * LDH + o_h];
                WI[I + o_wi] = ZERO;
            }
            else
            {
                if (L == I - 1)
                {
                    // *
                    // *        H(I-1,I-2) is negligible: a pair of eigenvalues have converged.
                    // *
                    // *        Transform the 2-by-2 submatrix to standard Schur form,
                    // *        and compute and store the eigenvalues.
                    // *
                    this._dlanv2.Run(ref H[I - 1+(I - 1) * LDH + o_h], ref H[I - 1+I * LDH + o_h], ref H[I+(I - 1) * LDH + o_h], ref H[I+I * LDH + o_h], ref WR[I - 1 + o_wr], ref WI[I - 1 + o_wi]
                                     , ref WR[I + o_wr], ref WI[I + o_wi], ref CS, ref SN);
                    // *
                    if (WANTT)
                    {
                        // *
                        // *           Apply the transformation to the rest of H.
                        // *
                        if (I2 > I)
                        {
                            this._drot.Run(I2 - I, ref H, I - 1+(I + 1) * LDH + o_h, LDH, ref H, I+(I + 1) * LDH + o_h, LDH, CS
                                           , SN);
                        }
                        this._drot.Run(I - I1 - 1, ref H, I1+(I - 1) * LDH + o_h, 1, ref H, I1+I * LDH + o_h, 1, CS
                                       , SN);
                    }
                    if (WANTZ)
                    {
                        // *
                        // *           Apply the transformation to Z.
                        // *
                        this._drot.Run(NZ, ref Z, ILOZ+(I - 1) * LDZ + o_z, 1, ref Z, ILOZ+I * LDZ + o_z, 1, CS
                                       , SN);
                    }
                }
            }
            // *
            // *     return to start of the main loop with new value of I.
            // *
            I = L - 1;
            goto LABEL20;
            // *
        LABEL160:;
            return;
            // *
            // *     End of DLAHQR
            // *

            #endregion

        }
    }
}
