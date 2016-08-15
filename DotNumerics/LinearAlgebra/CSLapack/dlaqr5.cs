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
    ///</summary>
    public class DLAQR5
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DGEMM _dgemm; DLABAD _dlabad; DLACPY _dlacpy; DLAQR1 _dlaqr1; DLARFG _dlarfg; DLASET _dlaset; 
        DTRMM _dtrmm;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; double[] VT = new double[3]; 

        #endregion

        public DLAQR5(DLAMCH dlamch, DGEMM dgemm, DLABAD dlabad, DLACPY dlacpy, DLAQR1 dlaqr1, DLARFG dlarfg, DLASET dlaset, DTRMM dtrmm)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dgemm = dgemm; this._dlabad = dlabad; this._dlacpy = dlacpy; this._dlaqr1 = dlaqr1; 
            this._dlarfg = dlarfg;this._dlaset = dlaset; this._dtrmm = dtrmm; 

            #endregion

        }
    
        public DLAQR5()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DLABAD dlabad = new DLABAD();
            DLAQR1 dlaqr1 = new DLAQR1();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DLASET dlaset = new DLASET(lsame);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dgemm = dgemm; this._dlabad = dlabad; this._dlacpy = dlacpy; this._dlaqr1 = dlaqr1; 
            this._dlarfg = dlarfg;this._dlaset = dlaset; this._dtrmm = dtrmm; 

            #endregion

        }
        /// <param name="WANTT">
        /// (input) logical scalar
        /// WANTT = .true. if the quasi-triangular Schur factor
        /// is being computed.  WANTT is set to .false. otherwise.
        ///</param>
        /// <param name="WANTZ">
        /// (input) logical scalar
        /// WANTZ = .true. if the orthogonal Schur factor is being
        /// computed.  WANTZ is set to .false. otherwise.
        ///</param>
        /// <param name="KACC22">
        /// (input) integer with value 0, 1, or 2.
        /// Specifies the computation mode of far-from-diagonal
        /// orthogonal updates.
        /// = 0: DLAQR5 does not accumulate reflections and does not
        /// use matrix-matrix multiply to update far-from-diagonal
        /// matrix entries.
        /// = 1: DLAQR5 accumulates reflections and uses matrix-matrix
        /// multiply to update the far-from-diagonal matrix entries.
        /// = 2: DLAQR5 accumulates reflections, uses matrix-matrix
        /// multiply to update the far-from-diagonal matrix entries,
        /// and takes advantage of 2-by-2 block structure during
        /// matrix multiplies.
        ///</param>
        /// <param name="N">
        /// (input) integer scalar
        /// N is the order of the Hessenberg matrix H upon which this
        /// subroutine operates.
        ///</param>
        /// <param name="KTOP">
        /// (input) integer scalar
        ///</param>
        /// <param name="KBOT">
        /// (input) integer scalar
        /// These are the first and last rows and columns of an
        /// isolated diagonal block upon which the QR sweep is to be
        /// applied. It is assumed without a check that
        /// either KTOP = 1  or   H(KTOP,KTOP-1) = 0
        /// and
        /// either KBOT = N  or   H(KBOT+1,KBOT) = 0.
        ///</param>
        /// <param name="NSHFTS">
        /// (input) integer scalar
        /// NSHFTS gives the number of simultaneous shifts.  NSHFTS
        /// must be positive and even.
        ///</param>
        /// <param name="SR">
        /// (input) DOUBLE PRECISION array of size (NSHFTS)
        ///</param>
        /// <param name="SI">
        /// (input) DOUBLE PRECISION array of size (NSHFTS)
        /// SR contains the real parts and SI contains the imaginary
        /// parts of the NSHFTS shifts of origin that define the
        /// multi-shift QR sweep.
        ///</param>
        /// <param name="H">
        /// (input/output) DOUBLE PRECISION array of size (LDH,N)
        /// On input H contains a Hessenberg matrix.  On output a
        /// multi-shift QR sweep with shifts SR(J)+i*SI(J) is applied
        /// to the isolated diagonal block in rows and columns KTOP
        /// through KBOT.
        ///</param>
        /// <param name="LDH">
        /// (input) integer scalar
        /// LDH is the leading dimension of H just as declared in the
        /// calling procedure.  LDH.GE.MAX(1,N).
        ///</param>
        /// <param name="ILOZ">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHIZ">
        /// (input) INTEGER
        /// Specify the rows of Z to which transformations must be
        /// applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array of size (LDZ,IHI)
        /// If WANTZ = .TRUE., then the QR Sweep orthogonal
        /// similarity transformation is accumulated into
        /// Z(ILOZ:IHIZ,ILO:IHI) from the right.
        /// If WANTZ = .FALSE., then Z is unreferenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) integer scalar
        /// LDA is the leading dimension of Z just as declared in
        /// the calling procedure. LDZ.GE.N.
        ///</param>
        /// <param name="V">
        /// (workspace) DOUBLE PRECISION array of size (LDV,NSHFTS/2)
        ///</param>
        /// <param name="LDV">
        /// (input) integer scalar
        /// LDV is the leading dimension of V as declared in the
        /// calling procedure.  LDV.GE.3.
        ///</param>
        /// <param name="U">
        /// (workspace) DOUBLE PRECISION array of size
        /// (LDU,3*NSHFTS-3)
        ///</param>
        /// <param name="LDU">
        /// (input) integer scalar
        /// LDU is the leading dimension of U just as declared in the
        /// in the calling subroutine.  LDU.GE.3*NSHFTS-3.
        ///</param>
        /// <param name="NV">
        /// (input) integer scalar
        /// NV is the number of rows in WV agailable for workspace.
        /// NV.GE.1.
        ///</param>
        /// <param name="WV">
        /// (workspace) DOUBLE PRECISION array of size
        /// (LDWV,3*NSHFTS-3)
        ///</param>
        /// <param name="LDWV">
        /// (input) integer scalar
        /// LDWV is the leading dimension of WV as declared in the
        /// in the calling subroutine.  LDWV.GE.NV.
        /// 
        ///</param>
        /// <param name="NH">
        /// (input) integer scalar
        /// NH is the number of columns in array WH available for
        /// workspace. NH.GE.1.
        ///</param>
        /// <param name="WH">
        /// (workspace) DOUBLE PRECISION array of size (LDWH,NH)
        ///</param>
        /// <param name="LDWH">
        /// (input) integer scalar
        /// Leading dimension of WH just as declared in the
        /// calling procedure.  LDWH.GE.3*NSHFTS-3.
        ///</param>
        public void Run(bool WANTT, bool WANTZ, int KACC22, int N, int KTOP, int KBOT
                         , int NSHFTS, ref double[] SR, int offset_sr, ref double[] SI, int offset_si, ref double[] H, int offset_h, int LDH, int ILOZ
                         , int IHIZ, ref double[] Z, int offset_z, int LDZ, ref double[] V, int offset_v, int LDV, ref double[] U, int offset_u
                         , int LDU, int NV, ref double[] WV, int offset_wv, int LDWV, int NH, ref double[] WH, int offset_wh
                         , int LDWH)
        {

            #region Variables
            
            double ALPHA = 0; double BETA = 0; double H11 = 0; double H12 = 0; double H21 = 0; double H22 = 0; double REFSUM = 0; 
            double SAFMAX = 0;double SAFMIN = 0; double SCL = 0; double SMLNUM = 0; double SWAP = 0; double TST1 = 0; 
            double TST2 = 0;double ULP = 0; int I = 0; int I2 = 0; int I4 = 0; int INCOL = 0; int J = 0; int J2 = 0; int J4 = 0; 
            int JBOT = 0;int JCOL = 0; int JLEN = 0; int JROW = 0; int JTOP = 0; int K = 0; int K1 = 0; int KDU = 0; int KMS = 0; 
            int KNZ = 0;int KRCOL = 0; int KZS = 0; int M = 0; int M22 = 0; int MBOT = 0; int MEND = 0; int MSTART = 0; 
            int MTOP = 0;int NBMPS = 0; int NDCOL = 0; int NS = 0; int NU = 0; bool ACCUM = false; bool BLK22 = false; 
            bool BMP22 = false;int offset_vt = 0; int o_vt = -1; 

            #endregion


            #region Implicit Variables
            
            int H_0 = 0; int H_1 = 0; int H_2 = 0; int H_3 = 0; int H_4 = 0; int H_5 = 0; int U_0 = 0; int U_1 = 0; int U_2 = 0; 
            int U_3 = 0;int U_4 = 0; int U_5 = 0; int Z_0 = 0; int Z_1 = 0; int Z_2 = 0; int Z_3 = 0; int Z_4 = 0; int Z_5 = 0; 
            int H_6 = 0;int H_7 = 0; int H_8 = 0; int H_9 = 0; int U_6 = 0; int U_7 = 0; int U_8 = 0; int U_9 = 0; int Z_6 = 0; 
            int Z_7 = 0;int Z_8 = 0; int Z_9 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_sr = -1 + offset_sr;  int o_si = -1 + offset_si;  int o_h = -1 - LDH + offset_h; 
             int o_z = -1 - LDZ + offset_z; int o_v = -1 - LDV + offset_v;  int o_u = -1 - LDU + offset_u; 
             int o_wv = -1 - LDWV + offset_wv; int o_wh = -1 - LDWH + offset_wh; 

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
            // *     This auxiliary subroutine called by DLAQR0 performs a
            // *     single small-bulge multi-shift QR sweep.
            // *
            // *      WANTT  (input) logical scalar
            // *             WANTT = .true. if the quasi-triangular Schur factor
            // *             is being computed.  WANTT is set to .false. otherwise.
            // *
            // *      WANTZ  (input) logical scalar
            // *             WANTZ = .true. if the orthogonal Schur factor is being
            // *             computed.  WANTZ is set to .false. otherwise.
            // *
            // *      KACC22 (input) integer with value 0, 1, or 2.
            // *             Specifies the computation mode of far-from-diagonal
            // *             orthogonal updates.
            // *        = 0: DLAQR5 does not accumulate reflections and does not
            // *             use matrix-matrix multiply to update far-from-diagonal
            // *             matrix entries.
            // *        = 1: DLAQR5 accumulates reflections and uses matrix-matrix
            // *             multiply to update the far-from-diagonal matrix entries.
            // *        = 2: DLAQR5 accumulates reflections, uses matrix-matrix
            // *             multiply to update the far-from-diagonal matrix entries,
            // *             and takes advantage of 2-by-2 block structure during
            // *             matrix multiplies.
            // *
            // *      N      (input) integer scalar
            // *             N is the order of the Hessenberg matrix H upon which this
            // *             subroutine operates.
            // *
            // *      KTOP   (input) integer scalar
            // *      KBOT   (input) integer scalar
            // *             These are the first and last rows and columns of an
            // *             isolated diagonal block upon which the QR sweep is to be
            // *             applied. It is assumed without a check that
            // *                       either KTOP = 1  or   H(KTOP,KTOP-1) = 0
            // *             and
            // *                       either KBOT = N  or   H(KBOT+1,KBOT) = 0.
            // *
            // *      NSHFTS (input) integer scalar
            // *             NSHFTS gives the number of simultaneous shifts.  NSHFTS
            // *             must be positive and even.
            // *
            // *      SR     (input) DOUBLE PRECISION array of size (NSHFTS)
            // *      SI     (input) DOUBLE PRECISION array of size (NSHFTS)
            // *             SR contains the real parts and SI contains the imaginary
            // *             parts of the NSHFTS shifts of origin that define the
            // *             multi-shift QR sweep.
            // *
            // *      H      (input/output) DOUBLE PRECISION array of size (LDH,N)
            // *             On input H contains a Hessenberg matrix.  On output a
            // *             multi-shift QR sweep with shifts SR(J)+i*SI(J) is applied
            // *             to the isolated diagonal block in rows and columns KTOP
            // *             through KBOT.
            // *
            // *      LDH    (input) integer scalar
            // *             LDH is the leading dimension of H just as declared in the
            // *             calling procedure.  LDH.GE.MAX(1,N).
            // *
            // *      ILOZ   (input) INTEGER
            // *      IHIZ   (input) INTEGER
            // *             Specify the rows of Z to which transformations must be
            // *             applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N
            // *
            // *      Z      (input/output) DOUBLE PRECISION array of size (LDZ,IHI)
            // *             If WANTZ = .TRUE., then the QR Sweep orthogonal
            // *             similarity transformation is accumulated into
            // *             Z(ILOZ:IHIZ,ILO:IHI) from the right.
            // *             If WANTZ = .FALSE., then Z is unreferenced.
            // *
            // *      LDZ    (input) integer scalar
            // *             LDA is the leading dimension of Z just as declared in
            // *             the calling procedure. LDZ.GE.N.
            // *
            // *      V      (workspace) DOUBLE PRECISION array of size (LDV,NSHFTS/2)
            // *
            // *      LDV    (input) integer scalar
            // *             LDV is the leading dimension of V as declared in the
            // *             calling procedure.  LDV.GE.3.
            // *
            // *      U      (workspace) DOUBLE PRECISION array of size
            // *             (LDU,3*NSHFTS-3)
            // *
            // *      LDU    (input) integer scalar
            // *             LDU is the leading dimension of U just as declared in the
            // *             in the calling subroutine.  LDU.GE.3*NSHFTS-3.
            // *
            // *      NH     (input) integer scalar
            // *             NH is the number of columns in array WH available for
            // *             workspace. NH.GE.1.
            // *
            // *      WH     (workspace) DOUBLE PRECISION array of size (LDWH,NH)
            // *
            // *      LDWH   (input) integer scalar
            // *             Leading dimension of WH just as declared in the
            // *             calling procedure.  LDWH.GE.3*NSHFTS-3.
            // *
            // *      NV     (input) integer scalar
            // *             NV is the number of rows in WV agailable for workspace.
            // *             NV.GE.1.
            // *
            // *      WV     (workspace) DOUBLE PRECISION array of size
            // *             (LDWV,3*NSHFTS-3)
            // *
            // *      LDWV   (input) integer scalar
            // *             LDWV is the leading dimension of WV as declared in the
            // *             in the calling subroutine.  LDWV.GE.NV.
            // *
            // *
            // *     ================================================================
            // *     Based on contributions by
            // *        Karen Braman and Ralph Byers, Department of Mathematics,
            // *        University of Kansas, USA
            // *
            // *     ============================================================
            // *     Reference:
            // *
            // *     K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
            // *     Algorithm Part I: Maintaining Well Focused Shifts, and
            // *     Level 3 Performance, SIAM Journal of Matrix Analysis,
            // *     volume 23, pages 929--947, 2002.
            // *
            // *     ============================================================
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            // *
            //      INTRINSIC          ABS, DBLE, MAX, MIN, MOD;
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     ==== If there are no shifts, then there is nothing to do. ====
            // *

            #endregion


            #region Body
            
            if (NSHFTS < 2) return;
            // *
            // *     ==== If the active block is empty or 1-by-1, then there
            // *     .    is nothing to do. ====
            // *
            if (KTOP >= KBOT) return;
            // *
            // *     ==== Shuffle shifts into pairs of real shifts and pairs
            // *     .    of complex conjugate shifts assuming complex
            // *     .    conjugate shifts are already adjacent to one
            // *     .    another. ====
            // *
            for (I = 1; I <= NSHFTS - 2; I += 2)
            {
                if (SI[I + o_si] !=  - SI[I + 1 + o_si])
                {
                    // *
                    SWAP = SR[I + o_sr];
                    SR[I + o_sr] = SR[I + 1 + o_sr];
                    SR[I + 1 + o_sr] = SR[I + 2 + o_sr];
                    SR[I + 2 + o_sr] = SWAP;
                    // *
                    SWAP = SI[I + o_si];
                    SI[I + o_si] = SI[I + 1 + o_si];
                    SI[I + 1 + o_si] = SI[I + 2 + o_si];
                    SI[I + 2 + o_si] = SWAP;
                }
            }
            // *
            // *     ==== NSHFTS is supposed to be even, but if is odd,
            // *     .    then simply reduce it by one.  The shuffle above
            // *     .    ensures that the dropped shift is real and that
            // *     .    the remaining shifts are paired. ====
            // *
            NS = NSHFTS - FortranLib.Mod(NSHFTS,2);
            // *
            // *     ==== Machine constants for deflation ====
            // *
            SAFMIN = this._dlamch.Run("SAFE MINIMUM");
            SAFMAX = ONE / SAFMIN;
            this._dlabad.Run(ref SAFMIN, ref SAFMAX);
            ULP = this._dlamch.Run("PRECISION");
            SMLNUM = SAFMIN * (Convert.ToDouble(N) / ULP);
            // *
            // *     ==== Use accumulated reflections to update far-from-diagonal
            // *     .    entries ? ====
            // *
            ACCUM = (KACC22 == 1) || (KACC22 == 2);
            // *
            // *     ==== If so, exploit the 2-by-2 block structure? ====
            // *
            BLK22 = (NS > 2) && (KACC22 == 2);
            // *
            // *     ==== clear trash ====
            // *
            if (KTOP + 2 <= KBOT) H[KTOP + 2+KTOP * LDH + o_h] = ZERO;
            // *
            // *     ==== NBMPS = number of 2-shift bulges in the chain ====
            // *
            NBMPS = NS / 2;
            // *
            // *     ==== KDU = width of slab ====
            // *
            KDU = 6 * NBMPS - 3;
            // *
            // *     ==== Create and chase chains of NBMPS bulges ====
            // *
            for (INCOL = 3 * (1 - NBMPS) + KTOP - 1; (3 * NBMPS - 2 >= 0) ? (INCOL <= KBOT - 2) : (INCOL >= KBOT - 2); INCOL += 3 * NBMPS - 2)
            {
                NDCOL = INCOL + KDU;
                if (ACCUM)
                {
                    this._dlaset.Run("ALL", KDU, KDU, ZERO, ONE, ref U, offset_u
                                     , LDU);
                }
                // *
                // *        ==== Near-the-diagonal bulge chase.  The following loop
                // *        .    performs the near-the-diagonal part of a small bulge
                // *        .    multi-shift QR sweep.  Each 6*NBMPS-2 column diagonal
                // *        .    chunk extends from column INCOL to column NDCOL
                // *        .    (including both column INCOL and column NDCOL). The
                // *        .    following loop chases a 3*NBMPS column long chain of
                // *        .    NBMPS bulges 3*NBMPS-2 columns to the right.  (INCOL
                // *        .    may be less than KTOP and and NDCOL may be greater than
                // *        .    KBOT indicating phantom columns from which to chase
                // *        .    bulges before they are actually introduced or to which
                // *        .    to chase bulges beyond column KBOT.)  ====
                // *
                for (KRCOL = INCOL; KRCOL <= Math.Min(INCOL + 3 * NBMPS - 3, KBOT - 2); KRCOL++)
                {
                    // *
                    // *           ==== Bulges number MTOP to MBOT are active double implicit
                    // *           .    shift bulges.  There may or may not also be small
                    // *           .    2-by-2 bulge, if there is room.  The inactive bulges
                    // *           .    (if any) must wait until the active bulges have moved
                    // *           .    down the diagonal to make room.  The phantom matrix
                    // *           .    paradigm described above helps keep track.  ====
                    // *
                    MTOP = Math.Max(1, ((KTOP - 1) - KRCOL + 2) / 3 + 1);
                    MBOT = Math.Min(NBMPS, (KBOT - KRCOL) / 3);
                    M22 = MBOT + 1;
                    BMP22 = (MBOT < NBMPS) && (KRCOL + 3 * (M22 - 1)) == (KBOT - 2);
                    // *
                    // *           ==== Generate reflections to chase the chain right
                    // *           .    one column.  (The minimum value of K is KTOP-1.) ====
                    // *
                    for (M = MTOP; M <= MBOT; M++)
                    {
                        K = KRCOL + 3 * (M - 1);
                        if (K == KTOP - 1)
                        {
                            this._dlaqr1.Run(3, H, KTOP+KTOP * LDH + o_h, LDH, SR[2 * M - 1 + o_sr], SI[2 * M - 1 + o_si], SR[2 * M + o_sr]
                                             , SI[2 * M + o_si], ref V, 1+M * LDV + o_v);
                            ALPHA = V[1+M * LDV + o_v];
                            this._dlarfg.Run(3, ref ALPHA, ref V, 2+M * LDV + o_v, 1, ref V[1+M * LDV + o_v]);
                        }
                        else
                        {
                            BETA = H[K + 1+K * LDH + o_h];
                            V[2+M * LDV + o_v] = H[K + 2+K * LDH + o_h];
                            V[3+M * LDV + o_v] = H[K + 3+K * LDH + o_h];
                            this._dlarfg.Run(3, ref BETA, ref V, 2+M * LDV + o_v, 1, ref V[1+M * LDV + o_v]);
                            // *
                            // *                 ==== A Bulge may collapse because of vigilant
                            // *                 .    deflation or destructive underflow.  (The
                            // *                 .    initial bulge is always collapsed.) Use
                            // *                 .    the two-small-subdiagonals trick to try
                            // *                 .    to get it started again. If V(2,M).NE.0 and
                            // *                 .    V(3,M) = H(K+3,K+1) = H(K+3,K+2) = 0, then
                            // *                 .    this bulge is collapsing into a zero
                            // *                 .    subdiagonal.  It will be restarted next
                            // *                 .    trip through the loop.)
                            // *
                            if (V[1+M * LDV + o_v] != ZERO && (V[3+M * LDV + o_v] != ZERO || (H[K + 3+(K + 1) * LDH + o_h] == ZERO && H[K + 3+(K + 2) * LDH + o_h] == ZERO)))
                            {
                                // *
                                // *                    ==== Typical case: not collapsed (yet). ====
                                // *
                                H[K + 1+K * LDH + o_h] = BETA;
                                H[K + 2+K * LDH + o_h] = ZERO;
                                H[K + 3+K * LDH + o_h] = ZERO;
                            }
                            else
                            {
                                // *
                                // *                    ==== Atypical case: collapsed.  Attempt to
                                // *                    .    reintroduce ignoring H(K+1,K).  If the
                                // *                    .    fill resulting from the new reflector
                                // *                    .    is too large, then abandon it.
                                // *                    .    Otherwise, use the new one. ====
                                // *
                                this._dlaqr1.Run(3, H, K + 1+(K + 1) * LDH + o_h, LDH, SR[2 * M - 1 + o_sr], SI[2 * M - 1 + o_si], SR[2 * M + o_sr]
                                                 , SI[2 * M + o_si], ref VT, offset_vt);
                                SCL = Math.Abs(VT[1 + o_vt]) + Math.Abs(VT[2 + o_vt]) + Math.Abs(VT[3 + o_vt]);
                                if (SCL != ZERO)
                                {
                                    VT[1 + o_vt] /= SCL;
                                    VT[2 + o_vt] /= SCL;
                                    VT[3 + o_vt] /= SCL;
                                }
                                // *
                                // *                    ==== The following is the traditional and
                                // *                    .    conservative two-small-subdiagonals
                                // *                    .    test.  ====
                                // *                    .
                                if (Math.Abs(H[K + 1+K * LDH + o_h]) * (Math.Abs(VT[2 + o_vt]) + Math.Abs(VT[3 + o_vt])) > ULP * Math.Abs(VT[1 + o_vt]) * (Math.Abs(H[K+K * LDH + o_h]) + Math.Abs(H[K + 1+(K + 1) * LDH + o_h]) + Math.Abs(H[K + 2+(K + 2) * LDH + o_h])))
                                {
                                    // *
                                    // *                       ==== Starting a new bulge here would
                                    // *                       .    create non-negligible fill.   If
                                    // *                       .    the old reflector is diagonal (only
                                    // *                       .    possible with underflows), then
                                    // *                       .    change it to I.  Otherwise, use
                                    // *                       .    it with trepidation. ====
                                    // *
                                    if (V[2+M * LDV + o_v] == ZERO && V[3+M * LDV + o_v] == ZERO)
                                    {
                                        V[1+M * LDV + o_v] = ZERO;
                                    }
                                    else
                                    {
                                        H[K + 1+K * LDH + o_h] = BETA;
                                        H[K + 2+K * LDH + o_h] = ZERO;
                                        H[K + 3+K * LDH + o_h] = ZERO;
                                    }
                                }
                                else
                                {
                                    // *
                                    // *                       ==== Stating a new bulge here would
                                    // *                       .    create only negligible fill.
                                    // *                       .    Replace the old reflector with
                                    // *                       .    the new one. ====
                                    // *
                                    ALPHA = VT[1 + o_vt];
                                    this._dlarfg.Run(3, ref ALPHA, ref VT, 2 + o_vt, 1, ref VT[1 + o_vt]);
                                    REFSUM = H[K + 1+K * LDH + o_h] + H[K + 2+K * LDH + o_h] * VT[2 + o_vt] + H[K + 3+K * LDH + o_h] * VT[3 + o_vt];
                                    H[K + 1+K * LDH + o_h] +=  - VT[1 + o_vt] * REFSUM;
                                    H[K + 2+K * LDH + o_h] = ZERO;
                                    H[K + 3+K * LDH + o_h] = ZERO;
                                    V[1+M * LDV + o_v] = VT[1 + o_vt];
                                    V[2+M * LDV + o_v] = VT[2 + o_vt];
                                    V[3+M * LDV + o_v] = VT[3 + o_vt];
                                }
                            }
                        }
                    }
                    // *
                    // *           ==== Generate a 2-by-2 reflection, if needed. ====
                    // *
                    K = KRCOL + 3 * (M22 - 1);
                    if (BMP22)
                    {
                        if (K == KTOP - 1)
                        {
                            this._dlaqr1.Run(2, H, K + 1+(K + 1) * LDH + o_h, LDH, SR[2 * M22 - 1 + o_sr], SI[2 * M22 - 1 + o_si], SR[2 * M22 + o_sr]
                                             , SI[2 * M22 + o_si], ref V, 1+M22 * LDV + o_v);
                            BETA = V[1+M22 * LDV + o_v];
                            this._dlarfg.Run(2, ref BETA, ref V, 2+M22 * LDV + o_v, 1, ref V[1+M22 * LDV + o_v]);
                        }
                        else
                        {
                            BETA = H[K + 1+K * LDH + o_h];
                            V[2+M22 * LDV + o_v] = H[K + 2+K * LDH + o_h];
                            this._dlarfg.Run(2, ref BETA, ref V, 2+M22 * LDV + o_v, 1, ref V[1+M22 * LDV + o_v]);
                            H[K + 1+K * LDH + o_h] = BETA;
                            H[K + 2+K * LDH + o_h] = ZERO;
                        }
                    }
                    else
                    {
                        // *
                        // *              ==== Initialize V(1,M22) here to avoid possible undefined
                        // *              .    variable problems later. ====
                        // *
                        V[1+M22 * LDV + o_v] = ZERO;
                    }
                    // *
                    // *           ==== Multiply H by reflections from the left ====
                    // *
                    if (ACCUM)
                    {
                        JBOT = Math.Min(NDCOL, KBOT);
                    }
                    else
                    {
                        if (WANTT)
                        {
                            JBOT = N;
                        }
                        else
                        {
                            JBOT = KBOT;
                        }
                    }
                    for (J = Math.Max(KTOP, KRCOL); J <= JBOT; J++)
                    {
                        MEND = Math.Min(MBOT, (J - KRCOL + 2) / 3);
                        for (M = MTOP; M <= MEND; M++)
                        {
                            K = KRCOL + 3 * (M - 1);
                            REFSUM = V[1+M * LDV + o_v] * (H[K + 1+J * LDH + o_h] + V[2+M * LDV + o_v] * H[K + 2+J * LDH + o_h] + V[3+M * LDV + o_v] * H[K + 3+J * LDH + o_h]);
                            H[K + 1+J * LDH + o_h] -= REFSUM;
                            H[K + 2+J * LDH + o_h] +=  - REFSUM * V[2+M * LDV + o_v];
                            H[K + 3+J * LDH + o_h] +=  - REFSUM * V[3+M * LDV + o_v];
                        }
                    }
                    if (BMP22)
                    {
                        K = KRCOL + 3 * (M22 - 1);
                        for (J = Math.Max(K + 1, KTOP); J <= JBOT; J++)
                        {
                            REFSUM = V[1+M22 * LDV + o_v] * (H[K + 1+J * LDH + o_h] + V[2+M22 * LDV + o_v] * H[K + 2+J * LDH + o_h]);
                            H[K + 1+J * LDH + o_h] -= REFSUM;
                            H[K + 2+J * LDH + o_h] +=  - REFSUM * V[2+M22 * LDV + o_v];
                        }
                    }
                    // *
                    // *           ==== Multiply H by reflections from the right.
                    // *           .    Delay filling in the last row until the
                    // *           .    vigilant deflation check is complete. ====
                    // *
                    if (ACCUM)
                    {
                        JTOP = Math.Max(KTOP, INCOL);
                    }
                    else
                    {
                        if (WANTT)
                        {
                            JTOP = 1;
                        }
                        else
                        {
                            JTOP = KTOP;
                        }
                    }
                    for (M = MTOP; M <= MBOT; M++)
                    {
                        if (V[1+M * LDV + o_v] != ZERO)
                        {
                            K = KRCOL + 3 * (M - 1);
                            H_0 = (K + 1) * LDH + o_h;
                            H_1 = (K + 2) * LDH + o_h;
                            H_2 = (K + 3) * LDH + o_h;
                            H_3 = (K + 1) * LDH + o_h;
                            H_4 = (K + 2) * LDH + o_h;
                            H_5 = (K + 3) * LDH + o_h;
                            for (J = JTOP; J <= Math.Min(KBOT, K + 3); J++)
                            {
                                REFSUM = V[1+M * LDV + o_v] * (H[J + H_0] + V[2+M * LDV + o_v] * H[J + H_1] + V[3+M * LDV + o_v] * H[J + H_2]);
                                H[J + H_3] -= REFSUM;
                                H[J + H_4] +=  - REFSUM * V[2+M * LDV + o_v];
                                H[J + H_5] +=  - REFSUM * V[3+M * LDV + o_v];
                            }
                            // *
                            if (ACCUM)
                            {
                                // *
                                // *                    ==== Accumulate U. (If necessary, update Z later
                                // *                    .    with with an efficient matrix-matrix
                                // *                    .    multiply.) ====
                                // *
                                KMS = K - INCOL;
                                U_0 = (KMS + 1) * LDU + o_u;
                                U_1 = (KMS + 2) * LDU + o_u;
                                U_2 = (KMS + 3) * LDU + o_u;
                                U_3 = (KMS + 1) * LDU + o_u;
                                U_4 = (KMS + 2) * LDU + o_u;
                                U_5 = (KMS + 3) * LDU + o_u;
                                for (J = Math.Max(1, KTOP - INCOL); J <= KDU; J++)
                                {
                                    REFSUM = V[1+M * LDV + o_v] * (U[J + U_0] + V[2+M * LDV + o_v] * U[J + U_1] + V[3+M * LDV + o_v] * U[J + U_2]);
                                    U[J + U_3] -= REFSUM;
                                    U[J + U_4] +=  - REFSUM * V[2+M * LDV + o_v];
                                    U[J + U_5] +=  - REFSUM * V[3+M * LDV + o_v];
                                }
                            }
                            else
                            {
                                if (WANTZ)
                                {
                                    // *
                                    // *                    ==== U is not accumulated, so update Z
                                    // *                    .    now by multiplying by reflections
                                    // *                    .    from the right. ====
                                    // *
                                    Z_0 = (K + 1) * LDZ + o_z;
                                    Z_1 = (K + 2) * LDZ + o_z;
                                    Z_2 = (K + 3) * LDZ + o_z;
                                    Z_3 = (K + 1) * LDZ + o_z;
                                    Z_4 = (K + 2) * LDZ + o_z;
                                    Z_5 = (K + 3) * LDZ + o_z;
                                    for (J = ILOZ; J <= IHIZ; J++)
                                    {
                                        REFSUM = V[1+M * LDV + o_v] * (Z[J + Z_0] + V[2+M * LDV + o_v] * Z[J + Z_1] + V[3+M * LDV + o_v] * Z[J + Z_2]);
                                        Z[J + Z_3] -= REFSUM;
                                        Z[J + Z_4] +=  - REFSUM * V[2+M * LDV + o_v];
                                        Z[J + Z_5] +=  - REFSUM * V[3+M * LDV + o_v];
                                    }
                                }
                            }
                        }
                    }
                    // *
                    // *           ==== Special case: 2-by-2 reflection (if needed) ====
                    // *
                    K = KRCOL + 3 * (M22 - 1);
                    if (BMP22 && (V[1+M22 * LDV + o_v] != ZERO))
                    {
                        H_6 = (K + 1) * LDH + o_h;
                        H_7 = (K + 2) * LDH + o_h;
                        H_8 = (K + 1) * LDH + o_h;
                        H_9 = (K + 2) * LDH + o_h;
                        for (J = JTOP; J <= Math.Min(KBOT, K + 3); J++)
                        {
                            REFSUM = V[1+M22 * LDV + o_v] * (H[J + H_6] + V[2+M22 * LDV + o_v] * H[J + H_7]);
                            H[J + H_8] -= REFSUM;
                            H[J + H_9] +=  - REFSUM * V[2+M22 * LDV + o_v];
                        }
                        // *
                        if (ACCUM)
                        {
                            KMS = K - INCOL;
                            U_6 = (KMS + 1) * LDU + o_u;
                            U_7 = (KMS + 2) * LDU + o_u;
                            U_8 = (KMS + 1) * LDU + o_u;
                            U_9 = (KMS + 2) * LDU + o_u;
                            for (J = Math.Max(1, KTOP - INCOL); J <= KDU; J++)
                            {
                                REFSUM = V[1+M22 * LDV + o_v] * (U[J + U_6] + V[2+M22 * LDV + o_v] * U[J + U_7]);
                                U[J + U_8] -= REFSUM;
                                U[J + U_9] +=  - REFSUM * V[2+M22 * LDV + o_v];
                            }
                        }
                        else
                        {
                            if (WANTZ)
                            {
                                Z_6 = (K + 1) * LDZ + o_z;
                                Z_7 = (K + 2) * LDZ + o_z;
                                Z_8 = (K + 1) * LDZ + o_z;
                                Z_9 = (K + 2) * LDZ + o_z;
                                for (J = ILOZ; J <= IHIZ; J++)
                                {
                                    REFSUM = V[1+M22 * LDV + o_v] * (Z[J + Z_6] + V[2+M22 * LDV + o_v] * Z[J + Z_7]);
                                    Z[J + Z_8] -= REFSUM;
                                    Z[J + Z_9] +=  - REFSUM * V[2+M22 * LDV + o_v];
                                }
                            }
                        }
                    }
                    // *
                    // *           ==== Vigilant deflation check ====
                    // *
                    MSTART = MTOP;
                    if (KRCOL + 3 * (MSTART - 1) < KTOP) MSTART += 1;
                    MEND = MBOT;
                    if (BMP22) MEND += 1;
                    if (KRCOL == KBOT - 2) MEND += 1;
                    for (M = MSTART; M <= MEND; M++)
                    {
                        K = Math.Min(KBOT - 1, KRCOL + 3 * (M - 1));
                        // *
                        // *              ==== The following convergence test requires that
                        // *              .    the tradition small-compared-to-nearby-diagonals
                        // *              .    criterion and the Ahues & Tisseur (LAWN 122, 1997)
                        // *              .    criteria both be satisfied.  The latter improves
                        // *              .    accuracy in some examples. Falling back on an
                        // *              .    alternate convergence criterion when TST1 or TST2
                        // *              .    is zero (as done here) is traditional but probably
                        // *              .    unnecessary. ====
                        // *
                        if (H[K + 1+K * LDH + o_h] != ZERO)
                        {
                            TST1 = Math.Abs(H[K+K * LDH + o_h]) + Math.Abs(H[K + 1+(K + 1) * LDH + o_h]);
                            if (TST1 == ZERO)
                            {
                                if (K >= KTOP + 1) TST1 += Math.Abs(H[K+(K - 1) * LDH + o_h]);
                                if (K >= KTOP + 2) TST1 += Math.Abs(H[K+(K - 2) * LDH + o_h]);
                                if (K >= KTOP + 3) TST1 += Math.Abs(H[K+(K - 3) * LDH + o_h]);
                                if (K <= KBOT - 2) TST1 += Math.Abs(H[K + 2+(K + 1) * LDH + o_h]);
                                if (K <= KBOT - 3) TST1 += Math.Abs(H[K + 3+(K + 1) * LDH + o_h]);
                                if (K <= KBOT - 4) TST1 += Math.Abs(H[K + 4+(K + 1) * LDH + o_h]);
                            }
                            if (Math.Abs(H[K + 1+K * LDH + o_h]) <= Math.Max(SMLNUM, ULP * TST1))
                            {
                                H12 = Math.Max(Math.Abs(H[K + 1+K * LDH + o_h]), Math.Abs(H[K+(K + 1) * LDH + o_h]));
                                H21 = Math.Min(Math.Abs(H[K + 1+K * LDH + o_h]), Math.Abs(H[K+(K + 1) * LDH + o_h]));
                                H11 = Math.Max(Math.Abs(H[K + 1+(K + 1) * LDH + o_h]), Math.Abs(H[K+K * LDH + o_h] - H[K + 1+(K + 1) * LDH + o_h]));
                                H22 = Math.Min(Math.Abs(H[K + 1+(K + 1) * LDH + o_h]), Math.Abs(H[K+K * LDH + o_h] - H[K + 1+(K + 1) * LDH + o_h]));
                                SCL = H11 + H12;
                                TST2 = H22 * (H11 / SCL);
                                // *
                                if (TST2 == ZERO || H21 * (H12 / SCL) <= Math.Max(SMLNUM, ULP * TST2)) H[K + 1+K * LDH + o_h] = ZERO;
                            }
                        }
                    }
                    // *
                    // *           ==== Fill in the last row of each bulge. ====
                    // *
                    MEND = Math.Min(NBMPS, (KBOT - KRCOL - 1) / 3);
                    for (M = MTOP; M <= MEND; M++)
                    {
                        K = KRCOL + 3 * (M - 1);
                        REFSUM = V[1+M * LDV + o_v] * V[3+M * LDV + o_v] * H[K + 4+(K + 3) * LDH + o_h];
                        H[K + 4+(K + 1) * LDH + o_h] =  - REFSUM;
                        H[K + 4+(K + 2) * LDH + o_h] =  - REFSUM * V[2+M * LDV + o_v];
                        H[K + 4+(K + 3) * LDH + o_h] +=  - REFSUM * V[3+M * LDV + o_v];
                    }
                    // *
                    // *           ==== End of near-the-diagonal bulge chase. ====
                    // *
                }
                // *
                // *        ==== Use U (if accumulated) to update far-from-diagonal
                // *        .    entries in H.  If required, use U to update Z as
                // *        .    well. ====
                // *
                if (ACCUM)
                {
                    if (WANTT)
                    {
                        JTOP = 1;
                        JBOT = N;
                    }
                    else
                    {
                        JTOP = KTOP;
                        JBOT = KBOT;
                    }
                    if ((!BLK22) || (INCOL < KTOP) || (NDCOL > KBOT) || (NS <= 2))
                    {
                        // *
                        // *              ==== Updates not exploiting the 2-by-2 block
                        // *              .    structure of U.  K1 and NU keep track of
                        // *              .    the location and size of U in the special
                        // *              .    cases of introducing bulges and chasing
                        // *              .    bulges off the bottom.  In these special
                        // *              .    cases and in case the number of shifts
                        // *              .    is NS = 2, there is no 2-by-2 block
                        // *              .    structure to exploit.  ====
                        // *
                        K1 = Math.Max(1, KTOP - INCOL);
                        NU = (KDU - Math.Max(0, NDCOL - KBOT)) - K1 + 1;
                        // *
                        // *              ==== Horizontal Multiply ====
                        // *
                        for (JCOL = Math.Min(NDCOL, KBOT) + 1; (NH >= 0) ? (JCOL <= JBOT) : (JCOL >= JBOT); JCOL += NH)
                        {
                            JLEN = Math.Min(NH, JBOT - JCOL + 1);
                            this._dgemm.Run("C", "N", NU, JLEN, NU, ONE
                                            , U, K1+K1 * LDU + o_u, LDU, H, INCOL + K1+JCOL * LDH + o_h, LDH, ZERO, ref WH, offset_wh
                                            , LDWH);
                            this._dlacpy.Run("ALL", NU, JLEN, WH, offset_wh, LDWH, ref H, INCOL + K1+JCOL * LDH + o_h
                                             , LDH);
                        }
                        // *
                        // *              ==== Vertical multiply ====
                        // *
                        for (JROW = JTOP; (NV >= 0) ? (JROW <= Math.Max(KTOP, INCOL) - 1) : (JROW >= Math.Max(KTOP, INCOL) - 1); JROW += NV)
                        {
                            JLEN = Math.Min(NV, Math.Max(KTOP, INCOL) - JROW);
                            this._dgemm.Run("N", "N", JLEN, NU, NU, ONE
                                            , H, JROW+(INCOL + K1) * LDH + o_h, LDH, U, K1+K1 * LDU + o_u, LDU, ZERO, ref WV, offset_wv
                                            , LDWV);
                            this._dlacpy.Run("ALL", JLEN, NU, WV, offset_wv, LDWV, ref H, JROW+(INCOL + K1) * LDH + o_h
                                             , LDH);
                        }
                        // *
                        // *              ==== Z multiply (also vertical) ====
                        // *
                        if (WANTZ)
                        {
                            for (JROW = ILOZ; (NV >= 0) ? (JROW <= IHIZ) : (JROW >= IHIZ); JROW += NV)
                            {
                                JLEN = Math.Min(NV, IHIZ - JROW + 1);
                                this._dgemm.Run("N", "N", JLEN, NU, NU, ONE
                                                , Z, JROW+(INCOL + K1) * LDZ + o_z, LDZ, U, K1+K1 * LDU + o_u, LDU, ZERO, ref WV, offset_wv
                                                , LDWV);
                                this._dlacpy.Run("ALL", JLEN, NU, WV, offset_wv, LDWV, ref Z, JROW+(INCOL + K1) * LDZ + o_z
                                                 , LDZ);
                            }
                        }
                    }
                    else
                    {
                        // *
                        // *              ==== Updates exploiting U's 2-by-2 block structure.
                        // *              .    (I2, I4, J2, J4 are the last rows and columns
                        // *              .    of the blocks.) ====
                        // *
                        I2 = (KDU + 1) / 2;
                        I4 = KDU;
                        J2 = I4 - I2;
                        J4 = KDU;
                        // *
                        // *              ==== KZS and KNZ deal with the band of zeros
                        // *              .    along the diagonal of one of the triangular
                        // *              .    blocks. ====
                        // *
                        KZS = (J4 - J2) - (NS + 1);
                        KNZ = NS + 1;
                        // *
                        // *              ==== Horizontal multiply ====
                        // *
                        for (JCOL = Math.Min(NDCOL, KBOT) + 1; (NH >= 0) ? (JCOL <= JBOT) : (JCOL >= JBOT); JCOL += NH)
                        {
                            JLEN = Math.Min(NH, JBOT - JCOL + 1);
                            // *
                            // *                 ==== Copy bottom of H to top+KZS of scratch ====
                            // *                  (The first KZS rows get multiplied by zero.) ====
                            // *
                            this._dlacpy.Run("ALL", KNZ, JLEN, H, INCOL + 1 + J2+JCOL * LDH + o_h, LDH, ref WH, KZS + 1+1 * LDWH + o_wh
                                             , LDWH);
                            // *
                            // *                 ==== Multiply by U21' ====
                            // *
                            this._dlaset.Run("ALL", KZS, JLEN, ZERO, ZERO, ref WH, offset_wh
                                             , LDWH);
                            this._dtrmm.Run("L", "U", "C", "N", KNZ, JLEN
                                            , ONE, U, J2 + 1+(1 + KZS) * LDU + o_u, LDU, ref WH, KZS + 1+1 * LDWH + o_wh, LDWH);
                            // *
                            // *                 ==== Multiply top of H by U11' ====
                            // *
                            this._dgemm.Run("C", "N", I2, JLEN, J2, ONE
                                            , U, offset_u, LDU, H, INCOL + 1+JCOL * LDH + o_h, LDH, ONE, ref WH, offset_wh
                                            , LDWH);
                            // *
                            // *                 ==== Copy top of H bottom of WH ====
                            // *
                            this._dlacpy.Run("ALL", J2, JLEN, H, INCOL + 1+JCOL * LDH + o_h, LDH, ref WH, I2 + 1+1 * LDWH + o_wh
                                             , LDWH);
                            // *
                            // *                 ==== Multiply by U21' ====
                            // *
                            this._dtrmm.Run("L", "L", "C", "N", J2, JLEN
                                            , ONE, U, 1+(I2 + 1) * LDU + o_u, LDU, ref WH, I2 + 1+1 * LDWH + o_wh, LDWH);
                            // *
                            // *                 ==== Multiply by U22 ====
                            // *
                            this._dgemm.Run("C", "N", I4 - I2, JLEN, J4 - J2, ONE
                                            , U, J2 + 1+(I2 + 1) * LDU + o_u, LDU, H, INCOL + 1 + J2+JCOL * LDH + o_h, LDH, ONE, ref WH, I2 + 1+1 * LDWH + o_wh
                                            , LDWH);
                            // *
                            // *                 ==== Copy it back ====
                            // *
                            this._dlacpy.Run("ALL", KDU, JLEN, WH, offset_wh, LDWH, ref H, INCOL + 1+JCOL * LDH + o_h
                                             , LDH);
                        }
                        // *
                        // *              ==== Vertical multiply ====
                        // *
                        for (JROW = JTOP; (NV >= 0) ? (JROW <= Math.Max(INCOL, KTOP) - 1) : (JROW >= Math.Max(INCOL, KTOP) - 1); JROW += NV)
                        {
                            JLEN = Math.Min(NV, Math.Max(INCOL, KTOP) - JROW);
                            // *
                            // *                 ==== Copy right of H to scratch (the first KZS
                            // *                 .    columns get multiplied by zero) ====
                            // *
                            this._dlacpy.Run("ALL", JLEN, KNZ, H, JROW+(INCOL + 1 + J2) * LDH + o_h, LDH, ref WV, 1+(1 + KZS) * LDWV + o_wv
                                             , LDWV);
                            // *
                            // *                 ==== Multiply by U21 ====
                            // *
                            this._dlaset.Run("ALL", JLEN, KZS, ZERO, ZERO, ref WV, offset_wv
                                             , LDWV);
                            this._dtrmm.Run("R", "U", "N", "N", JLEN, KNZ
                                            , ONE, U, J2 + 1+(1 + KZS) * LDU + o_u, LDU, ref WV, 1+(1 + KZS) * LDWV + o_wv, LDWV);
                            // *
                            // *                 ==== Multiply by U11 ====
                            // *
                            this._dgemm.Run("N", "N", JLEN, I2, J2, ONE
                                            , H, JROW+(INCOL + 1) * LDH + o_h, LDH, U, offset_u, LDU, ONE, ref WV, offset_wv
                                            , LDWV);
                            // *
                            // *                 ==== Copy left of H to right of scratch ====
                            // *
                            this._dlacpy.Run("ALL", JLEN, J2, H, JROW+(INCOL + 1) * LDH + o_h, LDH, ref WV, 1+(1 + I2) * LDWV + o_wv
                                             , LDWV);
                            // *
                            // *                 ==== Multiply by U21 ====
                            // *
                            this._dtrmm.Run("R", "L", "N", "N", JLEN, I4 - I2
                                            , ONE, U, 1+(I2 + 1) * LDU + o_u, LDU, ref WV, 1+(1 + I2) * LDWV + o_wv, LDWV);
                            // *
                            // *                 ==== Multiply by U22 ====
                            // *
                            this._dgemm.Run("N", "N", JLEN, I4 - I2, J4 - J2, ONE
                                            , H, JROW+(INCOL + 1 + J2) * LDH + o_h, LDH, U, J2 + 1+(I2 + 1) * LDU + o_u, LDU, ONE, ref WV, 1+(1 + I2) * LDWV + o_wv
                                            , LDWV);
                            // *
                            // *                 ==== Copy it back ====
                            // *
                            this._dlacpy.Run("ALL", JLEN, KDU, WV, offset_wv, LDWV, ref H, JROW+(INCOL + 1) * LDH + o_h
                                             , LDH);
                        }
                        // *
                        // *              ==== Multiply Z (also vertical) ====
                        // *
                        if (WANTZ)
                        {
                            for (JROW = ILOZ; (NV >= 0) ? (JROW <= IHIZ) : (JROW >= IHIZ); JROW += NV)
                            {
                                JLEN = Math.Min(NV, IHIZ - JROW + 1);
                                // *
                                // *                    ==== Copy right of Z to left of scratch (first
                                // *                    .     KZS columns get multiplied by zero) ====
                                // *
                                this._dlacpy.Run("ALL", JLEN, KNZ, Z, JROW+(INCOL + 1 + J2) * LDZ + o_z, LDZ, ref WV, 1+(1 + KZS) * LDWV + o_wv
                                                 , LDWV);
                                // *
                                // *                    ==== Multiply by U12 ====
                                // *
                                this._dlaset.Run("ALL", JLEN, KZS, ZERO, ZERO, ref WV, offset_wv
                                                 , LDWV);
                                this._dtrmm.Run("R", "U", "N", "N", JLEN, KNZ
                                                , ONE, U, J2 + 1+(1 + KZS) * LDU + o_u, LDU, ref WV, 1+(1 + KZS) * LDWV + o_wv, LDWV);
                                // *
                                // *                    ==== Multiply by U11 ====
                                // *
                                this._dgemm.Run("N", "N", JLEN, I2, J2, ONE
                                                , Z, JROW+(INCOL + 1) * LDZ + o_z, LDZ, U, offset_u, LDU, ONE, ref WV, offset_wv
                                                , LDWV);
                                // *
                                // *                    ==== Copy left of Z to right of scratch ====
                                // *
                                this._dlacpy.Run("ALL", JLEN, J2, Z, JROW+(INCOL + 1) * LDZ + o_z, LDZ, ref WV, 1+(1 + I2) * LDWV + o_wv
                                                 , LDWV);
                                // *
                                // *                    ==== Multiply by U21 ====
                                // *
                                this._dtrmm.Run("R", "L", "N", "N", JLEN, I4 - I2
                                                , ONE, U, 1+(I2 + 1) * LDU + o_u, LDU, ref WV, 1+(1 + I2) * LDWV + o_wv, LDWV);
                                // *
                                // *                    ==== Multiply by U22 ====
                                // *
                                this._dgemm.Run("N", "N", JLEN, I4 - I2, J4 - J2, ONE
                                                , Z, JROW+(INCOL + 1 + J2) * LDZ + o_z, LDZ, U, J2 + 1+(I2 + 1) * LDU + o_u, LDU, ONE, ref WV, 1+(1 + I2) * LDWV + o_wv
                                                , LDWV);
                                // *
                                // *                    ==== Copy the result back to Z ====
                                // *
                                this._dlacpy.Run("ALL", JLEN, KDU, WV, offset_wv, LDWV, ref Z, JROW+(INCOL + 1) * LDZ + o_z
                                                 , LDZ);
                            }
                        }
                    }
                }
            }
            // *
            // *     ==== End of DLAQR5 ====
            // *

            #endregion

        }
    }
}
