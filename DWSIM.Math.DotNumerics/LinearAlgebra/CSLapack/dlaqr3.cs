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
    public class DLAQR3
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; ILAENV _ilaenv; DCOPY _dcopy; DGEHRD _dgehrd; DGEMM _dgemm; DLABAD _dlabad; DLACPY _dlacpy; 
        DLAHQR _dlahqr;DLANV2 _dlanv2; DLAQR4 _dlaqr4; DLARF _dlarf; DLARFG _dlarfg; DLASET _dlaset; DORGHR _dorghr; 
        DTREXC _dtrexc;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DLAQR3(DLAMCH dlamch, ILAENV ilaenv, DCOPY dcopy, DGEHRD dgehrd, DGEMM dgemm, DLABAD dlabad, DLACPY dlacpy, DLAHQR dlahqr, DLANV2 dlanv2, DLAQR4 dlaqr4
                      , DLARF dlarf, DLARFG dlarfg, DLASET dlaset, DORGHR dorghr, DTREXC dtrexc)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._ilaenv = ilaenv; this._dcopy = dcopy; this._dgehrd = dgehrd; this._dgemm = dgemm; 
            this._dlabad = dlabad;this._dlacpy = dlacpy; this._dlahqr = dlahqr; this._dlanv2 = dlanv2; this._dlaqr4 = dlaqr4; 
            this._dlarf = dlarf;this._dlarfg = dlarfg; this._dlaset = dlaset; this._dorghr = dorghr; this._dtrexc = dtrexc; 

            #endregion

        }
    
        public DLAQR3()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            DAXPY daxpy = new DAXPY();
            XERBLA xerbla = new XERBLA();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DLABAD dlabad = new DLABAD();
            DROT drot = new DROT();
            DLASSQ dlassq = new DLASSQ();
            IDAMAX idamax = new IDAMAX();
            DSWAP dswap = new DSWAP();
            DLAQR1 dlaqr1 = new DLAQR1();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEHD2 dgehd2 = new DGEHD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLAHR2 dlahr2 = new DLAHR2(daxpy, dcopy, dgemm, dgemv, dlacpy, dlarfg, dscal, dtrmm, dtrmv);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEHRD dgehrd = new DGEHRD(daxpy, dgehd2, dgemm, dlahr2, dlarfb, dtrmm, xerbla, ilaenv);
            DLANV2 dlanv2 = new DLANV2(dlamch, dlapy2);
            DLAHQR dlahqr = new DLAHQR(dlamch, dcopy, dlabad, dlanv2, dlarfg, drot);
            DLASET dlaset = new DLASET(lsame);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORGQR dorgqr = new DORGQR(dlarfb, dlarft, dorg2r, xerbla, ilaenv);
            DORGHR dorghr = new DORGHR(dorgqr, xerbla, ilaenv);
            DLANGE dlange = new DLANGE(dlassq, lsame);
            DLARFX dlarfx = new DLARFX(lsame, dgemv, dger);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASY2 dlasy2 = new DLASY2(idamax, dlamch, dcopy, dswap);
            DLAEXC dlaexc = new DLAEXC(dlamch, dlange, dlacpy, dlanv2, dlarfg, dlarfx, dlartg, dlasy2, drot);
            DTREXC dtrexc = new DTREXC(lsame, dlaexc, xerbla);
            DLAQR2 dlaqr2 = new DLAQR2(dlamch, dcopy, dgehrd, dgemm, dlabad, dlacpy, dlahqr, dlanv2, dlarf, dlarfg
                                       , dlaset, dorghr, dtrexc);
            DLAQR5 dlaqr5 = new DLAQR5(dlamch, dgemm, dlabad, dlacpy, dlaqr1, dlarfg, dlaset, dtrmm);
            DLAQR4 dlaqr4 = new DLAQR4(ilaenv, dlacpy, dlahqr, dlanv2, dlaqr2, dlaqr5);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._ilaenv = ilaenv; this._dcopy = dcopy; this._dgehrd = dgehrd; this._dgemm = dgemm; 
            this._dlabad = dlabad;this._dlacpy = dlacpy; this._dlahqr = dlahqr; this._dlanv2 = dlanv2; this._dlaqr4 = dlaqr4; 
            this._dlarf = dlarf;this._dlarfg = dlarfg; this._dlaset = dlaset; this._dorghr = dorghr; this._dtrexc = dtrexc; 

            #endregion

        }
        /// <param name="WANTT">
        /// (input) LOGICAL
        /// If .TRUE., then the Hessenberg matrix H is fully updated
        /// so that the quasi-triangular Schur factor may be
        /// computed (in cooperation with the calling subroutine).
        /// If .FALSE., then only enough of H is updated to preserve
        /// the eigenvalues.
        ///</param>
        /// <param name="WANTZ">
        /// (input) LOGICAL
        /// If .TRUE., then the orthogonal matrix Z is updated so
        /// so that the orthogonal Schur factor may be computed
        /// (in cooperation with the calling subroutine).
        /// If .FALSE., then Z is not referenced.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix H and (if WANTZ is .TRUE.) the
        /// order of the orthogonal matrix Z.
        ///</param>
        /// <param name="KTOP">
        /// (input) INTEGER
        /// It is assumed that either KTOP = 1 or H(KTOP,KTOP-1)=0.
        /// KBOT and KTOP together determine an isolated block
        /// along the diagonal of the Hessenberg matrix.
        ///</param>
        /// <param name="KBOT">
        /// (input) INTEGER
        /// It is assumed without a check that either
        /// KBOT = N or H(KBOT+1,KBOT)=0.  KBOT and KTOP together
        /// determine an isolated block along the diagonal of the
        /// Hessenberg matrix.
        ///</param>
        /// <param name="NW">
        /// (input) INTEGER
        /// Deflation window size.  1 .LE. NW .LE. (KBOT-KTOP+1).
        ///</param>
        /// <param name="H">
        /// (input/output) DOUBLE PRECISION array, dimension (LDH,N)
        /// On input the initial N-by-N section of H stores the
        /// Hessenberg matrix undergoing aggressive early deflation.
        /// On output H has been transformed by an orthogonal
        /// similarity transformation, perturbed, and the returned
        /// to Hessenberg form that (it is to be hoped) has some
        /// zero subdiagonal entries.
        ///</param>
        /// <param name="LDH">
        /// (input) integer
        /// Leading dimension of H just as declared in the calling
        /// subroutine.  N .LE. LDH
        ///</param>
        /// <param name="ILOZ">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHIZ">
        /// (input) INTEGER
        /// Specify the rows of Z to which transformations must be
        /// applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N.
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
        /// IF WANTZ is .TRUE., then on output, the orthogonal
        /// similarity transformation mentioned above has been
        /// accumulated into Z(ILOZ:IHIZ,ILO:IHI) from the right.
        /// If WANTZ is .FALSE., then Z is unreferenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) integer
        /// The leading dimension of Z just as declared in the
        /// calling subroutine.  1 .LE. LDZ.
        ///</param>
        /// <param name="NS">
        /// (output) integer
        /// The number of unconverged (ie approximate) eigenvalues
        /// returned in SR and SI that may be used as shifts by the
        /// calling subroutine.
        ///</param>
        /// <param name="ND">
        /// (output) integer
        /// The number of converged eigenvalues uncovered by this
        /// subroutine.
        ///</param>
        /// <param name="SR">
        /// (output) DOUBLE PRECISION array, dimension KBOT
        ///</param>
        /// <param name="SI">
        /// (output) DOUBLE PRECISION array, dimension KBOT
        /// On output, the real and imaginary parts of approximate
        /// eigenvalues that may be used for shifts are stored in
        /// SR(KBOT-ND-NS+1) through SR(KBOT-ND) and
        /// SI(KBOT-ND-NS+1) through SI(KBOT-ND), respectively.
        /// The real and imaginary parts of converged eigenvalues
        /// are stored in SR(KBOT-ND+1) through SR(KBOT) and
        /// SI(KBOT-ND+1) through SI(KBOT), respectively.
        ///</param>
        /// <param name="V">
        /// (workspace) DOUBLE PRECISION array, dimension (LDV,NW)
        /// An NW-by-NW work array.
        ///</param>
        /// <param name="LDV">
        /// (input) integer scalar
        /// The leading dimension of V just as declared in the
        /// calling subroutine.  NW .LE. LDV
        ///</param>
        /// <param name="NH">
        /// (input) integer scalar
        /// The number of columns of T.  NH.GE.NW.
        ///</param>
        /// <param name="T">
        /// (workspace) DOUBLE PRECISION array, dimension (LDT,NW)
        ///</param>
        /// <param name="LDT">
        /// (input) integer
        /// The leading dimension of T just as declared in the
        /// calling subroutine.  NW .LE. LDT
        ///</param>
        /// <param name="NV">
        /// (input) integer
        /// The number of rows of work array WV available for
        /// workspace.  NV.GE.NW.
        ///</param>
        /// <param name="WV">
        /// (workspace) DOUBLE PRECISION array, dimension (LDWV,NW)
        ///</param>
        /// <param name="LDWV">
        /// (input) integer
        /// The leading dimension of W just as declared in the
        /// calling subroutine.  NW .LE. LDV
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension LWORK.
        /// On exit, WORK(1) is set to an estimate of the optimal value
        /// of LWORK for the given values of N, NW, KTOP and KBOT.
        ///</param>
        /// <param name="LWORK">
        /// (input) integer
        /// The dimension of the work array WORK.  LWORK = 2*NW
        /// suffices, but greater efficiency may result from larger
        /// values of LWORK.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; DLAQR3
        /// only estimates the optimal workspace size for the given
        /// values of N, NW, KTOP and KBOT.  The estimate is returned
        /// in WORK(1).  No error message related to LWORK is issued
        /// by XERBLA.  Neither H nor Z are accessed.
        ///</param>
        public void Run(bool WANTT, bool WANTZ, int N, int KTOP, int KBOT, int NW
                         , ref double[] H, int offset_h, int LDH, int ILOZ, int IHIZ, ref double[] Z, int offset_z, int LDZ
                         , ref int NS, ref int ND, ref double[] SR, int offset_sr, ref double[] SI, int offset_si, ref double[] V, int offset_v, int LDV
                         , int NH, ref double[] T, int offset_t, int LDT, int NV, ref double[] WV, int offset_wv, int LDWV
                         , ref double[] WORK, int offset_work, int LWORK)
        {

            #region Variables
            
            double AA = 0; double BB = 0; double BETA = 0; double CC = 0; double CS = 0; double DD = 0; double EVI = 0; 
            double EVK = 0;double FOO = 0; double S = 0; double SAFMAX = 0; double SAFMIN = 0; double SMLNUM = 0; double SN = 0; 
            double TAU = 0;double ULP = 0; int I = 0; int IFST = 0; int ILST = 0; int INFO = 0; int INFQR = 0; int J = 0; 
            int JW = 0;int K = 0; int KCOL = 0; int KEND = 0; int KLN = 0; int KROW = 0; int KWTOP = 0; int LTOP = 0; 
            int LWK1 = 0;int LWK2 = 0; int LWK3 = 0; int LWKOPT = 0; int NMIN = 0; bool BULGE = false; bool SORTED = false; 

            #endregion


            #region Array Index Correction
            
             int o_h = -1 - LDH + offset_h;  int o_z = -1 - LDZ + offset_z;  int o_sr = -1 + offset_sr; 
             int o_si = -1 + offset_si; int o_v = -1 - LDV + offset_v;  int o_t = -1 - LDT + offset_t; 
             int o_wv = -1 - LDWV + offset_wv; int o_work = -1 + offset_work; 

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
            // *     ******************************************************************
            // *     Aggressive early deflation:
            // *
            // *     This subroutine accepts as input an upper Hessenberg matrix
            // *     H and performs an orthogonal similarity transformation
            // *     designed to detect and deflate fully converged eigenvalues from
            // *     a trailing principal submatrix.  On output H has been over-
            // *     written by a new Hessenberg matrix that is a perturbation of
            // *     an orthogonal similarity transformation of H.  It is to be
            // *     hoped that the final version of H has many zero subdiagonal
            // *     entries.
            // *
            // *     ******************************************************************
            // *     WANTT   (input) LOGICAL
            // *          If .TRUE., then the Hessenberg matrix H is fully updated
            // *          so that the quasi-triangular Schur factor may be
            // *          computed (in cooperation with the calling subroutine).
            // *          If .FALSE., then only enough of H is updated to preserve
            // *          the eigenvalues.
            // *
            // *     WANTZ   (input) LOGICAL
            // *          If .TRUE., then the orthogonal matrix Z is updated so
            // *          so that the orthogonal Schur factor may be computed
            // *          (in cooperation with the calling subroutine).
            // *          If .FALSE., then Z is not referenced.
            // *
            // *     N       (input) INTEGER
            // *          The order of the matrix H and (if WANTZ is .TRUE.) the
            // *          order of the orthogonal matrix Z.
            // *
            // *     KTOP    (input) INTEGER
            // *          It is assumed that either KTOP = 1 or H(KTOP,KTOP-1)=0.
            // *          KBOT and KTOP together determine an isolated block
            // *          along the diagonal of the Hessenberg matrix.
            // *
            // *     KBOT    (input) INTEGER
            // *          It is assumed without a check that either
            // *          KBOT = N or H(KBOT+1,KBOT)=0.  KBOT and KTOP together
            // *          determine an isolated block along the diagonal of the
            // *          Hessenberg matrix.
            // *
            // *     NW      (input) INTEGER
            // *          Deflation window size.  1 .LE. NW .LE. (KBOT-KTOP+1).
            // *
            // *     H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
            // *          On input the initial N-by-N section of H stores the
            // *          Hessenberg matrix undergoing aggressive early deflation.
            // *          On output H has been transformed by an orthogonal
            // *          similarity transformation, perturbed, and the returned
            // *          to Hessenberg form that (it is to be hoped) has some
            // *          zero subdiagonal entries.
            // *
            // *     LDH     (input) integer
            // *          Leading dimension of H just as declared in the calling
            // *          subroutine.  N .LE. LDH
            // *
            // *     ILOZ    (input) INTEGER
            // *     IHIZ    (input) INTEGER
            // *          Specify the rows of Z to which transformations must be
            // *          applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N.
            // *
            // *     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
            // *          IF WANTZ is .TRUE., then on output, the orthogonal
            // *          similarity transformation mentioned above has been
            // *          accumulated into Z(ILOZ:IHIZ,ILO:IHI) from the right.
            // *          If WANTZ is .FALSE., then Z is unreferenced.
            // *
            // *     LDZ     (input) integer
            // *          The leading dimension of Z just as declared in the
            // *          calling subroutine.  1 .LE. LDZ.
            // *
            // *     NS      (output) integer
            // *          The number of unconverged (ie approximate) eigenvalues
            // *          returned in SR and SI that may be used as shifts by the
            // *          calling subroutine.
            // *
            // *     ND      (output) integer
            // *          The number of converged eigenvalues uncovered by this
            // *          subroutine.
            // *
            // *     SR      (output) DOUBLE PRECISION array, dimension KBOT
            // *     SI      (output) DOUBLE PRECISION array, dimension KBOT
            // *          On output, the real and imaginary parts of approximate
            // *          eigenvalues that may be used for shifts are stored in
            // *          SR(KBOT-ND-NS+1) through SR(KBOT-ND) and
            // *          SI(KBOT-ND-NS+1) through SI(KBOT-ND), respectively.
            // *          The real and imaginary parts of converged eigenvalues
            // *          are stored in SR(KBOT-ND+1) through SR(KBOT) and
            // *          SI(KBOT-ND+1) through SI(KBOT), respectively.
            // *
            // *     V       (workspace) DOUBLE PRECISION array, dimension (LDV,NW)
            // *          An NW-by-NW work array.
            // *
            // *     LDV     (input) integer scalar
            // *          The leading dimension of V just as declared in the
            // *          calling subroutine.  NW .LE. LDV
            // *
            // *     NH      (input) integer scalar
            // *          The number of columns of T.  NH.GE.NW.
            // *
            // *     T       (workspace) DOUBLE PRECISION array, dimension (LDT,NW)
            // *
            // *     LDT     (input) integer
            // *          The leading dimension of T just as declared in the
            // *          calling subroutine.  NW .LE. LDT
            // *
            // *     NV      (input) integer
            // *          The number of rows of work array WV available for
            // *          workspace.  NV.GE.NW.
            // *
            // *     WV      (workspace) DOUBLE PRECISION array, dimension (LDWV,NW)
            // *
            // *     LDWV    (input) integer
            // *          The leading dimension of W just as declared in the
            // *          calling subroutine.  NW .LE. LDV
            // *
            // *     WORK    (workspace) DOUBLE PRECISION array, dimension LWORK.
            // *          On exit, WORK(1) is set to an estimate of the optimal value
            // *          of LWORK for the given values of N, NW, KTOP and KBOT.
            // *
            // *     LWORK   (input) integer
            // *          The dimension of the work array WORK.  LWORK = 2*NW
            // *          suffices, but greater efficiency may result from larger
            // *          values of LWORK.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; DLAQR3
            // *          only estimates the optimal workspace size for the given
            // *          values of N, NW, KTOP and KBOT.  The estimate is returned
            // *          in WORK(1).  No error message related to LWORK is issued
            // *          by XERBLA.  Neither H nor Z are accessed.
            // *
            // *     ================================================================
            // *     Based on contributions by
            // *        Karen Braman and Ralph Byers, Department of Mathematics,
            // *        University of Kansas, USA
            // *
            // *     ==================================================================
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, DBLE, INT, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     ==== Estimate optimal workspace. ====
            // *

            #endregion


            #region Body
            
            JW = Math.Min(NW, KBOT - KTOP + 1);
            if (JW <= 2)
            {
                LWKOPT = 1;
            }
            else
            {
                // *
                // *        ==== Workspace query call to DGEHRD ====
                // *
                this._dgehrd.Run(JW, 1, JW - 1, ref T, offset_t, LDT, ref WORK, offset_work
                                 , ref WORK, offset_work,  - 1, ref INFO);
                LWK1 = Convert.ToInt32(Math.Truncate(WORK[1 + o_work]));
                // *
                // *        ==== Workspace query call to DORGHR ====
                // *
                this._dorghr.Run(JW, 1, JW - 1, ref T, offset_t, LDT, WORK, offset_work
                                 , ref WORK, offset_work,  - 1, ref INFO);
                LWK2 = Convert.ToInt32(Math.Truncate(WORK[1 + o_work]));
                // *
                // *        ==== Workspace query call to DLAQR4 ====
                // *
                this._dlaqr4.Run(true, true, JW, 1, JW, ref T, offset_t
                                 , LDT, ref SR, offset_sr, ref SI, offset_si, 1, JW, ref V, offset_v
                                 , LDV, ref WORK, offset_work,  - 1, ref INFQR);
                LWK3 = Convert.ToInt32(Math.Truncate(WORK[1 + o_work]));
                // *
                // *        ==== Optimal workspace ====
                // *
                LWKOPT = Math.Max(JW + Math.Max(LWK1, LWK2), LWK3);
            }
            // *
            // *     ==== Quick return in case of workspace query. ====
            // *
            if (LWORK ==  - 1)
            {
                WORK[1 + o_work] = Convert.ToDouble(LWKOPT);
                return;
            }
            // *
            // *     ==== Nothing to do ...
            // *     ... for an empty active block ... ====
            NS = 0;
            ND = 0;
            if (KTOP > KBOT) return;
            // *     ... nor for an empty deflation window. ====
            if (NW < 1) return;
            // *
            // *     ==== Machine constants ====
            // *
            SAFMIN = this._dlamch.Run("SAFE MINIMUM");
            SAFMAX = ONE / SAFMIN;
            this._dlabad.Run(ref SAFMIN, ref SAFMAX);
            ULP = this._dlamch.Run("PRECISION");
            SMLNUM = SAFMIN * (Convert.ToDouble(N) / ULP);
            // *
            // *     ==== Setup deflation window ====
            // *
            JW = Math.Min(NW, KBOT - KTOP + 1);
            KWTOP = KBOT - JW + 1;
            if (KWTOP == KTOP)
            {
                S = ZERO;
            }
            else
            {
                S = H[KWTOP+(KWTOP - 1) * LDH + o_h];
            }
            // *
            if (KBOT == KWTOP)
            {
                // *
                // *        ==== 1-by-1 deflation window: not much to do ====
                // *
                SR[KWTOP + o_sr] = H[KWTOP+KWTOP * LDH + o_h];
                SI[KWTOP + o_si] = ZERO;
                NS = 1;
                ND = 0;
                if (Math.Abs(S) <= Math.Max(SMLNUM, ULP * Math.Abs(H[KWTOP+KWTOP * LDH + o_h])))
                {
                    NS = 0;
                    ND = 1;
                    if (KWTOP > KTOP) H[KWTOP+(KWTOP - 1) * LDH + o_h] = ZERO;
                }
                return;
            }
            // *
            // *     ==== Convert to spike-triangular form.  (In case of a
            // *     .    rare QR failure, this routine continues to do
            // *     .    aggressive early deflation using that part of
            // *     .    the deflation window that converged using INFQR
            // *     .    here and there to keep track.) ====
            // *
            this._dlacpy.Run("U", JW, JW, H, KWTOP+KWTOP * LDH + o_h, LDH, ref T, offset_t
                             , LDT);
            this._dcopy.Run(JW - 1, H, KWTOP + 1+KWTOP * LDH + o_h, LDH + 1, ref T, 2+1 * LDT + o_t, LDT + 1);
            // *
            this._dlaset.Run("A", JW, JW, ZERO, ONE, ref V, offset_v
                             , LDV);
            NMIN = this._ilaenv.Run(12, "DLAQR3", "SV", JW, 1, JW, LWORK);
            if (JW > NMIN)
            {
                this._dlaqr4.Run(true, true, JW, 1, JW, ref T, offset_t
                                 , LDT, ref SR, KWTOP + o_sr, ref SI, KWTOP + o_si, 1, JW, ref V, offset_v
                                 , LDV, ref WORK, offset_work, LWORK, ref INFQR);
            }
            else
            {
                this._dlahqr.Run(true, true, JW, 1, JW, ref T, offset_t
                                 , LDT, ref SR, KWTOP + o_sr, ref SI, KWTOP + o_si, 1, JW, ref V, offset_v
                                 , LDV, ref INFQR);
            }
            // *
            // *     ==== DTREXC needs a clean margin near the diagonal ====
            // *
            for (J = 1; J <= JW - 3; J++)
            {
                T[J + 2+J * LDT + o_t] = ZERO;
                T[J + 3+J * LDT + o_t] = ZERO;
            }
            if (JW > 2) T[JW+(JW - 2) * LDT + o_t] = ZERO;
            // *
            // *     ==== Deflation detection loop ====
            // *
            NS = JW;
            ILST = INFQR + 1;
        LABEL20:;
            if (ILST <= NS)
            {
                if (NS == 1)
                {
                    BULGE = false;
                }
                else
                {
                    BULGE = T[NS+(NS - 1) * LDT + o_t] != ZERO;
                }
                // *
                // *        ==== Small spike tip test for deflation ====
                // *
                if (!BULGE)
                {
                    // *
                    // *           ==== Real eigenvalue ====
                    // *
                    FOO = Math.Abs(T[NS+NS * LDT + o_t]);
                    if (FOO == ZERO) FOO = Math.Abs(S);
                    if (Math.Abs(S * V[1+NS * LDV + o_v]) <= Math.Max(SMLNUM, ULP * FOO))
                    {
                        // *
                        // *              ==== Deflatable ====
                        // *
                        NS -= 1;
                    }
                    else
                    {
                        // *
                        // *              ==== Undeflatable.   Move it up out of the way.
                        // *              .    (DTREXC can not fail in this case.) ====
                        // *
                        IFST = NS;
                        this._dtrexc.Run("V", JW, ref T, offset_t, LDT, ref V, offset_v, LDV
                                         , ref IFST, ref ILST, ref WORK, offset_work, ref INFO);
                        ILST += 1;
                    }
                }
                else
                {
                    // *
                    // *           ==== Complex conjugate pair ====
                    // *
                    FOO = Math.Abs(T[NS+NS * LDT + o_t]) + Math.Sqrt(Math.Abs(T[NS+(NS - 1) * LDT + o_t])) * Math.Sqrt(Math.Abs(T[NS - 1+NS * LDT + o_t]));
                    if (FOO == ZERO) FOO = Math.Abs(S);
                    if (Math.Max(Math.Abs(S * V[1+NS * LDV + o_v]), Math.Abs(S * V[1+(NS - 1) * LDV + o_v])) <= Math.Max(SMLNUM, ULP * FOO))
                    {
                        // *
                        // *              ==== Deflatable ====
                        // *
                        NS -= 2;
                    }
                    else
                    {
                        // *
                        // *              ==== Undflatable. Move them up out of the way.
                        // *              .    Fortunately, DTREXC does the right thing with
                        // *              .    ILST in case of a rare exchange failure. ====
                        // *
                        IFST = NS;
                        this._dtrexc.Run("V", JW, ref T, offset_t, LDT, ref V, offset_v, LDV
                                         , ref IFST, ref ILST, ref WORK, offset_work, ref INFO);
                        ILST += 2;
                    }
                }
                // *
                // *        ==== End deflation detection loop ====
                // *
                goto LABEL20;
            }
            // *
            // *        ==== Return to Hessenberg form ====
            // *
            if (NS == 0) S = ZERO;
            // *
            if (NS < JW)
            {
                // *
                // *        ==== sorting diagonal blocks of T improves accuracy for
                // *        .    graded matrices.  Bubble sort deals well with
                // *        .    exchange failures. ====
                // *
                SORTED = false;
                I = NS + 1;
            LABEL30:;
                if (SORTED) goto LABEL50;
                SORTED = true;
                // *
                KEND = I - 1;
                I = INFQR + 1;
                if (I == NS)
                {
                    K = I + 1;
                }
                else
                {
                    if (T[I + 1+I * LDT + o_t] == ZERO)
                    {
                        K = I + 1;
                    }
                    else
                    {
                        K = I + 2;
                    }
                }
            LABEL40:;
                if (K <= KEND)
                {
                    if (K == I + 1)
                    {
                        EVI = Math.Abs(T[I+I * LDT + o_t]);
                    }
                    else
                    {
                        EVI = Math.Abs(T[I+I * LDT + o_t]) + Math.Sqrt(Math.Abs(T[I + 1+I * LDT + o_t])) * Math.Sqrt(Math.Abs(T[I+(I + 1) * LDT + o_t]));
                    }
                    // *
                    if (K == KEND)
                    {
                        EVK = Math.Abs(T[K+K * LDT + o_t]);
                    }
                    else
                    {
                        if (T[K + 1+K * LDT + o_t] == ZERO)
                        {
                            EVK = Math.Abs(T[K+K * LDT + o_t]);
                        }
                        else
                        {
                            EVK = Math.Abs(T[K+K * LDT + o_t]) + Math.Sqrt(Math.Abs(T[K + 1+K * LDT + o_t])) * Math.Sqrt(Math.Abs(T[K+(K + 1) * LDT + o_t]));
                        }
                    }
                    // *
                    if (EVI >= EVK)
                    {
                        I = K;
                    }
                    else
                    {
                        SORTED = false;
                        IFST = I;
                        ILST = K;
                        this._dtrexc.Run("V", JW, ref T, offset_t, LDT, ref V, offset_v, LDV
                                         , ref IFST, ref ILST, ref WORK, offset_work, ref INFO);
                        if (INFO == 0)
                        {
                            I = ILST;
                        }
                        else
                        {
                            I = K;
                        }
                    }
                    if (I == KEND)
                    {
                        K = I + 1;
                    }
                    else
                    {
                        if (T[I + 1+I * LDT + o_t] == ZERO)
                        {
                            K = I + 1;
                        }
                        else
                        {
                            K = I + 2;
                        }
                    }
                    goto LABEL40;
                }
                goto LABEL30;
            LABEL50:;
            }
            // *
            // *     ==== Restore shift/eigenvalue array from T ====
            // *
            I = JW;
        LABEL60:;
            if (I >= INFQR + 1)
            {
                if (I == INFQR + 1)
                {
                    SR[KWTOP + I - 1 + o_sr] = T[I+I * LDT + o_t];
                    SI[KWTOP + I - 1 + o_si] = ZERO;
                    I -= 1;
                }
                else
                {
                    if (T[I+(I - 1) * LDT + o_t] == ZERO)
                    {
                        SR[KWTOP + I - 1 + o_sr] = T[I+I * LDT + o_t];
                        SI[KWTOP + I - 1 + o_si] = ZERO;
                        I -= 1;
                    }
                    else
                    {
                        AA = T[I - 1+(I - 1) * LDT + o_t];
                        CC = T[I+(I - 1) * LDT + o_t];
                        BB = T[I - 1+I * LDT + o_t];
                        DD = T[I+I * LDT + o_t];
                        this._dlanv2.Run(ref AA, ref BB, ref CC, ref DD, ref SR[KWTOP + I - 2 + o_sr], ref SI[KWTOP + I - 2 + o_si]
                                         , ref SR[KWTOP + I - 1 + o_sr], ref SI[KWTOP + I - 1 + o_si], ref CS, ref SN);
                        I -= 2;
                    }
                }
                goto LABEL60;
            }
            // *
            if (NS < JW || S == ZERO)
            {
                if (NS > 1 && S != ZERO)
                {
                    // *
                    // *           ==== Reflect spike back into lower triangle ====
                    // *
                    this._dcopy.Run(NS, V, offset_v, LDV, ref WORK, offset_work, 1);
                    BETA = WORK[1 + o_work];
                    this._dlarfg.Run(NS, ref BETA, ref WORK, 2 + o_work, 1, ref TAU);
                    WORK[1 + o_work] = ONE;
                    // *
                    this._dlaset.Run("L", JW - 2, JW - 2, ZERO, ZERO, ref T, 3+1 * LDT + o_t
                                     , LDT);
                    // *
                    this._dlarf.Run("L", NS, JW, WORK, offset_work, 1, TAU
                                    , ref T, offset_t, LDT, ref WORK, JW + 1 + o_work);
                    this._dlarf.Run("R", NS, NS, WORK, offset_work, 1, TAU
                                    , ref T, offset_t, LDT, ref WORK, JW + 1 + o_work);
                    this._dlarf.Run("R", JW, NS, WORK, offset_work, 1, TAU
                                    , ref V, offset_v, LDV, ref WORK, JW + 1 + o_work);
                    // *
                    this._dgehrd.Run(JW, 1, NS, ref T, offset_t, LDT, ref WORK, offset_work
                                     , ref WORK, JW + 1 + o_work, LWORK - JW, ref INFO);
                }
                // *
                // *        ==== Copy updated reduced window into place ====
                // *
                if (KWTOP > 1) H[KWTOP+(KWTOP - 1) * LDH + o_h] = S * V[1+1 * LDV + o_v];
                this._dlacpy.Run("U", JW, JW, T, offset_t, LDT, ref H, KWTOP+KWTOP * LDH + o_h
                                 , LDH);
                this._dcopy.Run(JW - 1, T, 2+1 * LDT + o_t, LDT + 1, ref H, KWTOP + 1+KWTOP * LDH + o_h, LDH + 1);
                // *
                // *        ==== Accumulate orthogonal matrix in order update
                // *        .    H and Z, if requested.  (A modified version
                // *        .    of  DORGHR that accumulates block Householder
                // *        .    transformations into V directly might be
                // *        .    marginally more efficient than the following.) ====
                // *
                if (NS > 1 && S != ZERO)
                {
                    this._dorghr.Run(JW, 1, NS, ref T, offset_t, LDT, WORK, offset_work
                                     , ref WORK, JW + 1 + o_work, LWORK - JW, ref INFO);
                    this._dgemm.Run("N", "N", JW, NS, NS, ONE
                                    , V, offset_v, LDV, T, offset_t, LDT, ZERO, ref WV, offset_wv
                                    , LDWV);
                    this._dlacpy.Run("A", JW, NS, WV, offset_wv, LDWV, ref V, offset_v
                                     , LDV);
                }
                // *
                // *        ==== Update vertical slab in H ====
                // *
                if (WANTT)
                {
                    LTOP = 1;
                }
                else
                {
                    LTOP = KTOP;
                }
                for (KROW = LTOP; (NV >= 0) ? (KROW <= KWTOP - 1) : (KROW >= KWTOP - 1); KROW += NV)
                {
                    KLN = Math.Min(NV, KWTOP - KROW);
                    this._dgemm.Run("N", "N", KLN, JW, JW, ONE
                                    , H, KROW+KWTOP * LDH + o_h, LDH, V, offset_v, LDV, ZERO, ref WV, offset_wv
                                    , LDWV);
                    this._dlacpy.Run("A", KLN, JW, WV, offset_wv, LDWV, ref H, KROW+KWTOP * LDH + o_h
                                     , LDH);
                }
                // *
                // *        ==== Update horizontal slab in H ====
                // *
                if (WANTT)
                {
                    for (KCOL = KBOT + 1; (NH >= 0) ? (KCOL <= N) : (KCOL >= N); KCOL += NH)
                    {
                        KLN = Math.Min(NH, N - KCOL + 1);
                        this._dgemm.Run("C", "N", JW, KLN, JW, ONE
                                        , V, offset_v, LDV, H, KWTOP+KCOL * LDH + o_h, LDH, ZERO, ref T, offset_t
                                        , LDT);
                        this._dlacpy.Run("A", JW, KLN, T, offset_t, LDT, ref H, KWTOP+KCOL * LDH + o_h
                                         , LDH);
                    }
                }
                // *
                // *        ==== Update vertical slab in Z ====
                // *
                if (WANTZ)
                {
                    for (KROW = ILOZ; (NV >= 0) ? (KROW <= IHIZ) : (KROW >= IHIZ); KROW += NV)
                    {
                        KLN = Math.Min(NV, IHIZ - KROW + 1);
                        this._dgemm.Run("N", "N", KLN, JW, JW, ONE
                                        , Z, KROW+KWTOP * LDZ + o_z, LDZ, V, offset_v, LDV, ZERO, ref WV, offset_wv
                                        , LDWV);
                        this._dlacpy.Run("A", KLN, JW, WV, offset_wv, LDWV, ref Z, KROW+KWTOP * LDZ + o_z
                                         , LDZ);
                    }
                }
            }
            // *
            // *     ==== Return the number of deflations ... ====
            // *
            ND = JW - NS;
            // *
            // *     ==== ... and the number of shifts. (Subtracting
            // *     .    INFQR from the spike length takes care
            // *     .    of the case of a rare QR failure while
            // *     .    calculating eigenvalues of the deflation
            // *     .    window.)  ====
            // *
            NS -= INFQR;
            // *
            // *      ==== Return optimal workspace. ====
            // *
            WORK[1 + o_work] = Convert.ToDouble(LWKOPT);
            // *
            // *     ==== End of DLAQR3 ====
            // *

            #endregion

        }
    }
}
