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
    /// DLAQR4 computes the eigenvalues of a Hessenberg matrix H
    /// and, optionally, the matrices T and Z from the Schur decomposition
    /// H = Z T Z**T, where T is an upper quasi-triangular matrix (the
    /// Schur form), and Z is the orthogonal matrix of Schur vectors.
    /// 
    /// Optionally Z may be postmultiplied into an input orthogonal
    /// matrix Q so that this routine can give the Schur factorization
    /// of a matrix A which has been reduced to the Hessenberg form H
    /// by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
    /// 
    ///</summary>
    public class DLAQR4
    {
    

        #region Dependencies
        
        ILAENV _ilaenv; DLACPY _dlacpy; DLAHQR _dlahqr; DLANV2 _dlanv2; DLAQR2 _dlaqr2; DLAQR5 _dlaqr5; 

        #endregion


        #region Variables
        
        const int NTINY = 11; const int KEXNW = 5; const int KEXSH = 6; const double WILK1 = 0.75E0; 
        const double WILK2 =  - 0.4375E0;const double ZERO = 0.0E0; const double ONE = 1.0E0; double[] ZDUM = new double[1 * 1]; 

        #endregion

        public DLAQR4(ILAENV ilaenv, DLACPY dlacpy, DLAHQR dlahqr, DLANV2 dlanv2, DLAQR2 dlaqr2, DLAQR5 dlaqr5)
        {
    

            #region Set Dependencies
            
            this._ilaenv = ilaenv; this._dlacpy = dlacpy; this._dlahqr = dlahqr; this._dlanv2 = dlanv2; this._dlaqr2 = dlaqr2; 
            this._dlaqr5 = dlaqr5;

            #endregion

        }
    
        public DLAQR4()
        {
    

            #region Dependencies (Initialization)
            
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DCOPY dcopy = new DCOPY();
            DLABAD dlabad = new DLABAD();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DROT drot = new DROT();
            DAXPY daxpy = new DAXPY();
            XERBLA xerbla = new XERBLA();
            DLASSQ dlassq = new DLASSQ();
            IDAMAX idamax = new IDAMAX();
            DSWAP dswap = new DSWAP();
            DLAQR1 dlaqr1 = new DLAQR1();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANV2 dlanv2 = new DLANV2(dlamch, dlapy2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DLAHQR dlahqr = new DLAHQR(dlamch, dcopy, dlabad, dlanv2, dlarfg, drot);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DGEHD2 dgehd2 = new DGEHD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLAHR2 dlahr2 = new DLAHR2(daxpy, dcopy, dgemm, dgemv, dlacpy, dlarfg, dscal, dtrmm, dtrmv);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEHRD dgehrd = new DGEHRD(daxpy, dgehd2, dgemm, dlahr2, dlarfb, dtrmm, xerbla, ilaenv);
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

            #endregion


            #region Set Dependencies
            
            this._ilaenv = ilaenv; this._dlacpy = dlacpy; this._dlahqr = dlahqr; this._dlanv2 = dlanv2; this._dlaqr2 = dlaqr2; 
            this._dlaqr5 = dlaqr5;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAQR4 computes the eigenvalues of a Hessenberg matrix H
        /// and, optionally, the matrices T and Z from the Schur decomposition
        /// H = Z T Z**T, where T is an upper quasi-triangular matrix (the
        /// Schur form), and Z is the orthogonal matrix of Schur vectors.
        /// 
        /// Optionally Z may be postmultiplied into an input orthogonal
        /// matrix Q so that this routine can give the Schur factorization
        /// of a matrix A which has been reduced to the Hessenberg form H
        /// by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
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
        /// It is assumed that H is already upper triangular in rows
        /// and columns 1:ILO-1 and IHI+1:N and, if ILO.GT.1,
        /// H(ILO,ILO-1) is zero. ILO and IHI are normally set by a
        /// previous call to DGEBAL, and then passed to DGEHRD when the
        /// matrix output by DGEBAL is reduced to Hessenberg form.
        /// Otherwise, ILO and IHI should be set to 1 and N,
        /// respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
        /// If N = 0, then ILO = 1 and IHI = 0.
        ///</param>
        /// <param name="H">
        /// (input/output) DOUBLE PRECISION array, dimension (LDH,N)
        /// On entry, the upper Hessenberg matrix H.
        /// On exit, if INFO = 0 and WANTT is .TRUE., then H contains
        /// the upper quasi-triangular matrix T from the Schur
        /// decomposition (the Schur form); 2-by-2 diagonal blocks
        /// (corresponding to complex conjugate pairs of eigenvalues)
        /// are returned in standard form, with H(i,i) = H(i+1,i+1)
        /// and H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and WANTT is
        /// .FALSE., then the contents of H are unspecified on exit.
        /// (The output value of H when INFO.GT.0 is given under the
        /// description of INFO below.)
        /// 
        /// This subroutine may explicitly set H(i,j) = 0 for i.GT.j and
        /// j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.
        ///</param>
        /// <param name="LDH">
        /// (input) INTEGER
        /// The leading dimension of the array H. LDH .GE. max(1,N).
        ///</param>
        /// <param name="WR">
        /// (output) DOUBLE PRECISION array, dimension (IHI)
        ///</param>
        /// <param name="WI">
        /// (output) DOUBLE PRECISION array, dimension (IHI)
        /// The real and imaginary parts, respectively, of the computed
        /// eigenvalues of H(ILO:IHI,ILO:IHI) are stored WR(ILO:IHI)
        /// and WI(ILO:IHI). If two eigenvalues are computed as a
        /// complex conjugate pair, they are stored in consecutive
        /// elements of WR and WI, say the i-th and (i+1)th, with
        /// WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., then
        /// the eigenvalues are stored in the same order as on the
        /// diagonal of the Schur form returned in H, with
        /// WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2 diagonal
        /// block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
        /// WI(i+1) = -WI(i).
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
        /// (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
        /// If WANTZ is .FALSE., then Z is not referenced.
        /// If WANTZ is .TRUE., then Z(ILO:IHI,ILOZ:IHIZ) is
        /// replaced by Z(ILO:IHI,ILOZ:IHIZ)*U where U is the
        /// orthogonal Schur factor of H(ILO:IHI,ILO:IHI).
        /// (The output value of Z when INFO.GT.0 is given under
        /// the description of INFO below.)
        ///</param>
        /// <param name="LDZ">
        /// (input) INTEGER
        /// The leading dimension of the array Z.  if WANTZ is .TRUE.
        /// then LDZ.GE.MAX(1,IHIZ).  Otherwize, LDZ.GE.1.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension LWORK
        /// On exit, if LWORK = -1, WORK(1) returns an estimate of
        /// the optimal value for LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. max(1,N)
        /// is sufficient, but LWORK typically as large as 6*N may
        /// be required for optimal performance.  A workspace query
        /// to determine the optimal workspace size is recommended.
        /// 
        /// If LWORK = -1, then DLAQR4 does a workspace query.
        /// In this case, DLAQR4 checks the input parameters and
        /// estimates the optimal workspace size for the given
        /// values of N, ILO and IHI.  The estimate is returned
        /// in WORK(1).  No error message related to LWORK is
        /// issued by XERBLA.  Neither H nor Z are accessed.
        /// 
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// =  0:  successful exit
        /// .GT. 0:  if INFO = i, DLAQR4 failed to compute all of
        /// the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
        /// and WI contain those eigenvalues which have been
        /// successfully computed.  (Failures are rare.)
        /// 
        /// If INFO .GT. 0 and WANT is .FALSE., then on exit,
        /// the remaining unconverged eigenvalues are the eigen-
        /// values of the upper Hessenberg matrix rows and
        /// columns ILO through INFO of the final, output
        /// value of H.
        /// 
        /// If INFO .GT. 0 and WANTT is .TRUE., then on exit
        /// 
        /// (*)  (initial value of H)*U  = U*(final value of H)
        /// 
        /// where U is an orthogonal matrix.  The final
        /// value of H is upper Hessenberg and quasi-triangular
        /// in rows and columns INFO+1 through IHI.
        /// 
        /// If INFO .GT. 0 and WANTZ is .TRUE., then on exit
        /// 
        /// (final value of Z(ILO:IHI,ILOZ:IHIZ)
        /// =  (initial value of Z(ILO:IHI,ILOZ:IHIZ)*U
        /// 
        /// where U is the orthogonal matrix in (*) (regard-
        /// less of the value of WANTT.)
        /// 
        /// If INFO .GT. 0 and WANTZ is .FALSE., then Z is not
        /// accessed.
        ///</param>
        public void Run(bool WANTT, bool WANTZ, int N, int ILO, int IHI, ref double[] H, int offset_h
                         , int LDH, ref double[] WR, int offset_wr, ref double[] WI, int offset_wi, int ILOZ, int IHIZ, ref double[] Z, int offset_z
                         , int LDZ, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            double AA = 0; double BB = 0; double CC = 0; double CS = 0; double DD = 0; double SN = 0; double SS = 0; 
            double SWAP = 0;int I = 0; int INF = 0; int IT = 0; int ITMAX = 0; int K = 0; int KACC22 = 0; int KBOT = 0; 
            int KDU = 0;int KS = 0; int KT = 0; int KTOP = 0; int KU = 0; int KV = 0; int KWH = 0; int KWTOP = 0; int KWV = 0; 
            int LD = 0;int LS = 0; int LWKOPT = 0; int NDFL = 0; int NH = 0; int NHO = 0; int NIBBLE = 0; int NMIN = 0; 
            int NS = 0;int NSMAX = 0; int NSR = 0; int NVE = 0; int NW = 0; int NWMAX = 0; int NWR = 0; bool NWINC = false; 
            bool SORTED = false;string JBCMPZ = new string(' ', 2); int offset_zdum = 0;

            #endregion


            #region Array Index Correction
            
             int o_h = -1 - LDH + offset_h;  int o_wr = -1 + offset_wr;  int o_wi = -1 + offset_wi; 
             int o_z = -1 - LDZ + offset_z; int o_work = -1 + offset_work; 

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
            // *     This subroutine implements one level of recursion for DLAQR0.
            // *     It is a complete implementation of the small bulge multi-shift
            // *     QR algorithm.  It may be called by DLAQR0 and, for large enough
            // *     deflation window size, it may be called by DLAQR3.  This
            // *     subroutine is identical to DLAQR0 except that it calls DLAQR2
            // *     instead of DLAQR3.
            // *
            // *     Purpose
            // *     =======
            // *
            // *     DLAQR4 computes the eigenvalues of a Hessenberg matrix H
            // *     and, optionally, the matrices T and Z from the Schur decomposition
            // *     H = Z T Z**T, where T is an upper quasi-triangular matrix (the
            // *     Schur form), and Z is the orthogonal matrix of Schur vectors.
            // *
            // *     Optionally Z may be postmultiplied into an input orthogonal
            // *     matrix Q so that this routine can give the Schur factorization
            // *     of a matrix A which has been reduced to the Hessenberg form H
            // *     by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
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
            // *     N     (input) INTEGER
            // *           The order of the matrix H.  N .GE. 0.
            // *
            // *     ILO   (input) INTEGER
            // *     IHI   (input) INTEGER
            // *           It is assumed that H is already upper triangular in rows
            // *           and columns 1:ILO-1 and IHI+1:N and, if ILO.GT.1,
            // *           H(ILO,ILO-1) is zero. ILO and IHI are normally set by a
            // *           previous call to DGEBAL, and then passed to DGEHRD when the
            // *           matrix output by DGEBAL is reduced to Hessenberg form.
            // *           Otherwise, ILO and IHI should be set to 1 and N,
            // *           respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
            // *           If N = 0, then ILO = 1 and IHI = 0.
            // *
            // *     H     (input/output) DOUBLE PRECISION array, dimension (LDH,N)
            // *           On entry, the upper Hessenberg matrix H.
            // *           On exit, if INFO = 0 and WANTT is .TRUE., then H contains
            // *           the upper quasi-triangular matrix T from the Schur
            // *           decomposition (the Schur form); 2-by-2 diagonal blocks
            // *           (corresponding to complex conjugate pairs of eigenvalues)
            // *           are returned in standard form, with H(i,i) = H(i+1,i+1)
            // *           and H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and WANTT is
            // *           .FALSE., then the contents of H are unspecified on exit.
            // *           (The output value of H when INFO.GT.0 is given under the
            // *           description of INFO below.)
            // *
            // *           This subroutine may explicitly set H(i,j) = 0 for i.GT.j and
            // *           j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.
            // *
            // *     LDH   (input) INTEGER
            // *           The leading dimension of the array H. LDH .GE. max(1,N).
            // *
            // *     WR    (output) DOUBLE PRECISION array, dimension (IHI)
            // *     WI    (output) DOUBLE PRECISION array, dimension (IHI)
            // *           The real and imaginary parts, respectively, of the computed
            // *           eigenvalues of H(ILO:IHI,ILO:IHI) are stored WR(ILO:IHI)
            // *           and WI(ILO:IHI). If two eigenvalues are computed as a
            // *           complex conjugate pair, they are stored in consecutive
            // *           elements of WR and WI, say the i-th and (i+1)th, with
            // *           WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., then
            // *           the eigenvalues are stored in the same order as on the
            // *           diagonal of the Schur form returned in H, with
            // *           WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2 diagonal
            // *           block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
            // *           WI(i+1) = -WI(i).
            // *
            // *     ILOZ     (input) INTEGER
            // *     IHIZ     (input) INTEGER
            // *           Specify the rows of Z to which transformations must be
            // *           applied if WANTZ is .TRUE..
            // *           1 .LE. ILOZ .LE. ILO; IHI .LE. IHIZ .LE. N.
            // *
            // *     Z     (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
            // *           If WANTZ is .FALSE., then Z is not referenced.
            // *           If WANTZ is .TRUE., then Z(ILO:IHI,ILOZ:IHIZ) is
            // *           replaced by Z(ILO:IHI,ILOZ:IHIZ)*U where U is the
            // *           orthogonal Schur factor of H(ILO:IHI,ILO:IHI).
            // *           (The output value of Z when INFO.GT.0 is given under
            // *           the description of INFO below.)
            // *
            // *     LDZ   (input) INTEGER
            // *           The leading dimension of the array Z.  if WANTZ is .TRUE.
            // *           then LDZ.GE.MAX(1,IHIZ).  Otherwize, LDZ.GE.1.
            // *
            // *     WORK  (workspace/output) DOUBLE PRECISION array, dimension LWORK
            // *           On exit, if LWORK = -1, WORK(1) returns an estimate of
            // *           the optimal value for LWORK.
            // *
            // *     LWORK (input) INTEGER
            // *           The dimension of the array WORK.  LWORK .GE. max(1,N)
            // *           is sufficient, but LWORK typically as large as 6*N may
            // *           be required for optimal performance.  A workspace query
            // *           to determine the optimal workspace size is recommended.
            // *
            // *           If LWORK = -1, then DLAQR4 does a workspace query.
            // *           In this case, DLAQR4 checks the input parameters and
            // *           estimates the optimal workspace size for the given
            // *           values of N, ILO and IHI.  The estimate is returned
            // *           in WORK(1).  No error message related to LWORK is
            // *           issued by XERBLA.  Neither H nor Z are accessed.
            // *
            // *
            // *     INFO  (output) INTEGER
            // *             =  0:  successful exit
            // *           .GT. 0:  if INFO = i, DLAQR4 failed to compute all of
            // *                the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
            // *                and WI contain those eigenvalues which have been
            // *                successfully computed.  (Failures are rare.)
            // *
            // *                If INFO .GT. 0 and WANT is .FALSE., then on exit,
            // *                the remaining unconverged eigenvalues are the eigen-
            // *                values of the upper Hessenberg matrix rows and
            // *                columns ILO through INFO of the final, output
            // *                value of H.
            // *
            // *                If INFO .GT. 0 and WANTT is .TRUE., then on exit
            // *
            // *           (*)  (initial value of H)*U  = U*(final value of H)
            // *
            // *                where U is an orthogonal matrix.  The final
            // *                value of H is upper Hessenberg and quasi-triangular
            // *                in rows and columns INFO+1 through IHI.
            // *
            // *                If INFO .GT. 0 and WANTZ is .TRUE., then on exit
            // *
            // *                  (final value of Z(ILO:IHI,ILOZ:IHIZ)
            // *                   =  (initial value of Z(ILO:IHI,ILOZ:IHIZ)*U
            // *
            // *                where U is the orthogonal matrix in (*) (regard-
            // *                less of the value of WANTT.)
            // *
            // *                If INFO .GT. 0 and WANTZ is .FALSE., then Z is not
            // *                accessed.
            // *
            // *     ================================================================
            // *     Based on contributions by
            // *        Karen Braman and Ralph Byers, Department of Mathematics,
            // *        University of Kansas, USA
            // *
            // *     ================================================================
            // *     References:
            // *       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
            // *       Algorithm Part I: Maintaining Well Focused Shifts, and Level 3
            // *       Performance, SIAM Journal of Matrix Analysis, volume 23, pages
            // *       929--947, 2002.
            // *
            // *       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
            // *       Algorithm Part II: Aggressive Early Deflation, SIAM Journal
            // *       of Matrix Analysis, volume 23, pages 948--973, 2002.
            // *
            // *     ================================================================
            // *     .. Parameters ..
            // *
            // *     ==== Matrices of order NTINY or smaller must be processed by
            // *     .    DLAHQR because of insufficient subdiagonal scratch space.
            // *     .    (This is a hard limit.) ====
            // *
            // *     ==== Exceptional deflation windows:  try to cure rare
            // *     .    slow convergence by increasing the size of the
            // *     .    deflation window after KEXNW iterations. =====
            // *
            // *     ==== Exceptional shifts: try to cure rare slow convergence
            // *     .    with ad-hoc exceptional shifts every KEXSH iterations.
            // *     .    The constants WILK1 and WILK2 are used to form the
            // *     .    exceptional shifts. ====
            // *
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, DBLE, INT, MAX, MIN, MOD;
            // *     ..
            // *     .. Executable Statements ..

            #endregion


            #region Body
            
            INFO = 0;
            // *
            // *     ==== Quick return for N = 0: nothing to do. ====
            // *
            if (N == 0)
            {
                WORK[1 + o_work] = ONE;
                return;
            }
            // *
            // *     ==== Set up job flags for ILAENV. ====
            // *
            if (WANTT)
            {
                FortranLib.Copy(ref JBCMPZ, 1, 1, "S");
            }
            else
            {
                FortranLib.Copy(ref JBCMPZ, 1, 1, "E");
            }
            if (WANTZ)
            {
                FortranLib.Copy(ref JBCMPZ, 2, 2, "V");
            }
            else
            {
                FortranLib.Copy(ref JBCMPZ, 2, 2, "N");
            }
            // *
            // *     ==== Tiny matrices must use DLAHQR. ====
            // *
            if (N <= NTINY)
            {
                // *
                // *        ==== Estimate optimal workspace. ====
                // *
                LWKOPT = 1;
                if (LWORK !=  - 1)
                {
                    this._dlahqr.Run(WANTT, WANTZ, N, ILO, IHI, ref H, offset_h
                                     , LDH, ref WR, offset_wr, ref WI, offset_wi, ILOZ, IHIZ, ref Z, offset_z
                                     , LDZ, ref INFO);
                }
            }
            else
            {
                // *
                // *        ==== Use small bulge multi-shift QR with aggressive early
                // *        .    deflation on larger-than-tiny matrices. ====
                // *
                // *        ==== Hope for the best. ====
                // *
                INFO = 0;
                // *
                // *        ==== NWR = recommended deflation window size.  At this
                // *        .    point,  N .GT. NTINY = 11, so there is enough
                // *        .    subdiagonal workspace for NWR.GE.2 as required.
                // *        .    (In fact, there is enough subdiagonal space for
                // *        .    NWR.GE.3.) ====
                // *
                NWR = this._ilaenv.Run(13, "DLAQR4", JBCMPZ, N, ILO, IHI, LWORK);
                NWR = Math.Max(2, NWR);
                NWR = Math.Min(IHI - ILO + 1, Math.Min((N - 1) / 3, NWR));
                NW = NWR;
                // *
                // *        ==== NSR = recommended number of simultaneous shifts.
                // *        .    At this point N .GT. NTINY = 11, so there is at
                // *        .    enough subdiagonal workspace for NSR to be even
                // *        .    and greater than or equal to two as required. ====
                // *
                NSR = this._ilaenv.Run(15, "DLAQR4", JBCMPZ, N, ILO, IHI, LWORK);
                NSR = Math.Min(NSR, Math.Min((N + 6) / 9, IHI - ILO));
                NSR = Math.Max(2, NSR - FortranLib.Mod(NSR,2));
                // *
                // *        ==== Estimate optimal workspace ====
                // *
                // *        ==== Workspace query call to DLAQR2 ====
                // *
                this._dlaqr2.Run(WANTT, WANTZ, N, ILO, IHI, NWR + 1
                                 , ref H, offset_h, LDH, ILOZ, IHIZ, ref Z, offset_z, LDZ
                                 , ref LS, ref LD, ref WR, offset_wr, ref WI, offset_wi, ref H, offset_h, LDH
                                 , N, ref H, offset_h, LDH, N, ref H, offset_h, LDH
                                 , ref WORK, offset_work,  - 1);
                // *
                // *        ==== Optimal workspace = MAX(DLAQR5, DLAQR2) ====
                // *
                LWKOPT = Math.Max(3 * NSR / 2, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
                // *
                // *        ==== Quick return in case of workspace query. ====
                // *
                if (LWORK ==  - 1)
                {
                    WORK[1 + o_work] = Convert.ToDouble(LWKOPT);
                    return;
                }
                // *
                // *        ==== DLAHQR/DLAQR0 crossover point ====
                // *
                NMIN = this._ilaenv.Run(12, "DLAQR4", JBCMPZ, N, ILO, IHI, LWORK);
                NMIN = Math.Max(NTINY, NMIN);
                // *
                // *        ==== Nibble crossover point ====
                // *
                NIBBLE = this._ilaenv.Run(14, "DLAQR4", JBCMPZ, N, ILO, IHI, LWORK);
                NIBBLE = Math.Max(0, NIBBLE);
                // *
                // *        ==== Accumulate reflections during ttswp?  Use block
                // *        .    2-by-2 structure during matrix-matrix multiply? ====
                // *
                KACC22 = this._ilaenv.Run(16, "DLAQR4", JBCMPZ, N, ILO, IHI, LWORK);
                KACC22 = Math.Max(0, KACC22);
                KACC22 = Math.Min(2, KACC22);
                // *
                // *        ==== NWMAX = the largest possible deflation window for
                // *        .    which there is sufficient workspace. ====
                // *
                NWMAX = Math.Min((N - 1) / 3, LWORK / 2);
                // *
                // *        ==== NSMAX = the Largest number of simultaneous shifts
                // *        .    for which there is sufficient workspace. ====
                // *
                NSMAX = Math.Min((N + 6) / 9, 2 * LWORK / 3);
                NSMAX -= FortranLib.Mod(NSMAX,2);
                // *
                // *        ==== NDFL: an iteration count restarted at deflation. ====
                // *
                NDFL = 1;
                // *
                // *        ==== ITMAX = iteration limit ====
                // *
                ITMAX = Math.Max(30, 2 * KEXSH) * Math.Max(10, (IHI - ILO + 1));
                // *
                // *        ==== Last row and column in the active block ====
                // *
                KBOT = IHI;
                // *
                // *        ==== Main Loop ====
                // *
                for (IT = 1; IT <= ITMAX; IT++)
                {
                    // *
                    // *           ==== Done when KBOT falls below ILO ====
                    // *
                    if (KBOT < ILO) goto LABEL90;
                    // *
                    // *           ==== Locate active block ====
                    // *
                    for (K = KBOT; K >= ILO + 1; K +=  - 1)
                    {
                        if (H[K+(K - 1) * LDH + o_h] == ZERO) goto LABEL20;
                    }
                    K = ILO;
                LABEL20:;
                    KTOP = K;
                    // *
                    // *           ==== Select deflation window size ====
                    // *
                    NH = KBOT - KTOP + 1;
                    if (NDFL < KEXNW || NH < NW)
                    {
                        // *
                        // *              ==== Typical deflation window.  If possible and
                        // *              .    advisable, nibble the entire active block.
                        // *              .    If not, use size NWR or NWR+1 depending upon
                        // *              .    which has the smaller corresponding subdiagonal
                        // *              .    entry (a heuristic). ====
                        // *
                        NWINC = true;
                        if (NH <= Math.Min(NMIN, NWMAX))
                        {
                            NW = NH;
                        }
                        else
                        {
                            NW = Math.Min(NWR, Math.Min(NH, NWMAX));
                            if (NW < NWMAX)
                            {
                                if (NW >= NH - 1)
                                {
                                    NW = NH;
                                }
                                else
                                {
                                    KWTOP = KBOT - NW + 1;
                                    if (Math.Abs(H[KWTOP+(KWTOP - 1) * LDH + o_h]) > Math.Abs(H[KWTOP - 1+(KWTOP - 2) * LDH + o_h])) NW += 1;
                                }
                            }
                        }
                    }
                    else
                    {
                        // *
                        // *              ==== Exceptional deflation window.  If there have
                        // *              .    been no deflations in KEXNW or more iterations,
                        // *              .    then vary the deflation window size.   At first,
                        // *              .    because, larger windows are, in general, more
                        // *              .    powerful than smaller ones, rapidly increase the
                        // *              .    window up to the maximum reasonable and possible.
                        // *              .    Then maybe try a slightly smaller window.  ====
                        // *
                        if (NWINC && NW < Math.Min(NWMAX, NH))
                        {
                            NW = Math.Min(NWMAX, Math.Min(NH, 2 * NW));
                        }
                        else
                        {
                            NWINC = false;
                            if (NW == NH && NH > 2) NW = NH - 1;
                        }
                    }
                    // *
                    // *           ==== Aggressive early deflation:
                    // *           .    split workspace under the subdiagonal into
                    // *           .      - an nw-by-nw work array V in the lower
                    // *           .        left-hand-corner,
                    // *           .      - an NW-by-at-least-NW-but-more-is-better
                    // *           .        (NW-by-NHO) horizontal work array along
                    // *           .        the bottom edge,
                    // *           .      - an at-least-NW-but-more-is-better (NHV-by-NW)
                    // *           .        vertical work array along the left-hand-edge.
                    // *           .        ====
                    // *
                    KV = N - NW + 1;
                    KT = NW + 1;
                    NHO = (N - NW - 1) - KT + 1;
                    KWV = NW + 2;
                    NVE = (N - NW) - KWV + 1;
                    // *
                    // *           ==== Aggressive early deflation ====
                    // *
                    this._dlaqr2.Run(WANTT, WANTZ, N, KTOP, KBOT, NW
                                     , ref H, offset_h, LDH, ILOZ, IHIZ, ref Z, offset_z, LDZ
                                     , ref LS, ref LD, ref WR, offset_wr, ref WI, offset_wi, ref H, KV+1 * LDH + o_h, LDH
                                     , NHO, ref H, KV+KT * LDH + o_h, LDH, NVE, ref H, KWV+1 * LDH + o_h, LDH
                                     , ref WORK, offset_work, LWORK);
                    // *
                    // *           ==== Adjust KBOT accounting for new deflations. ====
                    // *
                    KBOT -= LD;
                    // *
                    // *           ==== KS points to the shifts. ====
                    // *
                    KS = KBOT - LS + 1;
                    // *
                    // *           ==== Skip an expensive QR sweep if there is a (partly
                    // *           .    heuristic) reason to expect that many eigenvalues
                    // *           .    will deflate without it.  Here, the QR sweep is
                    // *           .    skipped if many eigenvalues have just been deflated
                    // *           .    or if the remaining active block is small.
                    // *
                    if ((LD == 0) || ((100 * LD <= NW * NIBBLE) && (KBOT - KTOP + 1 > Math.Min(NMIN, NWMAX))))
                    {
                        // *
                        // *              ==== NS = nominal number of simultaneous shifts.
                        // *              .    This may be lowered (slightly) if DLAQR2
                        // *              .    did not provide that many shifts. ====
                        // *
                        NS = Math.Min(NSMAX, Math.Min(NSR, Math.Max(2, KBOT - KTOP)));
                        NS -= FortranLib.Mod(NS,2);
                        // *
                        // *              ==== If there have been no deflations
                        // *              .    in a multiple of KEXSH iterations,
                        // *              .    then try exceptional shifts.
                        // *              .    Otherwise use shifts provided by
                        // *              .    DLAQR2 above or from the eigenvalues
                        // *              .    of a trailing principal submatrix. ====
                        // *
                        if (FortranLib.Mod(NDFL,KEXSH) == 0)
                        {
                            KS = KBOT - NS + 1;
                            for (I = KBOT; I >= Math.Max(KS + 1, KTOP + 2); I +=  - 2)
                            {
                                SS = Math.Abs(H[I+(I - 1) * LDH + o_h]) + Math.Abs(H[I - 1+(I - 2) * LDH + o_h]);
                                AA = WILK1 * SS + H[I+I * LDH + o_h];
                                BB = SS;
                                CC = WILK2 * SS;
                                DD = AA;
                                this._dlanv2.Run(ref AA, ref BB, ref CC, ref DD, ref WR[I - 1 + o_wr], ref WI[I - 1 + o_wi]
                                                 , ref WR[I + o_wr], ref WI[I + o_wi], ref CS, ref SN);
                            }
                            if (KS == KTOP)
                            {
                                WR[KS + 1 + o_wr] = H[KS + 1+(KS + 1) * LDH + o_h];
                                WI[KS + 1 + o_wi] = ZERO;
                                WR[KS + o_wr] = WR[KS + 1 + o_wr];
                                WI[KS + o_wi] = WI[KS + 1 + o_wi];
                            }
                        }
                        else
                        {
                            // *
                            // *                 ==== Got NS/2 or fewer shifts? Use DLAHQR
                            // *                 .    on a trailing principal submatrix to
                            // *                 .    get more. (Since NS.LE.NSMAX.LE.(N+6)/9,
                            // *                 .    there is enough space below the subdiagonal
                            // *                 .    to fit an NS-by-NS scratch array.) ====
                            // *
                            if (KBOT - KS + 1 <= NS / 2)
                            {
                                KS = KBOT - NS + 1;
                                KT = N - NS + 1;
                                this._dlacpy.Run("A", NS, NS, H, KS+KS * LDH + o_h, LDH, ref H, KT+1 * LDH + o_h
                                                 , LDH);
                                this._dlahqr.Run(false, false, NS, 1, NS, ref H, KT+1 * LDH + o_h
                                                 , LDH, ref WR, KS + o_wr, ref WI, KS + o_wi, 1, 1, ref ZDUM, offset_zdum
                                                 , 1, ref INF);
                                KS += INF;
                                // *
                                // *                    ==== In case of a rare QR failure use
                                // *                    .    eigenvalues of the trailing 2-by-2
                                // *                    .    principal submatrix.  ====
                                // *
                                if (KS >= KBOT)
                                {
                                    AA = H[KBOT - 1+(KBOT - 1) * LDH + o_h];
                                    CC = H[KBOT+(KBOT - 1) * LDH + o_h];
                                    BB = H[KBOT - 1+KBOT * LDH + o_h];
                                    DD = H[KBOT+KBOT * LDH + o_h];
                                    this._dlanv2.Run(ref AA, ref BB, ref CC, ref DD, ref WR[KBOT - 1 + o_wr], ref WI[KBOT - 1 + o_wi]
                                                     , ref WR[KBOT + o_wr], ref WI[KBOT + o_wi], ref CS, ref SN);
                                    KS = KBOT - 1;
                                }
                            }
                            // *
                            if (KBOT - KS + 1 > NS)
                            {
                                // *
                                // *                    ==== Sort the shifts (Helps a little)
                                // *                    .    Bubble sort keeps complex conjugate
                                // *                    .    pairs together. ====
                                // *
                                SORTED = false;
                                for (K = KBOT; K >= KS + 1; K +=  - 1)
                                {
                                    if (SORTED) goto LABEL60;
                                    SORTED = true;
                                    for (I = KS; I <= K - 1; I++)
                                    {
                                        if (Math.Abs(WR[I + o_wr]) + Math.Abs(WI[I + o_wi]) < Math.Abs(WR[I + 1 + o_wr]) + Math.Abs(WI[I + 1 + o_wi]))
                                        {
                                            SORTED = false;
                                            // *
                                            SWAP = WR[I + o_wr];
                                            WR[I + o_wr] = WR[I + 1 + o_wr];
                                            WR[I + 1 + o_wr] = SWAP;
                                            // *
                                            SWAP = WI[I + o_wi];
                                            WI[I + o_wi] = WI[I + 1 + o_wi];
                                            WI[I + 1 + o_wi] = SWAP;
                                        }
                                    }
                                }
                            LABEL60:;
                            }
                            // *
                            // *                 ==== Shuffle shifts into pairs of real shifts
                            // *                 .    and pairs of complex conjugate shifts
                            // *                 .    assuming complex conjugate shifts are
                            // *                 .    already adjacent to one another. (Yes,
                            // *                 .    they are.)  ====
                            // *
                            for (I = KBOT; I >= KS + 2; I +=  - 2)
                            {
                                if (WI[I + o_wi] !=  - WI[I - 1 + o_wi])
                                {
                                    // *
                                    SWAP = WR[I + o_wr];
                                    WR[I + o_wr] = WR[I - 1 + o_wr];
                                    WR[I - 1 + o_wr] = WR[I - 2 + o_wr];
                                    WR[I - 2 + o_wr] = SWAP;
                                    // *
                                    SWAP = WI[I + o_wi];
                                    WI[I + o_wi] = WI[I - 1 + o_wi];
                                    WI[I - 1 + o_wi] = WI[I - 2 + o_wi];
                                    WI[I - 2 + o_wi] = SWAP;
                                }
                            }
                        }
                        // *
                        // *              ==== If there are only two shifts and both are
                        // *              .    real, then use only one.  ====
                        // *
                        if (KBOT - KS + 1 == 2)
                        {
                            if (WI[KBOT + o_wi] == ZERO)
                            {
                                if (Math.Abs(WR[KBOT + o_wr] - H[KBOT+KBOT * LDH + o_h]) < Math.Abs(WR[KBOT - 1 + o_wr] - H[KBOT+KBOT * LDH + o_h]))
                                {
                                    WR[KBOT - 1 + o_wr] = WR[KBOT + o_wr];
                                }
                                else
                                {
                                    WR[KBOT + o_wr] = WR[KBOT - 1 + o_wr];
                                }
                            }
                        }
                        // *
                        // *              ==== Use up to NS of the the smallest magnatiude
                        // *              .    shifts.  If there aren't NS shifts available,
                        // *              .    then use them all, possibly dropping one to
                        // *              .    make the number of shifts even. ====
                        // *
                        NS = Math.Min(NS, KBOT - KS + 1);
                        NS -= FortranLib.Mod(NS,2);
                        KS = KBOT - NS + 1;
                        // *
                        // *              ==== Small-bulge multi-shift QR sweep:
                        // *              .    split workspace under the subdiagonal into
                        // *              .    - a KDU-by-KDU work array U in the lower
                        // *              .      left-hand-corner,
                        // *              .    - a KDU-by-at-least-KDU-but-more-is-better
                        // *              .      (KDU-by-NHo) horizontal work array WH along
                        // *              .      the bottom edge,
                        // *              .    - and an at-least-KDU-but-more-is-better-by-KDU
                        // *              .      (NVE-by-KDU) vertical work WV arrow along
                        // *              .      the left-hand-edge. ====
                        // *
                        KDU = 3 * NS - 3;
                        KU = N - KDU + 1;
                        KWH = KDU + 1;
                        NHO = (N - KDU + 1 - 4) - (KDU + 1) + 1;
                        KWV = KDU + 4;
                        NVE = N - KDU - KWV + 1;
                        // *
                        // *              ==== Small-bulge multi-shift QR sweep ====
                        // *
                        this._dlaqr5.Run(WANTT, WANTZ, KACC22, N, KTOP, KBOT
                                         , NS, ref WR, KS + o_wr, ref WI, KS + o_wi, ref H, offset_h, LDH, ILOZ
                                         , IHIZ, ref Z, offset_z, LDZ, ref WORK, offset_work, 3, ref H, KU+1 * LDH + o_h
                                         , LDH, NVE, ref H, KWV+1 * LDH + o_h, LDH, NHO, ref H, KU+KWH * LDH + o_h
                                         , LDH);
                    }
                    // *
                    // *           ==== Note progress (or the lack of it). ====
                    // *
                    if (LD > 0)
                    {
                        NDFL = 1;
                    }
                    else
                    {
                        NDFL += 1;
                    }
                    // *
                    // *           ==== End of main loop ====
                }
                // *
                // *        ==== Iteration limit exceeded.  Set INFO to show where
                // *        .    the problem occurred and exit. ====
                // *
                INFO = KBOT;
            LABEL90:;
            }
            // *
            // *     ==== Return the optimal value of LWORK. ====
            // *
            WORK[1 + o_work] = Convert.ToDouble(LWKOPT);
            // *
            // *     ==== End of DLAQR4 ====
            // *

            #endregion

        }
    }
}
