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
    /// DGEEV computes for an N-by-N real nonsymmetric matrix A, the
    /// eigenvalues and, optionally, the left and/or right eigenvectors.
    /// 
    /// The right eigenvector v(j) of A satisfies
    /// A * v(j) = lambda(j) * v(j)
    /// where lambda(j) is its eigenvalue.
    /// The left eigenvector u(j) of A satisfies
    /// u(j)**H * A = lambda(j) * u(j)**H
    /// where u(j)**H denotes the conjugate transpose of u(j).
    /// 
    /// The computed eigenvectors are normalized to have Euclidean norm
    /// equal to 1 and largest component real.
    /// 
    ///</summary>
    public class DGEEV
    {
    

        #region Dependencies
        
        DGEBAK _dgebak; DGEBAL _dgebal; DGEHRD _dgehrd; DHSEQR _dhseqr; DLABAD _dlabad; DLACPY _dlacpy; DLARTG _dlartg; 
        DLASCL _dlascl;DORGHR _dorghr; DROT _drot; DSCAL _dscal; DTREVC _dtrevc; XERBLA _xerbla; LSAME _lsame; IDAMAX _idamax; 
        ILAENV _ilaenv;DLAMCH _dlamch; DLANGE _dlange; DLAPY2 _dlapy2; DNRM2 _dnrm2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; bool[] SELECT = new bool[1]; double[] DUM = new double[1]; 

        #endregion

        public DGEEV(DGEBAK dgebak, DGEBAL dgebal, DGEHRD dgehrd, DHSEQR dhseqr, DLABAD dlabad, DLACPY dlacpy, DLARTG dlartg, DLASCL dlascl, DORGHR dorghr, DROT drot
                     , DSCAL dscal, DTREVC dtrevc, XERBLA xerbla, LSAME lsame, IDAMAX idamax, ILAENV ilaenv, DLAMCH dlamch, DLANGE dlange, DLAPY2 dlapy2, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dgebak = dgebak; this._dgebal = dgebal; this._dgehrd = dgehrd; this._dhseqr = dhseqr; this._dlabad = dlabad; 
            this._dlacpy = dlacpy;this._dlartg = dlartg; this._dlascl = dlascl; this._dorghr = dorghr; this._drot = drot; 
            this._dscal = dscal;this._dtrevc = dtrevc; this._xerbla = xerbla; this._lsame = lsame; this._idamax = idamax; 
            this._ilaenv = ilaenv;this._dlamch = dlamch; this._dlange = dlange; this._dlapy2 = dlapy2; this._dnrm2 = dnrm2; 

            #endregion

        }
    
        public DGEEV()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            XERBLA xerbla = new XERBLA();
            IDAMAX idamax = new IDAMAX();
            DLAMC3 dlamc3 = new DLAMC3();
            DAXPY daxpy = new DAXPY();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DCOPY dcopy = new DCOPY();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLABAD dlabad = new DLABAD();
            DROT drot = new DROT();
            DLASSQ dlassq = new DLASSQ();
            DLAQR1 dlaqr1 = new DLAQR1();
            DDOT ddot = new DDOT();
            DLADIV dladiv = new DLADIV();
            DGEBAK dgebak = new DGEBAK(lsame, dscal, dswap, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DGEBAL dgebal = new DGEBAL(lsame, idamax, dlamch, dscal, dswap, xerbla);
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
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
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
            DLAQR3 dlaqr3 = new DLAQR3(dlamch, ilaenv, dcopy, dgehrd, dgemm, dlabad, dlacpy, dlahqr, dlanv2, dlaqr4
                                       , dlarf, dlarfg, dlaset, dorghr, dtrexc);
            DLAQR0 dlaqr0 = new DLAQR0(ilaenv, dlacpy, dlahqr, dlanv2, dlaqr3, dlaqr4, dlaqr5);
            DHSEQR dhseqr = new DHSEQR(ilaenv, lsame, dlacpy, dlahqr, dlaqr0, dlaset, xerbla);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLALN2 dlaln2 = new DLALN2(dlamch, dladiv);
            DTREVC dtrevc = new DTREVC(lsame, idamax, ddot, dlamch, daxpy, dcopy, dgemv, dlaln2, dscal, xerbla
                                       , dlabad);

            #endregion


            #region Set Dependencies
            
            this._dgebak = dgebak; this._dgebal = dgebal; this._dgehrd = dgehrd; this._dhseqr = dhseqr; this._dlabad = dlabad; 
            this._dlacpy = dlacpy;this._dlartg = dlartg; this._dlascl = dlascl; this._dorghr = dorghr; this._drot = drot; 
            this._dscal = dscal;this._dtrevc = dtrevc; this._xerbla = xerbla; this._lsame = lsame; this._idamax = idamax; 
            this._ilaenv = ilaenv;this._dlamch = dlamch; this._dlange = dlange; this._dlapy2 = dlapy2; this._dnrm2 = dnrm2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEEV computes for an N-by-N real nonsymmetric matrix A, the
        /// eigenvalues and, optionally, the left and/or right eigenvectors.
        /// 
        /// The right eigenvector v(j) of A satisfies
        /// A * v(j) = lambda(j) * v(j)
        /// where lambda(j) is its eigenvalue.
        /// The left eigenvector u(j) of A satisfies
        /// u(j)**H * A = lambda(j) * u(j)**H
        /// where u(j)**H denotes the conjugate transpose of u(j).
        /// 
        /// The computed eigenvectors are normalized to have Euclidean norm
        /// equal to 1 and largest component real.
        /// 
        ///</summary>
        /// <param name="JOBVL">
        /// (input) CHARACTER*1
        /// = 'N': left eigenvectors of A are not computed;
        /// = 'V': left eigenvectors of A are computed.
        ///</param>
        /// <param name="JOBVR">
        /// (input) CHARACTER*1
        /// = 'N': right eigenvectors of A are not computed;
        /// = 'V': right eigenvectors of A are computed.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A. N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the N-by-N matrix A.
        /// On exit, A has been overwritten.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="WR">
        /// (output) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="WI">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// WR and WI contain the real and imaginary parts,
        /// respectively, of the computed eigenvalues.  Complex
        /// conjugate pairs of eigenvalues appear consecutively
        /// with the eigenvalue having the positive imaginary part
        /// first.
        ///</param>
        /// <param name="VL">
        /// (output) DOUBLE PRECISION array, dimension (LDVL,N)
        /// If JOBVL = 'V', the left eigenvectors u(j) are stored one
        /// after another in the columns of VL, in the same order
        /// as their eigenvalues.
        /// If JOBVL = 'N', VL is not referenced.
        /// If the j-th eigenvalue is real, then u(j) = VL(:,j),
        /// the j-th column of VL.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
        /// u(j+1) = VL(:,j) - i*VL(:,j+1).
        ///</param>
        /// <param name="LDVL">
        /// (input) INTEGER
        /// The leading dimension of the array VL.  LDVL .GE. 1; if
        /// JOBVL = 'V', LDVL .GE. N.
        ///</param>
        /// <param name="VR">
        /// (output) DOUBLE PRECISION array, dimension (LDVR,N)
        /// If JOBVR = 'V', the right eigenvectors v(j) are stored one
        /// after another in the columns of VR, in the same order
        /// as their eigenvalues.
        /// If JOBVR = 'N', VR is not referenced.
        /// If the j-th eigenvalue is real, then v(j) = VR(:,j),
        /// the j-th column of VR.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
        /// v(j+1) = VR(:,j) - i*VR(:,j+1).
        ///</param>
        /// <param name="LDVR">
        /// (input) INTEGER
        /// The leading dimension of the array VR.  LDVR .GE. 1; if
        /// JOBVR = 'V', LDVR .GE. N.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. max(1,3*N), and
        /// if JOBVL = 'V' or JOBVR = 'V', LWORK .GE. 4*N.  For good
        /// performance, LWORK must generally be larger.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = i, the QR algorithm failed to compute all the
        /// eigenvalues, and no eigenvectors have been computed;
        /// elements i+1:N of WR and WI contain eigenvalues which
        /// have converged.
        ///</param>
        public void Run(string JOBVL, string JOBVR, int N, ref double[] A, int offset_a, int LDA, ref double[] WR, int offset_wr
                         , ref double[] WI, int offset_wi, ref double[] VL, int offset_vl, int LDVL, ref double[] VR, int offset_vr, int LDVR, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool SCALEA = false; bool WANTVL = false; bool WANTVR = false; string SIDE = new string(' ', 1); 
            int HSWORK = 0;int I = 0; int IBAL = 0; int IERR = 0; int IHI = 0; int ILO = 0; int ITAU = 0; int IWRK = 0; int K = 0; 
            int MAXWRK = 0;int MINWRK = 0; int NOUT = 0; double ANRM = 0; double BIGNUM = 0; double CS = 0; double CSCALE = 0; 
            double EPS = 0;double R = 0; double SCL = 0; double SMLNUM = 0; double SN = 0; 
            int offset_select = 0; int offset_dum = 0;

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_wr = -1 + offset_wr;  int o_wi = -1 + offset_wi; 
             int o_vl = -1 - LDVL + offset_vl; int o_vr = -1 - LDVR + offset_vr;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            JOBVL = JOBVL.Substring(0, 1);  JOBVR = JOBVR.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK driver routine (version 3.1) --
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
            // *  DGEEV computes for an N-by-N real nonsymmetric matrix A, the
            // *  eigenvalues and, optionally, the left and/or right eigenvectors.
            // *
            // *  The right eigenvector v(j) of A satisfies
            // *                   A * v(j) = lambda(j) * v(j)
            // *  where lambda(j) is its eigenvalue.
            // *  The left eigenvector u(j) of A satisfies
            // *                u(j)**H * A = lambda(j) * u(j)**H
            // *  where u(j)**H denotes the conjugate transpose of u(j).
            // *
            // *  The computed eigenvectors are normalized to have Euclidean norm
            // *  equal to 1 and largest component real.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOBVL   (input) CHARACTER*1
            // *          = 'N': left eigenvectors of A are not computed;
            // *          = 'V': left eigenvectors of A are computed.
            // *
            // *  JOBVR   (input) CHARACTER*1
            // *          = 'N': right eigenvectors of A are not computed;
            // *          = 'V': right eigenvectors of A are computed.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A. N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the N-by-N matrix A.
            // *          On exit, A has been overwritten.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  WR      (output) DOUBLE PRECISION array, dimension (N)
            // *  WI      (output) DOUBLE PRECISION array, dimension (N)
            // *          WR and WI contain the real and imaginary parts,
            // *          respectively, of the computed eigenvalues.  Complex
            // *          conjugate pairs of eigenvalues appear consecutively
            // *          with the eigenvalue having the positive imaginary part
            // *          first.
            // *
            // *  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
            // *          If JOBVL = 'V', the left eigenvectors u(j) are stored one
            // *          after another in the columns of VL, in the same order
            // *          as their eigenvalues.
            // *          If JOBVL = 'N', VL is not referenced.
            // *          If the j-th eigenvalue is real, then u(j) = VL(:,j),
            // *          the j-th column of VL.
            // *          If the j-th and (j+1)-st eigenvalues form a complex
            // *          conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
            // *          u(j+1) = VL(:,j) - i*VL(:,j+1).
            // *
            // *  LDVL    (input) INTEGER
            // *          The leading dimension of the array VL.  LDVL >= 1; if
            // *          JOBVL = 'V', LDVL >= N.
            // *
            // *  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
            // *          If JOBVR = 'V', the right eigenvectors v(j) are stored one
            // *          after another in the columns of VR, in the same order
            // *          as their eigenvalues.
            // *          If JOBVR = 'N', VR is not referenced.
            // *          If the j-th eigenvalue is real, then v(j) = VR(:,j),
            // *          the j-th column of VR.
            // *          If the j-th and (j+1)-st eigenvalues form a complex
            // *          conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
            // *          v(j+1) = VR(:,j) - i*VR(:,j+1).
            // *
            // *  LDVR    (input) INTEGER
            // *          The leading dimension of the array VR.  LDVR >= 1; if
            // *          JOBVR = 'V', LDVR >= N.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.  LWORK >= max(1,3*N), and
            // *          if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
            // *          performance, LWORK must generally be larger.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = i, the QR algorithm failed to compute all the
            // *                eigenvalues, and no eigenvectors have been computed;
            // *                elements i+1:N of WR and WI contain eigenvalues which
            // *                have converged.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LQUERY = (LWORK ==  - 1);
            WANTVL = this._lsame.Run(JOBVL, "V");
            WANTVR = this._lsame.Run(JOBVR, "V");
            if ((!WANTVL) && (!this._lsame.Run(JOBVL, "N")))
            {
                INFO =  - 1;
            }
            else
            {
                if ((!WANTVR) && (!this._lsame.Run(JOBVR, "N")))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, N))
                        {
                            INFO =  - 5;
                        }
                        else
                        {
                            if (LDVL < 1 || (WANTVL && LDVL < N))
                            {
                                INFO =  - 9;
                            }
                            else
                            {
                                if (LDVR < 1 || (WANTVR && LDVR < N))
                                {
                                    INFO =  - 11;
                                }
                            }
                        }
                    }
                }
            }
            // *
            // *     Compute workspace
            // *      (Note: Comments in the code beginning "Workspace:" describe the
            // *       minimal amount of workspace needed at that point in the code,
            // *       as well as the preferred amount for good performance.
            // *       NB refers to the optimal block size for the immediately
            // *       following subroutine, as returned by ILAENV.
            // *       HSWORK refers to the workspace preferred by DHSEQR, as
            // *       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
            // *       the worst case.)
            // *
            if (INFO == 0)
            {
                if (N == 0)
                {
                    MINWRK = 1;
                    MAXWRK = 1;
                }
                else
                {
                    MAXWRK = 2 * N + N * this._ilaenv.Run(1, "DGEHRD", " ", N, 1, N, 0);
                    if (WANTVL)
                    {
                        MINWRK = 4 * N;
                        MAXWRK = Math.Max(MAXWRK, 2 * N + (N - 1) * this._ilaenv.Run(1, "DORGHR", " ", N, 1, N,  - 1));
                        this._dhseqr.Run("S", "V", N, 1, N, ref A, offset_a
                                         , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VL, offset_vl, LDVL, ref WORK, offset_work
                                         ,  - 1, ref INFO);
                        HSWORK = (int)WORK[1 + o_work];
                        MAXWRK = Math.Max(MAXWRK, Math.Max(N + 1, N + HSWORK));
                        MAXWRK = Math.Max(MAXWRK, 4 * N);
                    }
                    else
                    {
                        if (WANTVR)
                        {
                            MINWRK = 4 * N;
                            MAXWRK = Math.Max(MAXWRK, 2 * N + (N - 1) * this._ilaenv.Run(1, "DORGHR", " ", N, 1, N,  - 1));
                            this._dhseqr.Run("S", "V", N, 1, N, ref A, offset_a
                                             , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VR, offset_vr, LDVR, ref WORK, offset_work
                                             ,  - 1, ref INFO);
                            HSWORK = (int)WORK[1 + o_work];
                            MAXWRK = Math.Max(MAXWRK, Math.Max(N + 1, N + HSWORK));
                            MAXWRK = Math.Max(MAXWRK, 4 * N);
                        }
                        else
                        {
                            MINWRK = 3 * N;
                            this._dhseqr.Run("E", "N", N, 1, N, ref A, offset_a
                                             , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VR, offset_vr, LDVR, ref WORK, offset_work
                                             ,  - 1, ref INFO);
                            HSWORK = (int)WORK[1 + o_work];
                            MAXWRK = Math.Max(MAXWRK, Math.Max(N + 1, N + HSWORK));
                        }
                    }
                    MAXWRK = Math.Max(MAXWRK, MINWRK);
                }
                WORK[1 + o_work] = MAXWRK;
                // *
                if (LWORK < MINWRK && !LQUERY)
                {
                    INFO =  - 13;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DGEEV ",  - INFO);
                return;
            }
            else
            {
                if (LQUERY)
                {
                    return;
                }
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Get machine constants
            // *
            EPS = this._dlamch.Run("P");
            SMLNUM = this._dlamch.Run("S");
            BIGNUM = ONE / SMLNUM;
            this._dlabad.Run(ref SMLNUM, ref BIGNUM);
            SMLNUM = Math.Sqrt(SMLNUM) / EPS;
            BIGNUM = ONE / SMLNUM;
            // *
            // *     Scale A if max element outside range [SMLNUM,BIGNUM]
            // *
            ANRM = this._dlange.Run("M", N, N, A, offset_a, LDA, ref DUM, offset_dum);
            SCALEA = false;
            if (ANRM > ZERO && ANRM < SMLNUM)
            {
                SCALEA = true;
                CSCALE = SMLNUM;
            }
            else
            {
                if (ANRM > BIGNUM)
                {
                    SCALEA = true;
                    CSCALE = BIGNUM;
                }
            }
            if (SCALEA)
            {
                this._dlascl.Run("G", 0, 0, ANRM, CSCALE, N
                                 , N, ref A, offset_a, LDA, ref IERR);
            }
            // *
            // *     Balance the matrix
            // *     (Workspace: need N)
            // *
            IBAL = 1;
            this._dgebal.Run("B", N, ref A, offset_a, LDA, ref ILO, ref IHI
                             , ref WORK, IBAL + o_work, ref IERR);
            // *
            // *     Reduce to upper Hessenberg form
            // *     (Workspace: need 3*N, prefer 2*N+N*NB)
            // *
            ITAU = IBAL + N;
            IWRK = ITAU + N;
            this._dgehrd.Run(N, ILO, IHI, ref A, offset_a, LDA, ref WORK, ITAU + o_work
                             , ref WORK, IWRK + o_work, LWORK - IWRK + 1, ref IERR);
            // *
            if (WANTVL)
            {
                // *
                // *        Want left eigenvectors
                // *        Copy Householder vectors to VL
                // *
                FortranLib.Copy(ref SIDE , "L");
                this._dlacpy.Run("L", N, N, A, offset_a, LDA, ref VL, offset_vl
                                 , LDVL);
                // *
                // *        Generate orthogonal matrix in VL
                // *        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
                // *
                this._dorghr.Run(N, ILO, IHI, ref VL, offset_vl, LDVL, WORK, ITAU + o_work
                                 , ref WORK, IWRK + o_work, LWORK - IWRK + 1, ref IERR);
                // *
                // *        Perform QR iteration, accumulating Schur vectors in VL
                // *        (Workspace: need N+1, prefer N+HSWORK (see comments) )
                // *
                IWRK = ITAU;
                this._dhseqr.Run("S", "V", N, ILO, IHI, ref A, offset_a
                                 , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VL, offset_vl, LDVL, ref WORK, IWRK + o_work
                                 , LWORK - IWRK + 1, ref INFO);
                // *
                if (WANTVR)
                {
                    // *
                    // *           Want left and right eigenvectors
                    // *           Copy Schur vectors to VR
                    // *
                    FortranLib.Copy(ref SIDE , "B");
                    this._dlacpy.Run("F", N, N, VL, offset_vl, LDVL, ref VR, offset_vr
                                     , LDVR);
                }
                // *
            }
            else
            {
                if (WANTVR)
                {
                    // *
                    // *        Want right eigenvectors
                    // *        Copy Householder vectors to VR
                    // *
                    FortranLib.Copy(ref SIDE , "R");
                    this._dlacpy.Run("L", N, N, A, offset_a, LDA, ref VR, offset_vr
                                     , LDVR);
                    // *
                    // *        Generate orthogonal matrix in VR
                    // *        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
                    // *
                    this._dorghr.Run(N, ILO, IHI, ref VR, offset_vr, LDVR, WORK, ITAU + o_work
                                     , ref WORK, IWRK + o_work, LWORK - IWRK + 1, ref IERR);
                    // *
                    // *        Perform QR iteration, accumulating Schur vectors in VR
                    // *        (Workspace: need N+1, prefer N+HSWORK (see comments) )
                    // *
                    IWRK = ITAU;
                    this._dhseqr.Run("S", "V", N, ILO, IHI, ref A, offset_a
                                     , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VR, offset_vr, LDVR, ref WORK, IWRK + o_work
                                     , LWORK - IWRK + 1, ref INFO);
                    // *
                }
                else
                {
                    // *
                    // *        Compute eigenvalues only
                    // *        (Workspace: need N+1, prefer N+HSWORK (see comments) )
                    // *
                    IWRK = ITAU;
                    this._dhseqr.Run("E", "N", N, ILO, IHI, ref A, offset_a
                                     , LDA, ref WR, offset_wr, ref WI, offset_wi, ref VR, offset_vr, LDVR, ref WORK, IWRK + o_work
                                     , LWORK - IWRK + 1, ref INFO);
                }
            }
            // *
            // *     If INFO > 0 from DHSEQR, then quit
            // *
            if (INFO > 0) goto LABEL50;
            // *
            if (WANTVL || WANTVR)
            {
                // *
                // *        Compute left and/or right eigenvectors
                // *        (Workspace: need 4*N)
                // *
                this._dtrevc.Run(SIDE, "B", ref SELECT, offset_select, N, A, offset_a, LDA
                                 , ref VL, offset_vl, LDVL, ref VR, offset_vr, LDVR, N, ref NOUT
                                 , ref WORK, IWRK + o_work, ref IERR);
            }
            // *
            if (WANTVL)
            {
                // *
                // *        Undo balancing of left eigenvectors
                // *        (Workspace: need N)
                // *
                this._dgebak.Run("B", "L", N, ILO, IHI, WORK, IBAL + o_work
                                 , N, ref VL, offset_vl, LDVL, ref IERR);
                // *
                // *        Normalize left eigenvectors and make largest component real
                // *
                for (I = 1; I <= N; I++)
                {
                    if (WI[I + o_wi] == ZERO)
                    {
                        SCL = ONE / this._dnrm2.Run(N, VL, 1+I * LDVL + o_vl, 1);
                        this._dscal.Run(N, SCL, ref VL, 1+I * LDVL + o_vl, 1);
                    }
                    else
                    {
                        if (WI[I + o_wi] > ZERO)
                        {
                            SCL = ONE / this._dlapy2.Run(this._dnrm2.Run(N, VL, 1+I * LDVL + o_vl, 1), this._dnrm2.Run(N, VL, 1+(I + 1) * LDVL + o_vl, 1));
                            this._dscal.Run(N, SCL, ref VL, 1+I * LDVL + o_vl, 1);
                            this._dscal.Run(N, SCL, ref VL, 1+(I + 1) * LDVL + o_vl, 1);
                            for (K = 1; K <= N; K++)
                            {
                                WORK[IWRK + K - 1 + o_work] = Math.Pow(VL[K+I * LDVL + o_vl],2) + Math.Pow(VL[K+(I + 1) * LDVL + o_vl],2);
                            }
                            K = this._idamax.Run(N, WORK, IWRK + o_work, 1);
                            this._dlartg.Run(VL[K+I * LDVL + o_vl], VL[K+(I + 1) * LDVL + o_vl], ref CS, ref SN, ref R);
                            this._drot.Run(N, ref VL, 1+I * LDVL + o_vl, 1, ref VL, 1+(I + 1) * LDVL + o_vl, 1, CS
                                           , SN);
                            VL[K+(I + 1) * LDVL + o_vl] = ZERO;
                        }
                    }
                }
            }
            // *
            if (WANTVR)
            {
                // *
                // *        Undo balancing of right eigenvectors
                // *        (Workspace: need N)
                // *
                this._dgebak.Run("B", "R", N, ILO, IHI, WORK, IBAL + o_work
                                 , N, ref VR, offset_vr, LDVR, ref IERR);
                // *
                // *        Normalize right eigenvectors and make largest component real
                // *
                for (I = 1; I <= N; I++)
                {
                    if (WI[I + o_wi] == ZERO)
                    {
                        SCL = ONE / this._dnrm2.Run(N, VR, 1+I * LDVR + o_vr, 1);
                        this._dscal.Run(N, SCL, ref VR, 1+I * LDVR + o_vr, 1);
                    }
                    else
                    {
                        if (WI[I + o_wi] > ZERO)
                        {
                            SCL = ONE / this._dlapy2.Run(this._dnrm2.Run(N, VR, 1+I * LDVR + o_vr, 1), this._dnrm2.Run(N, VR, 1+(I + 1) * LDVR + o_vr, 1));
                            this._dscal.Run(N, SCL, ref VR, 1+I * LDVR + o_vr, 1);
                            this._dscal.Run(N, SCL, ref VR, 1+(I + 1) * LDVR + o_vr, 1);
                            for (K = 1; K <= N; K++)
                            {
                                WORK[IWRK + K - 1 + o_work] = Math.Pow(VR[K+I * LDVR + o_vr],2) + Math.Pow(VR[K+(I + 1) * LDVR + o_vr],2);
                            }
                            K = this._idamax.Run(N, WORK, IWRK + o_work, 1);
                            this._dlartg.Run(VR[K+I * LDVR + o_vr], VR[K+(I + 1) * LDVR + o_vr], ref CS, ref SN, ref R);
                            this._drot.Run(N, ref VR, 1+I * LDVR + o_vr, 1, ref VR, 1+(I + 1) * LDVR + o_vr, 1, CS
                                           , SN);
                            VR[K+(I + 1) * LDVR + o_vr] = ZERO;
                        }
                    }
                }
            }
            // *
            // *     Undo scaling if necessary
            // *
        LABEL50:;
            if (SCALEA)
            {
                this._dlascl.Run("G", 0, 0, CSCALE, ANRM, N - INFO
                                 , 1, ref WR, INFO + 1 + o_wr, Math.Max(N - INFO, 1), ref IERR);
                this._dlascl.Run("G", 0, 0, CSCALE, ANRM, N - INFO
                                 , 1, ref WI, INFO + 1 + o_wi, Math.Max(N - INFO, 1), ref IERR);
                if (INFO > 0)
                {
                    this._dlascl.Run("G", 0, 0, CSCALE, ANRM, ILO - 1
                                     , 1, ref WR, offset_wr, N, ref IERR);
                    this._dlascl.Run("G", 0, 0, CSCALE, ANRM, ILO - 1
                                     , 1, ref WI, offset_wi, N, ref IERR);
                }
            }
            // *
            WORK[1 + o_work] = MAXWRK;
            return;
            // *
            // *     End of DGEEV
            // *

            #endregion

        }
    }
}
