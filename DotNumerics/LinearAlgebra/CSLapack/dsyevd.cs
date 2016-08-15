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
    /// DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
    /// real symmetric matrix A. If eigenvectors are desired, it uses a
    /// divide and conquer algorithm.
    /// 
    /// The divide and conquer algorithm makes very mild assumptions about
    /// floating point arithmetic. It will work on machines with a guard
    /// digit in add/subtract, or on those binary machines without guard
    /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
    /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    /// Because of large use of BLAS of level 3, DSYEVD needs N**2 more
    /// workspace than DSYEVX.
    /// 
    ///</summary>
    public class DSYEVD
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; DLANSY _dlansy; ILAENV _ilaenv; DLACPY _dlacpy; DLASCL _dlascl; DORMTR _dormtr; 
        DSCAL _dscal;DSTEDC _dstedc; DSTERF _dsterf; DSYTRD _dsytrd; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DSYEVD(LSAME lsame, DLAMCH dlamch, DLANSY dlansy, ILAENV ilaenv, DLACPY dlacpy, DLASCL dlascl, DORMTR dormtr, DSCAL dscal, DSTEDC dstedc, DSTERF dsterf
                      , DSYTRD dsytrd, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlansy = dlansy; this._ilaenv = ilaenv; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dormtr = dormtr; this._dscal = dscal; this._dstedc = dstedc; this._dsterf = dsterf; 
            this._dsytrd = dsytrd;this._xerbla = xerbla; 

            #endregion

        }
    
        public DSYEVD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            XERBLA xerbla = new XERBLA();
            DCOPY dcopy = new DCOPY();
            DSCAL dscal = new DSCAL();
            IDAMAX idamax = new IDAMAX();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            DNRM2 dnrm2 = new DNRM2();
            DLAED5 dlaed5 = new DLAED5();
            DLAE2 dlae2 = new DLAE2();
            DLAEV2 dlaev2 = new DLAEV2();
            DSWAP dswap = new DSWAP();
            DAXPY daxpy = new DAXPY();
            DDOT ddot = new DDOT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANSY dlansy = new DLANSY(dlassq, lsame);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORM2L dorm2l = new DORM2L(lsame, dlarf, xerbla);
            DORMQL dormql = new DORMQL(lsame, ilaenv, dlarfb, dlarft, dorm2l, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DORMTR dormtr = new DORMTR(lsame, ilaenv, dormql, dormqr, xerbla);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLAED2 dlaed2 = new DLAED2(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLASET dlaset = new DLASET(lsame);
            DLAED3 dlaed3 = new DLAED3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlaed4, dlaset, xerbla);
            DLAED1 dlaed1 = new DLAED1(dcopy, dlaed2, dlaed3, dlamrg, xerbla);
            DLAED8 dlaed8 = new DLAED8(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED9 dlaed9 = new DLAED9(dlamc3, dnrm2, dcopy, dlaed4, xerbla);
            DLAEDA dlaeda = new DLAEDA(dcopy, dgemv, drot, xerbla);
            DLAED7 dlaed7 = new DLAED7(dgemm, dlaed8, dlaed9, dlaeda, dlamrg, xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            DSTEQR dsteqr = new DSTEQR(lsame, dlamch, dlanst, dlapy2, dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr
                                       , dlasrt, dswap, xerbla);
            DLAED0 dlaed0 = new DLAED0(dcopy, dgemm, dlacpy, dlaed1, dlaed7, dsteqr, xerbla, ilaenv);
            DSTERF dsterf = new DSTERF(dlamch, dlanst, dlapy2, dlae2, dlascl, dlasrt, xerbla);
            DSTEDC dstedc = new DSTEDC(lsame, ilaenv, dlamch, dlanst, dgemm, dlacpy, dlaed0, dlascl, dlaset, dlasrt
                                       , dsteqr, dsterf, dswap, xerbla);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DSYMV dsymv = new DSYMV(lsame, xerbla);
            DLATRD dlatrd = new DLATRD(daxpy, dgemv, dlarfg, dscal, dsymv, lsame, ddot);
            DSYR2K dsyr2k = new DSYR2K(lsame, xerbla);
            DSYR2 dsyr2 = new DSYR2(lsame, xerbla);
            DSYTD2 dsytd2 = new DSYTD2(daxpy, dlarfg, dsymv, dsyr2, xerbla, lsame, ddot);
            DSYTRD dsytrd = new DSYTRD(dlatrd, dsyr2k, dsytd2, xerbla, lsame, ilaenv);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlansy = dlansy; this._ilaenv = ilaenv; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dormtr = dormtr; this._dscal = dscal; this._dstedc = dstedc; this._dsterf = dsterf; 
            this._dsytrd = dsytrd;this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
        /// real symmetric matrix A. If eigenvectors are desired, it uses a
        /// divide and conquer algorithm.
        /// 
        /// The divide and conquer algorithm makes very mild assumptions about
        /// floating point arithmetic. It will work on machines with a guard
        /// digit in add/subtract, or on those binary machines without guard
        /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
        /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
        /// 
        /// Because of large use of BLAS of level 3, DSYEVD needs N**2 more
        /// workspace than DSYEVX.
        /// 
        ///</summary>
        /// <param name="JOBZ">
        /// (input) CHARACTER*1
        /// = 'N':  Compute eigenvalues only;
        /// = 'V':  Compute eigenvalues and eigenvectors.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  Upper triangle of A is stored;
        /// = 'L':  Lower triangle of A is stored.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA, N)
        /// On entry, the symmetric matrix A.  If UPLO = 'U', the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = 'L',
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// On exit, if JOBZ = 'V', then if INFO = 0, A contains the
        /// orthonormal eigenvectors of the matrix A.
        /// If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
        /// or the upper triangle (if UPLO='U') of A, including the
        /// diagonal, is destroyed.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="W">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// If INFO = 0, the eigenvalues in ascending order.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array,
        /// dimension (LWORK)
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// If N .LE. 1,               LWORK must be at least 1.
        /// If JOBZ = 'N' and N .GT. 1, LWORK must be at least 2*N+1.
        /// If JOBZ = 'V' and N .GT. 1, LWORK must be at least
        /// 1 + 6*N + 2*N**2.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal sizes of the WORK and IWORK
        /// arrays, returns these values as the first entries of the WORK
        /// and IWORK arrays, and no error message related to LWORK or
        /// LIWORK is issued by XERBLA.
        ///</param>
        /// <param name="IWORK">
        /// (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
        /// On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
        ///</param>
        /// <param name="LIWORK">
        /// (input) INTEGER
        /// The dimension of the array IWORK.
        /// If N .LE. 1,                LIWORK must be at least 1.
        /// If JOBZ  = 'N' and N .GT. 1, LIWORK must be at least 1.
        /// If JOBZ  = 'V' and N .GT. 1, LIWORK must be at least 3 + 5*N.
        /// 
        /// If LIWORK = -1, then a workspace query is assumed; the
        /// routine only calculates the optimal sizes of the WORK and
        /// IWORK arrays, returns these values as the first entries of
        /// the WORK and IWORK arrays, and no error message related to
        /// LWORK or LIWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  if INFO = i and JOBZ = 'N', then the algorithm failed
        /// to converge; i off-diagonal elements of an intermediate
        /// tridiagonal form did not converge to zero;
        /// if INFO = i and JOBZ = 'V', then the algorithm failed
        /// to compute an eigenvalue while working on the submatrix
        /// lying in rows and columns INFO/(N+1) through
        /// mod(INFO,N+1).
        ///</param>
        public void Run(string JOBZ, string UPLO, int N, ref double[] A, int offset_a, int LDA, ref double[] W, int offset_w
                         , ref double[] WORK, int offset_work, int LWORK, ref int[] IWORK, int offset_iwork, int LIWORK, ref int INFO)
        {

            #region Variables
            
            bool LOWER = false; bool LQUERY = false; bool WANTZ = false; int IINFO = 0; int INDE = 0; int INDTAU = 0; 
            int INDWK2 = 0;int INDWRK = 0; int ISCALE = 0; int LIOPT = 0; int LIWMIN = 0; int LLWORK = 0; int LLWRK2 = 0; 
            int LOPT = 0;int LWMIN = 0; double ANRM = 0; double BIGNUM = 0; double EPS = 0; double RMAX = 0; double RMIN = 0; 
            double SAFMIN = 0;double SIGMA = 0; double SMLNUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_w = -1 + offset_w;  int o_work = -1 + offset_work; 
             int o_iwork = -1 + offset_iwork;

            #endregion


            #region Strings
            
            JOBZ = JOBZ.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  

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
            // *  DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
            // *  real symmetric matrix A. If eigenvectors are desired, it uses a
            // *  divide and conquer algorithm.
            // *
            // *  The divide and conquer algorithm makes very mild assumptions about
            // *  floating point arithmetic. It will work on machines with a guard
            // *  digit in add/subtract, or on those binary machines without guard
            // *  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
            // *  Cray-2. It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.
            // *
            // *  Because of large use of BLAS of level 3, DSYEVD needs N**2 more
            // *  workspace than DSYEVX.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOBZ    (input) CHARACTER*1
            // *          = 'N':  Compute eigenvalues only;
            // *          = 'V':  Compute eigenvalues and eigenvectors.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  Upper triangle of A is stored;
            // *          = 'L':  Lower triangle of A is stored.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
            // *          On entry, the symmetric matrix A.  If UPLO = 'U', the
            // *          leading N-by-N upper triangular part of A contains the
            // *          upper triangular part of the matrix A.  If UPLO = 'L',
            // *          the leading N-by-N lower triangular part of A contains
            // *          the lower triangular part of the matrix A.
            // *          On exit, if JOBZ = 'V', then if INFO = 0, A contains the
            // *          orthonormal eigenvectors of the matrix A.
            // *          If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
            // *          or the upper triangle (if UPLO='U') of A, including the
            // *          diagonal, is destroyed.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  W       (output) DOUBLE PRECISION array, dimension (N)
            // *          If INFO = 0, the eigenvalues in ascending order.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array,
            // *                                         dimension (LWORK)
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          If N <= 1,               LWORK must be at least 1.
            // *          If JOBZ = 'N' and N > 1, LWORK must be at least 2*N+1.
            // *          If JOBZ = 'V' and N > 1, LWORK must be at least
            // *                                                1 + 6*N + 2*N**2.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal sizes of the WORK and IWORK
            // *          arrays, returns these values as the first entries of the WORK
            // *          and IWORK arrays, and no error message related to LWORK or
            // *          LIWORK is issued by XERBLA.
            // *
            // *  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
            // *          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
            // *
            // *  LIWORK  (input) INTEGER
            // *          The dimension of the array IWORK.
            // *          If N <= 1,                LIWORK must be at least 1.
            // *          If JOBZ  = 'N' and N > 1, LIWORK must be at least 1.
            // *          If JOBZ  = 'V' and N > 1, LIWORK must be at least 3 + 5*N.
            // *
            // *          If LIWORK = -1, then a workspace query is assumed; the
            // *          routine only calculates the optimal sizes of the WORK and
            // *          IWORK arrays, returns these values as the first entries of
            // *          the WORK and IWORK arrays, and no error message related to
            // *          LWORK or LIWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i and JOBZ = 'N', then the algorithm failed
            // *                to converge; i off-diagonal elements of an intermediate
            // *                tridiagonal form did not converge to zero;
            // *                if INFO = i and JOBZ = 'V', then the algorithm failed
            // *                to compute an eigenvalue while working on the submatrix
            // *                lying in rows and columns INFO/(N+1) through
            // *                mod(INFO,N+1).
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Jeff Rutter, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *  Modified by Francoise Tisseur, University of Tennessee.
            // *
            // *  Modified description of INFO. Sven, 16 Feb 05.
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            WANTZ = this._lsame.Run(JOBZ, "V");
            LOWER = this._lsame.Run(UPLO, "L");
            LQUERY = (LWORK ==  - 1 || LIWORK ==  - 1);
            // *
            INFO = 0;
            if (!(WANTZ || this._lsame.Run(JOBZ, "N")))
            {
                INFO =  - 1;
            }
            else
            {
                if (!(LOWER || this._lsame.Run(UPLO, "U")))
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
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                if (N <= 1)
                {
                    LIWMIN = 1;
                    LWMIN = 1;
                    LOPT = LWMIN;
                    LIOPT = LIWMIN;
                }
                else
                {
                    if (WANTZ)
                    {
                        LIWMIN = 3 + 5 * N;
                        LWMIN = 1 + 6 * N + 2 * (int)Math.Pow(N, 2);
                    }
                    else
                    {
                        LIWMIN = 1;
                        LWMIN = 2 * N + 1;
                    }
                    LOPT = Math.Max(LWMIN, 2 * N + this._ilaenv.Run(1, "DSYTRD", UPLO, N,  - 1,  - 1,  - 1));
                    LIOPT = LIWMIN;
                }
                WORK[1 + o_work] = LOPT;
                IWORK[1 + o_iwork] = LIOPT;
                // *
                if (LWORK < LWMIN && !LQUERY)
                {
                    INFO =  - 8;
                }
                else
                {
                    if (LIWORK < LIWMIN && !LQUERY)
                    {
                        INFO =  - 10;
                    }
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DSYEVD",  - INFO);
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
            if (N == 1)
            {
                W[1 + o_w] = A[1+1 * LDA + o_a];
                if (WANTZ) A[1+1 * LDA + o_a] = ONE;
                return;
            }
            // *
            // *     Get machine constants.
            // *
            SAFMIN = this._dlamch.Run("Safe minimum");
            EPS = this._dlamch.Run("Precision");
            SMLNUM = SAFMIN / EPS;
            BIGNUM = ONE / SMLNUM;
            RMIN = Math.Sqrt(SMLNUM);
            RMAX = Math.Sqrt(BIGNUM);
            // *
            // *     Scale matrix to allowable range, if necessary.
            // *
            ANRM = this._dlansy.Run("M", UPLO, N, A, offset_a, LDA, ref WORK, offset_work);
            ISCALE = 0;
            if (ANRM > ZERO && ANRM < RMIN)
            {
                ISCALE = 1;
                SIGMA = RMIN / ANRM;
            }
            else
            {
                if (ANRM > RMAX)
                {
                    ISCALE = 1;
                    SIGMA = RMAX / ANRM;
                }
            }
            if (ISCALE == 1)
            {
                this._dlascl.Run(UPLO, 0, 0, ONE, SIGMA, N
                                 , N, ref A, offset_a, LDA, ref INFO);
            }
            // *
            // *     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
            // *
            INDE = 1;
            INDTAU = INDE + N;
            INDWRK = INDTAU + N;
            LLWORK = LWORK - INDWRK + 1;
            INDWK2 = INDWRK + N * N;
            LLWRK2 = LWORK - INDWK2 + 1;
            // *
            this._dsytrd.Run(UPLO, N, ref A, offset_a, LDA, ref W, offset_w, ref WORK, INDE + o_work
                             , ref WORK, INDTAU + o_work, ref WORK, INDWRK + o_work, LLWORK, ref IINFO);
            LOPT = 2 * N + (int)WORK[INDWRK + o_work];
            // *
            // *     For eigenvalues only, call DSTERF.  For eigenvectors, first call
            // *     DSTEDC to generate the eigenvector matrix, WORK(INDWRK), of the
            // *     tridiagonal matrix, then call DORMTR to multiply it by the
            // *     Householder transformations stored in A.
            // *
            if (!WANTZ)
            {
                this._dsterf.Run(N, ref W, offset_w, ref WORK, INDE + o_work, ref INFO);
            }
            else
            {
                this._dstedc.Run("I", N, ref W, offset_w, ref WORK, INDE + o_work, ref WORK, INDWRK + o_work, N
                                 , ref WORK, INDWK2 + o_work, LLWRK2, ref IWORK, offset_iwork, LIWORK, ref INFO);
                this._dormtr.Run("L", UPLO, "N", N, N, ref A, offset_a
                                 , LDA, WORK, INDTAU + o_work, ref WORK, INDWRK + o_work, N, ref WORK, INDWK2 + o_work, LLWRK2
                                 , ref IINFO);
                this._dlacpy.Run("A", N, N, WORK, INDWRK + o_work, N, ref A, offset_a
                                 , LDA);
                LOPT = (int)Math.Max(LOPT, 1 + 6 * N + 2 * Math.Pow(N, 2));
            }
            // *
            // *     If matrix was scaled, then rescale eigenvalues appropriately.
            // *
            if (ISCALE == 1) this._dscal.Run(N, ONE / SIGMA, ref W, offset_w, 1);
            // *
            WORK[1 + o_work] = LOPT;
            IWORK[1 + o_iwork] = LIOPT;
            // *
            return;
            // *
            // *     End of DSYEVD
            // *

            #endregion

        }
    }
}
