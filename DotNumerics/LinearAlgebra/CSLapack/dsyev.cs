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
    /// DSYEV computes all eigenvalues and, optionally, eigenvectors of a
    /// real symmetric matrix A.
    /// 
    ///</summary>
    public class DSYEV
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DLAMCH _dlamch; DLANSY _dlansy; DLASCL _dlascl; DORGTR _dorgtr; DSCAL _dscal; 
        DSTEQR _dsteqr;DSTERF _dsterf; DSYTRD _dsytrd; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DSYEV(LSAME lsame, ILAENV ilaenv, DLAMCH dlamch, DLANSY dlansy, DLASCL dlascl, DORGTR dorgtr, DSCAL dscal, DSTEQR dsteqr, DSTERF dsterf, DSYTRD dsytrd
                     , XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlansy = dlansy; this._dlascl = dlascl; 
            this._dorgtr = dorgtr;this._dscal = dscal; this._dsteqr = dsteqr; this._dsterf = dsterf; this._dsytrd = dsytrd; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DSYEV()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            XERBLA xerbla = new XERBLA();
            DCOPY dcopy = new DCOPY();
            DSCAL dscal = new DSCAL();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAE2 dlae2 = new DLAE2();
            DLAEV2 dlaev2 = new DLAEV2();
            DSWAP dswap = new DSWAP();
            DAXPY daxpy = new DAXPY();
            DNRM2 dnrm2 = new DNRM2();
            DDOT ddot = new DDOT();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANSY dlansy = new DLANSY(dlassq, lsame);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORG2L dorg2l = new DORG2L(dlarf, dscal, xerbla);
            DORGQL dorgql = new DORGQL(dlarfb, dlarft, dorg2l, xerbla, ilaenv);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORGQR dorgqr = new DORGQR(dlarfb, dlarft, dorg2r, xerbla, ilaenv);
            DORGTR dorgtr = new DORGTR(lsame, ilaenv, dorgql, dorgqr, xerbla);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASET dlaset = new DLASET(lsame);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            DSTEQR dsteqr = new DSTEQR(lsame, dlamch, dlanst, dlapy2, dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr
                                       , dlasrt, dswap, xerbla);
            DSTERF dsterf = new DSTERF(dlamch, dlanst, dlapy2, dlae2, dlascl, dlasrt, xerbla);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DSYMV dsymv = new DSYMV(lsame, xerbla);
            DLATRD dlatrd = new DLATRD(daxpy, dgemv, dlarfg, dscal, dsymv, lsame, ddot);
            DSYR2K dsyr2k = new DSYR2K(lsame, xerbla);
            DSYR2 dsyr2 = new DSYR2(lsame, xerbla);
            DSYTD2 dsytd2 = new DSYTD2(daxpy, dlarfg, dsymv, dsyr2, xerbla, lsame, ddot);
            DSYTRD dsytrd = new DSYTRD(dlatrd, dsyr2k, dsytd2, xerbla, lsame, ilaenv);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlansy = dlansy; this._dlascl = dlascl; 
            this._dorgtr = dorgtr;this._dscal = dscal; this._dsteqr = dsteqr; this._dsterf = dsterf; this._dsytrd = dsytrd; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSYEV computes all eigenvalues and, optionally, eigenvectors of a
        /// real symmetric matrix A.
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
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The length of the array WORK.  LWORK .GE. max(1,3*N-1).
        /// For optimal efficiency, LWORK .GE. (NB+2)*N,
        /// where NB is the blocksize for DSYTRD returned by ILAENV.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of an intermediate tridiagonal
        /// form did not converge to zero.
        ///</param>
        public void Run(string JOBZ, string UPLO, int N, ref double[] A, int offset_a, int LDA, ref double[] W, int offset_w
                         , ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LOWER = false; bool LQUERY = false; bool WANTZ = false; int IINFO = 0; int IMAX = 0; int INDE = 0; 
            int INDTAU = 0;int INDWRK = 0; int ISCALE = 0; int LLWORK = 0; int LWKOPT = 0; int NB = 0; double ANRM = 0; 
            double BIGNUM = 0;double EPS = 0; double RMAX = 0; double RMIN = 0; double SAFMIN = 0; double SIGMA = 0; 
            double SMLNUM = 0;

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_w = -1 + offset_w;  int o_work = -1 + offset_work; 

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
            // *  DSYEV computes all eigenvalues and, optionally, eigenvectors of a
            // *  real symmetric matrix A.
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
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The length of the array WORK.  LWORK >= max(1,3*N-1).
            // *          For optimal efficiency, LWORK >= (NB+2)*N,
            // *          where NB is the blocksize for DSYTRD returned by ILAENV.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i, the algorithm failed to converge; i
            // *                off-diagonal elements of an intermediate tridiagonal
            // *                form did not converge to zero.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
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
            LQUERY = (LWORK ==  - 1);
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
                NB = this._ilaenv.Run(1, "DSYTRD", UPLO, N,  - 1,  - 1,  - 1);
                LWKOPT = Math.Max(1, (NB + 2) * N);
                WORK[1 + o_work] = LWKOPT;
                // *
                if (LWORK < Math.Max(1, 3 * N - 1) && !LQUERY) INFO =  - 8;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DSYEV ",  - INFO);
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
            if (N == 0)
            {
                return;
            }
            // *
            if (N == 1)
            {
                W[1 + o_w] = A[1+1 * LDA + o_a];
                WORK[1 + o_work] = 2;
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
            this._dsytrd.Run(UPLO, N, ref A, offset_a, LDA, ref W, offset_w, ref WORK, INDE + o_work
                             , ref WORK, INDTAU + o_work, ref WORK, INDWRK + o_work, LLWORK, ref IINFO);
            // *
            // *     For eigenvalues only, call DSTERF.  For eigenvectors, first call
            // *     DORGTR to generate the orthogonal matrix, then call DSTEQR.
            // *
            if (!WANTZ)
            {
                this._dsterf.Run(N, ref W, offset_w, ref WORK, INDE + o_work, ref INFO);
            }
            else
            {
                this._dorgtr.Run(UPLO, N, ref A, offset_a, LDA, WORK, INDTAU + o_work, ref WORK, INDWRK + o_work
                                 , LLWORK, ref IINFO);
                this._dsteqr.Run(JOBZ, N, ref W, offset_w, ref WORK, INDE + o_work, ref A, offset_a, LDA
                                 , ref WORK, INDTAU + o_work, ref INFO);
            }
            // *
            // *     If matrix was scaled, then rescale eigenvalues appropriately.
            // *
            if (ISCALE == 1)
            {
                if (INFO == 0)
                {
                    IMAX = N;
                }
                else
                {
                    IMAX = INFO - 1;
                }
                this._dscal.Run(IMAX, ONE / SIGMA, ref W, offset_w, 1);
            }
            // *
            // *     Set WORK(1) to optimal workspace size.
            // *
            WORK[1 + o_work] = LWKOPT;
            // *
            return;
            // *
            // *     End of DSYEV
            // *

            #endregion

        }
    }
}
