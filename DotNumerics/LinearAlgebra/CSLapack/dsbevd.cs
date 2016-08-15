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
    /// DSBEVD computes all the eigenvalues and, optionally, eigenvectors of
    /// a real symmetric band matrix A. If eigenvectors are desired, it uses
    /// a divide and conquer algorithm.
    /// 
    /// The divide and conquer algorithm makes very mild assumptions about
    /// floating point arithmetic. It will work on machines with a guard
    /// digit in add/subtract, or on those binary machines without guard
    /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
    /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    ///</summary>
    public class DSBEVD
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; DLANSB _dlansb; DGEMM _dgemm; DLACPY _dlacpy; DLASCL _dlascl; DSBTRD _dsbtrd; DSCAL _dscal; 
        DSTEDC _dstedc;DSTERF _dsterf; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DSBEVD(LSAME lsame, DLAMCH dlamch, DLANSB dlansb, DGEMM dgemm, DLACPY dlacpy, DLASCL dlascl, DSBTRD dsbtrd, DSCAL dscal, DSTEDC dstedc, DSTERF dsterf
                      , XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlansb = dlansb; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dsbtrd = dsbtrd; this._dscal = dscal; this._dstedc = dstedc; this._dsterf = dsterf; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DSBEVD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            XERBLA xerbla = new XERBLA();
            DLAR2V dlar2v = new DLAR2V();
            DLARGV dlargv = new DLARGV();
            DLARTV dlartv = new DLARTV();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            IDAMAX idamax = new IDAMAX();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMRG dlamrg = new DLAMRG();
            DNRM2 dnrm2 = new DNRM2();
            DLAED5 dlaed5 = new DLAED5();
            DLAE2 dlae2 = new DLAE2();
            DLAEV2 dlaev2 = new DLAEV2();
            DSWAP dswap = new DSWAP();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANSB dlansb = new DLANSB(dlassq, lsame);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASET dlaset = new DLASET(lsame);
            DSBTRD dsbtrd = new DSBTRD(dlar2v, dlargv, dlartg, dlartv, dlaset, drot, xerbla, lsame);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLAED2 dlaed2 = new DLAED2(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLAED3 dlaed3 = new DLAED3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlaed4, dlaset, xerbla);
            DLAED1 dlaed1 = new DLAED1(dcopy, dlaed2, dlaed3, dlamrg, xerbla);
            DLAED8 dlaed8 = new DLAED8(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED9 dlaed9 = new DLAED9(dlamc3, dnrm2, dcopy, dlaed4, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAEDA dlaeda = new DLAEDA(dcopy, dgemv, drot, xerbla);
            DLAED7 dlaed7 = new DLAED7(dgemm, dlaed8, dlaed9, dlaeda, dlamrg, xerbla);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            DSTEQR dsteqr = new DSTEQR(lsame, dlamch, dlanst, dlapy2, dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr
                                       , dlasrt, dswap, xerbla);
            DLAED0 dlaed0 = new DLAED0(dcopy, dgemm, dlacpy, dlaed1, dlaed7, dsteqr, xerbla, ilaenv);
            DSTERF dsterf = new DSTERF(dlamch, dlanst, dlapy2, dlae2, dlascl, dlasrt, xerbla);
            DSTEDC dstedc = new DSTEDC(lsame, ilaenv, dlamch, dlanst, dgemm, dlacpy, dlaed0, dlascl, dlaset, dlasrt
                                       , dsteqr, dsterf, dswap, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlansb = dlansb; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dsbtrd = dsbtrd; this._dscal = dscal; this._dstedc = dstedc; this._dsterf = dsterf; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSBEVD computes all the eigenvalues and, optionally, eigenvectors of
        /// a real symmetric band matrix A. If eigenvectors are desired, it uses
        /// a divide and conquer algorithm.
        /// 
        /// The divide and conquer algorithm makes very mild assumptions about
        /// floating point arithmetic. It will work on machines with a guard
        /// digit in add/subtract, or on those binary machines without guard
        /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
        /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
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
        /// <param name="KD">
        /// (input) INTEGER
        /// The number of superdiagonals of the matrix A if UPLO = 'U',
        /// or the number of subdiagonals if UPLO = 'L'.  KD .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix A, stored in the first KD+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd).LE.i.LE.j;
        /// if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j.LE.i.LE.min(n,j+kd).
        /// 
        /// On exit, AB is overwritten by values generated during the
        /// reduction to tridiagonal form.  If UPLO = 'U', the first
        /// superdiagonal and the diagonal of the tridiagonal matrix T
        /// are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
        /// the diagonal and first subdiagonal of T are returned in the
        /// first two rows of AB.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. KD + 1.
        ///</param>
        /// <param name="W">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// If INFO = 0, the eigenvalues in ascending order.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array, dimension (LDZ, N)
        /// If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
        /// eigenvectors of the matrix A, with the i-th column of Z
        /// holding the eigenvector associated with W(i).
        /// If JOBZ = 'N', then Z is not referenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) INTEGER
        /// The leading dimension of the array Z.  LDZ .GE. 1, and if
        /// JOBZ = 'V', LDZ .GE. max(1,N).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array,
        /// dimension (LWORK)
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// IF N .LE. 1,                LWORK must be at least 1.
        /// If JOBZ  = 'N' and N .GT. 2, LWORK must be at least 2*N.
        /// If JOBZ  = 'V' and N .GT. 2, LWORK must be at least
        /// ( 1 + 5*N + 2*N**2 ).
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
        /// The dimension of the array LIWORK.
        /// If JOBZ  = 'N' or N .LE. 1, LIWORK must be at least 1.
        /// If JOBZ  = 'V' and N .GT. 2, LIWORK must be at least 3 + 5*N.
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
        /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
        /// off-diagonal elements of an intermediate tridiagonal
        /// form did not converge to zero.
        ///</param>
        public void Run(string JOBZ, string UPLO, int N, int KD, ref double[] AB, int offset_ab, int LDAB
                         , ref double[] W, int offset_w, ref double[] Z, int offset_z, int LDZ, ref double[] WORK, int offset_work, int LWORK, ref int[] IWORK, int offset_iwork
                         , int LIWORK, ref int INFO)
        {

            #region Variables
            
            bool LOWER = false; bool LQUERY = false; bool WANTZ = false; int IINFO = 0; int INDE = 0; int INDWK2 = 0; 
            int INDWRK = 0;int ISCALE = 0; int LIWMIN = 0; int LLWRK2 = 0; int LWMIN = 0; double ANRM = 0; double BIGNUM = 0; 
            double EPS = 0;double RMAX = 0; double RMIN = 0; double SAFMIN = 0; double SIGMA = 0; double SMLNUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_w = -1 + offset_w;  int o_z = -1 - LDZ + offset_z; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork; 

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
            // *  DSBEVD computes all the eigenvalues and, optionally, eigenvectors of
            // *  a real symmetric band matrix A. If eigenvectors are desired, it uses
            // *  a divide and conquer algorithm.
            // *
            // *  The divide and conquer algorithm makes very mild assumptions about
            // *  floating point arithmetic. It will work on machines with a guard
            // *  digit in add/subtract, or on those binary machines without guard
            // *  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
            // *  Cray-2. It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.
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
            // *  KD      (input) INTEGER
            // *          The number of superdiagonals of the matrix A if UPLO = 'U',
            // *          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
            // *          On entry, the upper or lower triangle of the symmetric band
            // *          matrix A, stored in the first KD+1 rows of the array.  The
            // *          j-th column of A is stored in the j-th column of the array AB
            // *          as follows:
            // *          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
            // *          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
            // *
            // *          On exit, AB is overwritten by values generated during the
            // *          reduction to tridiagonal form.  If UPLO = 'U', the first
            // *          superdiagonal and the diagonal of the tridiagonal matrix T
            // *          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
            // *          the diagonal and first subdiagonal of T are returned in the
            // *          first two rows of AB.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= KD + 1.
            // *
            // *  W       (output) DOUBLE PRECISION array, dimension (N)
            // *          If INFO = 0, the eigenvalues in ascending order.
            // *
            // *  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
            // *          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
            // *          eigenvectors of the matrix A, with the i-th column of Z
            // *          holding the eigenvector associated with W(i).
            // *          If JOBZ = 'N', then Z is not referenced.
            // *
            // *  LDZ     (input) INTEGER
            // *          The leading dimension of the array Z.  LDZ >= 1, and if
            // *          JOBZ = 'V', LDZ >= max(1,N).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array,
            // *                                         dimension (LWORK)
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          IF N <= 1,                LWORK must be at least 1.
            // *          If JOBZ  = 'N' and N > 2, LWORK must be at least 2*N.
            // *          If JOBZ  = 'V' and N > 2, LWORK must be at least
            // *                         ( 1 + 5*N + 2*N**2 ).
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
            // *          The dimension of the array LIWORK.
            // *          If JOBZ  = 'N' or N <= 1, LIWORK must be at least 1.
            // *          If JOBZ  = 'V' and N > 2, LIWORK must be at least 3 + 5*N.
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
            //      INTRINSIC          SQRT;
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
            if (N <= 1)
            {
                LIWMIN = 1;
                LWMIN = 1;
            }
            else
            {
                if (WANTZ)
                {
                    LIWMIN = 3 + 5 * N;
                    LWMIN = 1 + 5 * N + 2 * (int)Math.Pow(N, 2);
                }
                else
                {
                    LIWMIN = 1;
                    LWMIN = 2 * N;
                }
            }
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
                        if (KD < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDAB < KD + 1)
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LDZ < 1 || (WANTZ && LDZ < N))
                                {
                                    INFO =  - 9;
                                }
                            }
                        }
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                WORK[1 + o_work] = LWMIN;
                IWORK[1 + o_iwork] = LIWMIN;
                // *
                if (LWORK < LWMIN && !LQUERY)
                {
                    INFO =  - 11;
                }
                else
                {
                    if (LIWORK < LIWMIN && !LQUERY)
                    {
                        INFO =  - 13;
                    }
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DSBEVD",  - INFO);
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
                W[1 + o_w] = AB[1+1 * LDAB + o_ab];
                if (WANTZ) Z[1+1 * LDZ + o_z] = ONE;
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
            ANRM = this._dlansb.Run("M", UPLO, N, KD, AB, offset_ab, LDAB, ref WORK, offset_work);
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
                if (LOWER)
                {
                    this._dlascl.Run("B", KD, KD, ONE, SIGMA, N
                                     , N, ref AB, offset_ab, LDAB, ref INFO);
                }
                else
                {
                    this._dlascl.Run("Q", KD, KD, ONE, SIGMA, N
                                     , N, ref AB, offset_ab, LDAB, ref INFO);
                }
            }
            // *
            // *     Call DSBTRD to reduce symmetric band matrix to tridiagonal form.
            // *
            INDE = 1;
            INDWRK = INDE + N;
            INDWK2 = INDWRK + N * N;
            LLWRK2 = LWORK - INDWK2 + 1;
            this._dsbtrd.Run(JOBZ, UPLO, N, KD, ref AB, offset_ab, LDAB
                             , ref W, offset_w, ref WORK, INDE + o_work, ref Z, offset_z, LDZ, ref WORK, INDWRK + o_work, ref IINFO);
            // *
            // *     For eigenvalues only, call DSTERF.  For eigenvectors, call SSTEDC.
            // *
            if (!WANTZ)
            {
                this._dsterf.Run(N, ref W, offset_w, ref WORK, INDE + o_work, ref INFO);
            }
            else
            {
                this._dstedc.Run("I", N, ref W, offset_w, ref WORK, INDE + o_work, ref WORK, INDWRK + o_work, N
                                 , ref WORK, INDWK2 + o_work, LLWRK2, ref IWORK, offset_iwork, LIWORK, ref INFO);
                this._dgemm.Run("N", "N", N, N, N, ONE
                                , Z, offset_z, LDZ, WORK, INDWRK + o_work, N, ZERO, ref WORK, INDWK2 + o_work
                                , N);
                this._dlacpy.Run("A", N, N, WORK, INDWK2 + o_work, N, ref Z, offset_z
                                 , LDZ);
            }
            // *
            // *     If matrix was scaled, then rescale eigenvalues appropriately.
            // *
            if (ISCALE == 1) this._dscal.Run(N, ONE / SIGMA, ref W, offset_w, 1);
            // *
            WORK[1 + o_work] = LWMIN;
            IWORK[1 + o_iwork] = LIWMIN;
            return;
            // *
            // *     End of DSBEVD
            // *

            #endregion

        }
    }
}
