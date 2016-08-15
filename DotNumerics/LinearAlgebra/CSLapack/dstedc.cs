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
    /// DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
    /// symmetric tridiagonal matrix using the divide and conquer method.
    /// The eigenvectors of a full or band real symmetric matrix can also be
    /// found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
    /// matrix to tridiagonal form.
    /// 
    /// This code makes very mild assumptions about floating point
    /// arithmetic. It will work on machines with a guard digit in
    /// add/subtract, or on those binary machines without guard digits
    /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
    /// It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.  See DLAED3 for details.
    /// 
    ///</summary>
    public class DSTEDC
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DLAMCH _dlamch; DLANST _dlanst; DGEMM _dgemm; DLACPY _dlacpy; DLAED0 _dlaed0; 
        DLASCL _dlascl;DLASET _dlaset; DLASRT _dlasrt; DSTEQR _dsteqr; DSTERF _dsterf; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 

        #endregion

        public DSTEDC(LSAME lsame, ILAENV ilaenv, DLAMCH dlamch, DLANST dlanst, DGEMM dgemm, DLACPY dlacpy, DLAED0 dlaed0, DLASCL dlascl, DLASET dlaset, DLASRT dlasrt
                      , DSTEQR dsteqr, DSTERF dsterf, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlanst = dlanst; this._dgemm = dgemm; 
            this._dlacpy = dlacpy;this._dlaed0 = dlaed0; this._dlascl = dlascl; this._dlaset = dlaset; this._dlasrt = dlasrt; 
            this._dsteqr = dsteqr;this._dsterf = dsterf; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
    
        public DSTEDC()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            XERBLA xerbla = new XERBLA();
            DCOPY dcopy = new DCOPY();
            IDAMAX idamax = new IDAMAX();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DNRM2 dnrm2 = new DNRM2();
            DLAED5 dlaed5 = new DLAED5();
            DLAE2 dlae2 = new DLAE2();
            DLAEV2 dlaev2 = new DLAEV2();
            DSWAP dswap = new DSWAP();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAED2 dlaed2 = new DLAED2(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLASET dlaset = new DLASET(lsame);
            DLAED3 dlaed3 = new DLAED3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlaed4, dlaset, xerbla);
            DLAED1 dlaed1 = new DLAED1(dcopy, dlaed2, dlaed3, dlamrg, xerbla);
            DLAED8 dlaed8 = new DLAED8(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED9 dlaed9 = new DLAED9(dlamc3, dnrm2, dcopy, dlaed4, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAEDA dlaeda = new DLAEDA(dcopy, dgemv, drot, xerbla);
            DLAED7 dlaed7 = new DLAED7(dgemm, dlaed8, dlaed9, dlaeda, dlamrg, xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            DSTEQR dsteqr = new DSTEQR(lsame, dlamch, dlanst, dlapy2, dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr
                                       , dlasrt, dswap, xerbla);
            DLAED0 dlaed0 = new DLAED0(dcopy, dgemm, dlacpy, dlaed1, dlaed7, dsteqr, xerbla, ilaenv);
            DSTERF dsterf = new DSTERF(dlamch, dlanst, dlapy2, dlae2, dlascl, dlasrt, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlanst = dlanst; this._dgemm = dgemm; 
            this._dlacpy = dlacpy;this._dlaed0 = dlaed0; this._dlascl = dlascl; this._dlaset = dlaset; this._dlasrt = dlasrt; 
            this._dsteqr = dsteqr;this._dsterf = dsterf; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
        /// symmetric tridiagonal matrix using the divide and conquer method.
        /// The eigenvectors of a full or band real symmetric matrix can also be
        /// found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
        /// matrix to tridiagonal form.
        /// 
        /// This code makes very mild assumptions about floating point
        /// arithmetic. It will work on machines with a guard digit in
        /// add/subtract, or on those binary machines without guard digits
        /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
        /// It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.  See DLAED3 for details.
        /// 
        ///</summary>
        /// <param name="COMPZ">
        /// (input) CHARACTER*1
        /// = 'N':  Compute eigenvalues only.
        /// = 'I':  Compute eigenvectors of tridiagonal matrix also.
        /// = 'V':  Compute eigenvectors of original dense symmetric
        /// matrix also.  On entry, Z contains the orthogonal
        /// matrix used to reduce the original matrix to
        /// tridiagonal form.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the symmetric tridiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the diagonal elements of the tridiagonal matrix.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, the subdiagonal elements of the tridiagonal matrix.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
        /// On entry, if COMPZ = 'V', then Z contains the orthogonal
        /// matrix used in the reduction to tridiagonal form.
        /// On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
        /// orthonormal eigenvectors of the original symmetric matrix,
        /// and if COMPZ = 'I', Z contains the orthonormal eigenvectors
        /// of the symmetric tridiagonal matrix.
        /// If  COMPZ = 'N', then Z is not referenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) INTEGER
        /// The leading dimension of the array Z.  LDZ .GE. 1.
        /// If eigenvectors are desired, then LDZ .GE. max(1,N).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array,
        /// dimension (LWORK)
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// If COMPZ = 'N' or N .LE. 1 then LWORK must be at least 1.
        /// If COMPZ = 'V' and N .GT. 1 then LWORK must be at least
        /// ( 1 + 3*N + 2*N*lg N + 3*N**2 ),
        /// where lg( N ) = smallest integer k such
        /// that 2**k .GE. N.
        /// If COMPZ = 'I' and N .GT. 1 then LWORK must be at least
        /// ( 1 + 4*N + N**2 ).
        /// Note that for COMPZ = 'I' or 'V', then if N is less than or
        /// equal to the minimum divide size, usually 25, then LWORK need
        /// only be max(1,2*(N-1)).
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="IWORK">
        /// (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
        /// On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
        ///</param>
        /// <param name="LIWORK">
        /// (input) INTEGER
        /// The dimension of the array IWORK.
        /// If COMPZ = 'N' or N .LE. 1 then LIWORK must be at least 1.
        /// If COMPZ = 'V' and N .GT. 1 then LIWORK must be at least
        /// ( 6 + 6*N + 5*N*lg N ).
        /// If COMPZ = 'I' and N .GT. 1 then LIWORK must be at least
        /// ( 3 + 5*N ).
        /// Note that for COMPZ = 'I' or 'V', then if N is less than or
        /// equal to the minimum divide size, usually 25, then LIWORK
        /// need only be 1.
        /// 
        /// If LIWORK = -1, then a workspace query is assumed; the
        /// routine only calculates the optimal size of the IWORK array,
        /// returns this value as the first entry of the IWORK array, and
        /// no error message related to LIWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  The algorithm failed to compute an eigenvalue while
        /// working on the submatrix lying in rows and columns
        /// INFO/(N+1) through mod(INFO,N+1).
        ///</param>
        public void Run(string COMPZ, int N, ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] Z, int offset_z, int LDZ
                         , ref double[] WORK, int offset_work, int LWORK, ref int[] IWORK, int offset_iwork, int LIWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int FINISH = 0; int I = 0; int ICOMPZ = 0; int II = 0; int J = 0; int K = 0; int LGN = 0; 
            int LIWMIN = 0;int LWMIN = 0; int M = 0; int SMLSIZ = 0; int START = 0; int STOREZ = 0; int STRTRW = 0; 
            double EPS = 0;double ORGNRM = 0; double P = 0; double TINY = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_z = -1 - LDZ + offset_z;  int o_work = -1 + offset_work; 
             int o_iwork = -1 + offset_iwork;

            #endregion


            #region Strings
            
            COMPZ = COMPZ.Substring(0, 1);  

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
            // *  DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
            // *  symmetric tridiagonal matrix using the divide and conquer method.
            // *  The eigenvectors of a full or band real symmetric matrix can also be
            // *  found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
            // *  matrix to tridiagonal form.
            // *
            // *  This code makes very mild assumptions about floating point
            // *  arithmetic. It will work on machines with a guard digit in
            // *  add/subtract, or on those binary machines without guard digits
            // *  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
            // *  It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.  See DLAED3 for details.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  COMPZ   (input) CHARACTER*1
            // *          = 'N':  Compute eigenvalues only.
            // *          = 'I':  Compute eigenvectors of tridiagonal matrix also.
            // *          = 'V':  Compute eigenvectors of original dense symmetric
            // *                  matrix also.  On entry, Z contains the orthogonal
            // *                  matrix used to reduce the original matrix to
            // *                  tridiagonal form.
            // *
            // *  N       (input) INTEGER
            // *          The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the diagonal elements of the tridiagonal matrix.
            // *          On exit, if INFO = 0, the eigenvalues in ascending order.
            // *
            // *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, the subdiagonal elements of the tridiagonal matrix.
            // *          On exit, E has been destroyed.
            // *
            // *  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
            // *          On entry, if COMPZ = 'V', then Z contains the orthogonal
            // *          matrix used in the reduction to tridiagonal form.
            // *          On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
            // *          orthonormal eigenvectors of the original symmetric matrix,
            // *          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
            // *          of the symmetric tridiagonal matrix.
            // *          If  COMPZ = 'N', then Z is not referenced.
            // *
            // *  LDZ     (input) INTEGER
            // *          The leading dimension of the array Z.  LDZ >= 1.
            // *          If eigenvectors are desired, then LDZ >= max(1,N).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array,
            // *                                         dimension (LWORK)
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          If COMPZ = 'N' or N <= 1 then LWORK must be at least 1.
            // *          If COMPZ = 'V' and N > 1 then LWORK must be at least
            // *                         ( 1 + 3*N + 2*N*lg N + 3*N**2 ),
            // *                         where lg( N ) = smallest integer k such
            // *                         that 2**k >= N.
            // *          If COMPZ = 'I' and N > 1 then LWORK must be at least
            // *                         ( 1 + 4*N + N**2 ).
            // *          Note that for COMPZ = 'I' or 'V', then if N is less than or
            // *          equal to the minimum divide size, usually 25, then LWORK need
            // *          only be max(1,2*(N-1)).
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
            // *          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
            // *
            // *  LIWORK  (input) INTEGER
            // *          The dimension of the array IWORK.
            // *          If COMPZ = 'N' or N <= 1 then LIWORK must be at least 1.
            // *          If COMPZ = 'V' and N > 1 then LIWORK must be at least
            // *                         ( 6 + 6*N + 5*N*lg N ).
            // *          If COMPZ = 'I' and N > 1 then LIWORK must be at least
            // *                         ( 3 + 5*N ).
            // *          Note that for COMPZ = 'I' or 'V', then if N is less than or
            // *          equal to the minimum divide size, usually 25, then LIWORK
            // *          need only be 1.
            // *
            // *          If LIWORK = -1, then a workspace query is assumed; the
            // *          routine only calculates the optimal size of the IWORK array,
            // *          returns this value as the first entry of the IWORK array, and
            // *          no error message related to LIWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  The algorithm failed to compute an eigenvalue while
            // *                working on the submatrix lying in rows and columns
            // *                INFO/(N+1) through mod(INFO,N+1).
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Jeff Rutter, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *  Modified by Francoise Tisseur, University of Tennessee.
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
            //      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LQUERY = (LWORK ==  - 1 || LIWORK ==  - 1);
            // *
            if (this._lsame.Run(COMPZ, "N"))
            {
                ICOMPZ = 0;
            }
            else
            {
                if (this._lsame.Run(COMPZ, "V"))
                {
                    ICOMPZ = 1;
                }
                else
                {
                    if (this._lsame.Run(COMPZ, "I"))
                    {
                        ICOMPZ = 2;
                    }
                    else
                    {
                        ICOMPZ =  - 1;
                    }
                }
            }
            if (ICOMPZ < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if ((LDZ < 1) || (ICOMPZ > 0 && LDZ < Math.Max(1, N)))
                    {
                        INFO =  - 6;
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                // *
                // *        Compute the workspace requirements
                // *
                SMLSIZ = this._ilaenv.Run(9, "DSTEDC", " ", 0, 0, 0, 0);
                if (N <= 1 || ICOMPZ == 0)
                {
                    LIWMIN = 1;
                    LWMIN = 1;
                }
                else
                {
                    if (N <= SMLSIZ)
                    {
                        LIWMIN = 1;
                        LWMIN = 2 * (N - 1);
                    }
                    else
                    {
                        LGN = Convert.ToInt32(Math.Truncate(Math.Log(Convert.ToDouble(N)) / Math.Log(TWO)));
                        if (Math.Pow(2,LGN) < N) LGN += 1;
                        if (Math.Pow(2,LGN) < N) LGN += 1;
                        if (ICOMPZ == 1)
                        {
                            LWMIN = 1 + 3 * N + 2 * N * LGN + 3 * (int)Math.Pow(N, 2);
                            LIWMIN = 6 + 6 * N + 5 * N * LGN;
                        }
                        else
                        {
                            if (ICOMPZ == 2)
                            {
                                LWMIN = 1 + 4 * N + (int)Math.Pow(N, 2);
                                LIWMIN = 3 + 5 * N;
                            }
                        }
                    }
                }
                WORK[1 + o_work] = LWMIN;
                IWORK[1 + o_iwork] = LIWMIN;
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
                this._xerbla.Run("DSTEDC",  - INFO);
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
            if (N == 1)
            {
                if (ICOMPZ != 0) Z[1+1 * LDZ + o_z] = ONE;
                return;
            }
            // *
            // *     If the following conditional clause is removed, then the routine
            // *     will use the Divide and Conquer routine to compute only the
            // *     eigenvalues, which requires (3N + 3N**2) real workspace and
            // *     (2 + 5N + 2N lg(N)) integer workspace.
            // *     Since on many architectures DSTERF is much faster than any other
            // *     algorithm for finding eigenvalues only, it is used here
            // *     as the default. If the conditional clause is removed, then
            // *     information on the size of workspace needs to be changed.
            // *
            // *     If COMPZ = 'N', use DSTERF to compute the eigenvalues.
            // *
            if (ICOMPZ == 0)
            {
                this._dsterf.Run(N, ref D, offset_d, ref E, offset_e, ref INFO);
                goto LABEL50;
            }
            // *
            // *     If N is smaller than the minimum divide size (SMLSIZ+1), then
            // *     solve the problem with another solver.
            // *
            if (N <= SMLSIZ)
            {
                // *
                this._dsteqr.Run(COMPZ, N, ref D, offset_d, ref E, offset_e, ref Z, offset_z, LDZ
                                 , ref WORK, offset_work, ref INFO);
                // *
            }
            else
            {
                // *
                // *        If COMPZ = 'V', the Z matrix must be stored elsewhere for later
                // *        use.
                // *
                if (ICOMPZ == 1)
                {
                    STOREZ = 1 + N * N;
                }
                else
                {
                    STOREZ = 1;
                }
                // *
                if (ICOMPZ == 2)
                {
                    this._dlaset.Run("Full", N, N, ZERO, ONE, ref Z, offset_z
                                     , LDZ);
                }
                // *
                // *        Scale.
                // *
                ORGNRM = this._dlanst.Run("M", N, D, offset_d, E, offset_e);
                if (ORGNRM == ZERO) goto LABEL50;
                // *
                EPS = this._dlamch.Run("Epsilon");
                // *
                START = 1;
                // *
                // *        while ( START <= N )
                // *
            LABEL10:;
                if (START <= N)
                {
                    // *
                    // *           Let FINISH be the position of the next subdiagonal entry
                    // *           such that E( FINISH ) <= TINY or FINISH = N if no such
                    // *           subdiagonal exists.  The matrix identified by the elements
                    // *           between START and FINISH constitutes an independent
                    // *           sub-problem.
                    // *
                    FINISH = START;
                LABEL20:;
                    if (FINISH < N)
                    {
                        TINY = EPS * Math.Sqrt(Math.Abs(D[FINISH + o_d])) * Math.Sqrt(Math.Abs(D[FINISH + 1 + o_d]));
                        if (Math.Abs(E[FINISH + o_e]) > TINY)
                        {
                            FINISH += 1;
                            goto LABEL20;
                        }
                    }
                    // *
                    // *           (Sub) Problem determined.  Compute its size and solve it.
                    // *
                    M = FINISH - START + 1;
                    if (M == 1)
                    {
                        START = FINISH + 1;
                        goto LABEL10;
                    }
                    if (M > SMLSIZ)
                    {
                        // *
                        // *              Scale.
                        // *
                        ORGNRM = this._dlanst.Run("M", M, D, START + o_d, E, START + o_e);
                        this._dlascl.Run("G", 0, 0, ORGNRM, ONE, M
                                         , 1, ref D, START + o_d, M, ref INFO);
                        this._dlascl.Run("G", 0, 0, ORGNRM, ONE, M - 1
                                         , 1, ref E, START + o_e, M - 1, ref INFO);
                        // *
                        if (ICOMPZ == 1)
                        {
                            STRTRW = 1;
                        }
                        else
                        {
                            STRTRW = START;
                        }
                        this._dlaed0.Run(ICOMPZ, N, M, ref D, START + o_d, ref E, START + o_e, ref Z, STRTRW+START * LDZ + o_z
                                         , LDZ, ref WORK, 1 + o_work, N, ref WORK, STOREZ + o_work, ref IWORK, offset_iwork, ref INFO);
                        if (INFO != 0)
                        {
                            INFO = (INFO / (M + 1) + START - 1) * (N + 1) + FortranLib.Mod(INFO,(M + 1)) + START - 1;
                            goto LABEL50;
                        }
                        // *
                        // *              Scale back.
                        // *
                        this._dlascl.Run("G", 0, 0, ONE, ORGNRM, M
                                         , 1, ref D, START + o_d, M, ref INFO);
                        // *
                    }
                    else
                    {
                        if (ICOMPZ == 1)
                        {
                            // *
                            // *                 Since QR won't update a Z matrix which is larger than
                            // *                 the length of D, we must solve the sub-problem in a
                            // *                 workspace and then multiply back into Z.
                            // *
                            this._dsteqr.Run("I", M, ref D, START + o_d, ref E, START + o_e, ref WORK, offset_work, M
                                             , ref WORK, M * M + 1 + o_work, ref INFO);
                            this._dlacpy.Run("A", N, M, Z, 1+START * LDZ + o_z, LDZ, ref WORK, STOREZ + o_work
                                             , N);
                            this._dgemm.Run("N", "N", N, M, M, ONE
                                            , WORK, STOREZ + o_work, N, WORK, offset_work, M, ZERO, ref Z, 1+START * LDZ + o_z
                                            , LDZ);
                        }
                        else
                        {
                            if (ICOMPZ == 2)
                            {
                                this._dsteqr.Run("I", M, ref D, START + o_d, ref E, START + o_e, ref Z, START+START * LDZ + o_z, LDZ
                                                 , ref WORK, offset_work, ref INFO);
                            }
                            else
                            {
                                this._dsterf.Run(M, ref D, START + o_d, ref E, START + o_e, ref INFO);
                            }
                        }
                        if (INFO != 0)
                        {
                            INFO = START * (N + 1) + FINISH;
                            goto LABEL50;
                        }
                    }
                    // *
                    START = FINISH + 1;
                    goto LABEL10;
                }
                // *
                // *        endwhile
                // *
                // *        If the problem split any number of times, then the eigenvalues
                // *        will not be properly ordered.  Here we permute the eigenvalues
                // *        (and the associated eigenvectors) into ascending order.
                // *
                if (M != N)
                {
                    if (ICOMPZ == 0)
                    {
                        // *
                        // *              Use Quick Sort
                        // *
                        this._dlasrt.Run("I", N, ref D, offset_d, ref INFO);
                        // *
                    }
                    else
                    {
                        // *
                        // *              Use Selection Sort to minimize swaps of eigenvectors
                        // *
                        for (II = 2; II <= N; II++)
                        {
                            I = II - 1;
                            K = I;
                            P = D[I + o_d];
                            for (J = II; J <= N; J++)
                            {
                                if (D[J + o_d] < P)
                                {
                                    K = J;
                                    P = D[J + o_d];
                                }
                            }
                            if (K != I)
                            {
                                D[K + o_d] = D[I + o_d];
                                D[I + o_d] = P;
                                this._dswap.Run(N, ref Z, 1+I * LDZ + o_z, 1, ref Z, 1+K * LDZ + o_z, 1);
                            }
                        }
                    }
                }
            }
            // *
        LABEL50:;
            WORK[1 + o_work] = LWMIN;
            IWORK[1 + o_iwork] = LIWMIN;
            // *
            return;
            // *
            // *     End of DSTEDC
            // *

            #endregion

        }
    }
}
