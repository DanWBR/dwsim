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
    /// DGELS solves overdetermined or underdetermined real linear systems
    /// involving an M-by-N matrix A, or its transpose, using a QR or LQ
    /// factorization of A.  It is assumed that A has full rank.
    /// 
    /// The following options are provided:
    /// 
    /// 1. If TRANS = 'N' and m .GE. n:  find the least squares solution of
    /// an overdetermined system, i.e., solve the least squares problem
    /// minimize || B - A*X ||.
    /// 
    /// 2. If TRANS = 'N' and m .LT. n:  find the minimum norm solution of
    /// an underdetermined system A * X = B.
    /// 
    /// 3. If TRANS = 'T' and m .GE. n:  find the minimum norm solution of
    /// an undetermined system A**T * X = B.
    /// 
    /// 4. If TRANS = 'T' and m .LT. n:  find the least squares solution of
    /// an overdetermined system, i.e., solve the least squares problem
    /// minimize || B - A**T * X ||.
    /// 
    /// Several right hand side vectors b and solution vectors x can be
    /// handled in a single call; they are stored as the columns of the
    /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
    /// matrix X.
    /// 
    ///</summary>
    public class DGELS
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DLABAD _dlabad; DLAMCH _dlamch; DLANGE _dlange; DGELQF _dgelqf; DGEQRF _dgeqrf; 
        DLASCL _dlascl;DLASET _dlaset; DORMLQ _dormlq; DORMQR _dormqr; DTRTRS _dtrtrs; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; double[] RWORK = new double[1]; 

        #endregion

        public DGELS(LSAME lsame, ILAENV ilaenv, DLABAD dlabad, DLAMCH dlamch, DLANGE dlange, DGELQF dgelqf, DGEQRF dgeqrf, DLASCL dlascl, DLASET dlaset, DORMLQ dormlq
                     , DORMQR dormqr, DTRTRS dtrtrs, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlabad = dlabad; this._dlamch = dlamch; this._dlange = dlange; 
            this._dgelqf = dgelqf;this._dgeqrf = dgeqrf; this._dlascl = dlascl; this._dlaset = dlaset; this._dormlq = dormlq; 
            this._dormqr = dormqr;this._dtrtrs = dtrtrs; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGELS()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLABAD dlabad = new DLABAD();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            XERBLA xerbla = new XERBLA();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DCOPY dcopy = new DCOPY();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANGE dlange = new DLANGE(dlassq, lsame);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGELQ2 dgelq2 = new DGELQ2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGELQF dgelqf = new DGELQF(dgelq2, dlarfb, dlarft, xerbla, ilaenv);
            DGEQR2 dgeqr2 = new DGEQR2(dlarf, dlarfg, xerbla);
            DGEQRF dgeqrf = new DGEQRF(dgeqr2, dlarfb, dlarft, xerbla, ilaenv);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLASET dlaset = new DLASET(lsame);
            DORML2 dorml2 = new DORML2(lsame, dlarf, xerbla);
            DORMLQ dormlq = new DORMLQ(lsame, ilaenv, dlarfb, dlarft, dorml2, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DTRTRS dtrtrs = new DTRTRS(lsame, dtrsm, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlabad = dlabad; this._dlamch = dlamch; this._dlange = dlange; 
            this._dgelqf = dgelqf;this._dgeqrf = dgeqrf; this._dlascl = dlascl; this._dlaset = dlaset; this._dormlq = dormlq; 
            this._dormqr = dormqr;this._dtrtrs = dtrtrs; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGELS solves overdetermined or underdetermined real linear systems
        /// involving an M-by-N matrix A, or its transpose, using a QR or LQ
        /// factorization of A.  It is assumed that A has full rank.
        /// 
        /// The following options are provided:
        /// 
        /// 1. If TRANS = 'N' and m .GE. n:  find the least squares solution of
        /// an overdetermined system, i.e., solve the least squares problem
        /// minimize || B - A*X ||.
        /// 
        /// 2. If TRANS = 'N' and m .LT. n:  find the minimum norm solution of
        /// an underdetermined system A * X = B.
        /// 
        /// 3. If TRANS = 'T' and m .GE. n:  find the minimum norm solution of
        /// an undetermined system A**T * X = B.
        /// 
        /// 4. If TRANS = 'T' and m .LT. n:  find the least squares solution of
        /// an overdetermined system, i.e., solve the least squares problem
        /// minimize || B - A**T * X ||.
        /// 
        /// Several right hand side vectors b and solution vectors x can be
        /// handled in a single call; they are stored as the columns of the
        /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
        /// matrix X.
        /// 
        ///</summary>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// = 'N': the linear system involves A;
        /// = 'T': the linear system involves A**T.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of
        /// columns of the matrices B and X. NRHS .GE.0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// if M .GE. N, A is overwritten by details of its QR
        /// factorization as returned by DGEQRF;
        /// if M .LT.  N, A is overwritten by details of its LQ
        /// factorization as returned by DGELQF.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the matrix B of right hand side vectors, stored
        /// columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
        /// if TRANS = 'T'.
        /// On exit, if INFO = 0, B is overwritten by the solution
        /// vectors, stored columnwise:
        /// if TRANS = 'N' and m .GE. n, rows 1 to n of B contain the least
        /// squares solution vectors; the residual sum of squares for the
        /// solution in each column is given by the sum of squares of
        /// elements N+1 to M in that column;
        /// if TRANS = 'N' and m .LT. n, rows 1 to N of B contain the
        /// minimum norm solution vectors;
        /// if TRANS = 'T' and m .GE. n, rows 1 to M of B contain the
        /// minimum norm solution vectors;
        /// if TRANS = 'T' and m .LT. n, rows 1 to M of B contain the
        /// least squares solution vectors; the residual sum of squares
        /// for the solution in each column is given by the sum of
        /// squares of elements M+1 to N in that column.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. MAX(1,M,N).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// LWORK .GE. max( 1, MN + max( MN, NRHS ) ).
        /// For optimal performance,
        /// LWORK .GE. max( 1, MN + max( MN, NRHS )*NB ).
        /// where MN = min(M,N) and NB is the optimum block size.
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
        /// .GT. 0:  if INFO =  i, the i-th diagonal element of the
        /// triangular factor of A is zero, so that A does not have
        /// full rank; the least squares solution could not be
        /// computed.
        ///</param>
        public void Run(string TRANS, int M, int N, int NRHS, ref double[] A, int offset_a, int LDA
                         , ref double[] B, int offset_b, int LDB, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool TPSD = false; int BROW = 0; int I = 0; int IASCL = 0; int IBSCL = 0; int J = 0; int MN = 0; 
            int NB = 0;int SCLLEN = 0; int WSIZE = 0; double ANRM = 0; double BIGNUM = 0; double BNRM = 0; double SMLNUM = 0; 
            int offset_rwork = 0;

            #endregion


            #region Implicit Variables
            
            int B_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            TRANS = TRANS.Substring(0, 1);  

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
            // *  DGELS solves overdetermined or underdetermined real linear systems
            // *  involving an M-by-N matrix A, or its transpose, using a QR or LQ
            // *  factorization of A.  It is assumed that A has full rank.
            // *
            // *  The following options are provided:
            // *
            // *  1. If TRANS = 'N' and m >= n:  find the least squares solution of
            // *     an overdetermined system, i.e., solve the least squares problem
            // *                  minimize || B - A*X ||.
            // *
            // *  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
            // *     an underdetermined system A * X = B.
            // *
            // *  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
            // *     an undetermined system A**T * X = B.
            // *
            // *  4. If TRANS = 'T' and m < n:  find the least squares solution of
            // *     an overdetermined system, i.e., solve the least squares problem
            // *                  minimize || B - A**T * X ||.
            // *
            // *  Several right hand side vectors b and solution vectors x can be
            // *  handled in a single call; they are stored as the columns of the
            // *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
            // *  matrix X.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          = 'N': the linear system involves A;
            // *          = 'T': the linear system involves A**T.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of
            // *          columns of the matrices B and X. NRHS >=0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit,
            // *            if M >= N, A is overwritten by details of its QR
            // *                       factorization as returned by DGEQRF;
            // *            if M <  N, A is overwritten by details of its LQ
            // *                       factorization as returned by DGELQF.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the matrix B of right hand side vectors, stored
            // *          columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
            // *          if TRANS = 'T'.
            // *          On exit, if INFO = 0, B is overwritten by the solution
            // *          vectors, stored columnwise:
            // *          if TRANS = 'N' and m >= n, rows 1 to n of B contain the least
            // *          squares solution vectors; the residual sum of squares for the
            // *          solution in each column is given by the sum of squares of
            // *          elements N+1 to M in that column;
            // *          if TRANS = 'N' and m < n, rows 1 to N of B contain the
            // *          minimum norm solution vectors;
            // *          if TRANS = 'T' and m >= n, rows 1 to M of B contain the
            // *          minimum norm solution vectors;
            // *          if TRANS = 'T' and m < n, rows 1 to M of B contain the
            // *          least squares solution vectors; the residual sum of squares
            // *          for the solution in each column is given by the sum of
            // *          squares of elements M+1 to N in that column.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= MAX(1,M,N).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          LWORK >= max( 1, MN + max( MN, NRHS ) ).
            // *          For optimal performance,
            // *          LWORK >= max( 1, MN + max( MN, NRHS )*NB ).
            // *          where MN = min(M,N) and NB is the optimum block size.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO =  i, the i-th diagonal element of the
            // *                triangular factor of A is zero, so that A does not have
            // *                full rank; the least squares solution could not be
            // *                computed.
            // *
            // *  =====================================================================
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
            //      INTRINSIC          DBLE, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            MN = Math.Min(M, N);
            LQUERY = (LWORK ==  - 1);
            if (!(this._lsame.Run(TRANS, "N") || this._lsame.Run(TRANS, "T")))
            {
                INFO =  - 1;
            }
            else
            {
                if (M < 0)
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
                        if (NRHS < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDA < Math.Max(1, M))
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LDB < Math.Max(1, Math.Max(M, N)))
                                {
                                    INFO =  - 8;
                                }
                                else
                                {
                                    if (LWORK < Math.Max(1, MN + Math.Max(MN, NRHS)) && !LQUERY)
                                    {
                                        INFO =  - 10;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // *
            // *     Figure out optimal block size
            // *
            if (INFO == 0 || INFO ==  - 10)
            {
                // *
                TPSD = true;
                if (this._lsame.Run(TRANS, "N")) TPSD = false;
                // *
                if (M >= N)
                {
                    NB = this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                    if (TPSD)
                    {
                        NB = Math.Max(NB, this._ilaenv.Run(1, "DORMQR", "LN", M, NRHS, N,  - 1));
                    }
                    else
                    {
                        NB = Math.Max(NB, this._ilaenv.Run(1, "DORMQR", "LT", M, NRHS, N,  - 1));
                    }
                }
                else
                {
                    NB = this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                    if (TPSD)
                    {
                        NB = Math.Max(NB, this._ilaenv.Run(1, "DORMLQ", "LT", N, NRHS, M,  - 1));
                    }
                    else
                    {
                        NB = Math.Max(NB, this._ilaenv.Run(1, "DORMLQ", "LN", N, NRHS, M,  - 1));
                    }
                }
                // *
                WSIZE = Math.Max(1, MN + Math.Max(MN, NRHS) * NB);
                WORK[1 + o_work] = Convert.ToDouble(WSIZE);
                // *
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DGELS ",  - INFO);
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
            if (Math.Min(M, Math.Min(N, NRHS)) == 0)
            {
                this._dlaset.Run("Full", Math.Max(M, N), NRHS, ZERO, ZERO, ref B, offset_b
                                 , LDB);
                return;
            }
            // *
            // *     Get machine parameters
            // *
            SMLNUM = this._dlamch.Run("S") / this._dlamch.Run("P");
            BIGNUM = ONE / SMLNUM;
            this._dlabad.Run(ref SMLNUM, ref BIGNUM);
            // *
            // *     Scale A, B if max element outside range [SMLNUM,BIGNUM]
            // *
            ANRM = this._dlange.Run("M", M, N, A, offset_a, LDA, ref RWORK, offset_rwork);
            IASCL = 0;
            if (ANRM > ZERO && ANRM < SMLNUM)
            {
                // *
                // *        Scale matrix norm up to SMLNUM
                // *
                this._dlascl.Run("G", 0, 0, ANRM, SMLNUM, M
                                 , N, ref A, offset_a, LDA, ref INFO);
                IASCL = 1;
            }
            else
            {
                if (ANRM > BIGNUM)
                {
                    // *
                    // *        Scale matrix norm down to BIGNUM
                    // *
                    this._dlascl.Run("G", 0, 0, ANRM, BIGNUM, M
                                     , N, ref A, offset_a, LDA, ref INFO);
                    IASCL = 2;
                }
                else
                {
                    if (ANRM == ZERO)
                    {
                        // *
                        // *        Matrix all zero. Return zero solution.
                        // *
                        this._dlaset.Run("F", Math.Max(M, N), NRHS, ZERO, ZERO, ref B, offset_b
                                         , LDB);
                        goto LABEL50;
                    }
                }
            }
            // *
            BROW = M;
            if (TPSD) BROW = N;
            BNRM = this._dlange.Run("M", BROW, NRHS, B, offset_b, LDB, ref RWORK, offset_rwork);
            IBSCL = 0;
            if (BNRM > ZERO && BNRM < SMLNUM)
            {
                // *
                // *        Scale matrix norm up to SMLNUM
                // *
                this._dlascl.Run("G", 0, 0, BNRM, SMLNUM, BROW
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
                IBSCL = 1;
            }
            else
            {
                if (BNRM > BIGNUM)
                {
                    // *
                    // *        Scale matrix norm down to BIGNUM
                    // *
                    this._dlascl.Run("G", 0, 0, BNRM, BIGNUM, BROW
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                    IBSCL = 2;
                }
            }
            // *
            if (M >= N)
            {
                // *
                // *        compute QR factorization of A
                // *
                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, 1 + o_work, ref WORK, MN + 1 + o_work
                                 , LWORK - MN, ref INFO);
                // *
                // *        workspace at least N, optimally N*NB
                // *
                if (!TPSD)
                {
                    // *
                    // *           Least-Squares Problem min || A * X - B ||
                    // *
                    // *           B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
                    // *
                    this._dormqr.Run("Left", "Transpose", M, NRHS, N, ref A, offset_a
                                     , LDA, WORK, 1 + o_work, ref B, offset_b, LDB, ref WORK, MN + 1 + o_work, LWORK - MN
                                     , ref INFO);
                    // *
                    // *           workspace at least NRHS, optimally NRHS*NB
                    // *
                    // *           B(1:N,1:NRHS) := inv(R) * B(1:N,1:NRHS)
                    // *
                    this._dtrtrs.Run("Upper", "No transpose", "Non-unit", N, NRHS, A, offset_a
                                     , LDA, ref B, offset_b, LDB, ref INFO);
                    // *
                    if (INFO > 0)
                    {
                        return;
                    }
                    // *
                    SCLLEN = N;
                    // *
                }
                else
                {
                    // *
                    // *           Overdetermined system of equations A' * X = B
                    // *
                    // *           B(1:N,1:NRHS) := inv(R') * B(1:N,1:NRHS)
                    // *
                    this._dtrtrs.Run("Upper", "Transpose", "Non-unit", N, NRHS, A, offset_a
                                     , LDA, ref B, offset_b, LDB, ref INFO);
                    // *
                    if (INFO > 0)
                    {
                        return;
                    }
                    // *
                    // *           B(N+1:M,1:NRHS) = ZERO
                    // *
                    for (J = 1; J <= NRHS; J++)
                    {
                        B_J = J * LDB + o_b;
                        for (I = N + 1; I <= M; I++)
                        {
                            B[I + B_J] = ZERO;
                        }
                    }
                    // *
                    // *           B(1:M,1:NRHS) := Q(1:N,:) * B(1:N,1:NRHS)
                    // *
                    this._dormqr.Run("Left", "No transpose", M, NRHS, N, ref A, offset_a
                                     , LDA, WORK, 1 + o_work, ref B, offset_b, LDB, ref WORK, MN + 1 + o_work, LWORK - MN
                                     , ref INFO);
                    // *
                    // *           workspace at least NRHS, optimally NRHS*NB
                    // *
                    SCLLEN = M;
                    // *
                }
                // *
            }
            else
            {
                // *
                // *        Compute LQ factorization of A
                // *
                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, 1 + o_work, ref WORK, MN + 1 + o_work
                                 , LWORK - MN, ref INFO);
                // *
                // *        workspace at least M, optimally M*NB.
                // *
                if (!TPSD)
                {
                    // *
                    // *           underdetermined system of equations A * X = B
                    // *
                    // *           B(1:M,1:NRHS) := inv(L) * B(1:M,1:NRHS)
                    // *
                    this._dtrtrs.Run("Lower", "No transpose", "Non-unit", M, NRHS, A, offset_a
                                     , LDA, ref B, offset_b, LDB, ref INFO);
                    // *
                    if (INFO > 0)
                    {
                        return;
                    }
                    // *
                    // *           B(M+1:N,1:NRHS) = 0
                    // *
                    for (J = 1; J <= NRHS; J++)
                    {
                        B_J = J * LDB + o_b;
                        for (I = M + 1; I <= N; I++)
                        {
                            B[I + B_J] = ZERO;
                        }
                    }
                    // *
                    // *           B(1:N,1:NRHS) := Q(1:N,:)' * B(1:M,1:NRHS)
                    // *
                    this._dormlq.Run("Left", "Transpose", N, NRHS, M, ref A, offset_a
                                     , LDA, WORK, 1 + o_work, ref B, offset_b, LDB, ref WORK, MN + 1 + o_work, LWORK - MN
                                     , ref INFO);
                    // *
                    // *           workspace at least NRHS, optimally NRHS*NB
                    // *
                    SCLLEN = N;
                    // *
                }
                else
                {
                    // *
                    // *           overdetermined system min || A' * X - B ||
                    // *
                    // *           B(1:N,1:NRHS) := Q * B(1:N,1:NRHS)
                    // *
                    this._dormlq.Run("Left", "No transpose", N, NRHS, M, ref A, offset_a
                                     , LDA, WORK, 1 + o_work, ref B, offset_b, LDB, ref WORK, MN + 1 + o_work, LWORK - MN
                                     , ref INFO);
                    // *
                    // *           workspace at least NRHS, optimally NRHS*NB
                    // *
                    // *           B(1:M,1:NRHS) := inv(L') * B(1:M,1:NRHS)
                    // *
                    this._dtrtrs.Run("Lower", "Transpose", "Non-unit", M, NRHS, A, offset_a
                                     , LDA, ref B, offset_b, LDB, ref INFO);
                    // *
                    if (INFO > 0)
                    {
                        return;
                    }
                    // *
                    SCLLEN = M;
                    // *
                }
                // *
            }
            // *
            // *     Undo scaling
            // *
            if (IASCL == 1)
            {
                this._dlascl.Run("G", 0, 0, ANRM, SMLNUM, SCLLEN
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
            }
            else
            {
                if (IASCL == 2)
                {
                    this._dlascl.Run("G", 0, 0, ANRM, BIGNUM, SCLLEN
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                }
            }
            if (IBSCL == 1)
            {
                this._dlascl.Run("G", 0, 0, SMLNUM, BNRM, SCLLEN
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
            }
            else
            {
                if (IBSCL == 2)
                {
                    this._dlascl.Run("G", 0, 0, BIGNUM, BNRM, SCLLEN
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                }
            }
            // *
        LABEL50:;
            WORK[1 + o_work] = Convert.ToDouble(WSIZE);
            // *
            return;
            // *
            // *     End of DGELS
            // *

            #endregion

        }
    }
}
