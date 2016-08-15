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
    /// DGELSD computes the minimum-norm solution to a real linear least
    /// squares problem:
    /// minimize 2-norm(| b - A*x |)
    /// using the singular value decomposition (SVD) of A. A is an M-by-N
    /// matrix which may be rank-deficient.
    /// 
    /// Several right hand side vectors b and solution vectors x can be
    /// handled in a single call; they are stored as the columns of the
    /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
    /// matrix X.
    /// 
    /// The problem is solved in three steps:
    /// (1) Reduce the coefficient matrix A to bidiagonal form with
    /// Householder transformations, reducing the original problem
    /// into a "bidiagonal least squares problem" (BLS)
    /// (2) Solve the BLS using a divide and conquer approach.
    /// (3) Apply back all the Householder tranformations to solve
    /// the original least squares problem.
    /// 
    /// The effective rank of A is determined by treating as zero those
    /// singular values which are less than RCOND times the largest singular
    /// value.
    /// 
    /// The divide and conquer algorithm makes very mild assumptions about
    /// floating point arithmetic. It will work on machines with a guard
    /// digit in add/subtract, or on those binary machines without guard
    /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
    /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    ///</summary>
    public class DGELSD
    {
    

        #region Dependencies
        
        DGEBRD _dgebrd; DGELQF _dgelqf; DGEQRF _dgeqrf; DLABAD _dlabad; DLACPY _dlacpy; DLALSD _dlalsd; DLASCL _dlascl; 
        DLASET _dlaset;DORMBR _dormbr; DORMLQ _dormlq; DORMQR _dormqr; XERBLA _xerbla; ILAENV _ilaenv; DLAMCH _dlamch; 
        DLANGE _dlange;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 

        #endregion

        public DGELSD(DGEBRD dgebrd, DGELQF dgelqf, DGEQRF dgeqrf, DLABAD dlabad, DLACPY dlacpy, DLALSD dlalsd, DLASCL dlascl, DLASET dlaset, DORMBR dormbr, DORMLQ dormlq
                      , DORMQR dormqr, XERBLA xerbla, ILAENV ilaenv, DLAMCH dlamch, DLANGE dlange)
        {
    

            #region Set Dependencies
            
            this._dgebrd = dgebrd; this._dgelqf = dgelqf; this._dgeqrf = dgeqrf; this._dlabad = dlabad; this._dlacpy = dlacpy; 
            this._dlalsd = dlalsd;this._dlascl = dlascl; this._dlaset = dlaset; this._dormbr = dormbr; this._dormlq = dormlq; 
            this._dormqr = dormqr;this._xerbla = xerbla; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlange = dlange; 

            #endregion

        }
    
        public DGELSD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            DLABAD dlabad = new DLABAD();
            IDAMAX idamax = new IDAMAX();
            DLASSQ dlassq = new DLASSQ();
            DROT drot = new DROT();
            DLASDT dlasdt = new DLASDT();
            DLAMRG dlamrg = new DLAMRG();
            DLASD5 dlasd5 = new DLASD5();
            DDOT ddot = new DDOT();
            DLAS2 dlas2 = new DLAS2();
            DLASQ5 dlasq5 = new DLASQ5();
            DLAZQ4 dlazq4 = new DLAZQ4();
            DSWAP dswap = new DSWAP();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEBD2 dgebd2 = new DGEBD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLABRD dlabrd = new DLABRD(dgemv, dlarfg, dscal);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEBRD dgebrd = new DGEBRD(dgebd2, dgemm, dlabrd, xerbla, ilaenv);
            DGELQ2 dgelq2 = new DGELQ2(dlarf, dlarfg, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGELQF dgelqf = new DGELQF(dgelq2, dlarfb, dlarft, xerbla, ilaenv);
            DGEQR2 dgeqr2 = new DGEQR2(dlarf, dlarfg, xerbla);
            DGEQRF dgeqrf = new DGEQRF(dgeqr2, dlarfb, dlarft, xerbla, ilaenv);
            DLACPY dlacpy = new DLACPY(lsame);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLALS0 dlals0 = new DLALS0(dcopy, dgemv, dlacpy, dlascl, drot, dscal, xerbla, dlamc3, dnrm2);
            DLALSA dlalsa = new DLALSA(dcopy, dgemm, dlals0, dlasdt, xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASD7 dlasd7 = new DLASD7(dcopy, dlamrg, drot, xerbla, dlamch, dlapy2);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);
            DLASET dlaset = new DLASET(lsame);
            DLASD8 dlasd8 = new DLASD8(dcopy, dlascl, dlasd4, dlaset, xerbla, ddot, dlamc3, dnrm2);
            DLASD6 dlasd6 = new DLASD6(dcopy, dlamrg, dlascl, dlasd7, dlasd8, xerbla);
            DLASQ6 dlasq6 = new DLASQ6(dlamch);
            DLAZQ3 dlazq3 = new DLAZQ3(dlasq5, dlasq6, dlazq4, dlamch);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            DLASQ2 dlasq2 = new DLASQ2(dlazq3, dlasrt, xerbla, dlamch, ilaenv);
            DLASQ1 dlasq1 = new DLASQ1(dcopy, dlas2, dlascl, dlasq2, dlasrt, xerbla, dlamch);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASV2 dlasv2 = new DLASV2(dlamch);
            DBDSQR dbdsqr = new DBDSQR(lsame, dlamch, dlartg, dlas2, dlasq1, dlasr, dlasv2, drot, dscal, dswap
                                       , xerbla);
            DLASDQ dlasdq = new DLASDQ(dbdsqr, dlartg, dlasr, dswap, xerbla, lsame);
            DLASDA dlasda = new DLASDA(dcopy, dlasd6, dlasdq, dlasdt, dlaset, xerbla);
            DLALSD dlalsd = new DLALSD(idamax, dlamch, dlanst, dcopy, dgemm, dlacpy, dlalsa, dlartg, dlascl, dlasda
                                       , dlasdq, dlaset, dlasrt, drot, xerbla);
            DORML2 dorml2 = new DORML2(lsame, dlarf, xerbla);
            DORMLQ dormlq = new DORMLQ(lsame, ilaenv, dlarfb, dlarft, dorml2, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DORMBR dormbr = new DORMBR(lsame, ilaenv, dormlq, dormqr, xerbla);
            DLANGE dlange = new DLANGE(dlassq, lsame);

            #endregion


            #region Set Dependencies
            
            this._dgebrd = dgebrd; this._dgelqf = dgelqf; this._dgeqrf = dgeqrf; this._dlabad = dlabad; this._dlacpy = dlacpy; 
            this._dlalsd = dlalsd;this._dlascl = dlascl; this._dlaset = dlaset; this._dormbr = dormbr; this._dormlq = dormlq; 
            this._dormqr = dormqr;this._xerbla = xerbla; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlange = dlange; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGELSD computes the minimum-norm solution to a real linear least
        /// squares problem:
        /// minimize 2-norm(| b - A*x |)
        /// using the singular value decomposition (SVD) of A. A is an M-by-N
        /// matrix which may be rank-deficient.
        /// 
        /// Several right hand side vectors b and solution vectors x can be
        /// handled in a single call; they are stored as the columns of the
        /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
        /// matrix X.
        /// 
        /// The problem is solved in three steps:
        /// (1) Reduce the coefficient matrix A to bidiagonal form with
        /// Householder transformations, reducing the original problem
        /// into a "bidiagonal least squares problem" (BLS)
        /// (2) Solve the BLS using a divide and conquer approach.
        /// (3) Apply back all the Householder tranformations to solve
        /// the original least squares problem.
        /// 
        /// The effective rank of A is determined by treating as zero those
        /// singular values which are less than RCOND times the largest singular
        /// value.
        /// 
        /// The divide and conquer algorithm makes very mild assumptions about
        /// floating point arithmetic. It will work on machines with a guard
        /// digit in add/subtract, or on those binary machines without guard
        /// digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
        /// Cray-2. It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of A. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of A. N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X. NRHS .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, A has been destroyed.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the M-by-NRHS right hand side matrix B.
        /// On exit, B is overwritten by the N-by-NRHS solution
        /// matrix X.  If m .GE. n and RANK = n, the residual
        /// sum-of-squares for the solution in the i-th column is given
        /// by the sum of squares of elements n+1:m in that column.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,max(M,N)).
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The singular values of A in decreasing order.
        /// The condition number of A in the 2-norm = S(1)/S(min(m,n)).
        ///</param>
        /// <param name="RCOND">
        /// (input) DOUBLE PRECISION
        /// RCOND is used to determine the effective rank of A.
        /// Singular values S(i) .LE. RCOND*S(1) are treated as zero.
        /// If RCOND .LT. 0, machine precision is used instead.
        ///</param>
        /// <param name="RANK">
        /// (output) INTEGER
        /// The effective rank of A, i.e., the number of singular values
        /// which are greater than RCOND*S(1).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK must be at least 1.
        /// The exact minimum amount of workspace needed depends on M,
        /// N and NRHS. As long as LWORK is at least
        /// 12*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2,
        /// if M is greater than or equal to N or
        /// 12*M + 2*M*SMLSIZ + 8*M*NLVL + M*NRHS + (SMLSIZ+1)**2,
        /// if M is less than N, the code will execute correctly.
        /// SMLSIZ is returned by ILAENV and is equal to the maximum
        /// size of the subproblems at the bottom of the computation
        /// tree (usually about 25), and
        /// NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
        /// For good performance, LWORK should generally be larger.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (MAX(1,LIWORK))
        /// LIWORK .GE. 3 * MINMN * NLVL + 11 * MINMN,
        /// where MINMN = MIN( M,N ).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  the algorithm for computing the SVD failed to converge;
        /// if INFO = i, i off-diagonal elements of an intermediate
        /// bidiagonal form did not converge to zero.
        ///</param>
        public void Run(int M, int N, int NRHS, ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b
                         , int LDB, ref double[] S, int offset_s, double RCOND, ref int RANK, ref double[] WORK, int offset_work, int LWORK
                         , ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int IASCL = 0; int IBSCL = 0; int IE = 0; int IL = 0; int ITAU = 0; int ITAUP = 0; int ITAUQ = 0; 
            int LDWORK = 0;int MAXMN = 0; int MAXWRK = 0; int MINMN = 0; int MINWRK = 0; int MM = 0; int MNTHR = 0; int NLVL = 0; 
            int NWORK = 0;int SMLSIZ = 0; int WLALSD = 0; double ANRM = 0; double BIGNUM = 0; double BNRM = 0; double EPS = 0; 
            double SFMIN = 0;double SMLNUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_s = -1 + offset_s; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork; 

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
            // *  DGELSD computes the minimum-norm solution to a real linear least
            // *  squares problem:
            // *      minimize 2-norm(| b - A*x |)
            // *  using the singular value decomposition (SVD) of A. A is an M-by-N
            // *  matrix which may be rank-deficient.
            // *
            // *  Several right hand side vectors b and solution vectors x can be
            // *  handled in a single call; they are stored as the columns of the
            // *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
            // *  matrix X.
            // *
            // *  The problem is solved in three steps:
            // *  (1) Reduce the coefficient matrix A to bidiagonal form with
            // *      Householder transformations, reducing the original problem
            // *      into a "bidiagonal least squares problem" (BLS)
            // *  (2) Solve the BLS using a divide and conquer approach.
            // *  (3) Apply back all the Householder tranformations to solve
            // *      the original least squares problem.
            // *
            // *  The effective rank of A is determined by treating as zero those
            // *  singular values which are less than RCOND times the largest singular
            // *  value.
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
            // *  M       (input) INTEGER
            // *          The number of rows of A. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of A. N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrices B and X. NRHS >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, A has been destroyed.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the M-by-NRHS right hand side matrix B.
            // *          On exit, B is overwritten by the N-by-NRHS solution
            // *          matrix X.  If m >= n and RANK = n, the residual
            // *          sum-of-squares for the solution in the i-th column is given
            // *          by the sum of squares of elements n+1:m in that column.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,max(M,N)).
            // *
            // *  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The singular values of A in decreasing order.
            // *          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            // *
            // *  RCOND   (input) DOUBLE PRECISION
            // *          RCOND is used to determine the effective rank of A.
            // *          Singular values S(i) <= RCOND*S(1) are treated as zero.
            // *          If RCOND < 0, machine precision is used instead.
            // *
            // *  RANK    (output) INTEGER
            // *          The effective rank of A, i.e., the number of singular values
            // *          which are greater than RCOND*S(1).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK must be at least 1.
            // *          The exact minimum amount of workspace needed depends on M,
            // *          N and NRHS. As long as LWORK is at least
            // *              12*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2,
            // *          if M is greater than or equal to N or
            // *              12*M + 2*M*SMLSIZ + 8*M*NLVL + M*NRHS + (SMLSIZ+1)**2,
            // *          if M is less than N, the code will execute correctly.
            // *          SMLSIZ is returned by ILAENV and is equal to the maximum
            // *          size of the subproblems at the bottom of the computation
            // *          tree (usually about 25), and
            // *             NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
            // *          For good performance, LWORK should generally be larger.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  IWORK   (workspace) INTEGER array, dimension (MAX(1,LIWORK))
            // *          LIWORK >= 3 * MINMN * NLVL + 11 * MINMN,
            // *          where MINMN = MIN( M,N ).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  the algorithm for computing the SVD failed to converge;
            // *                if INFO = i, i off-diagonal elements of an intermediate
            // *                bidiagonal form did not converge to zero.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Ren-Cang Li, Computer Science Division, University of
            // *       California at Berkeley, USA
            // *     Osni Marques, LBNL/NERSC, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          DBLE, INT, LOG, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            MINMN = Math.Min(M, N);
            MAXMN = Math.Max(M, N);
            MNTHR = this._ilaenv.Run(6, "DGELSD", " ", M, N, NRHS,  - 1);
            LQUERY = (LWORK ==  - 1);
            if (M < 0)
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
                    if (NRHS < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, M))
                        {
                            INFO =  - 5;
                        }
                        else
                        {
                            if (LDB < Math.Max(1, MAXMN))
                            {
                                INFO =  - 7;
                            }
                        }
                    }
                }
            }
            // *
            SMLSIZ = this._ilaenv.Run(9, "DGELSD", " ", 0, 0, 0, 0);
            // *
            // *     Compute workspace.
            // *     (Note: Comments in the code beginning "Workspace:" describe the
            // *     minimal amount of workspace needed at that point in the code,
            // *     as well as the preferred amount for good performance.
            // *     NB refers to the optimal block size for the immediately
            // *     following subroutine, as returned by ILAENV.)
            // *
            MINWRK = 1;
            MINMN = Math.Max(1, MINMN);
            NLVL = Math.Max(Convert.ToInt32(Math.Truncate(Math.Log(Convert.ToDouble(MINMN) / Convert.ToDouble(SMLSIZ + 1)) / Math.Log(TWO))) + 1, 0);
            // *
            if (INFO == 0)
            {
                MAXWRK = 0;
                MM = M;
                if (M >= N && M >= MNTHR)
                {
                    // *
                    // *           Path 1a - overdetermined, with many more rows than columns.
                    // *
                    MM = N;
                    MAXWRK = Math.Max(MAXWRK, N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1));
                    MAXWRK = Math.Max(MAXWRK, N + NRHS * this._ilaenv.Run(1, "DORMQR", "LT", M, NRHS, N,  - 1));
                }
                if (M >= N)
                {
                    // *
                    // *           Path 1 - overdetermined or exactly determined.
                    // *
                    MAXWRK = Math.Max(MAXWRK, 3 * N + (MM + N) * this._ilaenv.Run(1, "DGEBRD", " ", MM, N,  - 1,  - 1));
                    MAXWRK = Math.Max(MAXWRK, 3 * N + NRHS * this._ilaenv.Run(1, "DORMBR", "QLT", MM, NRHS, N,  - 1));
                    MAXWRK = Math.Max(MAXWRK, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORMBR", "PLN", N, NRHS, N,  - 1));
                    WLALSD = 9 * N + 2 * N * SMLSIZ + 8 * N * NLVL + N * NRHS + (int)Math.Pow(SMLSIZ + 1, 2);
                    MAXWRK = Math.Max(MAXWRK, 3 * N + WLALSD);
                    MINWRK = Math.Max(3 * N + MM, Math.Max(3 * N + NRHS, 3 * N + WLALSD));
                }
                if (N > M)
                {
                    WLALSD = 9 * M + 2 * M * SMLSIZ + 8 * M * NLVL + M * NRHS + (int)Math.Pow(SMLSIZ + 1, 2);
                    if (N >= MNTHR)
                    {
                        // *
                        // *              Path 2a - underdetermined, with many more columns
                        // *              than rows.
                        // *
                        MAXWRK = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                        MAXWRK = Math.Max(MAXWRK, M * M + 4 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                        MAXWRK = Math.Max(MAXWRK, M * M + 4 * M + NRHS * this._ilaenv.Run(1, "DORMBR", "QLT", M, NRHS, M,  - 1));
                        MAXWRK = Math.Max(MAXWRK, M * M + 4 * M + (M - 1) * this._ilaenv.Run(1, "DORMBR", "PLN", M, NRHS, M,  - 1));
                        if (NRHS > 1)
                        {
                            MAXWRK = Math.Max(MAXWRK, M * M + M + M * NRHS);
                        }
                        else
                        {
                            MAXWRK = Math.Max(MAXWRK, M * M + 2 * M);
                        }
                        MAXWRK = Math.Max(MAXWRK, M + NRHS * this._ilaenv.Run(1, "DORMLQ", "LT", N, NRHS, M,  - 1));
                        MAXWRK = Math.Max(MAXWRK, M * M + 4 * M + WLALSD);
                    }
                    else
                    {
                        // *
                        // *              Path 2 - remaining underdetermined cases.
                        // *
                        MAXWRK = 3 * M + (N + M) * this._ilaenv.Run(1, "DGEBRD", " ", M, N,  - 1,  - 1);
                        MAXWRK = Math.Max(MAXWRK, 3 * M + NRHS * this._ilaenv.Run(1, "DORMBR", "QLT", M, NRHS, N,  - 1));
                        MAXWRK = Math.Max(MAXWRK, 3 * M + M * this._ilaenv.Run(1, "DORMBR", "PLN", N, NRHS, M,  - 1));
                        MAXWRK = Math.Max(MAXWRK, 3 * M + WLALSD);
                    }
                    MINWRK = Math.Max(3 * M + NRHS, Math.Max(3 * M + M, 3 * M + WLALSD));
                }
                MINWRK = Math.Min(MINWRK, MAXWRK);
                WORK[1 + o_work] = MAXWRK;
                if (LWORK < MINWRK && !LQUERY)
                {
                    INFO =  - 12;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DGELSD",  - INFO);
                return;
            }
            else
            {
                if (LQUERY)
                {
                    goto LABEL10;
                }
            }
            // *
            // *     Quick return if possible.
            // *
            if (M == 0 || N == 0)
            {
                RANK = 0;
                return;
            }
            // *
            // *     Get machine parameters.
            // *
            EPS = this._dlamch.Run("P");
            SFMIN = this._dlamch.Run("S");
            SMLNUM = SFMIN / EPS;
            BIGNUM = ONE / SMLNUM;
            this._dlabad.Run(ref SMLNUM, ref BIGNUM);
            // *
            // *     Scale A if max entry outside range [SMLNUM,BIGNUM].
            // *
            ANRM = this._dlange.Run("M", M, N, A, offset_a, LDA, ref WORK, offset_work);
            IASCL = 0;
            if (ANRM > ZERO && ANRM < SMLNUM)
            {
                // *
                // *        Scale matrix norm up to SMLNUM.
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
                    // *        Scale matrix norm down to BIGNUM.
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
                        this._dlaset.Run("F", MINMN, 1, ZERO, ZERO, ref S, offset_s
                                         , 1);
                        RANK = 0;
                        goto LABEL10;
                    }
                }
            }
            // *
            // *     Scale B if max entry outside range [SMLNUM,BIGNUM].
            // *
            BNRM = this._dlange.Run("M", M, NRHS, B, offset_b, LDB, ref WORK, offset_work);
            IBSCL = 0;
            if (BNRM > ZERO && BNRM < SMLNUM)
            {
                // *
                // *        Scale matrix norm up to SMLNUM.
                // *
                this._dlascl.Run("G", 0, 0, BNRM, SMLNUM, M
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
                IBSCL = 1;
            }
            else
            {
                if (BNRM > BIGNUM)
                {
                    // *
                    // *        Scale matrix norm down to BIGNUM.
                    // *
                    this._dlascl.Run("G", 0, 0, BNRM, BIGNUM, M
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                    IBSCL = 2;
                }
            }
            // *
            // *     If M < N make sure certain entries of B are zero.
            // *
            if (M < N)
            {
                this._dlaset.Run("F", N - M, NRHS, ZERO, ZERO, ref B, M + 1+1 * LDB + o_b
                                 , LDB);
            }
            // *
            // *     Overdetermined case.
            // *
            if (M >= N)
            {
                // *
                // *        Path 1 - overdetermined or exactly determined.
                // *
                MM = M;
                if (M >= MNTHR)
                {
                    // *
                    // *           Path 1a - overdetermined, with many more rows than columns.
                    // *
                    MM = N;
                    ITAU = 1;
                    NWORK = ITAU + N;
                    // *
                    // *           Compute A=Q*R.
                    // *           (Workspace: need 2*N, prefer N+N*NB)
                    // *
                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    // *
                    // *           Multiply B by transpose(Q).
                    // *           (Workspace: need N+NRHS, prefer N+NRHS*NB)
                    // *
                    this._dormqr.Run("L", "T", M, NRHS, N, ref A, offset_a
                                     , LDA, WORK, ITAU + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work, LWORK - NWORK + 1
                                     , ref INFO);
                    // *
                    // *           Zero out below R.
                    // *
                    if (N > 1)
                    {
                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                         , LDA);
                    }
                }
                // *
                IE = 1;
                ITAUQ = IE + N;
                ITAUP = ITAUQ + N;
                NWORK = ITAUP + N;
                // *
                // *        Bidiagonalize R in A.
                // *        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
                // *
                this._dgebrd.Run(MM, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, NWORK + o_work, LWORK - NWORK + 1, ref INFO);
                // *
                // *        Multiply B by transpose of left bidiagonalizing vectors of R.
                // *        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
                // *
                this._dormbr.Run("Q", "L", "T", MM, NRHS, N
                                 , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                 , LWORK - NWORK + 1, ref INFO);
                // *
                // *        Solve the bidiagonal least squares problem.
                // *
                this._dlalsd.Run("U", SMLSIZ, N, NRHS, ref S, offset_s, ref WORK, IE + o_work
                                 , ref B, offset_b, LDB, RCOND, ref RANK, ref WORK, NWORK + o_work, ref IWORK, offset_iwork
                                 , ref INFO);
                if (INFO != 0)
                {
                    goto LABEL10;
                }
                // *
                // *        Multiply B by right bidiagonalizing vectors of R.
                // *
                this._dormbr.Run("P", "L", "N", N, NRHS, N
                                 , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                 , LWORK - NWORK + 1, ref INFO);
                // *
            }
            else
            {
                if (N >= MNTHR && LWORK >= 4 * M + M * M + Math.Max(M, Math.Max(2 * M - 4, Math.Max(NRHS, Math.Max(N - 3 * M, WLALSD)))))
                {
                    // *
                    // *        Path 2a - underdetermined, with many more columns than rows
                    // *        and sufficient workspace for an efficient algorithm.
                    // *
                    LDWORK = M;
                    if (LWORK >= Math.Max(4 * M + M * LDA + Math.Max(M, Math.Max(2 * M - 4, Math.Max(NRHS, N - 3 * M))), Math.Max(M * LDA + M + M * NRHS, 4 * M + M * LDA + WLALSD))) LDWORK = LDA;
                    ITAU = 1;
                    NWORK = M + 1;
                    // *
                    // *        Compute A=L*Q.
                    // *        (Workspace: need 2*M, prefer M+M*NB)
                    // *
                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    IL = NWORK;
                    // *
                    // *        Copy L to WORK(IL), zeroing out above its diagonal.
                    // *
                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IL + o_work
                                     , LDWORK);
                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IL + LDWORK + o_work
                                     , LDWORK);
                    IE = IL + LDWORK * M;
                    ITAUQ = IE + M;
                    ITAUP = ITAUQ + M;
                    NWORK = ITAUP + M;
                    // *
                    // *        Bidiagonalize L in WORK(IL).
                    // *        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
                    // *
                    this._dgebrd.Run(M, M, ref WORK, IL + o_work, LDWORK, ref S, offset_s, ref WORK, IE + o_work
                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, NWORK + o_work, LWORK - NWORK + 1, ref INFO);
                    // *
                    // *        Multiply B by transpose of left bidiagonalizing vectors of L.
                    // *        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
                    // *
                    this._dormbr.Run("Q", "L", "T", M, NRHS, M
                                     , ref WORK, IL + o_work, LDWORK, WORK, ITAUQ + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    // *
                    // *        Solve the bidiagonal least squares problem.
                    // *
                    this._dlalsd.Run("U", SMLSIZ, M, NRHS, ref S, offset_s, ref WORK, IE + o_work
                                     , ref B, offset_b, LDB, RCOND, ref RANK, ref WORK, NWORK + o_work, ref IWORK, offset_iwork
                                     , ref INFO);
                    if (INFO != 0)
                    {
                        goto LABEL10;
                    }
                    // *
                    // *        Multiply B by right bidiagonalizing vectors of L.
                    // *
                    this._dormbr.Run("P", "L", "N", M, NRHS, M
                                     , ref WORK, IL + o_work, LDWORK, WORK, ITAUP + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    // *
                    // *        Zero out below first M rows of B.
                    // *
                    this._dlaset.Run("F", N - M, NRHS, ZERO, ZERO, ref B, M + 1+1 * LDB + o_b
                                     , LDB);
                    NWORK = ITAU + M;
                    // *
                    // *        Multiply transpose(Q) by B.
                    // *        (Workspace: need M+NRHS, prefer M+NRHS*NB)
                    // *
                    this._dormlq.Run("L", "T", N, NRHS, M, ref A, offset_a
                                     , LDA, WORK, ITAU + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work, LWORK - NWORK + 1
                                     , ref INFO);
                    // *
                }
                else
                {
                    // *
                    // *        Path 2 - remaining underdetermined cases.
                    // *
                    IE = 1;
                    ITAUQ = IE + M;
                    ITAUP = ITAUQ + M;
                    NWORK = ITAUP + M;
                    // *
                    // *        Bidiagonalize A.
                    // *        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
                    // *
                    this._dgebrd.Run(M, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, NWORK + o_work, LWORK - NWORK + 1, ref INFO);
                    // *
                    // *        Multiply B by transpose of left bidiagonalizing vectors.
                    // *        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
                    // *
                    this._dormbr.Run("Q", "L", "T", M, NRHS, N
                                     , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    // *
                    // *        Solve the bidiagonal least squares problem.
                    // *
                    this._dlalsd.Run("L", SMLSIZ, M, NRHS, ref S, offset_s, ref WORK, IE + o_work
                                     , ref B, offset_b, LDB, RCOND, ref RANK, ref WORK, NWORK + o_work, ref IWORK, offset_iwork
                                     , ref INFO);
                    if (INFO != 0)
                    {
                        goto LABEL10;
                    }
                    // *
                    // *        Multiply B by right bidiagonalizing vectors of A.
                    // *
                    this._dormbr.Run("P", "L", "N", N, NRHS, M
                                     , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref B, offset_b, LDB, ref WORK, NWORK + o_work
                                     , LWORK - NWORK + 1, ref INFO);
                    // *
                }
            }
            // *
            // *     Undo scaling.
            // *
            if (IASCL == 1)
            {
                this._dlascl.Run("G", 0, 0, ANRM, SMLNUM, N
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
                this._dlascl.Run("G", 0, 0, SMLNUM, ANRM, MINMN
                                 , 1, ref S, offset_s, MINMN, ref INFO);
            }
            else
            {
                if (IASCL == 2)
                {
                    this._dlascl.Run("G", 0, 0, ANRM, BIGNUM, N
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                    this._dlascl.Run("G", 0, 0, BIGNUM, ANRM, MINMN
                                     , 1, ref S, offset_s, MINMN, ref INFO);
                }
            }
            if (IBSCL == 1)
            {
                this._dlascl.Run("G", 0, 0, SMLNUM, BNRM, N
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
            }
            else
            {
                if (IBSCL == 2)
                {
                    this._dlascl.Run("G", 0, 0, BIGNUM, BNRM, N
                                     , NRHS, ref B, offset_b, LDB, ref INFO);
                }
            }
            // *
        LABEL10:;
            WORK[1 + o_work] = MAXWRK;
            return;
            // *
            // *     End of DGELSD
            // *

            #endregion

        }
    }
}
