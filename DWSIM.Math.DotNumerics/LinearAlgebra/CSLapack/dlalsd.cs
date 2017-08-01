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
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLALSD uses the singular value decomposition of A to solve the least
    /// squares problem of finding X to minimize the Euclidean norm of each
    /// column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
    /// are N-by-NRHS. The solution X overwrites B.
    /// 
    /// The singular values of A smaller than RCOND times the largest
    /// singular value are treated as zero in solving the least squares
    /// problem; in this case a minimum norm solution is returned.
    /// The actual singular values are returned in D in ascending order.
    /// 
    /// This code makes very mild assumptions about floating point
    /// arithmetic. It will work on machines with a guard digit in
    /// add/subtract, or on those binary machines without guard digits
    /// which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
    /// It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    ///</summary>
    public class DLALSD
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DLAMCH _dlamch; DLANST _dlanst; DCOPY _dcopy; DGEMM _dgemm; DLACPY _dlacpy; DLALSA _dlalsa; 
        DLARTG _dlartg;DLASCL _dlascl; DLASDA _dlasda; DLASDQ _dlasdq; DLASET _dlaset; DLASRT _dlasrt; DROT _drot; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 

        #endregion

        public DLALSD(IDAMAX idamax, DLAMCH dlamch, DLANST dlanst, DCOPY dcopy, DGEMM dgemm, DLACPY dlacpy, DLALSA dlalsa, DLARTG dlartg, DLASCL dlascl, DLASDA dlasda
                      , DLASDQ dlasdq, DLASET dlaset, DLASRT dlasrt, DROT drot, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dlanst = dlanst; this._dcopy = dcopy; this._dgemm = dgemm; 
            this._dlacpy = dlacpy;this._dlalsa = dlalsa; this._dlartg = dlartg; this._dlascl = dlascl; this._dlasda = dlasda; 
            this._dlasdq = dlasdq;this._dlaset = dlaset; this._dlasrt = dlasrt; this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLALSD()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DNRM2 dnrm2 = new DNRM2();
            DLASDT dlasdt = new DLASDT();
            DLAMRG dlamrg = new DLAMRG();
            DLAPY2 dlapy2 = new DLAPY2();
            DLASD5 dlasd5 = new DLASD5();
            DDOT ddot = new DDOT();
            DLAS2 dlas2 = new DLAS2();
            DLASQ5 dlasq5 = new DLASQ5();
            DLAZQ4 dlazq4 = new DLAZQ4();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DSWAP dswap = new DSWAP();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
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
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLASQ2 dlasq2 = new DLASQ2(dlazq3, dlasrt, xerbla, dlamch, ilaenv);
            DLASQ1 dlasq1 = new DLASQ1(dcopy, dlas2, dlascl, dlasq2, dlasrt, xerbla, dlamch);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASV2 dlasv2 = new DLASV2(dlamch);
            DBDSQR dbdsqr = new DBDSQR(lsame, dlamch, dlartg, dlas2, dlasq1, dlasr, dlasv2, drot, dscal, dswap
                                       , xerbla);
            DLASDQ dlasdq = new DLASDQ(dbdsqr, dlartg, dlasr, dswap, xerbla, lsame);
            DLASDA dlasda = new DLASDA(dcopy, dlasd6, dlasdq, dlasdt, dlaset, xerbla);

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dlanst = dlanst; this._dcopy = dcopy; this._dgemm = dgemm; 
            this._dlacpy = dlacpy;this._dlalsa = dlalsa; this._dlartg = dlartg; this._dlascl = dlascl; this._dlasda = dlasda; 
            this._dlasdq = dlasdq;this._dlaset = dlaset; this._dlasrt = dlasrt; this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLALSD uses the singular value decomposition of A to solve the least
        /// squares problem of finding X to minimize the Euclidean norm of each
        /// column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
        /// are N-by-NRHS. The solution X overwrites B.
        /// 
        /// The singular values of A smaller than RCOND times the largest
        /// singular value are treated as zero in solving the least squares
        /// problem; in this case a minimum norm solution is returned.
        /// The actual singular values are returned in D in ascending order.
        /// 
        /// This code makes very mild assumptions about floating point
        /// arithmetic. It will work on machines with a guard digit in
        /// add/subtract, or on those binary machines without guard digits
        /// which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
        /// It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U': D and E define an upper bidiagonal matrix.
        /// = 'L': D and E define a  lower bidiagonal matrix.
        ///</param>
        /// <param name="SMLSIZ">
        /// (input) INTEGER
        /// The maximum size of the subproblems at the bottom of the
        /// computation tree.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the  bidiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of columns of B. NRHS must be at least 1.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry D contains the main diagonal of the bidiagonal
        /// matrix. On exit, if INFO = 0, D contains its singular values.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// Contains the super-diagonal entries of the bidiagonal matrix.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On input, B contains the right hand sides of the least
        /// squares problem. On output, B contains the solution X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of B in the calling subprogram.
        /// LDB must be at least max(1,N).
        ///</param>
        /// <param name="RCOND">
        /// (input) DOUBLE PRECISION
        /// The singular values of A less than or equal to RCOND times
        /// the largest singular value are treated as zero in solving
        /// the least squares problem. If RCOND is negative,
        /// machine precision is used instead.
        /// For example, if diag(S)*X=B were the least squares problem,
        /// where diag(S) is a diagonal matrix of singular values, the
        /// solution would be X(i) = B(i) / S(i) if S(i) is greater than
        /// RCOND*max(S), and X(i) = 0 if S(i) is less than or equal to
        /// RCOND*max(S).
        ///</param>
        /// <param name="RANK">
        /// (output) INTEGER
        /// The number of singular values of A greater than RCOND times
        /// the largest singular value.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension at least
        /// (9*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2),
        /// where NLVL = max(0, INT(log_2 (N/(SMLSIZ+1))) + 1).
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension at least
        /// (3*N*NLVL + 11*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  The algorithm failed to compute an singular value while
        /// working on the submatrix lying in rows and columns
        /// INFO/(N+1) through MOD(INFO,N+1).
        ///</param>
        public void Run(string UPLO, int SMLSIZ, int N, int NRHS, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] B, int offset_b, int LDB, double RCOND, ref int RANK, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork
                         , ref int INFO)
        {

            #region Variables
            
            int BX = 0; int BXST = 0; int C = 0; int DIFL = 0; int DIFR = 0; int GIVCOL = 0; int GIVNUM = 0; int GIVPTR = 0; 
            int I = 0;int ICMPQ1 = 0; int ICMPQ2 = 0; int IWK = 0; int J = 0; int K = 0; int NLVL = 0; int NM1 = 0; int NSIZE = 0; 
            int NSUB = 0;int NWORK = 0; int PERM = 0; int POLES = 0; int S = 0; int SIZEI = 0; int SMLSZP = 0; int SQRE = 0; 
            int ST = 0;int ST1 = 0; int U = 0; int VT = 0; int Z = 0; double CS = 0; double EPS = 0; double ORGNRM = 0; 
            double R = 0;double RCND = 0; double SN = 0; double TOL = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_b = -1 - LDB + offset_b;  int o_work = -1 + offset_work; 
             int o_iwork = -1 + offset_iwork;

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
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
            // *  DLALSD uses the singular value decomposition of A to solve the least
            // *  squares problem of finding X to minimize the Euclidean norm of each
            // *  column of A*X-B, where A is N-by-N upper bidiagonal, and X and B
            // *  are N-by-NRHS. The solution X overwrites B.
            // *
            // *  The singular values of A smaller than RCOND times the largest
            // *  singular value are treated as zero in solving the least squares
            // *  problem; in this case a minimum norm solution is returned.
            // *  The actual singular values are returned in D in ascending order.
            // *
            // *  This code makes very mild assumptions about floating point
            // *  arithmetic. It will work on machines with a guard digit in
            // *  add/subtract, or on those binary machines without guard digits
            // *  which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
            // *  It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO   (input) CHARACTER*1
            // *         = 'U': D and E define an upper bidiagonal matrix.
            // *         = 'L': D and E define a  lower bidiagonal matrix.
            // *
            // *  SMLSIZ (input) INTEGER
            // *         The maximum size of the subproblems at the bottom of the
            // *         computation tree.
            // *
            // *  N      (input) INTEGER
            // *         The dimension of the  bidiagonal matrix.  N >= 0.
            // *
            // *  NRHS   (input) INTEGER
            // *         The number of columns of B. NRHS must be at least 1.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry D contains the main diagonal of the bidiagonal
            // *         matrix. On exit, if INFO = 0, D contains its singular values.
            // *
            // *  E      (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *         Contains the super-diagonal entries of the bidiagonal matrix.
            // *         On exit, E has been destroyed.
            // *
            // *  B      (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *         On input, B contains the right hand sides of the least
            // *         squares problem. On output, B contains the solution X.
            // *
            // *  LDB    (input) INTEGER
            // *         The leading dimension of B in the calling subprogram.
            // *         LDB must be at least max(1,N).
            // *
            // *  RCOND  (input) DOUBLE PRECISION
            // *         The singular values of A less than or equal to RCOND times
            // *         the largest singular value are treated as zero in solving
            // *         the least squares problem. If RCOND is negative,
            // *         machine precision is used instead.
            // *         For example, if diag(S)*X=B were the least squares problem,
            // *         where diag(S) is a diagonal matrix of singular values, the
            // *         solution would be X(i) = B(i) / S(i) if S(i) is greater than
            // *         RCOND*max(S), and X(i) = 0 if S(i) is less than or equal to
            // *         RCOND*max(S).
            // *
            // *  RANK   (output) INTEGER
            // *         The number of singular values of A greater than RCOND times
            // *         the largest singular value.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension at least
            // *         (9*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2),
            // *         where NLVL = max(0, INT(log_2 (N/(SMLSIZ+1))) + 1).
            // *
            // *  IWORK  (workspace) INTEGER array, dimension at least
            // *         (3*N*NLVL + 11*N)
            // *
            // *  INFO   (output) INTEGER
            // *         = 0:  successful exit.
            // *         < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *         > 0:  The algorithm failed to compute an singular value while
            // *               working on the submatrix lying in rows and columns
            // *               INFO/(N+1) through MOD(INFO,N+1).
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
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, DBLE, INT, LOG, SIGN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (N < 0)
            {
                INFO =  - 3;
            }
            else
            {
                if (NRHS < 1)
                {
                    INFO =  - 4;
                }
                else
                {
                    if ((LDB < 1) || (LDB < N))
                    {
                        INFO =  - 8;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLALSD",  - INFO);
                return;
            }
            // *
            EPS = this._dlamch.Run("Epsilon");
            // *
            // *     Set up the tolerance.
            // *
            if ((RCOND <= ZERO) || (RCOND >= ONE))
            {
                RCND = EPS;
            }
            else
            {
                RCND = RCOND;
            }
            // *
            RANK = 0;
            // *
            // *     Quick return if possible.
            // *
            if (N == 0)
            {
                return;
            }
            else
            {
                if (N == 1)
                {
                    if (D[1 + o_d] == ZERO)
                    {
                        this._dlaset.Run("A", 1, NRHS, ZERO, ZERO, ref B, offset_b
                                         , LDB);
                    }
                    else
                    {
                        RANK = 1;
                        this._dlascl.Run("G", 0, 0, D[1 + o_d], ONE, 1
                                         , NRHS, ref B, offset_b, LDB, ref INFO);
                        D[1 + o_d] = Math.Abs(D[1 + o_d]);
                    }
                    return;
                }
            }
            // *
            // *     Rotate the matrix if it is lower bidiagonal.
            // *
            if (UPLO == "L")
            {
                for (I = 1; I <= N - 1; I++)
                {
                    this._dlartg.Run(D[I + o_d], E[I + o_e], ref CS, ref SN, ref R);
                    D[I + o_d] = R;
                    E[I + o_e] = SN * D[I + 1 + o_d];
                    D[I + 1 + o_d] *= CS;
                    if (NRHS == 1)
                    {
                        this._drot.Run(1, ref B, I+1 * LDB + o_b, 1, ref B, I + 1+1 * LDB + o_b, 1, CS
                                       , SN);
                    }
                    else
                    {
                        WORK[I * 2 - 1 + o_work] = CS;
                        WORK[I * 2 + o_work] = SN;
                    }
                }
                if (NRHS > 1)
                {
                    for (I = 1; I <= NRHS; I++)
                    {
                        for (J = 1; J <= N - 1; J++)
                        {
                            CS = WORK[J * 2 - 1 + o_work];
                            SN = WORK[J * 2 + o_work];
                            this._drot.Run(1, ref B, J+I * LDB + o_b, 1, ref B, J + 1+I * LDB + o_b, 1, CS
                                           , SN);
                        }
                    }
                }
            }
            // *
            // *     Scale.
            // *
            NM1 = N - 1;
            ORGNRM = this._dlanst.Run("M", N, D, offset_d, E, offset_e);
            if (ORGNRM == ZERO)
            {
                this._dlaset.Run("A", N, NRHS, ZERO, ZERO, ref B, offset_b
                                 , LDB);
                return;
            }
            // *
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, N
                             , 1, ref D, offset_d, N, ref INFO);
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, NM1
                             , 1, ref E, offset_e, NM1, ref INFO);
            // *
            // *     If N is smaller than the minimum divide size SMLSIZ, then solve
            // *     the problem with another solver.
            // *
            if (N <= SMLSIZ)
            {
                NWORK = 1 + N * N;
                this._dlaset.Run("A", N, N, ZERO, ONE, ref WORK, offset_work
                                 , N);
                this._dlasdq.Run("U", 0, N, N, 0, NRHS
                                 , ref D, offset_d, ref E, offset_e, ref WORK, offset_work, N, ref WORK, offset_work, N
                                 , ref B, offset_b, LDB, ref WORK, NWORK + o_work, ref INFO);
                if (INFO != 0)
                {
                    return;
                }
                TOL = RCND * Math.Abs(D[this._idamax.Run(N, D, offset_d, 1) + o_d]);
                for (I = 1; I <= N; I++)
                {
                    if (D[I + o_d] <= TOL)
                    {
                        this._dlaset.Run("A", 1, NRHS, ZERO, ZERO, ref B, I+1 * LDB + o_b
                                         , LDB);
                    }
                    else
                    {
                        this._dlascl.Run("G", 0, 0, D[I + o_d], ONE, 1
                                         , NRHS, ref B, I+1 * LDB + o_b, LDB, ref INFO);
                        RANK += 1;
                    }
                }
                this._dgemm.Run("T", "N", N, NRHS, N, ONE
                                , WORK, offset_work, N, B, offset_b, LDB, ZERO, ref WORK, NWORK + o_work
                                , N);
                this._dlacpy.Run("A", N, NRHS, WORK, NWORK + o_work, N, ref B, offset_b
                                 , LDB);
                // *
                // *        Unscale.
                // *
                this._dlascl.Run("G", 0, 0, ONE, ORGNRM, N
                                 , 1, ref D, offset_d, N, ref INFO);
                this._dlasrt.Run("D", N, ref D, offset_d, ref INFO);
                this._dlascl.Run("G", 0, 0, ORGNRM, ONE, N
                                 , NRHS, ref B, offset_b, LDB, ref INFO);
                // *
                return;
            }
            // *
            // *     Book-keeping and setting up some constants.
            // *
            NLVL = Convert.ToInt32(Math.Truncate(Math.Log(Convert.ToDouble(N) / Convert.ToDouble(SMLSIZ + 1)) / Math.Log(TWO))) + 1;
            // *
            SMLSZP = SMLSIZ + 1;
            // *
            U = 1;
            VT = 1 + SMLSIZ * N;
            DIFL = VT + SMLSZP * N;
            DIFR = DIFL + NLVL * N;
            Z = DIFR + NLVL * N * 2;
            C = Z + NLVL * N;
            S = C + N;
            POLES = S + N;
            GIVNUM = POLES + 2 * NLVL * N;
            BX = GIVNUM + 2 * NLVL * N;
            NWORK = BX + N * NRHS;
            // *
            SIZEI = 1 + N;
            K = SIZEI + N;
            GIVPTR = K + N;
            PERM = GIVPTR + N;
            GIVCOL = PERM + NLVL * N;
            IWK = GIVCOL + NLVL * N * 2;
            // *
            ST = 1;
            SQRE = 0;
            ICMPQ1 = 1;
            ICMPQ2 = 0;
            NSUB = 0;
            // *
            for (I = 1; I <= N; I++)
            {
                if (Math.Abs(D[I + o_d]) < EPS)
                {
                    D[I + o_d] = FortranLib.Sign(EPS,D[I + o_d]);
                }
            }
            // *
            for (I = 1; I <= NM1; I++)
            {
                if ((Math.Abs(E[I + o_e]) < EPS) || (I == NM1))
                {
                    NSUB += 1;
                    IWORK[NSUB + o_iwork] = ST;
                    // *
                    // *           Subproblem found. First determine its size and then
                    // *           apply divide and conquer on it.
                    // *
                    if (I < NM1)
                    {
                        // *
                        // *              A subproblem with E(I) small for I < NM1.
                        // *
                        NSIZE = I - ST + 1;
                        IWORK[SIZEI + NSUB - 1 + o_iwork] = NSIZE;
                    }
                    else
                    {
                        if (Math.Abs(E[I + o_e]) >= EPS)
                        {
                            // *
                            // *              A subproblem with E(NM1) not too small but I = NM1.
                            // *
                            NSIZE = N - ST + 1;
                            IWORK[SIZEI + NSUB - 1 + o_iwork] = NSIZE;
                        }
                        else
                        {
                            // *
                            // *              A subproblem with E(NM1) small. This implies an
                            // *              1-by-1 subproblem at D(N), which is not solved
                            // *              explicitly.
                            // *
                            NSIZE = I - ST + 1;
                            IWORK[SIZEI + NSUB - 1 + o_iwork] = NSIZE;
                            NSUB += 1;
                            IWORK[NSUB + o_iwork] = N;
                            IWORK[SIZEI + NSUB - 1 + o_iwork] = 1;
                            this._dcopy.Run(NRHS, B, N+1 * LDB + o_b, LDB, ref WORK, BX + NM1 + o_work, N);
                        }
                    }
                    ST1 = ST - 1;
                    if (NSIZE == 1)
                    {
                        // *
                        // *              This is a 1-by-1 subproblem and is not solved
                        // *              explicitly.
                        // *
                        this._dcopy.Run(NRHS, B, ST+1 * LDB + o_b, LDB, ref WORK, BX + ST1 + o_work, N);
                    }
                    else
                    {
                        if (NSIZE <= SMLSIZ)
                        {
                            // *
                            // *              This is a small subproblem and is solved by DLASDQ.
                            // *
                            this._dlaset.Run("A", NSIZE, NSIZE, ZERO, ONE, ref WORK, VT + ST1 + o_work
                                             , N);
                            this._dlasdq.Run("U", 0, NSIZE, NSIZE, 0, NRHS
                                             , ref D, ST + o_d, ref E, ST + o_e, ref WORK, VT + ST1 + o_work, N, ref WORK, NWORK + o_work, N
                                             , ref B, ST+1 * LDB + o_b, LDB, ref WORK, NWORK + o_work, ref INFO);
                            if (INFO != 0)
                            {
                                return;
                            }
                            this._dlacpy.Run("A", NSIZE, NRHS, B, ST+1 * LDB + o_b, LDB, ref WORK, BX + ST1 + o_work
                                             , N);
                        }
                        else
                        {
                            // *
                            // *              A large problem. Solve it using divide and conquer.
                            // *
                            this._dlasda.Run(ICMPQ1, SMLSIZ, NSIZE, SQRE, ref D, ST + o_d, ref E, ST + o_e
                                             , ref WORK, U + ST1 + o_work, N, ref WORK, VT + ST1 + o_work, ref IWORK, K + ST1 + o_iwork, ref WORK, DIFL + ST1 + o_work, ref WORK, DIFR + ST1 + o_work
                                             , ref WORK, Z + ST1 + o_work, ref WORK, POLES + ST1 + o_work, ref IWORK, GIVPTR + ST1 + o_iwork, ref IWORK, GIVCOL + ST1 + o_iwork, N, ref IWORK, PERM + ST1 + o_iwork
                                             , ref WORK, GIVNUM + ST1 + o_work, ref WORK, C + ST1 + o_work, ref WORK, S + ST1 + o_work, ref WORK, NWORK + o_work, ref IWORK, IWK + o_iwork, ref INFO);
                            if (INFO != 0)
                            {
                                return;
                            }
                            BXST = BX + ST1;
                            this._dlalsa.Run(ICMPQ2, SMLSIZ, NSIZE, NRHS, ref B, ST+1 * LDB + o_b, LDB
                                             , ref WORK, BXST + o_work, N, WORK, U + ST1 + o_work, N, WORK, VT + ST1 + o_work, IWORK, K + ST1 + o_iwork
                                             , WORK, DIFL + ST1 + o_work, WORK, DIFR + ST1 + o_work, WORK, Z + ST1 + o_work, WORK, POLES + ST1 + o_work, IWORK, GIVPTR + ST1 + o_iwork, IWORK, GIVCOL + ST1 + o_iwork
                                             , N, IWORK, PERM + ST1 + o_iwork, WORK, GIVNUM + ST1 + o_work, WORK, C + ST1 + o_work, WORK, S + ST1 + o_work, ref WORK, NWORK + o_work
                                             , ref IWORK, IWK + o_iwork, ref INFO);
                            if (INFO != 0)
                            {
                                return;
                            }
                        }
                    }
                    ST = I + 1;
                }
            }
            // *
            // *     Apply the singular values and treat the tiny ones as zero.
            // *
            TOL = RCND * Math.Abs(D[this._idamax.Run(N, D, offset_d, 1) + o_d]);
            // *
            for (I = 1; I <= N; I++)
            {
                // *
                // *        Some of the elements in D can be negative because 1-by-1
                // *        subproblems were not solved explicitly.
                // *
                if (Math.Abs(D[I + o_d]) <= TOL)
                {
                    this._dlaset.Run("A", 1, NRHS, ZERO, ZERO, ref WORK, BX + I - 1 + o_work
                                     , N);
                }
                else
                {
                    RANK += 1;
                    this._dlascl.Run("G", 0, 0, D[I + o_d], ONE, 1
                                     , NRHS, ref WORK, BX + I - 1 + o_work, N, ref INFO);
                }
                D[I + o_d] = Math.Abs(D[I + o_d]);
            }
            // *
            // *     Now apply back the right singular vectors.
            // *
            ICMPQ2 = 1;
            for (I = 1; I <= NSUB; I++)
            {
                ST = IWORK[I + o_iwork];
                ST1 = ST - 1;
                NSIZE = IWORK[SIZEI + I - 1 + o_iwork];
                BXST = BX + ST1;
                if (NSIZE == 1)
                {
                    this._dcopy.Run(NRHS, WORK, BXST + o_work, N, ref B, ST+1 * LDB + o_b, LDB);
                }
                else
                {
                    if (NSIZE <= SMLSIZ)
                    {
                        this._dgemm.Run("T", "N", NSIZE, NRHS, NSIZE, ONE
                                        , WORK, VT + ST1 + o_work, N, WORK, BXST + o_work, N, ZERO, ref B, ST+1 * LDB + o_b
                                        , LDB);
                    }
                    else
                    {
                        this._dlalsa.Run(ICMPQ2, SMLSIZ, NSIZE, NRHS, ref WORK, BXST + o_work, N
                                         , ref B, ST+1 * LDB + o_b, LDB, WORK, U + ST1 + o_work, N, WORK, VT + ST1 + o_work, IWORK, K + ST1 + o_iwork
                                         , WORK, DIFL + ST1 + o_work, WORK, DIFR + ST1 + o_work, WORK, Z + ST1 + o_work, WORK, POLES + ST1 + o_work, IWORK, GIVPTR + ST1 + o_iwork, IWORK, GIVCOL + ST1 + o_iwork
                                         , N, IWORK, PERM + ST1 + o_iwork, WORK, GIVNUM + ST1 + o_work, WORK, C + ST1 + o_work, WORK, S + ST1 + o_work, ref WORK, NWORK + o_work
                                         , ref IWORK, IWK + o_iwork, ref INFO);
                        if (INFO != 0)
                        {
                            return;
                        }
                    }
                }
            }
            // *
            // *     Unscale and sort the singular values.
            // *
            this._dlascl.Run("G", 0, 0, ONE, ORGNRM, N
                             , 1, ref D, offset_d, N, ref INFO);
            this._dlasrt.Run("D", N, ref D, offset_d, ref INFO);
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, N
                             , NRHS, ref B, offset_b, LDB, ref INFO);
            // *
            return;
            // *
            // *     End of DLALSD
            // *

            #endregion

        }
    }
}
