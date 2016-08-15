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
    /// DGESVD computes the singular value decomposition (SVD) of a real
    /// M-by-N matrix A, optionally computing the left and/or right singular
    /// vectors. The SVD is written
    /// 
    /// A = U * SIGMA * transpose(V)
    /// 
    /// where SIGMA is an M-by-N matrix which is zero except for its
    /// min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
    /// V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
    /// are the singular values of A; they are real and non-negative, and
    /// are returned in descending order.  The first min(m,n) columns of
    /// U and V are the left and right singular vectors of A.
    /// 
    /// Note that the routine returns V**T, not V.
    /// 
    ///</summary>
    public class DGESVD
    {
    

        #region Dependencies
        
        DBDSQR _dbdsqr; DGEBRD _dgebrd; DGELQF _dgelqf; DGEMM _dgemm; DGEQRF _dgeqrf; DLACPY _dlacpy; DLASCL _dlascl; 
        DLASET _dlaset;DORGBR _dorgbr; DORGLQ _dorglq; DORGQR _dorgqr; DORMBR _dormbr; XERBLA _xerbla; LSAME _lsame; 
        ILAENV _ilaenv;DLAMCH _dlamch; DLANGE _dlange; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; double[] DUM = new double[1]; 

        #endregion

        public DGESVD(DBDSQR dbdsqr, DGEBRD dgebrd, DGELQF dgelqf, DGEMM dgemm, DGEQRF dgeqrf, DLACPY dlacpy, DLASCL dlascl, DLASET dlaset, DORGBR dorgbr, DORGLQ dorglq
                      , DORGQR dorgqr, DORMBR dormbr, XERBLA xerbla, LSAME lsame, ILAENV ilaenv, DLAMCH dlamch, DLANGE dlange)
        {
    

            #region Set Dependencies
            
            this._dbdsqr = dbdsqr; this._dgebrd = dgebrd; this._dgelqf = dgelqf; this._dgemm = dgemm; this._dgeqrf = dgeqrf; 
            this._dlacpy = dlacpy;this._dlascl = dlascl; this._dlaset = dlaset; this._dorgbr = dorgbr; this._dorglq = dorglq; 
            this._dorgqr = dorgqr;this._dormbr = dormbr; this._xerbla = xerbla; this._lsame = lsame; this._ilaenv = ilaenv; 
            this._dlamch = dlamch;this._dlange = dlange; 

            #endregion

        }
    
        public DGESVD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAS2 dlas2 = new DLAS2();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLASQ5 dlasq5 = new DLASQ5();
            DLAZQ4 dlazq4 = new DLAZQ4();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DLASSQ dlassq = new DLASSQ();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
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
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEBD2 dgebd2 = new DGEBD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLABRD dlabrd = new DLABRD(dgemv, dlarfg, dscal);
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
            DLASET dlaset = new DLASET(lsame);
            DORGL2 dorgl2 = new DORGL2(dlarf, dscal, xerbla);
            DORGLQ dorglq = new DORGLQ(dlarfb, dlarft, dorgl2, xerbla, ilaenv);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORGQR dorgqr = new DORGQR(dlarfb, dlarft, dorg2r, xerbla, ilaenv);
            DORGBR dorgbr = new DORGBR(lsame, ilaenv, dorglq, dorgqr, xerbla);
            DORML2 dorml2 = new DORML2(lsame, dlarf, xerbla);
            DORMLQ dormlq = new DORMLQ(lsame, ilaenv, dlarfb, dlarft, dorml2, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DORMBR dormbr = new DORMBR(lsame, ilaenv, dormlq, dormqr, xerbla);
            DLANGE dlange = new DLANGE(dlassq, lsame);

            #endregion


            #region Set Dependencies
            
            this._dbdsqr = dbdsqr; this._dgebrd = dgebrd; this._dgelqf = dgelqf; this._dgemm = dgemm; this._dgeqrf = dgeqrf; 
            this._dlacpy = dlacpy;this._dlascl = dlascl; this._dlaset = dlaset; this._dorgbr = dorgbr; this._dorglq = dorglq; 
            this._dorgqr = dorgqr;this._dormbr = dormbr; this._xerbla = xerbla; this._lsame = lsame; this._ilaenv = ilaenv; 
            this._dlamch = dlamch;this._dlange = dlange; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGESVD computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A, optionally computing the left and/or right singular
        /// vectors. The SVD is written
        /// 
        /// A = U * SIGMA * transpose(V)
        /// 
        /// where SIGMA is an M-by-N matrix which is zero except for its
        /// min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
        /// V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
        /// are the singular values of A; they are real and non-negative, and
        /// are returned in descending order.  The first min(m,n) columns of
        /// U and V are the left and right singular vectors of A.
        /// 
        /// Note that the routine returns V**T, not V.
        /// 
        ///</summary>
        /// <param name="JOBU">
        /// (input) CHARACTER*1
        /// Specifies options for computing all or part of the matrix U:
        /// = 'A':  all M columns of U are returned in array U:
        /// = 'S':  the first min(m,n) columns of U (the left singular
        /// vectors) are returned in the array U;
        /// = 'O':  the first min(m,n) columns of U (the left singular
        /// vectors) are overwritten on the array A;
        /// = 'N':  no columns of U (no left singular vectors) are
        /// computed.
        ///</param>
        /// <param name="JOBVT">
        /// (input) CHARACTER*1
        /// Specifies options for computing all or part of the matrix
        /// V**T:
        /// = 'A':  all N rows of V**T are returned in the array VT;
        /// = 'S':  the first min(m,n) rows of V**T (the right singular
        /// vectors) are returned in the array VT;
        /// = 'O':  the first min(m,n) rows of V**T (the right singular
        /// vectors) are overwritten on the array A;
        /// = 'N':  no rows of V**T (no right singular vectors) are
        /// computed.
        /// 
        /// JOBVT and JOBU cannot both be 'O'.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the input matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the input matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// = U * SIGMA * transpose(V)
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The singular values of A, sorted so that S(i) .GE. S(i+1).
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
        /// (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
        /// If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
        /// if JOBU = 'S', U contains the first min(m,n) columns of U
        /// (the left singular vectors, stored columnwise);
        /// if JOBU = 'N' or 'O', U is not referenced.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. 1; if
        /// JOBU = 'S' or 'A', LDU .GE. M.
        ///</param>
        /// <param name="VT">
        /// (output) DOUBLE PRECISION array, dimension (LDVT,N)
        /// If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
        /// V**T;
        /// if JOBVT = 'S', VT contains the first min(m,n) rows of
        /// V**T (the right singular vectors, stored rowwise);
        /// if JOBVT = 'N' or 'O', VT is not referenced.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.  LDVT .GE. 1; if
        /// JOBVT = 'A', LDVT .GE. N; if JOBVT = 'S', LDVT .GE. min(M,N).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
        /// if INFO .GT. 0, WORK(2:MIN(M,N)) contains the unconverged
        /// superdiagonal elements of an upper bidiagonal matrix B
        /// whose diagonal is in S (not necessarily sorted). B
        /// satisfies A = U * B * VT, so it has the same singular values
        /// as A, and singular vectors related by U and VT.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// LWORK .GE. MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
        /// For good performance, LWORK should generally be larger.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if DBDSQR did not converge, INFO specifies how many
        /// superdiagonals of an intermediate bidiagonal form B
        /// did not converge to zero. See the description of WORK
        /// above for details.
        ///</param>
        public void Run(string JOBU, string JOBVT, int M, int N, ref double[] A, int offset_a, int LDA
                         , ref double[] S, int offset_s, ref double[] U, int offset_u, int LDU, ref double[] VT, int offset_vt, int LDVT, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool WNTUA = false; bool WNTUAS = false; bool WNTUN = false; bool WNTUO = false; 
            bool WNTUS = false;bool WNTVA = false; bool WNTVAS = false; bool WNTVN = false; bool WNTVO = false; 
            bool WNTVS = false;int BDSPAC = 0; int BLK = 0; int CHUNK = 0; int I = 0; int IE = 0; int IERR = 0; int IR = 0; 
            int ISCL = 0;int ITAU = 0; int ITAUP = 0; int ITAUQ = 0; int IU = 0; int IWORK = 0; int LDWRKR = 0; int LDWRKU = 0; 
            int MAXWRK = 0;int MINMN = 0; int MINWRK = 0; int MNTHR = 0; int NCU = 0; int NCVT = 0; int NRU = 0; int NRVT = 0; 
            int WRKBL = 0;double ANRM = 0; double BIGNUM = 0; double EPS = 0; double SMLNUM = 0; 
            int offset_dum = 0;

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_s = -1 + offset_s;  int o_u = -1 - LDU + offset_u; 
             int o_vt = -1 - LDVT + offset_vt; int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            JOBU = JOBU.Substring(0, 1);  JOBVT = JOBVT.Substring(0, 1);  

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
            // *  DGESVD computes the singular value decomposition (SVD) of a real
            // *  M-by-N matrix A, optionally computing the left and/or right singular
            // *  vectors. The SVD is written
            // *
            // *       A = U * SIGMA * transpose(V)
            // *
            // *  where SIGMA is an M-by-N matrix which is zero except for its
            // *  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
            // *  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
            // *  are the singular values of A; they are real and non-negative, and
            // *  are returned in descending order.  The first min(m,n) columns of
            // *  U and V are the left and right singular vectors of A.
            // *
            // *  Note that the routine returns V**T, not V.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOBU    (input) CHARACTER*1
            // *          Specifies options for computing all or part of the matrix U:
            // *          = 'A':  all M columns of U are returned in array U:
            // *          = 'S':  the first min(m,n) columns of U (the left singular
            // *                  vectors) are returned in the array U;
            // *          = 'O':  the first min(m,n) columns of U (the left singular
            // *                  vectors) are overwritten on the array A;
            // *          = 'N':  no columns of U (no left singular vectors) are
            // *                  computed.
            // *
            // *  JOBVT   (input) CHARACTER*1
            // *          Specifies options for computing all or part of the matrix
            // *          V**T:
            // *          = 'A':  all N rows of V**T are returned in the array VT;
            // *          = 'S':  the first min(m,n) rows of V**T (the right singular
            // *                  vectors) are returned in the array VT;
            // *          = 'O':  the first min(m,n) rows of V**T (the right singular
            // *                  vectors) are overwritten on the array A;
            // *          = 'N':  no rows of V**T (no right singular vectors) are
            // *                  computed.
            // *
            // *          JOBVT and JOBU cannot both be 'O'.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the input matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the input matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit,
            // *          if JOBU = 'O',  A is overwritten with the first min(m,n)
            // *                          columns of U (the left singular vectors,
            // *                          stored columnwise);
            // *          if JOBVT = 'O', A is overwritten with the first min(m,n)
            // *                          rows of V**T (the right singular vectors,
            // *                          stored rowwise);
            // *          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
            // *                          are destroyed.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The singular values of A, sorted so that S(i) >= S(i+1).
            // *
            // *  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
            // *          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
            // *          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
            // *          if JOBU = 'S', U contains the first min(m,n) columns of U
            // *          (the left singular vectors, stored columnwise);
            // *          if JOBU = 'N' or 'O', U is not referenced.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U.  LDU >= 1; if
            // *          JOBU = 'S' or 'A', LDU >= M.
            // *
            // *  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
            // *          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
            // *          V**T;
            // *          if JOBVT = 'S', VT contains the first min(m,n) rows of
            // *          V**T (the right singular vectors, stored rowwise);
            // *          if JOBVT = 'N' or 'O', VT is not referenced.
            // *
            // *  LDVT    (input) INTEGER
            // *          The leading dimension of the array VT.  LDVT >= 1; if
            // *          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
            // *          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
            // *          superdiagonal elements of an upper bidiagonal matrix B
            // *          whose diagonal is in S (not necessarily sorted). B
            // *          satisfies A = U * B * VT, so it has the same singular values
            // *          as A, and singular vectors related by U and VT.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          LWORK >= MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
            // *          For good performance, LWORK should generally be larger.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if DBDSQR did not converge, INFO specifies how many
            // *                superdiagonals of an intermediate bidiagonal form B
            // *                did not converge to zero. See the description of WORK
            // *                above for details.
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
            //      INTRINSIC          MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            MINMN = Math.Min(M, N);
            WNTUA = this._lsame.Run(JOBU, "A");
            WNTUS = this._lsame.Run(JOBU, "S");
            WNTUAS = WNTUA || WNTUS;
            WNTUO = this._lsame.Run(JOBU, "O");
            WNTUN = this._lsame.Run(JOBU, "N");
            WNTVA = this._lsame.Run(JOBVT, "A");
            WNTVS = this._lsame.Run(JOBVT, "S");
            WNTVAS = WNTVA || WNTVS;
            WNTVO = this._lsame.Run(JOBVT, "O");
            WNTVN = this._lsame.Run(JOBVT, "N");
            LQUERY = (LWORK ==  - 1);
            // *
            if (!(WNTUA || WNTUS || WNTUO || WNTUN))
            {
                INFO =  - 1;
            }
            else
            {
                if (!(WNTVA || WNTVS || WNTVO || WNTVN) || (WNTVO && WNTUO))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (M < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (N < 0)
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
                                if (LDU < 1 || (WNTUAS && LDU < M))
                                {
                                    INFO =  - 9;
                                }
                                else
                                {
                                    if (LDVT < 1 || (WNTVA && LDVT < N) || (WNTVS && LDVT < MINMN))
                                    {
                                        INFO =  - 11;
                                    }
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
            // *       following subroutine, as returned by ILAENV.)
            // *
            if (INFO == 0)
            {
                MINWRK = 1;
                MAXWRK = 1;
                if (M >= N && MINMN > 0)
                {
                    // *
                    // *           Compute space needed for DBDSQR
                    // *
                    MNTHR = this._ilaenv.Run(6, "DGESVD", JOBU + JOBVT, M, N, 0, 0);
                    BDSPAC = 5 * N;
                    if (M >= MNTHR)
                    {
                        if (WNTUN)
                        {
                            // *
                            // *                 Path 1 (M much larger than N, JOBU='N')
                            // *
                            MAXWRK = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                            MAXWRK = Math.Max(MAXWRK, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                            if (WNTVO || WNTVAS) MAXWRK = Math.Max(MAXWRK, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                            MAXWRK = Math.Max(MAXWRK, BDSPAC);
                            MINWRK = Math.Max(4 * N, BDSPAC);
                        }
                        else
                        {
                            if (WNTUO && WNTVN)
                            {
                                // *
                                // *                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
                                // *
                                WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                WRKBL = Math.Max(WRKBL, N + N * this._ilaenv.Run(1, "DORGQR", " ", M, N, N,  - 1));
                                WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                WRKBL = Math.Max(WRKBL, BDSPAC);
                                MAXWRK = Math.Max(N * N + WRKBL, N * N + M * N + N);
                                MINWRK = Math.Max(3 * N + M, BDSPAC);
                            }
                            else
                            {
                                if (WNTUO && WNTVAS)
                                {
                                    // *
                                    // *                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
                                    // *                 'A')
                                    // *
                                    WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                    WRKBL = Math.Max(WRKBL, N + N * this._ilaenv.Run(1, "DORGQR", " ", M, N, N,  - 1));
                                    WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                    WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                    WRKBL = Math.Max(WRKBL, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                                    WRKBL = Math.Max(WRKBL, BDSPAC);
                                    MAXWRK = Math.Max(N * N + WRKBL, N * N + M * N + N);
                                    MINWRK = Math.Max(3 * N + M, BDSPAC);
                                }
                                else
                                {
                                    if (WNTUS && WNTVN)
                                    {
                                        // *
                                        // *                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
                                        // *
                                        WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                        WRKBL = Math.Max(WRKBL, N + N * this._ilaenv.Run(1, "DORGQR", " ", M, N, N,  - 1));
                                        WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                        WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                        WRKBL = Math.Max(WRKBL, BDSPAC);
                                        MAXWRK = N * N + WRKBL;
                                        MINWRK = Math.Max(3 * N + M, BDSPAC);
                                    }
                                    else
                                    {
                                        if (WNTUS && WNTVO)
                                        {
                                            // *
                                            // *                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
                                            // *
                                            WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                            WRKBL = Math.Max(WRKBL, N + N * this._ilaenv.Run(1, "DORGQR", " ", M, N, N,  - 1));
                                            WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                            WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                            WRKBL = Math.Max(WRKBL, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                                            WRKBL = Math.Max(WRKBL, BDSPAC);
                                            MAXWRK = 2 * N * N + WRKBL;
                                            MINWRK = Math.Max(3 * N + M, BDSPAC);
                                        }
                                        else
                                        {
                                            if (WNTUS && WNTVAS)
                                            {
                                                // *
                                                // *                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
                                                // *                 'A')
                                                // *
                                                WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                                WRKBL = Math.Max(WRKBL, N + N * this._ilaenv.Run(1, "DORGQR", " ", M, N, N,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                                                WRKBL = Math.Max(WRKBL, BDSPAC);
                                                MAXWRK = N * N + WRKBL;
                                                MINWRK = Math.Max(3 * N + M, BDSPAC);
                                            }
                                            else
                                            {
                                                if (WNTUA && WNTVN)
                                                {
                                                    // *
                                                    // *                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
                                                    // *
                                                    WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                                    WRKBL = Math.Max(WRKBL, N + M * this._ilaenv.Run(1, "DORGQR", " ", M, M, N,  - 1));
                                                    WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                                    WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                                    WRKBL = Math.Max(WRKBL, BDSPAC);
                                                    MAXWRK = N * N + WRKBL;
                                                    MINWRK = Math.Max(3 * N + M, BDSPAC);
                                                }
                                                else
                                                {
                                                    if (WNTUA && WNTVO)
                                                    {
                                                        // *
                                                        // *                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
                                                        // *
                                                        WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                                        WRKBL = Math.Max(WRKBL, N + M * this._ilaenv.Run(1, "DORGQR", " ", M, M, N,  - 1));
                                                        WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                                        WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                                        WRKBL = Math.Max(WRKBL, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                                                        WRKBL = Math.Max(WRKBL, BDSPAC);
                                                        MAXWRK = 2 * N * N + WRKBL;
                                                        MINWRK = Math.Max(3 * N + M, BDSPAC);
                                                    }
                                                    else
                                                    {
                                                        if (WNTUA && WNTVAS)
                                                        {
                                                            // *
                                                            // *                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
                                                            // *                 'A')
                                                            // *
                                                            WRKBL = N + N * this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                                                            WRKBL = Math.Max(WRKBL, N + M * this._ilaenv.Run(1, "DORGQR", " ", M, M, N,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * N + 2 * N * this._ilaenv.Run(1, "DGEBRD", " ", N, N,  - 1,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", N, N, N,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                                                            WRKBL = Math.Max(WRKBL, BDSPAC);
                                                            MAXWRK = N * N + WRKBL;
                                                            MINWRK = Math.Max(3 * N + M, BDSPAC);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        // *
                        // *              Path 10 (M at least N, but not much larger)
                        // *
                        MAXWRK = 3 * N + (M + N) * this._ilaenv.Run(1, "DGEBRD", " ", M, N,  - 1,  - 1);
                        if (WNTUS || WNTUO) MAXWRK = Math.Max(MAXWRK, 3 * N + N * this._ilaenv.Run(1, "DORGBR", "Q", M, N, N,  - 1));
                        if (WNTUA) MAXWRK = Math.Max(MAXWRK, 3 * N + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, N,  - 1));
                        if (!WNTVN) MAXWRK = Math.Max(MAXWRK, 3 * N + (N - 1) * this._ilaenv.Run(1, "DORGBR", "P", N, N, N,  - 1));
                        MAXWRK = Math.Max(MAXWRK, BDSPAC);
                        MINWRK = Math.Max(3 * N + M, BDSPAC);
                    }
                }
                else
                {
                    if (MINMN > 0)
                    {
                        // *
                        // *           Compute space needed for DBDSQR
                        // *
                        MNTHR = this._ilaenv.Run(6, "DGESVD", JOBU + JOBVT, M, N, 0, 0);
                        BDSPAC = 5 * M;
                        if (N >= MNTHR)
                        {
                            if (WNTVN)
                            {
                                // *
                                // *                 Path 1t(N much larger than M, JOBVT='N')
                                // *
                                MAXWRK = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                MAXWRK = Math.Max(MAXWRK, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                if (WNTUO || WNTUAS) MAXWRK = Math.Max(MAXWRK, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                MAXWRK = Math.Max(MAXWRK, BDSPAC);
                                MINWRK = Math.Max(4 * M, BDSPAC);
                            }
                            else
                            {
                                if (WNTVO && WNTUN)
                                {
                                    // *
                                    // *                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
                                    // *
                                    WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                    WRKBL = Math.Max(WRKBL, M + M * this._ilaenv.Run(1, "DORGLQ", " ", M, N, M,  - 1));
                                    WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                    WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                    WRKBL = Math.Max(WRKBL, BDSPAC);
                                    MAXWRK = Math.Max(M * M + WRKBL, M * M + M * N + M);
                                    MINWRK = Math.Max(3 * M + N, BDSPAC);
                                }
                                else
                                {
                                    if (WNTVO && WNTUAS)
                                    {
                                        // *
                                        // *                 Path 3t(N much larger than M, JOBU='S' or 'A',
                                        // *                 JOBVT='O')
                                        // *
                                        WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                        WRKBL = Math.Max(WRKBL, M + M * this._ilaenv.Run(1, "DORGLQ", " ", M, N, M,  - 1));
                                        WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                        WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                        WRKBL = Math.Max(WRKBL, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                        WRKBL = Math.Max(WRKBL, BDSPAC);
                                        MAXWRK = Math.Max(M * M + WRKBL, M * M + M * N + M);
                                        MINWRK = Math.Max(3 * M + N, BDSPAC);
                                    }
                                    else
                                    {
                                        if (WNTVS && WNTUN)
                                        {
                                            // *
                                            // *                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
                                            // *
                                            WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                            WRKBL = Math.Max(WRKBL, M + M * this._ilaenv.Run(1, "DORGLQ", " ", M, N, M,  - 1));
                                            WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                            WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                            WRKBL = Math.Max(WRKBL, BDSPAC);
                                            MAXWRK = M * M + WRKBL;
                                            MINWRK = Math.Max(3 * M + N, BDSPAC);
                                        }
                                        else
                                        {
                                            if (WNTVS && WNTUO)
                                            {
                                                // *
                                                // *                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
                                                // *
                                                WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                                WRKBL = Math.Max(WRKBL, M + M * this._ilaenv.Run(1, "DORGLQ", " ", M, N, M,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                                WRKBL = Math.Max(WRKBL, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                                WRKBL = Math.Max(WRKBL, BDSPAC);
                                                MAXWRK = 2 * M * M + WRKBL;
                                                MINWRK = Math.Max(3 * M + N, BDSPAC);
                                            }
                                            else
                                            {
                                                if (WNTVS && WNTUAS)
                                                {
                                                    // *
                                                    // *                 Path 6t(N much larger than M, JOBU='S' or 'A',
                                                    // *                 JOBVT='S')
                                                    // *
                                                    WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                                    WRKBL = Math.Max(WRKBL, M + M * this._ilaenv.Run(1, "DORGLQ", " ", M, N, M,  - 1));
                                                    WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                                    WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                                    WRKBL = Math.Max(WRKBL, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                                    WRKBL = Math.Max(WRKBL, BDSPAC);
                                                    MAXWRK = M * M + WRKBL;
                                                    MINWRK = Math.Max(3 * M + N, BDSPAC);
                                                }
                                                else
                                                {
                                                    if (WNTVA && WNTUN)
                                                    {
                                                        // *
                                                        // *                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
                                                        // *
                                                        WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                                        WRKBL = Math.Max(WRKBL, M + N * this._ilaenv.Run(1, "DORGLQ", " ", N, N, M,  - 1));
                                                        WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                                        WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                                        WRKBL = Math.Max(WRKBL, BDSPAC);
                                                        MAXWRK = M * M + WRKBL;
                                                        MINWRK = Math.Max(3 * M + N, BDSPAC);
                                                    }
                                                    else
                                                    {
                                                        if (WNTVA && WNTUO)
                                                        {
                                                            // *
                                                            // *                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
                                                            // *
                                                            WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                                            WRKBL = Math.Max(WRKBL, M + N * this._ilaenv.Run(1, "DORGLQ", " ", N, N, M,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                                            WRKBL = Math.Max(WRKBL, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                                            WRKBL = Math.Max(WRKBL, BDSPAC);
                                                            MAXWRK = 2 * M * M + WRKBL;
                                                            MINWRK = Math.Max(3 * M + N, BDSPAC);
                                                        }
                                                        else
                                                        {
                                                            if (WNTVA && WNTUAS)
                                                            {
                                                                // *
                                                                // *                 Path 9t(N much larger than M, JOBU='S' or 'A',
                                                                // *                 JOBVT='A')
                                                                // *
                                                                WRKBL = M + M * this._ilaenv.Run(1, "DGELQF", " ", M, N,  - 1,  - 1);
                                                                WRKBL = Math.Max(WRKBL, M + N * this._ilaenv.Run(1, "DORGLQ", " ", N, N, M,  - 1));
                                                                WRKBL = Math.Max(WRKBL, 3 * M + 2 * M * this._ilaenv.Run(1, "DGEBRD", " ", M, M,  - 1,  - 1));
                                                                WRKBL = Math.Max(WRKBL, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "P", M, M, M,  - 1));
                                                                WRKBL = Math.Max(WRKBL, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                                                                WRKBL = Math.Max(WRKBL, BDSPAC);
                                                                MAXWRK = M * M + WRKBL;
                                                                MINWRK = Math.Max(3 * M + N, BDSPAC);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        else
                        {
                            // *
                            // *              Path 10t(N greater than M, but not much larger)
                            // *
                            MAXWRK = 3 * M + (M + N) * this._ilaenv.Run(1, "DGEBRD", " ", M, N,  - 1,  - 1);
                            if (WNTVS || WNTVO) MAXWRK = Math.Max(MAXWRK, 3 * M + M * this._ilaenv.Run(1, "DORGBR", "P", M, N, M,  - 1));
                            if (WNTVA) MAXWRK = Math.Max(MAXWRK, 3 * M + N * this._ilaenv.Run(1, "DORGBR", "P", N, N, M,  - 1));
                            if (!WNTUN) MAXWRK = Math.Max(MAXWRK, 3 * M + (M - 1) * this._ilaenv.Run(1, "DORGBR", "Q", M, M, M,  - 1));
                            MAXWRK = Math.Max(MAXWRK, BDSPAC);
                            MINWRK = Math.Max(3 * M + N, BDSPAC);
                        }
                    }
                }
                MAXWRK = Math.Max(MAXWRK, MINWRK);
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
                this._xerbla.Run("DGESVD",  - INFO);
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
            if (M == 0 || N == 0)
            {
                return;
            }
            // *
            // *     Get machine constants
            // *
            EPS = this._dlamch.Run("P");
            SMLNUM = Math.Sqrt(this._dlamch.Run("S")) / EPS;
            BIGNUM = ONE / SMLNUM;
            // *
            // *     Scale A if max element outside range [SMLNUM,BIGNUM]
            // *
            ANRM = this._dlange.Run("M", M, N, A, offset_a, LDA, ref DUM, offset_dum);
            ISCL = 0;
            if (ANRM > ZERO && ANRM < SMLNUM)
            {
                ISCL = 1;
                this._dlascl.Run("G", 0, 0, ANRM, SMLNUM, M
                                 , N, ref A, offset_a, LDA, ref IERR);
            }
            else
            {
                if (ANRM > BIGNUM)
                {
                    ISCL = 1;
                    this._dlascl.Run("G", 0, 0, ANRM, BIGNUM, M
                                     , N, ref A, offset_a, LDA, ref IERR);
                }
            }
            // *
            if (M >= N)
            {
                // *
                // *        A has at least as many rows as columns. If A has sufficiently
                // *        more rows than columns, first reduce using the QR
                // *        decomposition (if sufficient workspace available)
                // *
                if (M >= MNTHR)
                {
                    // *
                    if (WNTUN)
                    {
                        // *
                        // *              Path 1 (M much larger than N, JOBU='N')
                        // *              No left singular vectors to be computed
                        // *
                        ITAU = 1;
                        IWORK = ITAU + N;
                        // *
                        // *              Compute A=Q*R
                        // *              (Workspace: need 2*N, prefer N+N*NB)
                        // *
                        this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                         , LWORK - IWORK + 1, ref IERR);
                        // *
                        // *              Zero out below R
                        // *
                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                         , LDA);
                        IE = 1;
                        ITAUQ = IE + N;
                        ITAUP = ITAUQ + N;
                        IWORK = ITAUP + N;
                        // *
                        // *              Bidiagonalize R in A
                        // *              (Workspace: need 4*N, prefer 3*N+2*N*NB)
                        // *
                        this._dgebrd.Run(N, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                        NCVT = 0;
                        if (WNTVO || WNTVAS)
                        {
                            // *
                            // *                 If right singular vectors desired, generate P'.
                            // *                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                            // *
                            this._dorgbr.Run("P", N, N, N, ref A, offset_a, LDA
                                             , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                            NCVT = N;
                        }
                        IWORK = IE + N;
                        // *
                        // *              Perform bidiagonal QR iteration, computing right
                        // *              singular vectors of A in A if desired
                        // *              (Workspace: need BDSPAC)
                        // *
                        this._dbdsqr.Run("U", N, NCVT, 0, 0, ref S, offset_s
                                         , ref WORK, IE + o_work, ref A, offset_a, LDA, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                        // *
                        // *              If right singular vectors desired in VT, copy them there
                        // *
                        if (WNTVAS)
                        {
                            this._dlacpy.Run("F", N, N, A, offset_a, LDA, ref VT, offset_vt
                                             , LDVT);
                        }
                        // *
                    }
                    else
                    {
                        if (WNTUO && WNTVN)
                        {
                            // *
                            // *              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
                            // *              N left singular vectors to be overwritten on A and
                            // *              no right singular vectors to be computed
                            // *
                            if (LWORK >= N * N + Math.Max(4 * N, BDSPAC))
                            {
                                // *
                                // *                 Sufficient workspace for a fast algorithm
                                // *
                                IR = 1;
                                if (LWORK >= Math.Max(WRKBL, LDA * N + N) + LDA * N)
                                {
                                    // *
                                    // *                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
                                    // *
                                    LDWRKU = LDA;
                                    LDWRKR = LDA;
                                }
                                else
                                {
                                    if (LWORK >= Math.Max(WRKBL, LDA * N + N) + N * N)
                                    {
                                        // *
                                        // *                    WORK(IU) is LDA by N, WORK(IR) is N by N
                                        // *
                                        LDWRKU = LDA;
                                        LDWRKR = N;
                                    }
                                    else
                                    {
                                        // *
                                        // *                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
                                        // *
                                        LDWRKU = (LWORK - N * N - N) / N;
                                        LDWRKR = N;
                                    }
                                }
                                ITAU = IR + LDWRKR * N;
                                IWORK = ITAU + N;
                                // *
                                // *                 Compute A=Q*R
                                // *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                // *
                                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                 , LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Copy R to WORK(IR) and zero out below it
                                // *
                                this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IR + o_work
                                                 , LDWRKR);
                                this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IR + 1 + o_work
                                                 , LDWRKR);
                                // *
                                // *                 Generate Q in A
                                // *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                // *
                                this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IE = ITAU;
                                ITAUQ = IE + N;
                                ITAUP = ITAUQ + N;
                                IWORK = ITAUP + N;
                                // *
                                // *                 Bidiagonalize R in WORK(IR)
                                // *                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                // *
                                this._dgebrd.Run(N, N, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Generate left vectors bidiagonalizing R
                                // *                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                // *
                                this._dorgbr.Run("Q", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IWORK = IE + N;
                                // *
                                // *                 Perform bidiagonal QR iteration, computing left
                                // *                 singular vectors of R in WORK(IR)
                                // *                 (Workspace: need N*N+BDSPAC)
                                // *
                                this._dbdsqr.Run("U", N, 0, N, 0, ref S, offset_s
                                                 , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                IU = IE + N;
                                // *
                                // *                 Multiply Q in A by left singular vectors of R in
                                // *                 WORK(IR), storing result in WORK(IU) and copying to A
                                // *                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
                                // *
                                for (I = 1; (LDWRKU >= 0) ? (I <= M) : (I >= M); I += LDWRKU)
                                {
                                    CHUNK = Math.Min(M - I + 1, LDWRKU);
                                    this._dgemm.Run("N", "N", CHUNK, N, N, ONE
                                                    , A, I+1 * LDA + o_a, LDA, WORK, IR + o_work, LDWRKR, ZERO, ref WORK, IU + o_work
                                                    , LDWRKU);
                                    this._dlacpy.Run("F", CHUNK, N, WORK, IU + o_work, LDWRKU, ref A, I+1 * LDA + o_a
                                                     , LDA);
                                }
                                // *
                            }
                            else
                            {
                                // *
                                // *                 Insufficient workspace for a fast algorithm
                                // *
                                IE = 1;
                                ITAUQ = IE + N;
                                ITAUP = ITAUQ + N;
                                IWORK = ITAUP + N;
                                // *
                                // *                 Bidiagonalize A
                                // *                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
                                // *
                                this._dgebrd.Run(M, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Generate left vectors bidiagonalizing A
                                // *                 (Workspace: need 4*N, prefer 3*N+N*NB)
                                // *
                                this._dorgbr.Run("Q", M, N, N, ref A, offset_a, LDA
                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IWORK = IE + N;
                                // *
                                // *                 Perform bidiagonal QR iteration, computing left
                                // *                 singular vectors of A in A
                                // *                 (Workspace: need BDSPAC)
                                // *
                                this._dbdsqr.Run("U", N, 0, M, 0, ref S, offset_s
                                                 , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref A, offset_a, LDA, ref DUM, offset_dum
                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                // *
                            }
                            // *
                        }
                        else
                        {
                            if (WNTUO && WNTVAS)
                            {
                                // *
                                // *              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
                                // *              N left singular vectors to be overwritten on A and
                                // *              N right singular vectors to be computed in VT
                                // *
                                if (LWORK >= N * N + Math.Max(4 * N, BDSPAC))
                                {
                                    // *
                                    // *                 Sufficient workspace for a fast algorithm
                                    // *
                                    IR = 1;
                                    if (LWORK >= Math.Max(WRKBL, LDA * N + N) + LDA * N)
                                    {
                                        // *
                                        // *                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
                                        // *
                                        LDWRKU = LDA;
                                        LDWRKR = LDA;
                                    }
                                    else
                                    {
                                        if (LWORK >= Math.Max(WRKBL, LDA * N + N) + N * N)
                                        {
                                            // *
                                            // *                    WORK(IU) is LDA by N and WORK(IR) is N by N
                                            // *
                                            LDWRKU = LDA;
                                            LDWRKR = N;
                                        }
                                        else
                                        {
                                            // *
                                            // *                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
                                            // *
                                            LDWRKU = (LWORK - N * N - N) / N;
                                            LDWRKR = N;
                                        }
                                    }
                                    ITAU = IR + LDWRKR * N;
                                    IWORK = ITAU + N;
                                    // *
                                    // *                 Compute A=Q*R
                                    // *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                    // *
                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Copy R to VT, zeroing out below it
                                    // *
                                    this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref VT, offset_vt
                                                     , LDVT);
                                    if (N > 1)
                                    {
                                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref VT, 2+1 * LDVT + o_vt
                                                         , LDVT);
                                    }
                                    // *
                                    // *                 Generate Q in A
                                    // *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                    // *
                                    this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IE = ITAU;
                                    ITAUQ = IE + N;
                                    ITAUP = ITAUQ + N;
                                    IWORK = ITAUP + N;
                                    // *
                                    // *                 Bidiagonalize R in VT, copying result to WORK(IR)
                                    // *                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                    // *
                                    this._dgebrd.Run(N, N, ref VT, offset_vt, LDVT, ref S, offset_s, ref WORK, IE + o_work
                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    this._dlacpy.Run("L", N, N, VT, offset_vt, LDVT, ref WORK, IR + o_work
                                                     , LDWRKR);
                                    // *
                                    // *                 Generate left vectors bidiagonalizing R in WORK(IR)
                                    // *                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                    // *
                                    this._dorgbr.Run("Q", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Generate right vectors bidiagonalizing R in VT
                                    // *                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
                                    // *
                                    this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IWORK = IE + N;
                                    // *
                                    // *                 Perform bidiagonal QR iteration, computing left
                                    // *                 singular vectors of R in WORK(IR) and computing right
                                    // *                 singular vectors of R in VT
                                    // *                 (Workspace: need N*N+BDSPAC)
                                    // *
                                    this._dbdsqr.Run("U", N, N, N, 0, ref S, offset_s
                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                    IU = IE + N;
                                    // *
                                    // *                 Multiply Q in A by left singular vectors of R in
                                    // *                 WORK(IR), storing result in WORK(IU) and copying to A
                                    // *                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
                                    // *
                                    for (I = 1; (LDWRKU >= 0) ? (I <= M) : (I >= M); I += LDWRKU)
                                    {
                                        CHUNK = Math.Min(M - I + 1, LDWRKU);
                                        this._dgemm.Run("N", "N", CHUNK, N, N, ONE
                                                        , A, I+1 * LDA + o_a, LDA, WORK, IR + o_work, LDWRKR, ZERO, ref WORK, IU + o_work
                                                        , LDWRKU);
                                        this._dlacpy.Run("F", CHUNK, N, WORK, IU + o_work, LDWRKU, ref A, I+1 * LDA + o_a
                                                         , LDA);
                                    }
                                    // *
                                }
                                else
                                {
                                    // *
                                    // *                 Insufficient workspace for a fast algorithm
                                    // *
                                    ITAU = 1;
                                    IWORK = ITAU + N;
                                    // *
                                    // *                 Compute A=Q*R
                                    // *                 (Workspace: need 2*N, prefer N+N*NB)
                                    // *
                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Copy R to VT, zeroing out below it
                                    // *
                                    this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref VT, offset_vt
                                                     , LDVT);
                                    if (N > 1)
                                    {
                                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref VT, 2+1 * LDVT + o_vt
                                                         , LDVT);
                                    }
                                    // *
                                    // *                 Generate Q in A
                                    // *                 (Workspace: need 2*N, prefer N+N*NB)
                                    // *
                                    this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IE = ITAU;
                                    ITAUQ = IE + N;
                                    ITAUP = ITAUQ + N;
                                    IWORK = ITAUP + N;
                                    // *
                                    // *                 Bidiagonalize R in VT
                                    // *                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                    // *
                                    this._dgebrd.Run(N, N, ref VT, offset_vt, LDVT, ref S, offset_s, ref WORK, IE + o_work
                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Multiply Q in A by left vectors bidiagonalizing R
                                    // *                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                    // *
                                    this._dormbr.Run("Q", "R", "N", M, N, N
                                                     , ref VT, offset_vt, LDVT, WORK, ITAUQ + o_work, ref A, offset_a, LDA, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Generate right vectors bidiagonalizing R in VT
                                    // *                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                                    // *
                                    this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IWORK = IE + N;
                                    // *
                                    // *                 Perform bidiagonal QR iteration, computing left
                                    // *                 singular vectors of A in A and computing right
                                    // *                 singular vectors of A in VT
                                    // *                 (Workspace: need BDSPAC)
                                    // *
                                    this._dbdsqr.Run("U", N, N, M, 0, ref S, offset_s
                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref A, offset_a, LDA, ref DUM, offset_dum
                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                    // *
                                }
                                // *
                            }
                            else
                            {
                                if (WNTUS)
                                {
                                    // *
                                    if (WNTVN)
                                    {
                                        // *
                                        // *                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
                                        // *                 N left singular vectors to be computed in U and
                                        // *                 no right singular vectors to be computed
                                        // *
                                        if (LWORK >= N * N + Math.Max(4 * N, BDSPAC))
                                        {
                                            // *
                                            // *                    Sufficient workspace for a fast algorithm
                                            // *
                                            IR = 1;
                                            if (LWORK >= WRKBL + LDA * N)
                                            {
                                                // *
                                                // *                       WORK(IR) is LDA by N
                                                // *
                                                LDWRKR = LDA;
                                            }
                                            else
                                            {
                                                // *
                                                // *                       WORK(IR) is N by N
                                                // *
                                                LDWRKR = N;
                                            }
                                            ITAU = IR + LDWRKR * N;
                                            IWORK = ITAU + N;
                                            // *
                                            // *                    Compute A=Q*R
                                            // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                            // *
                                            this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Copy R to WORK(IR), zeroing out below it
                                            // *
                                            this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IR + o_work
                                                             , LDWRKR);
                                            this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IR + 1 + o_work
                                                             , LDWRKR);
                                            // *
                                            // *                    Generate Q in A
                                            // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                            // *
                                            this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                             , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IE = ITAU;
                                            ITAUQ = IE + N;
                                            ITAUP = ITAUQ + N;
                                            IWORK = ITAUP + N;
                                            // *
                                            // *                    Bidiagonalize R in WORK(IR)
                                            // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                            // *
                                            this._dgebrd.Run(N, N, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                             , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Generate left vectors bidiagonalizing R in WORK(IR)
                                            // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                            // *
                                            this._dorgbr.Run("Q", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                             , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IWORK = IE + N;
                                            // *
                                            // *                    Perform bidiagonal QR iteration, computing left
                                            // *                    singular vectors of R in WORK(IR)
                                            // *                    (Workspace: need N*N+BDSPAC)
                                            // *
                                            this._dbdsqr.Run("U", N, 0, N, 0, ref S, offset_s
                                                             , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                                            // *
                                            // *                    Multiply Q in A by left singular vectors of R in
                                            // *                    WORK(IR), storing result in U
                                            // *                    (Workspace: need N*N)
                                            // *
                                            this._dgemm.Run("N", "N", M, N, N, ONE
                                                            , A, offset_a, LDA, WORK, IR + o_work, LDWRKR, ZERO, ref U, offset_u
                                                            , LDU);
                                            // *
                                        }
                                        else
                                        {
                                            // *
                                            // *                    Insufficient workspace for a fast algorithm
                                            // *
                                            ITAU = 1;
                                            IWORK = ITAU + N;
                                            // *
                                            // *                    Compute A=Q*R, copying result to U
                                            // *                    (Workspace: need 2*N, prefer N+N*NB)
                                            // *
                                            this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                             , LDU);
                                            // *
                                            // *                    Generate Q in U
                                            // *                    (Workspace: need 2*N, prefer N+N*NB)
                                            // *
                                            this._dorgqr.Run(M, N, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                             , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IE = ITAU;
                                            ITAUQ = IE + N;
                                            ITAUP = ITAUQ + N;
                                            IWORK = ITAUP + N;
                                            // *
                                            // *                    Zero out below R in A
                                            // *
                                            this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                                             , LDA);
                                            // *
                                            // *                    Bidiagonalize R in A
                                            // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                            // *
                                            this._dgebrd.Run(N, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                             , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Multiply Q in U by left vectors bidiagonalizing R
                                            // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                            // *
                                            this._dormbr.Run("Q", "R", "N", M, N, N
                                                             , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            IWORK = IE + N;
                                            // *
                                            // *                    Perform bidiagonal QR iteration, computing left
                                            // *                    singular vectors of A in U
                                            // *                    (Workspace: need BDSPAC)
                                            // *
                                            this._dbdsqr.Run("U", N, 0, M, 0, ref S, offset_s
                                                             , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref U, offset_u, LDU, ref DUM, offset_dum
                                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                                            // *
                                        }
                                        // *
                                    }
                                    else
                                    {
                                        if (WNTVO)
                                        {
                                            // *
                                            // *                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
                                            // *                 N left singular vectors to be computed in U and
                                            // *                 N right singular vectors to be overwritten on A
                                            // *
                                            if (LWORK >= 2 * N * N + Math.Max(4 * N, BDSPAC))
                                            {
                                                // *
                                                // *                    Sufficient workspace for a fast algorithm
                                                // *
                                                IU = 1;
                                                if (LWORK >= WRKBL + 2 * LDA * N)
                                                {
                                                    // *
                                                    // *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
                                                    // *
                                                    LDWRKU = LDA;
                                                    IR = IU + LDWRKU * N;
                                                    LDWRKR = LDA;
                                                }
                                                else
                                                {
                                                    if (LWORK >= WRKBL + (LDA + N) * N)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by N and WORK(IR) is N by N
                                                        // *
                                                        LDWRKU = LDA;
                                                        IR = IU + LDWRKU * N;
                                                        LDWRKR = N;
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is N by N and WORK(IR) is N by N
                                                        // *
                                                        LDWRKU = N;
                                                        IR = IU + LDWRKU * N;
                                                        LDWRKR = N;
                                                    }
                                                }
                                                ITAU = IR + LDWRKR * N;
                                                IWORK = ITAU + N;
                                                // *
                                                // *                    Compute A=Q*R
                                                // *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
                                                // *
                                                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Copy R to WORK(IU), zeroing out below it
                                                // *
                                                this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IU + o_work
                                                                 , LDWRKU);
                                                this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IU + 1 + o_work
                                                                 , LDWRKU);
                                                // *
                                                // *                    Generate Q in A
                                                // *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
                                                // *
                                                this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + N;
                                                ITAUP = ITAUQ + N;
                                                IWORK = ITAUP + N;
                                                // *
                                                // *                    Bidiagonalize R in WORK(IU), copying result to
                                                // *                    WORK(IR)
                                                // *                    (Workspace: need 2*N*N+4*N,
                                                // *                                prefer 2*N*N+3*N+2*N*NB)
                                                // *
                                                this._dgebrd.Run(N, N, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("U", N, N, WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work
                                                                 , LDWRKR);
                                                // *
                                                // *                    Generate left bidiagonalizing vectors in WORK(IU)
                                                // *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
                                                // *
                                                this._dorgbr.Run("Q", N, N, N, ref WORK, IU + o_work, LDWRKU
                                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate right bidiagonalizing vectors in WORK(IR)
                                                // *                    (Workspace: need 2*N*N+4*N-1,
                                                // *                                prefer 2*N*N+3*N+(N-1)*NB)
                                                // *
                                                this._dorgbr.Run("P", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + N;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing left
                                                // *                    singular vectors of R in WORK(IU) and computing
                                                // *                    right singular vectors of R in WORK(IR)
                                                // *                    (Workspace: need 2*N*N+BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", N, N, N, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref WORK, IU + o_work, LDWRKU, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                                // *                    Multiply Q in A by left singular vectors of R in
                                                // *                    WORK(IU), storing result in U
                                                // *                    (Workspace: need N*N)
                                                // *
                                                this._dgemm.Run("N", "N", M, N, N, ONE
                                                                , A, offset_a, LDA, WORK, IU + o_work, LDWRKU, ZERO, ref U, offset_u
                                                                , LDU);
                                                // *
                                                // *                    Copy right singular vectors of R to A
                                                // *                    (Workspace: need N*N)
                                                // *
                                                this._dlacpy.Run("F", N, N, WORK, IR + o_work, LDWRKR, ref A, offset_a
                                                                 , LDA);
                                                // *
                                            }
                                            else
                                            {
                                                // *
                                                // *                    Insufficient workspace for a fast algorithm
                                                // *
                                                ITAU = 1;
                                                IWORK = ITAU + N;
                                                // *
                                                // *                    Compute A=Q*R, copying result to U
                                                // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                // *
                                                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                 , LDU);
                                                // *
                                                // *                    Generate Q in U
                                                // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                // *
                                                this._dorgqr.Run(M, N, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + N;
                                                ITAUP = ITAUQ + N;
                                                IWORK = ITAUP + N;
                                                // *
                                                // *                    Zero out below R in A
                                                // *
                                                this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                                                 , LDA);
                                                // *
                                                // *                    Bidiagonalize R in A
                                                // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                                // *
                                                this._dgebrd.Run(N, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Multiply Q in U by left vectors bidiagonalizing R
                                                // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                                // *
                                                this._dormbr.Run("Q", "R", "N", M, N, N
                                                                 , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate right vectors bidiagonalizing R in A
                                                // *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                                                // *
                                                this._dorgbr.Run("P", N, N, N, ref A, offset_a, LDA
                                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + N;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing left
                                                // *                    singular vectors of A in U and computing right
                                                // *                    singular vectors of A in A
                                                // *                    (Workspace: need BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", N, N, M, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref A, offset_a, LDA, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                            }
                                            // *
                                        }
                                        else
                                        {
                                            if (WNTVAS)
                                            {
                                                // *
                                                // *                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
                                                // *                         or 'A')
                                                // *                 N left singular vectors to be computed in U and
                                                // *                 N right singular vectors to be computed in VT
                                                // *
                                                if (LWORK >= N * N + Math.Max(4 * N, BDSPAC))
                                                {
                                                    // *
                                                    // *                    Sufficient workspace for a fast algorithm
                                                    // *
                                                    IU = 1;
                                                    if (LWORK >= WRKBL + LDA * N)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by N
                                                        // *
                                                        LDWRKU = LDA;
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is N by N
                                                        // *
                                                        LDWRKU = N;
                                                    }
                                                    ITAU = IU + LDWRKU * N;
                                                    IWORK = ITAU + N;
                                                    // *
                                                    // *                    Compute A=Q*R
                                                    // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                                    // *
                                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy R to WORK(IU), zeroing out below it
                                                    // *
                                                    this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IU + o_work
                                                                     , LDWRKU);
                                                    this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IU + 1 + o_work
                                                                     , LDWRKU);
                                                    // *
                                                    // *                    Generate Q in A
                                                    // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                                    // *
                                                    this._dorgqr.Run(M, N, N, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IE = ITAU;
                                                    ITAUQ = IE + N;
                                                    ITAUP = ITAUQ + N;
                                                    IWORK = ITAUP + N;
                                                    // *
                                                    // *                    Bidiagonalize R in WORK(IU), copying result to VT
                                                    // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                                    // *
                                                    this._dgebrd.Run(N, N, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("U", N, N, WORK, IU + o_work, LDWRKU, ref VT, offset_vt
                                                                     , LDVT);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in WORK(IU)
                                                    // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", N, N, N, ref WORK, IU + o_work, LDWRKU
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in VT
                                                    // *                    (Workspace: need N*N+4*N-1,
                                                    // *                                prefer N*N+3*N+(N-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + N;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of R in WORK(IU) and computing
                                                    // *                    right singular vectors of R in VT
                                                    // *                    (Workspace: need N*N+BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", N, N, N, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref WORK, IU + o_work, LDWRKU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                    // *                    Multiply Q in A by left singular vectors of R in
                                                    // *                    WORK(IU), storing result in U
                                                    // *                    (Workspace: need N*N)
                                                    // *
                                                    this._dgemm.Run("N", "N", M, N, N, ONE
                                                                    , A, offset_a, LDA, WORK, IU + o_work, LDWRKU, ZERO, ref U, offset_u
                                                                    , LDU);
                                                    // *
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                    Insufficient workspace for a fast algorithm
                                                    // *
                                                    ITAU = 1;
                                                    IWORK = ITAU + N;
                                                    // *
                                                    // *                    Compute A=Q*R, copying result to U
                                                    // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                    // *
                                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                     , LDU);
                                                    // *
                                                    // *                    Generate Q in U
                                                    // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                    // *
                                                    this._dorgqr.Run(M, N, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy R to VT, zeroing out below it
                                                    // *
                                                    this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref VT, offset_vt
                                                                     , LDVT);
                                                    if (N > 1)
                                                    {
                                                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref VT, 2+1 * LDVT + o_vt
                                                                         , LDVT);
                                                    }
                                                    IE = ITAU;
                                                    ITAUQ = IE + N;
                                                    ITAUP = ITAUQ + N;
                                                    IWORK = ITAUP + N;
                                                    // *
                                                    // *                    Bidiagonalize R in VT
                                                    // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                                    // *
                                                    this._dgebrd.Run(N, N, ref VT, offset_vt, LDVT, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Multiply Q in U by left bidiagonalizing vectors
                                                    // *                    in VT
                                                    // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                                    // *
                                                    this._dormbr.Run("Q", "R", "N", M, N, N
                                                                     , ref VT, offset_vt, LDVT, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in VT
                                                    // *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + N;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of A in U and computing right
                                                    // *                    singular vectors of A in VT
                                                    // *                    (Workspace: need BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", N, N, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                }
                                                // *
                                            }
                                        }
                                    }
                                    // *
                                }
                                else
                                {
                                    if (WNTUA)
                                    {
                                        // *
                                        if (WNTVN)
                                        {
                                            // *
                                            // *                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
                                            // *                 M left singular vectors to be computed in U and
                                            // *                 no right singular vectors to be computed
                                            // *
                                            if (LWORK >= N * N + Math.Max(N + M, Math.Max(4 * N, BDSPAC)))
                                            {
                                                // *
                                                // *                    Sufficient workspace for a fast algorithm
                                                // *
                                                IR = 1;
                                                if (LWORK >= WRKBL + LDA * N)
                                                {
                                                    // *
                                                    // *                       WORK(IR) is LDA by N
                                                    // *
                                                    LDWRKR = LDA;
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                       WORK(IR) is N by N
                                                    // *
                                                    LDWRKR = N;
                                                }
                                                ITAU = IR + LDWRKR * N;
                                                IWORK = ITAU + N;
                                                // *
                                                // *                    Compute A=Q*R, copying result to U
                                                // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                                // *
                                                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                 , LDU);
                                                // *
                                                // *                    Copy R to WORK(IR), zeroing out below it
                                                // *
                                                this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IR + o_work
                                                                 , LDWRKR);
                                                this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IR + 1 + o_work
                                                                 , LDWRKR);
                                                // *
                                                // *                    Generate Q in U
                                                // *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
                                                // *
                                                this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + N;
                                                ITAUP = ITAUQ + N;
                                                IWORK = ITAUP + N;
                                                // *
                                                // *                    Bidiagonalize R in WORK(IR)
                                                // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                                // *
                                                this._dgebrd.Run(N, N, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate left bidiagonalizing vectors in WORK(IR)
                                                // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                                // *
                                                this._dorgbr.Run("Q", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + N;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing left
                                                // *                    singular vectors of R in WORK(IR)
                                                // *                    (Workspace: need N*N+BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", N, 0, N, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                                // *                    Multiply Q in U by left singular vectors of R in
                                                // *                    WORK(IR), storing result in A
                                                // *                    (Workspace: need N*N)
                                                // *
                                                this._dgemm.Run("N", "N", M, N, N, ONE
                                                                , U, offset_u, LDU, WORK, IR + o_work, LDWRKR, ZERO, ref A, offset_a
                                                                , LDA);
                                                // *
                                                // *                    Copy left singular vectors of A from A to U
                                                // *
                                                this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref U, offset_u
                                                                 , LDU);
                                                // *
                                            }
                                            else
                                            {
                                                // *
                                                // *                    Insufficient workspace for a fast algorithm
                                                // *
                                                ITAU = 1;
                                                IWORK = ITAU + N;
                                                // *
                                                // *                    Compute A=Q*R, copying result to U
                                                // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                // *
                                                this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                 , LDU);
                                                // *
                                                // *                    Generate Q in U
                                                // *                    (Workspace: need N+M, prefer N+M*NB)
                                                // *
                                                this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + N;
                                                ITAUP = ITAUQ + N;
                                                IWORK = ITAUP + N;
                                                // *
                                                // *                    Zero out below R in A
                                                // *
                                                this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                                                 , LDA);
                                                // *
                                                // *                    Bidiagonalize R in A
                                                // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                                // *
                                                this._dgebrd.Run(N, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Multiply Q in U by left bidiagonalizing vectors
                                                // *                    in A
                                                // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                                // *
                                                this._dormbr.Run("Q", "R", "N", M, N, N
                                                                 , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + N;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing left
                                                // *                    singular vectors of A in U
                                                // *                    (Workspace: need BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", N, 0, M, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                            }
                                            // *
                                        }
                                        else
                                        {
                                            if (WNTVO)
                                            {
                                                // *
                                                // *                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
                                                // *                 M left singular vectors to be computed in U and
                                                // *                 N right singular vectors to be overwritten on A
                                                // *
                                                if (LWORK >= 2 * N * N + Math.Max(N + M, Math.Max(4 * N, BDSPAC)))
                                                {
                                                    // *
                                                    // *                    Sufficient workspace for a fast algorithm
                                                    // *
                                                    IU = 1;
                                                    if (LWORK >= WRKBL + 2 * LDA * N)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
                                                        // *
                                                        LDWRKU = LDA;
                                                        IR = IU + LDWRKU * N;
                                                        LDWRKR = LDA;
                                                    }
                                                    else
                                                    {
                                                        if (LWORK >= WRKBL + (LDA + N) * N)
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is LDA by N and WORK(IR) is N by N
                                                            // *
                                                            LDWRKU = LDA;
                                                            IR = IU + LDWRKU * N;
                                                            LDWRKR = N;
                                                        }
                                                        else
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is N by N and WORK(IR) is N by N
                                                            // *
                                                            LDWRKU = N;
                                                            IR = IU + LDWRKU * N;
                                                            LDWRKR = N;
                                                        }
                                                    }
                                                    ITAU = IR + LDWRKR * N;
                                                    IWORK = ITAU + N;
                                                    // *
                                                    // *                    Compute A=Q*R, copying result to U
                                                    // *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
                                                    // *
                                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                     , LDU);
                                                    // *
                                                    // *                    Generate Q in U
                                                    // *                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
                                                    // *
                                                    this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy R to WORK(IU), zeroing out below it
                                                    // *
                                                    this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IU + o_work
                                                                     , LDWRKU);
                                                    this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IU + 1 + o_work
                                                                     , LDWRKU);
                                                    IE = ITAU;
                                                    ITAUQ = IE + N;
                                                    ITAUP = ITAUQ + N;
                                                    IWORK = ITAUP + N;
                                                    // *
                                                    // *                    Bidiagonalize R in WORK(IU), copying result to
                                                    // *                    WORK(IR)
                                                    // *                    (Workspace: need 2*N*N+4*N,
                                                    // *                                prefer 2*N*N+3*N+2*N*NB)
                                                    // *
                                                    this._dgebrd.Run(N, N, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("U", N, N, WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work
                                                                     , LDWRKR);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in WORK(IU)
                                                    // *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", N, N, N, ref WORK, IU + o_work, LDWRKU
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in WORK(IR)
                                                    // *                    (Workspace: need 2*N*N+4*N-1,
                                                    // *                                prefer 2*N*N+3*N+(N-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", N, N, N, ref WORK, IR + o_work, LDWRKR
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + N;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of R in WORK(IU) and computing
                                                    // *                    right singular vectors of R in WORK(IR)
                                                    // *                    (Workspace: need 2*N*N+BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", N, N, N, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref WORK, IU + o_work, LDWRKU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                    // *                    Multiply Q in U by left singular vectors of R in
                                                    // *                    WORK(IU), storing result in A
                                                    // *                    (Workspace: need N*N)
                                                    // *
                                                    this._dgemm.Run("N", "N", M, N, N, ONE
                                                                    , U, offset_u, LDU, WORK, IU + o_work, LDWRKU, ZERO, ref A, offset_a
                                                                    , LDA);
                                                    // *
                                                    // *                    Copy left singular vectors of A from A to U
                                                    // *
                                                    this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref U, offset_u
                                                                     , LDU);
                                                    // *
                                                    // *                    Copy right singular vectors of R from WORK(IR) to A
                                                    // *
                                                    this._dlacpy.Run("F", N, N, WORK, IR + o_work, LDWRKR, ref A, offset_a
                                                                     , LDA);
                                                    // *
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                    Insufficient workspace for a fast algorithm
                                                    // *
                                                    ITAU = 1;
                                                    IWORK = ITAU + N;
                                                    // *
                                                    // *                    Compute A=Q*R, copying result to U
                                                    // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                    // *
                                                    this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                     , LDU);
                                                    // *
                                                    // *                    Generate Q in U
                                                    // *                    (Workspace: need N+M, prefer N+M*NB)
                                                    // *
                                                    this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IE = ITAU;
                                                    ITAUQ = IE + N;
                                                    ITAUP = ITAUQ + N;
                                                    IWORK = ITAUP + N;
                                                    // *
                                                    // *                    Zero out below R in A
                                                    // *
                                                    this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref A, 2+1 * LDA + o_a
                                                                     , LDA);
                                                    // *
                                                    // *                    Bidiagonalize R in A
                                                    // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                                    // *
                                                    this._dgebrd.Run(N, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Multiply Q in U by left bidiagonalizing vectors
                                                    // *                    in A
                                                    // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                                    // *
                                                    this._dormbr.Run("Q", "R", "N", M, N, N
                                                                     , ref A, offset_a, LDA, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in A
                                                    // *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", N, N, N, ref A, offset_a, LDA
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + N;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of A in U and computing right
                                                    // *                    singular vectors of A in A
                                                    // *                    (Workspace: need BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", N, N, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref A, offset_a, LDA, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                }
                                                // *
                                            }
                                            else
                                            {
                                                if (WNTVAS)
                                                {
                                                    // *
                                                    // *                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
                                                    // *                         or 'A')
                                                    // *                 M left singular vectors to be computed in U and
                                                    // *                 N right singular vectors to be computed in VT
                                                    // *
                                                    if (LWORK >= N * N + Math.Max(N + M, Math.Max(4 * N, BDSPAC)))
                                                    {
                                                        // *
                                                        // *                    Sufficient workspace for a fast algorithm
                                                        // *
                                                        IU = 1;
                                                        if (LWORK >= WRKBL + LDA * N)
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is LDA by N
                                                            // *
                                                            LDWRKU = LDA;
                                                        }
                                                        else
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is N by N
                                                            // *
                                                            LDWRKU = N;
                                                        }
                                                        ITAU = IU + LDWRKU * N;
                                                        IWORK = ITAU + N;
                                                        // *
                                                        // *                    Compute A=Q*R, copying result to U
                                                        // *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
                                                        // *
                                                        this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                         , LDU);
                                                        // *
                                                        // *                    Generate Q in U
                                                        // *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
                                                        // *
                                                        this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                         , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Copy R to WORK(IU), zeroing out below it
                                                        // *
                                                        this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref WORK, IU + o_work
                                                                         , LDWRKU);
                                                        this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref WORK, IU + 1 + o_work
                                                                         , LDWRKU);
                                                        IE = ITAU;
                                                        ITAUQ = IE + N;
                                                        ITAUP = ITAUQ + N;
                                                        IWORK = ITAUP + N;
                                                        // *
                                                        // *                    Bidiagonalize R in WORK(IU), copying result to VT
                                                        // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
                                                        // *
                                                        this._dgebrd.Run(N, N, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("U", N, N, WORK, IU + o_work, LDWRKU, ref VT, offset_vt
                                                                         , LDVT);
                                                        // *
                                                        // *                    Generate left bidiagonalizing vectors in WORK(IU)
                                                        // *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
                                                        // *
                                                        this._dorgbr.Run("Q", N, N, N, ref WORK, IU + o_work, LDWRKU
                                                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Generate right bidiagonalizing vectors in VT
                                                        // *                    (Workspace: need N*N+4*N-1,
                                                        // *                                prefer N*N+3*N+(N-1)*NB)
                                                        // *
                                                        this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        IWORK = IE + N;
                                                        // *
                                                        // *                    Perform bidiagonal QR iteration, computing left
                                                        // *                    singular vectors of R in WORK(IU) and computing
                                                        // *                    right singular vectors of R in VT
                                                        // *                    (Workspace: need N*N+BDSPAC)
                                                        // *
                                                        this._dbdsqr.Run("U", N, N, N, 0, ref S, offset_s
                                                                         , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref WORK, IU + o_work, LDWRKU, ref DUM, offset_dum
                                                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                                                        // *
                                                        // *                    Multiply Q in U by left singular vectors of R in
                                                        // *                    WORK(IU), storing result in A
                                                        // *                    (Workspace: need N*N)
                                                        // *
                                                        this._dgemm.Run("N", "N", M, N, N, ONE
                                                                        , U, offset_u, LDU, WORK, IU + o_work, LDWRKU, ZERO, ref A, offset_a
                                                                        , LDA);
                                                        // *
                                                        // *                    Copy left singular vectors of A from A to U
                                                        // *
                                                        this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref U, offset_u
                                                                         , LDU);
                                                        // *
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                    Insufficient workspace for a fast algorithm
                                                        // *
                                                        ITAU = 1;
                                                        IWORK = ITAU + N;
                                                        // *
                                                        // *                    Compute A=Q*R, copying result to U
                                                        // *                    (Workspace: need 2*N, prefer N+N*NB)
                                                        // *
                                                        this._dgeqrf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                                                         , LDU);
                                                        // *
                                                        // *                    Generate Q in U
                                                        // *                    (Workspace: need N+M, prefer N+M*NB)
                                                        // *
                                                        this._dorgqr.Run(M, M, N, ref U, offset_u, LDU, WORK, ITAU + o_work
                                                                         , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Copy R from A to VT, zeroing out below it
                                                        // *
                                                        this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref VT, offset_vt
                                                                         , LDVT);
                                                        if (N > 1)
                                                        {
                                                            this._dlaset.Run("L", N - 1, N - 1, ZERO, ZERO, ref VT, 2+1 * LDVT + o_vt
                                                                             , LDVT);
                                                        }
                                                        IE = ITAU;
                                                        ITAUQ = IE + N;
                                                        ITAUP = ITAUQ + N;
                                                        IWORK = ITAUP + N;
                                                        // *
                                                        // *                    Bidiagonalize R in VT
                                                        // *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
                                                        // *
                                                        this._dgebrd.Run(N, N, ref VT, offset_vt, LDVT, ref S, offset_s, ref WORK, IE + o_work
                                                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Multiply Q in U by left bidiagonalizing vectors
                                                        // *                    in VT
                                                        // *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
                                                        // *
                                                        this._dormbr.Run("Q", "R", "N", M, N, N
                                                                         , ref VT, offset_vt, LDVT, WORK, ITAUQ + o_work, ref U, offset_u, LDU, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Generate right bidiagonalizing vectors in VT
                                                        // *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                                                        // *
                                                        this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        IWORK = IE + N;
                                                        // *
                                                        // *                    Perform bidiagonal QR iteration, computing left
                                                        // *                    singular vectors of A in U and computing right
                                                        // *                    singular vectors of A in VT
                                                        // *                    (Workspace: need BDSPAC)
                                                        // *
                                                        this._dbdsqr.Run("U", N, N, M, 0, ref S, offset_s
                                                                         , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                                                        // *
                                                    }
                                                    // *
                                                }
                                            }
                                        }
                                        // *
                                    }
                                }
                            }
                        }
                    }
                    // *
                }
                else
                {
                    // *
                    // *           M .LT. MNTHR
                    // *
                    // *           Path 10 (M at least N, but not much larger)
                    // *           Reduce to bidiagonal form without QR decomposition
                    // *
                    IE = 1;
                    ITAUQ = IE + N;
                    ITAUP = ITAUQ + N;
                    IWORK = ITAUP + N;
                    // *
                    // *           Bidiagonalize A
                    // *           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
                    // *
                    this._dgebrd.Run(M, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    if (WNTUAS)
                    {
                        // *
                        // *              If left singular vectors desired in U, copy result to U
                        // *              and generate left bidiagonalizing vectors in U
                        // *              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
                        // *
                        this._dlacpy.Run("L", M, N, A, offset_a, LDA, ref U, offset_u
                                         , LDU);
                        if (WNTUS) NCU = N;
                        if (WNTUA) NCU = M;
                        this._dorgbr.Run("Q", M, NCU, N, ref U, offset_u, LDU
                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTVAS)
                    {
                        // *
                        // *              If right singular vectors desired in VT, copy result to
                        // *              VT and generate right bidiagonalizing vectors in VT
                        // *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                        // *
                        this._dlacpy.Run("U", N, N, A, offset_a, LDA, ref VT, offset_vt
                                         , LDVT);
                        this._dorgbr.Run("P", N, N, N, ref VT, offset_vt, LDVT
                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTUO)
                    {
                        // *
                        // *              If left singular vectors desired in A, generate left
                        // *              bidiagonalizing vectors in A
                        // *              (Workspace: need 4*N, prefer 3*N+N*NB)
                        // *
                        this._dorgbr.Run("Q", M, N, N, ref A, offset_a, LDA
                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTVO)
                    {
                        // *
                        // *              If right singular vectors desired in A, generate right
                        // *              bidiagonalizing vectors in A
                        // *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
                        // *
                        this._dorgbr.Run("P", N, N, N, ref A, offset_a, LDA
                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    IWORK = IE + N;
                    if (WNTUAS || WNTUO) NRU = M;
                    if (WNTUN) NRU = 0;
                    if (WNTVAS || WNTVO) NCVT = N;
                    if (WNTVN) NCVT = 0;
                    if ((!WNTUO) && (!WNTVO))
                    {
                        // *
                        // *              Perform bidiagonal QR iteration, if desired, computing
                        // *              left singular vectors in U and computing right singular
                        // *              vectors in VT
                        // *              (Workspace: need BDSPAC)
                        // *
                        this._dbdsqr.Run("U", N, NCVT, NRU, 0, ref S, offset_s
                                         , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                    }
                    else
                    {
                        if ((!WNTUO) && WNTVO)
                        {
                            // *
                            // *              Perform bidiagonal QR iteration, if desired, computing
                            // *              left singular vectors in U and computing right singular
                            // *              vectors in A
                            // *              (Workspace: need BDSPAC)
                            // *
                            this._dbdsqr.Run("U", N, NCVT, NRU, 0, ref S, offset_s
                                             , ref WORK, IE + o_work, ref A, offset_a, LDA, ref U, offset_u, LDU, ref DUM, offset_dum
                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                        }
                        else
                        {
                            // *
                            // *              Perform bidiagonal QR iteration, if desired, computing
                            // *              left singular vectors in A and computing right singular
                            // *              vectors in VT
                            // *              (Workspace: need BDSPAC)
                            // *
                            this._dbdsqr.Run("U", N, NCVT, NRU, 0, ref S, offset_s
                                             , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref A, offset_a, LDA, ref DUM, offset_dum
                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                        }
                    }
                    // *
                }
                // *
            }
            else
            {
                // *
                // *        A has more columns than rows. If A has sufficiently more
                // *        columns than rows, first reduce using the LQ decomposition (if
                // *        sufficient workspace available)
                // *
                if (N >= MNTHR)
                {
                    // *
                    if (WNTVN)
                    {
                        // *
                        // *              Path 1t(N much larger than M, JOBVT='N')
                        // *              No right singular vectors to be computed
                        // *
                        ITAU = 1;
                        IWORK = ITAU + M;
                        // *
                        // *              Compute A=L*Q
                        // *              (Workspace: need 2*M, prefer M+M*NB)
                        // *
                        this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                         , LWORK - IWORK + 1, ref IERR);
                        // *
                        // *              Zero out above L
                        // *
                        this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref A, 1+2 * LDA + o_a
                                         , LDA);
                        IE = 1;
                        ITAUQ = IE + M;
                        ITAUP = ITAUQ + M;
                        IWORK = ITAUP + M;
                        // *
                        // *              Bidiagonalize L in A
                        // *              (Workspace: need 4*M, prefer 3*M+2*M*NB)
                        // *
                        this._dgebrd.Run(M, M, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                        if (WNTUO || WNTUAS)
                        {
                            // *
                            // *                 If left singular vectors desired, generate Q
                            // *                 (Workspace: need 4*M, prefer 3*M+M*NB)
                            // *
                            this._dorgbr.Run("Q", M, M, M, ref A, offset_a, LDA
                                             , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                        }
                        IWORK = IE + M;
                        NRU = 0;
                        if (WNTUO || WNTUAS) NRU = M;
                        // *
                        // *              Perform bidiagonal QR iteration, computing left singular
                        // *              vectors of A in A if desired
                        // *              (Workspace: need BDSPAC)
                        // *
                        this._dbdsqr.Run("U", M, 0, NRU, 0, ref S, offset_s
                                         , ref WORK, IE + o_work, ref DUM, offset_dum, 1, ref A, offset_a, LDA, ref DUM, offset_dum
                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                        // *
                        // *              If left singular vectors desired in U, copy them there
                        // *
                        if (WNTUAS)
                        {
                            this._dlacpy.Run("F", M, M, A, offset_a, LDA, ref U, offset_u
                                             , LDU);
                        }
                        // *
                    }
                    else
                    {
                        if (WNTVO && WNTUN)
                        {
                            // *
                            // *              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
                            // *              M right singular vectors to be overwritten on A and
                            // *              no left singular vectors to be computed
                            // *
                            if (LWORK >= M * M + Math.Max(4 * M, BDSPAC))
                            {
                                // *
                                // *                 Sufficient workspace for a fast algorithm
                                // *
                                IR = 1;
                                if (LWORK >= Math.Max(WRKBL, LDA * N + M) + LDA * M)
                                {
                                    // *
                                    // *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
                                    // *
                                    LDWRKU = LDA;
                                    CHUNK = N;
                                    LDWRKR = LDA;
                                }
                                else
                                {
                                    if (LWORK >= Math.Max(WRKBL, LDA * N + M) + M * M)
                                    {
                                        // *
                                        // *                    WORK(IU) is LDA by N and WORK(IR) is M by M
                                        // *
                                        LDWRKU = LDA;
                                        CHUNK = N;
                                        LDWRKR = M;
                                    }
                                    else
                                    {
                                        // *
                                        // *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
                                        // *
                                        LDWRKU = M;
                                        CHUNK = (LWORK - M * M - M) / M;
                                        LDWRKR = M;
                                    }
                                }
                                ITAU = IR + LDWRKR * M;
                                IWORK = ITAU + M;
                                // *
                                // *                 Compute A=L*Q
                                // *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                // *
                                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                 , LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Copy L to WORK(IR) and zero out above it
                                // *
                                this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IR + o_work
                                                 , LDWRKR);
                                this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IR + LDWRKR + o_work
                                                 , LDWRKR);
                                // *
                                // *                 Generate Q in A
                                // *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                // *
                                this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IE = ITAU;
                                ITAUQ = IE + M;
                                ITAUP = ITAUQ + M;
                                IWORK = ITAUP + M;
                                // *
                                // *                 Bidiagonalize L in WORK(IR)
                                // *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                // *
                                this._dgebrd.Run(M, M, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Generate right vectors bidiagonalizing L
                                // *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
                                // *
                                this._dorgbr.Run("P", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IWORK = IE + M;
                                // *
                                // *                 Perform bidiagonal QR iteration, computing right
                                // *                 singular vectors of L in WORK(IR)
                                // *                 (Workspace: need M*M+BDSPAC)
                                // *
                                this._dbdsqr.Run("U", M, M, 0, 0, ref S, offset_s
                                                 , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                IU = IE + M;
                                // *
                                // *                 Multiply right singular vectors of L in WORK(IR) by Q
                                // *                 in A, storing result in WORK(IU) and copying to A
                                // *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
                                // *
                                for (I = 1; (CHUNK >= 0) ? (I <= N) : (I >= N); I += CHUNK)
                                {
                                    BLK = Math.Min(N - I + 1, CHUNK);
                                    this._dgemm.Run("N", "N", M, BLK, M, ONE
                                                    , WORK, IR + o_work, LDWRKR, A, 1+I * LDA + o_a, LDA, ZERO, ref WORK, IU + o_work
                                                    , LDWRKU);
                                    this._dlacpy.Run("F", M, BLK, WORK, IU + o_work, LDWRKU, ref A, 1+I * LDA + o_a
                                                     , LDA);
                                }
                                // *
                            }
                            else
                            {
                                // *
                                // *                 Insufficient workspace for a fast algorithm
                                // *
                                IE = 1;
                                ITAUQ = IE + M;
                                ITAUP = ITAUQ + M;
                                IWORK = ITAUP + M;
                                // *
                                // *                 Bidiagonalize A
                                // *                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
                                // *
                                this._dgebrd.Run(M, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                // *
                                // *                 Generate right vectors bidiagonalizing A
                                // *                 (Workspace: need 4*M, prefer 3*M+M*NB)
                                // *
                                this._dorgbr.Run("P", M, N, M, ref A, offset_a, LDA
                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                IWORK = IE + M;
                                // *
                                // *                 Perform bidiagonal QR iteration, computing right
                                // *                 singular vectors of A in A
                                // *                 (Workspace: need BDSPAC)
                                // *
                                this._dbdsqr.Run("L", M, N, 0, 0, ref S, offset_s
                                                 , ref WORK, IE + o_work, ref A, offset_a, LDA, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                // *
                            }
                            // *
                        }
                        else
                        {
                            if (WNTVO && WNTUAS)
                            {
                                // *
                                // *              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
                                // *              M right singular vectors to be overwritten on A and
                                // *              M left singular vectors to be computed in U
                                // *
                                if (LWORK >= M * M + Math.Max(4 * M, BDSPAC))
                                {
                                    // *
                                    // *                 Sufficient workspace for a fast algorithm
                                    // *
                                    IR = 1;
                                    if (LWORK >= Math.Max(WRKBL, LDA * N + M) + LDA * M)
                                    {
                                        // *
                                        // *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
                                        // *
                                        LDWRKU = LDA;
                                        CHUNK = N;
                                        LDWRKR = LDA;
                                    }
                                    else
                                    {
                                        if (LWORK >= Math.Max(WRKBL, LDA * N + M) + M * M)
                                        {
                                            // *
                                            // *                    WORK(IU) is LDA by N and WORK(IR) is M by M
                                            // *
                                            LDWRKU = LDA;
                                            CHUNK = N;
                                            LDWRKR = M;
                                        }
                                        else
                                        {
                                            // *
                                            // *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
                                            // *
                                            LDWRKU = M;
                                            CHUNK = (LWORK - M * M - M) / M;
                                            LDWRKR = M;
                                        }
                                    }
                                    ITAU = IR + LDWRKR * M;
                                    IWORK = ITAU + M;
                                    // *
                                    // *                 Compute A=L*Q
                                    // *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                    // *
                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Copy L to U, zeroing about above it
                                    // *
                                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref U, offset_u
                                                     , LDU);
                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref U, 1+2 * LDU + o_u
                                                     , LDU);
                                    // *
                                    // *                 Generate Q in A
                                    // *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                    // *
                                    this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IE = ITAU;
                                    ITAUQ = IE + M;
                                    ITAUP = ITAUQ + M;
                                    IWORK = ITAUP + M;
                                    // *
                                    // *                 Bidiagonalize L in U, copying result to WORK(IR)
                                    // *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                    // *
                                    this._dgebrd.Run(M, M, ref U, offset_u, LDU, ref S, offset_s, ref WORK, IE + o_work
                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    this._dlacpy.Run("U", M, M, U, offset_u, LDU, ref WORK, IR + o_work
                                                     , LDWRKR);
                                    // *
                                    // *                 Generate right vectors bidiagonalizing L in WORK(IR)
                                    // *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
                                    // *
                                    this._dorgbr.Run("P", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Generate left vectors bidiagonalizing L in U
                                    // *                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
                                    // *
                                    this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IWORK = IE + M;
                                    // *
                                    // *                 Perform bidiagonal QR iteration, computing left
                                    // *                 singular vectors of L in U, and computing right
                                    // *                 singular vectors of L in WORK(IR)
                                    // *                 (Workspace: need M*M+BDSPAC)
                                    // *
                                    this._dbdsqr.Run("U", M, M, M, 0, ref S, offset_s
                                                     , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref U, offset_u, LDU, ref DUM, offset_dum
                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                    IU = IE + M;
                                    // *
                                    // *                 Multiply right singular vectors of L in WORK(IR) by Q
                                    // *                 in A, storing result in WORK(IU) and copying to A
                                    // *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
                                    // *
                                    for (I = 1; (CHUNK >= 0) ? (I <= N) : (I >= N); I += CHUNK)
                                    {
                                        BLK = Math.Min(N - I + 1, CHUNK);
                                        this._dgemm.Run("N", "N", M, BLK, M, ONE
                                                        , WORK, IR + o_work, LDWRKR, A, 1+I * LDA + o_a, LDA, ZERO, ref WORK, IU + o_work
                                                        , LDWRKU);
                                        this._dlacpy.Run("F", M, BLK, WORK, IU + o_work, LDWRKU, ref A, 1+I * LDA + o_a
                                                         , LDA);
                                    }
                                    // *
                                }
                                else
                                {
                                    // *
                                    // *                 Insufficient workspace for a fast algorithm
                                    // *
                                    ITAU = 1;
                                    IWORK = ITAU + M;
                                    // *
                                    // *                 Compute A=L*Q
                                    // *                 (Workspace: need 2*M, prefer M+M*NB)
                                    // *
                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Copy L to U, zeroing out above it
                                    // *
                                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref U, offset_u
                                                     , LDU);
                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref U, 1+2 * LDU + o_u
                                                     , LDU);
                                    // *
                                    // *                 Generate Q in A
                                    // *                 (Workspace: need 2*M, prefer M+M*NB)
                                    // *
                                    this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IE = ITAU;
                                    ITAUQ = IE + M;
                                    ITAUP = ITAUQ + M;
                                    IWORK = ITAUP + M;
                                    // *
                                    // *                 Bidiagonalize L in U
                                    // *                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                    // *
                                    this._dgebrd.Run(M, M, ref U, offset_u, LDU, ref S, offset_s, ref WORK, IE + o_work
                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Multiply right vectors bidiagonalizing L by Q in A
                                    // *                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                    // *
                                    this._dormbr.Run("P", "L", "T", M, N, M
                                                     , ref U, offset_u, LDU, WORK, ITAUP + o_work, ref A, offset_a, LDA, ref WORK, IWORK + o_work
                                                     , LWORK - IWORK + 1, ref IERR);
                                    // *
                                    // *                 Generate left vectors bidiagonalizing L in U
                                    // *                 (Workspace: need 4*M, prefer 3*M+M*NB)
                                    // *
                                    this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                    IWORK = IE + M;
                                    // *
                                    // *                 Perform bidiagonal QR iteration, computing left
                                    // *                 singular vectors of A in U and computing right
                                    // *                 singular vectors of A in A
                                    // *                 (Workspace: need BDSPAC)
                                    // *
                                    this._dbdsqr.Run("U", M, N, M, 0, ref S, offset_s
                                                     , ref WORK, IE + o_work, ref A, offset_a, LDA, ref U, offset_u, LDU, ref DUM, offset_dum
                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                    // *
                                }
                                // *
                            }
                            else
                            {
                                if (WNTVS)
                                {
                                    // *
                                    if (WNTUN)
                                    {
                                        // *
                                        // *                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
                                        // *                 M right singular vectors to be computed in VT and
                                        // *                 no left singular vectors to be computed
                                        // *
                                        if (LWORK >= M * M + Math.Max(4 * M, BDSPAC))
                                        {
                                            // *
                                            // *                    Sufficient workspace for a fast algorithm
                                            // *
                                            IR = 1;
                                            if (LWORK >= WRKBL + LDA * M)
                                            {
                                                // *
                                                // *                       WORK(IR) is LDA by M
                                                // *
                                                LDWRKR = LDA;
                                            }
                                            else
                                            {
                                                // *
                                                // *                       WORK(IR) is M by M
                                                // *
                                                LDWRKR = M;
                                            }
                                            ITAU = IR + LDWRKR * M;
                                            IWORK = ITAU + M;
                                            // *
                                            // *                    Compute A=L*Q
                                            // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                            // *
                                            this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Copy L to WORK(IR), zeroing out above it
                                            // *
                                            this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IR + o_work
                                                             , LDWRKR);
                                            this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IR + LDWRKR + o_work
                                                             , LDWRKR);
                                            // *
                                            // *                    Generate Q in A
                                            // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                            // *
                                            this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                             , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IE = ITAU;
                                            ITAUQ = IE + M;
                                            ITAUP = ITAUQ + M;
                                            IWORK = ITAUP + M;
                                            // *
                                            // *                    Bidiagonalize L in WORK(IR)
                                            // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                            // *
                                            this._dgebrd.Run(M, M, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                             , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Generate right vectors bidiagonalizing L in
                                            // *                    WORK(IR)
                                            // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
                                            // *
                                            this._dorgbr.Run("P", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                             , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IWORK = IE + M;
                                            // *
                                            // *                    Perform bidiagonal QR iteration, computing right
                                            // *                    singular vectors of L in WORK(IR)
                                            // *                    (Workspace: need M*M+BDSPAC)
                                            // *
                                            this._dbdsqr.Run("U", M, M, 0, 0, ref S, offset_s
                                                             , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                                            // *
                                            // *                    Multiply right singular vectors of L in WORK(IR) by
                                            // *                    Q in A, storing result in VT
                                            // *                    (Workspace: need M*M)
                                            // *
                                            this._dgemm.Run("N", "N", M, N, M, ONE
                                                            , WORK, IR + o_work, LDWRKR, A, offset_a, LDA, ZERO, ref VT, offset_vt
                                                            , LDVT);
                                            // *
                                        }
                                        else
                                        {
                                            // *
                                            // *                    Insufficient workspace for a fast algorithm
                                            // *
                                            ITAU = 1;
                                            IWORK = ITAU + M;
                                            // *
                                            // *                    Compute A=L*Q
                                            // *                    (Workspace: need 2*M, prefer M+M*NB)
                                            // *
                                            this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Copy result to VT
                                            // *
                                            this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                             , LDVT);
                                            // *
                                            // *                    Generate Q in VT
                                            // *                    (Workspace: need 2*M, prefer M+M*NB)
                                            // *
                                            this._dorglq.Run(M, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                             , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            IE = ITAU;
                                            ITAUQ = IE + M;
                                            ITAUP = ITAUQ + M;
                                            IWORK = ITAUP + M;
                                            // *
                                            // *                    Zero out above L in A
                                            // *
                                            this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref A, 1+2 * LDA + o_a
                                                             , LDA);
                                            // *
                                            // *                    Bidiagonalize L in A
                                            // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                            // *
                                            this._dgebrd.Run(M, M, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                             , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                            // *
                                            // *                    Multiply right vectors bidiagonalizing L by Q in VT
                                            // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                            // *
                                            this._dormbr.Run("P", "L", "T", M, N, M
                                                             , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                             , LWORK - IWORK + 1, ref IERR);
                                            IWORK = IE + M;
                                            // *
                                            // *                    Perform bidiagonal QR iteration, computing right
                                            // *                    singular vectors of A in VT
                                            // *                    (Workspace: need BDSPAC)
                                            // *
                                            this._dbdsqr.Run("U", M, N, 0, 0, ref S, offset_s
                                                             , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                                            // *
                                        }
                                        // *
                                    }
                                    else
                                    {
                                        if (WNTUO)
                                        {
                                            // *
                                            // *                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
                                            // *                 M right singular vectors to be computed in VT and
                                            // *                 M left singular vectors to be overwritten on A
                                            // *
                                            if (LWORK >= 2 * M * M + Math.Max(4 * M, BDSPAC))
                                            {
                                                // *
                                                // *                    Sufficient workspace for a fast algorithm
                                                // *
                                                IU = 1;
                                                if (LWORK >= WRKBL + 2 * LDA * M)
                                                {
                                                    // *
                                                    // *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
                                                    // *
                                                    LDWRKU = LDA;
                                                    IR = IU + LDWRKU * M;
                                                    LDWRKR = LDA;
                                                }
                                                else
                                                {
                                                    if (LWORK >= WRKBL + (LDA + M) * M)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by M and WORK(IR) is M by M
                                                        // *
                                                        LDWRKU = LDA;
                                                        IR = IU + LDWRKU * M;
                                                        LDWRKR = M;
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is M by M and WORK(IR) is M by M
                                                        // *
                                                        LDWRKU = M;
                                                        IR = IU + LDWRKU * M;
                                                        LDWRKR = M;
                                                    }
                                                }
                                                ITAU = IR + LDWRKR * M;
                                                IWORK = ITAU + M;
                                                // *
                                                // *                    Compute A=L*Q
                                                // *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
                                                // *
                                                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Copy L to WORK(IU), zeroing out below it
                                                // *
                                                this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IU + o_work
                                                                 , LDWRKU);
                                                this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IU + LDWRKU + o_work
                                                                 , LDWRKU);
                                                // *
                                                // *                    Generate Q in A
                                                // *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
                                                // *
                                                this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + M;
                                                ITAUP = ITAUQ + M;
                                                IWORK = ITAUP + M;
                                                // *
                                                // *                    Bidiagonalize L in WORK(IU), copying result to
                                                // *                    WORK(IR)
                                                // *                    (Workspace: need 2*M*M+4*M,
                                                // *                                prefer 2*M*M+3*M+2*M*NB)
                                                // *
                                                this._dgebrd.Run(M, M, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("L", M, M, WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work
                                                                 , LDWRKR);
                                                // *
                                                // *                    Generate right bidiagonalizing vectors in WORK(IU)
                                                // *                    (Workspace: need 2*M*M+4*M-1,
                                                // *                                prefer 2*M*M+3*M+(M-1)*NB)
                                                // *
                                                this._dorgbr.Run("P", M, M, M, ref WORK, IU + o_work, LDWRKU
                                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate left bidiagonalizing vectors in WORK(IR)
                                                // *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
                                                // *
                                                this._dorgbr.Run("Q", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + M;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing left
                                                // *                    singular vectors of L in WORK(IR) and computing
                                                // *                    right singular vectors of L in WORK(IU)
                                                // *                    (Workspace: need 2*M*M+BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", M, M, M, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                                // *                    Multiply right singular vectors of L in WORK(IU) by
                                                // *                    Q in A, storing result in VT
                                                // *                    (Workspace: need M*M)
                                                // *
                                                this._dgemm.Run("N", "N", M, N, M, ONE
                                                                , WORK, IU + o_work, LDWRKU, A, offset_a, LDA, ZERO, ref VT, offset_vt
                                                                , LDVT);
                                                // *
                                                // *                    Copy left singular vectors of L to A
                                                // *                    (Workspace: need M*M)
                                                // *
                                                this._dlacpy.Run("F", M, M, WORK, IR + o_work, LDWRKR, ref A, offset_a
                                                                 , LDA);
                                                // *
                                            }
                                            else
                                            {
                                                // *
                                                // *                    Insufficient workspace for a fast algorithm
                                                // *
                                                ITAU = 1;
                                                IWORK = ITAU + M;
                                                // *
                                                // *                    Compute A=L*Q, copying result to VT
                                                // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                // *
                                                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                 , LDVT);
                                                // *
                                                // *                    Generate Q in VT
                                                // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                // *
                                                this._dorglq.Run(M, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + M;
                                                ITAUP = ITAUQ + M;
                                                IWORK = ITAUP + M;
                                                // *
                                                // *                    Zero out above L in A
                                                // *
                                                this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref A, 1+2 * LDA + o_a
                                                                 , LDA);
                                                // *
                                                // *                    Bidiagonalize L in A
                                                // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                                // *
                                                this._dgebrd.Run(M, M, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Multiply right vectors bidiagonalizing L by Q in VT
                                                // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                                // *
                                                this._dormbr.Run("P", "L", "T", M, N, M
                                                                 , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate left bidiagonalizing vectors of L in A
                                                // *                    (Workspace: need 4*M, prefer 3*M+M*NB)
                                                // *
                                                this._dorgbr.Run("Q", M, M, M, ref A, offset_a, LDA
                                                                 , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + M;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, compute left
                                                // *                    singular vectors of A in A and compute right
                                                // *                    singular vectors of A in VT
                                                // *                    (Workspace: need BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", M, N, M, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref A, offset_a, LDA, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                            }
                                            // *
                                        }
                                        else
                                        {
                                            if (WNTUAS)
                                            {
                                                // *
                                                // *                 Path 6t(N much larger than M, JOBU='S' or 'A',
                                                // *                         JOBVT='S')
                                                // *                 M right singular vectors to be computed in VT and
                                                // *                 M left singular vectors to be computed in U
                                                // *
                                                if (LWORK >= M * M + Math.Max(4 * M, BDSPAC))
                                                {
                                                    // *
                                                    // *                    Sufficient workspace for a fast algorithm
                                                    // *
                                                    IU = 1;
                                                    if (LWORK >= WRKBL + LDA * M)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by N
                                                        // *
                                                        LDWRKU = LDA;
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by M
                                                        // *
                                                        LDWRKU = M;
                                                    }
                                                    ITAU = IU + LDWRKU * M;
                                                    IWORK = ITAU + M;
                                                    // *
                                                    // *                    Compute A=L*Q
                                                    // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                                    // *
                                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy L to WORK(IU), zeroing out above it
                                                    // *
                                                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IU + o_work
                                                                     , LDWRKU);
                                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IU + LDWRKU + o_work
                                                                     , LDWRKU);
                                                    // *
                                                    // *                    Generate Q in A
                                                    // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                                    // *
                                                    this._dorglq.Run(M, N, M, ref A, offset_a, LDA, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IE = ITAU;
                                                    ITAUQ = IE + M;
                                                    ITAUP = ITAUQ + M;
                                                    IWORK = ITAUP + M;
                                                    // *
                                                    // *                    Bidiagonalize L in WORK(IU), copying result to U
                                                    // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                                    // *
                                                    this._dgebrd.Run(M, M, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("L", M, M, WORK, IU + o_work, LDWRKU, ref U, offset_u
                                                                     , LDU);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in WORK(IU)
                                                    // *                    (Workspace: need M*M+4*M-1,
                                                    // *                                prefer M*M+3*M+(M-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", M, M, M, ref WORK, IU + o_work, LDWRKU
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in U
                                                    // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + M;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of L in U and computing right
                                                    // *                    singular vectors of L in WORK(IU)
                                                    // *                    (Workspace: need M*M+BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", M, M, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref WORK, IU + o_work, LDWRKU, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                    // *                    Multiply right singular vectors of L in WORK(IU) by
                                                    // *                    Q in A, storing result in VT
                                                    // *                    (Workspace: need M*M)
                                                    // *
                                                    this._dgemm.Run("N", "N", M, N, M, ONE
                                                                    , WORK, IU + o_work, LDWRKU, A, offset_a, LDA, ZERO, ref VT, offset_vt
                                                                    , LDVT);
                                                    // *
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                    Insufficient workspace for a fast algorithm
                                                    // *
                                                    ITAU = 1;
                                                    IWORK = ITAU + M;
                                                    // *
                                                    // *                    Compute A=L*Q, copying result to VT
                                                    // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                    // *
                                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                     , LDVT);
                                                    // *
                                                    // *                    Generate Q in VT
                                                    // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                    // *
                                                    this._dorglq.Run(M, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy L to U, zeroing out above it
                                                    // *
                                                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref U, offset_u
                                                                     , LDU);
                                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref U, 1+2 * LDU + o_u
                                                                     , LDU);
                                                    IE = ITAU;
                                                    ITAUQ = IE + M;
                                                    ITAUP = ITAUQ + M;
                                                    IWORK = ITAUP + M;
                                                    // *
                                                    // *                    Bidiagonalize L in U
                                                    // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                                    // *
                                                    this._dgebrd.Run(M, M, ref U, offset_u, LDU, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Multiply right bidiagonalizing vectors in U by Q
                                                    // *                    in VT
                                                    // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                                    // *
                                                    this._dormbr.Run("P", "L", "T", M, N, M
                                                                     , ref U, offset_u, LDU, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in U
                                                    // *                    (Workspace: need 4*M, prefer 3*M+M*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + M;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of A in U and computing right
                                                    // *                    singular vectors of A in VT
                                                    // *                    (Workspace: need BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", M, N, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                }
                                                // *
                                            }
                                        }
                                    }
                                    // *
                                }
                                else
                                {
                                    if (WNTVA)
                                    {
                                        // *
                                        if (WNTUN)
                                        {
                                            // *
                                            // *                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
                                            // *                 N right singular vectors to be computed in VT and
                                            // *                 no left singular vectors to be computed
                                            // *
                                            if (LWORK >= M * M + Math.Max(N + M, Math.Max(4 * M, BDSPAC)))
                                            {
                                                // *
                                                // *                    Sufficient workspace for a fast algorithm
                                                // *
                                                IR = 1;
                                                if (LWORK >= WRKBL + LDA * M)
                                                {
                                                    // *
                                                    // *                       WORK(IR) is LDA by M
                                                    // *
                                                    LDWRKR = LDA;
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                       WORK(IR) is M by M
                                                    // *
                                                    LDWRKR = M;
                                                }
                                                ITAU = IR + LDWRKR * M;
                                                IWORK = ITAU + M;
                                                // *
                                                // *                    Compute A=L*Q, copying result to VT
                                                // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                                // *
                                                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                 , LDVT);
                                                // *
                                                // *                    Copy L to WORK(IR), zeroing out above it
                                                // *
                                                this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IR + o_work
                                                                 , LDWRKR);
                                                this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IR + LDWRKR + o_work
                                                                 , LDWRKR);
                                                // *
                                                // *                    Generate Q in VT
                                                // *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
                                                // *
                                                this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + M;
                                                ITAUP = ITAUQ + M;
                                                IWORK = ITAUP + M;
                                                // *
                                                // *                    Bidiagonalize L in WORK(IR)
                                                // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                                // *
                                                this._dgebrd.Run(M, M, ref WORK, IR + o_work, LDWRKR, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Generate right bidiagonalizing vectors in WORK(IR)
                                                // *                    (Workspace: need M*M+4*M-1,
                                                // *                                prefer M*M+3*M+(M-1)*NB)
                                                // *
                                                this._dorgbr.Run("P", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                                 , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + M;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing right
                                                // *                    singular vectors of L in WORK(IR)
                                                // *                    (Workspace: need M*M+BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", M, M, 0, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                                // *                    Multiply right singular vectors of L in WORK(IR) by
                                                // *                    Q in VT, storing result in A
                                                // *                    (Workspace: need M*M)
                                                // *
                                                this._dgemm.Run("N", "N", M, N, M, ONE
                                                                , WORK, IR + o_work, LDWRKR, VT, offset_vt, LDVT, ZERO, ref A, offset_a
                                                                , LDA);
                                                // *
                                                // *                    Copy right singular vectors of A from A to VT
                                                // *
                                                this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                 , LDVT);
                                                // *
                                            }
                                            else
                                            {
                                                // *
                                                // *                    Insufficient workspace for a fast algorithm
                                                // *
                                                ITAU = 1;
                                                IWORK = ITAU + M;
                                                // *
                                                // *                    Compute A=L*Q, copying result to VT
                                                // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                // *
                                                this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                 , LDVT);
                                                // *
                                                // *                    Generate Q in VT
                                                // *                    (Workspace: need M+N, prefer M+N*NB)
                                                // *
                                                this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                 , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                IE = ITAU;
                                                ITAUQ = IE + M;
                                                ITAUP = ITAUQ + M;
                                                IWORK = ITAUP + M;
                                                // *
                                                // *                    Zero out above L in A
                                                // *
                                                this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref A, 1+2 * LDA + o_a
                                                                 , LDA);
                                                // *
                                                // *                    Bidiagonalize L in A
                                                // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                                // *
                                                this._dgebrd.Run(M, M, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                 , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                // *
                                                // *                    Multiply right bidiagonalizing vectors in A by Q
                                                // *                    in VT
                                                // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                                // *
                                                this._dormbr.Run("P", "L", "T", M, N, M
                                                                 , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                                 , LWORK - IWORK + 1, ref IERR);
                                                IWORK = IE + M;
                                                // *
                                                // *                    Perform bidiagonal QR iteration, computing right
                                                // *                    singular vectors of A in VT
                                                // *                    (Workspace: need BDSPAC)
                                                // *
                                                this._dbdsqr.Run("U", M, N, 0, 0, ref S, offset_s
                                                                 , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref DUM, offset_dum, 1, ref DUM, offset_dum
                                                                 , 1, ref WORK, IWORK + o_work, ref INFO);
                                                // *
                                            }
                                            // *
                                        }
                                        else
                                        {
                                            if (WNTUO)
                                            {
                                                // *
                                                // *                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
                                                // *                 N right singular vectors to be computed in VT and
                                                // *                 M left singular vectors to be overwritten on A
                                                // *
                                                if (LWORK >= 2 * M * M + Math.Max(N + M, Math.Max(4 * M, BDSPAC)))
                                                {
                                                    // *
                                                    // *                    Sufficient workspace for a fast algorithm
                                                    // *
                                                    IU = 1;
                                                    if (LWORK >= WRKBL + 2 * LDA * M)
                                                    {
                                                        // *
                                                        // *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
                                                        // *
                                                        LDWRKU = LDA;
                                                        IR = IU + LDWRKU * M;
                                                        LDWRKR = LDA;
                                                    }
                                                    else
                                                    {
                                                        if (LWORK >= WRKBL + (LDA + M) * M)
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is LDA by M and WORK(IR) is M by M
                                                            // *
                                                            LDWRKU = LDA;
                                                            IR = IU + LDWRKU * M;
                                                            LDWRKR = M;
                                                        }
                                                        else
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is M by M and WORK(IR) is M by M
                                                            // *
                                                            LDWRKU = M;
                                                            IR = IU + LDWRKU * M;
                                                            LDWRKR = M;
                                                        }
                                                    }
                                                    ITAU = IR + LDWRKR * M;
                                                    IWORK = ITAU + M;
                                                    // *
                                                    // *                    Compute A=L*Q, copying result to VT
                                                    // *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
                                                    // *
                                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                     , LDVT);
                                                    // *
                                                    // *                    Generate Q in VT
                                                    // *                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
                                                    // *
                                                    this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Copy L to WORK(IU), zeroing out above it
                                                    // *
                                                    this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IU + o_work
                                                                     , LDWRKU);
                                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IU + LDWRKU + o_work
                                                                     , LDWRKU);
                                                    IE = ITAU;
                                                    ITAUQ = IE + M;
                                                    ITAUP = ITAUQ + M;
                                                    IWORK = ITAUP + M;
                                                    // *
                                                    // *                    Bidiagonalize L in WORK(IU), copying result to
                                                    // *                    WORK(IR)
                                                    // *                    (Workspace: need 2*M*M+4*M,
                                                    // *                                prefer 2*M*M+3*M+2*M*NB)
                                                    // *
                                                    this._dgebrd.Run(M, M, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("L", M, M, WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work
                                                                     , LDWRKR);
                                                    // *
                                                    // *                    Generate right bidiagonalizing vectors in WORK(IU)
                                                    // *                    (Workspace: need 2*M*M+4*M-1,
                                                    // *                                prefer 2*M*M+3*M+(M-1)*NB)
                                                    // *
                                                    this._dorgbr.Run("P", M, M, M, ref WORK, IU + o_work, LDWRKU
                                                                     , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in WORK(IR)
                                                    // *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", M, M, M, ref WORK, IR + o_work, LDWRKR
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + M;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of L in WORK(IR) and computing
                                                    // *                    right singular vectors of L in WORK(IU)
                                                    // *                    (Workspace: need 2*M*M+BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", M, M, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref WORK, IU + o_work, LDWRKU, ref WORK, IR + o_work, LDWRKR, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                    // *                    Multiply right singular vectors of L in WORK(IU) by
                                                    // *                    Q in VT, storing result in A
                                                    // *                    (Workspace: need M*M)
                                                    // *
                                                    this._dgemm.Run("N", "N", M, N, M, ONE
                                                                    , WORK, IU + o_work, LDWRKU, VT, offset_vt, LDVT, ZERO, ref A, offset_a
                                                                    , LDA);
                                                    // *
                                                    // *                    Copy right singular vectors of A from A to VT
                                                    // *
                                                    this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                     , LDVT);
                                                    // *
                                                    // *                    Copy left singular vectors of A from WORK(IR) to A
                                                    // *
                                                    this._dlacpy.Run("F", M, M, WORK, IR + o_work, LDWRKR, ref A, offset_a
                                                                     , LDA);
                                                    // *
                                                }
                                                else
                                                {
                                                    // *
                                                    // *                    Insufficient workspace for a fast algorithm
                                                    // *
                                                    ITAU = 1;
                                                    IWORK = ITAU + M;
                                                    // *
                                                    // *                    Compute A=L*Q, copying result to VT
                                                    // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                    // *
                                                    this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                     , LDVT);
                                                    // *
                                                    // *                    Generate Q in VT
                                                    // *                    (Workspace: need M+N, prefer M+N*NB)
                                                    // *
                                                    this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                     , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IE = ITAU;
                                                    ITAUQ = IE + M;
                                                    ITAUP = ITAUQ + M;
                                                    IWORK = ITAUP + M;
                                                    // *
                                                    // *                    Zero out above L in A
                                                    // *
                                                    this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref A, 1+2 * LDA + o_a
                                                                     , LDA);
                                                    // *
                                                    // *                    Bidiagonalize L in A
                                                    // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                                    // *
                                                    this._dgebrd.Run(M, M, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Multiply right bidiagonalizing vectors in A by Q
                                                    // *                    in VT
                                                    // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                                    // *
                                                    this._dormbr.Run("P", "L", "T", M, N, M
                                                                     , ref A, offset_a, LDA, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                                     , LWORK - IWORK + 1, ref IERR);
                                                    // *
                                                    // *                    Generate left bidiagonalizing vectors in A
                                                    // *                    (Workspace: need 4*M, prefer 3*M+M*NB)
                                                    // *
                                                    this._dorgbr.Run("Q", M, M, M, ref A, offset_a, LDA
                                                                     , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                    IWORK = IE + M;
                                                    // *
                                                    // *                    Perform bidiagonal QR iteration, computing left
                                                    // *                    singular vectors of A in A and computing right
                                                    // *                    singular vectors of A in VT
                                                    // *                    (Workspace: need BDSPAC)
                                                    // *
                                                    this._dbdsqr.Run("U", M, N, M, 0, ref S, offset_s
                                                                     , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref A, offset_a, LDA, ref DUM, offset_dum
                                                                     , 1, ref WORK, IWORK + o_work, ref INFO);
                                                    // *
                                                }
                                                // *
                                            }
                                            else
                                            {
                                                if (WNTUAS)
                                                {
                                                    // *
                                                    // *                 Path 9t(N much larger than M, JOBU='S' or 'A',
                                                    // *                         JOBVT='A')
                                                    // *                 N right singular vectors to be computed in VT and
                                                    // *                 M left singular vectors to be computed in U
                                                    // *
                                                    if (LWORK >= M * M + Math.Max(N + M, Math.Max(4 * M, BDSPAC)))
                                                    {
                                                        // *
                                                        // *                    Sufficient workspace for a fast algorithm
                                                        // *
                                                        IU = 1;
                                                        if (LWORK >= WRKBL + LDA * M)
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is LDA by M
                                                            // *
                                                            LDWRKU = LDA;
                                                        }
                                                        else
                                                        {
                                                            // *
                                                            // *                       WORK(IU) is M by M
                                                            // *
                                                            LDWRKU = M;
                                                        }
                                                        ITAU = IU + LDWRKU * M;
                                                        IWORK = ITAU + M;
                                                        // *
                                                        // *                    Compute A=L*Q, copying result to VT
                                                        // *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
                                                        // *
                                                        this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                         , LDVT);
                                                        // *
                                                        // *                    Generate Q in VT
                                                        // *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
                                                        // *
                                                        this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                         , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Copy L to WORK(IU), zeroing out above it
                                                        // *
                                                        this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref WORK, IU + o_work
                                                                         , LDWRKU);
                                                        this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref WORK, IU + LDWRKU + o_work
                                                                         , LDWRKU);
                                                        IE = ITAU;
                                                        ITAUQ = IE + M;
                                                        ITAUP = ITAUQ + M;
                                                        IWORK = ITAUP + M;
                                                        // *
                                                        // *                    Bidiagonalize L in WORK(IU), copying result to U
                                                        // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
                                                        // *
                                                        this._dgebrd.Run(M, M, ref WORK, IU + o_work, LDWRKU, ref S, offset_s, ref WORK, IE + o_work
                                                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("L", M, M, WORK, IU + o_work, LDWRKU, ref U, offset_u
                                                                         , LDU);
                                                        // *
                                                        // *                    Generate right bidiagonalizing vectors in WORK(IU)
                                                        // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
                                                        // *
                                                        this._dorgbr.Run("P", M, M, M, ref WORK, IU + o_work, LDWRKU
                                                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Generate left bidiagonalizing vectors in U
                                                        // *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
                                                        // *
                                                        this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        IWORK = IE + M;
                                                        // *
                                                        // *                    Perform bidiagonal QR iteration, computing left
                                                        // *                    singular vectors of L in U and computing right
                                                        // *                    singular vectors of L in WORK(IU)
                                                        // *                    (Workspace: need M*M+BDSPAC)
                                                        // *
                                                        this._dbdsqr.Run("U", M, M, M, 0, ref S, offset_s
                                                                         , ref WORK, IE + o_work, ref WORK, IU + o_work, LDWRKU, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                                                        // *
                                                        // *                    Multiply right singular vectors of L in WORK(IU) by
                                                        // *                    Q in VT, storing result in A
                                                        // *                    (Workspace: need M*M)
                                                        // *
                                                        this._dgemm.Run("N", "N", M, N, M, ONE
                                                                        , WORK, IU + o_work, LDWRKU, VT, offset_vt, LDVT, ZERO, ref A, offset_a
                                                                        , LDA);
                                                        // *
                                                        // *                    Copy right singular vectors of A from A to VT
                                                        // *
                                                        this._dlacpy.Run("F", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                         , LDVT);
                                                        // *
                                                    }
                                                    else
                                                    {
                                                        // *
                                                        // *                    Insufficient workspace for a fast algorithm
                                                        // *
                                                        ITAU = 1;
                                                        IWORK = ITAU + M;
                                                        // *
                                                        // *                    Compute A=L*Q, copying result to VT
                                                        // *                    (Workspace: need 2*M, prefer M+M*NB)
                                                        // *
                                                        this._dgelqf.Run(M, N, ref A, offset_a, LDA, ref WORK, ITAU + o_work, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                                                         , LDVT);
                                                        // *
                                                        // *                    Generate Q in VT
                                                        // *                    (Workspace: need M+N, prefer M+N*NB)
                                                        // *
                                                        this._dorglq.Run(N, N, M, ref VT, offset_vt, LDVT, WORK, ITAU + o_work
                                                                         , ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Copy L to U, zeroing out above it
                                                        // *
                                                        this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref U, offset_u
                                                                         , LDU);
                                                        this._dlaset.Run("U", M - 1, M - 1, ZERO, ZERO, ref U, 1+2 * LDU + o_u
                                                                         , LDU);
                                                        IE = ITAU;
                                                        ITAUQ = IE + M;
                                                        ITAUP = ITAUQ + M;
                                                        IWORK = ITAUP + M;
                                                        // *
                                                        // *                    Bidiagonalize L in U
                                                        // *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
                                                        // *
                                                        this._dgebrd.Run(M, M, ref U, offset_u, LDU, ref S, offset_s, ref WORK, IE + o_work
                                                                         , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Multiply right bidiagonalizing vectors in U by Q
                                                        // *                    in VT
                                                        // *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
                                                        // *
                                                        this._dormbr.Run("P", "L", "T", M, N, M
                                                                         , ref U, offset_u, LDU, WORK, ITAUP + o_work, ref VT, offset_vt, LDVT, ref WORK, IWORK + o_work
                                                                         , LWORK - IWORK + 1, ref IERR);
                                                        // *
                                                        // *                    Generate left bidiagonalizing vectors in U
                                                        // *                    (Workspace: need 4*M, prefer 3*M+M*NB)
                                                        // *
                                                        this._dorgbr.Run("Q", M, M, M, ref U, offset_u, LDU
                                                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                                                        IWORK = IE + M;
                                                        // *
                                                        // *                    Perform bidiagonal QR iteration, computing left
                                                        // *                    singular vectors of A in U and computing right
                                                        // *                    singular vectors of A in VT
                                                        // *                    (Workspace: need BDSPAC)
                                                        // *
                                                        this._dbdsqr.Run("U", M, N, M, 0, ref S, offset_s
                                                                         , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                                                        // *
                                                    }
                                                    // *
                                                }
                                            }
                                        }
                                        // *
                                    }
                                }
                            }
                        }
                    }
                    // *
                }
                else
                {
                    // *
                    // *           N .LT. MNTHR
                    // *
                    // *           Path 10t(N greater than M, but not much larger)
                    // *           Reduce to bidiagonal form without LQ decomposition
                    // *
                    IE = 1;
                    ITAUQ = IE + M;
                    ITAUP = ITAUQ + M;
                    IWORK = ITAUP + M;
                    // *
                    // *           Bidiagonalize A
                    // *           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
                    // *
                    this._dgebrd.Run(M, N, ref A, offset_a, LDA, ref S, offset_s, ref WORK, IE + o_work
                                     , ref WORK, ITAUQ + o_work, ref WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    if (WNTUAS)
                    {
                        // *
                        // *              If left singular vectors desired in U, copy result to U
                        // *              and generate left bidiagonalizing vectors in U
                        // *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
                        // *
                        this._dlacpy.Run("L", M, M, A, offset_a, LDA, ref U, offset_u
                                         , LDU);
                        this._dorgbr.Run("Q", M, M, N, ref U, offset_u, LDU
                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTVAS)
                    {
                        // *
                        // *              If right singular vectors desired in VT, copy result to
                        // *              VT and generate right bidiagonalizing vectors in VT
                        // *              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
                        // *
                        this._dlacpy.Run("U", M, N, A, offset_a, LDA, ref VT, offset_vt
                                         , LDVT);
                        if (WNTVA) NRVT = N;
                        if (WNTVS) NRVT = M;
                        this._dorgbr.Run("P", NRVT, N, M, ref VT, offset_vt, LDVT
                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTUO)
                    {
                        // *
                        // *              If left singular vectors desired in A, generate left
                        // *              bidiagonalizing vectors in A
                        // *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
                        // *
                        this._dorgbr.Run("Q", M, M, N, ref A, offset_a, LDA
                                         , WORK, ITAUQ + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    if (WNTVO)
                    {
                        // *
                        // *              If right singular vectors desired in A, generate right
                        // *              bidiagonalizing vectors in A
                        // *              (Workspace: need 4*M, prefer 3*M+M*NB)
                        // *
                        this._dorgbr.Run("P", M, N, M, ref A, offset_a, LDA
                                         , WORK, ITAUP + o_work, ref WORK, IWORK + o_work, LWORK - IWORK + 1, ref IERR);
                    }
                    IWORK = IE + M;
                    if (WNTUAS || WNTUO) NRU = M;
                    if (WNTUN) NRU = 0;
                    if (WNTVAS || WNTVO) NCVT = N;
                    if (WNTVN) NCVT = 0;
                    if ((!WNTUO) && (!WNTVO))
                    {
                        // *
                        // *              Perform bidiagonal QR iteration, if desired, computing
                        // *              left singular vectors in U and computing right singular
                        // *              vectors in VT
                        // *              (Workspace: need BDSPAC)
                        // *
                        this._dbdsqr.Run("L", M, NCVT, NRU, 0, ref S, offset_s
                                         , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref DUM, offset_dum
                                         , 1, ref WORK, IWORK + o_work, ref INFO);
                    }
                    else
                    {
                        if ((!WNTUO) && WNTVO)
                        {
                            // *
                            // *              Perform bidiagonal QR iteration, if desired, computing
                            // *              left singular vectors in U and computing right singular
                            // *              vectors in A
                            // *              (Workspace: need BDSPAC)
                            // *
                            this._dbdsqr.Run("L", M, NCVT, NRU, 0, ref S, offset_s
                                             , ref WORK, IE + o_work, ref A, offset_a, LDA, ref U, offset_u, LDU, ref DUM, offset_dum
                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                        }
                        else
                        {
                            // *
                            // *              Perform bidiagonal QR iteration, if desired, computing
                            // *              left singular vectors in A and computing right singular
                            // *              vectors in VT
                            // *              (Workspace: need BDSPAC)
                            // *
                            this._dbdsqr.Run("L", M, NCVT, NRU, 0, ref S, offset_s
                                             , ref WORK, IE + o_work, ref VT, offset_vt, LDVT, ref A, offset_a, LDA, ref DUM, offset_dum
                                             , 1, ref WORK, IWORK + o_work, ref INFO);
                        }
                    }
                    // *
                }
                // *
            }
            // *
            // *     If DBDSQR failed to converge, copy unconverged superdiagonals
            // *     to WORK( 2:MINMN )
            // *
            if (INFO != 0)
            {
                if (IE > 2)
                {
                    for (I = 1; I <= MINMN - 1; I++)
                    {
                        WORK[I + 1 + o_work] = WORK[I + IE - 1 + o_work];
                    }
                }
                if (IE < 2)
                {
                    for (I = MINMN - 1; I >= 1; I +=  - 1)
                    {
                        WORK[I + 1 + o_work] = WORK[I + IE - 1 + o_work];
                    }
                }
            }
            // *
            // *     Undo scaling if necessary
            // *
            if (ISCL == 1)
            {
                if (ANRM > BIGNUM)
                {
                    this._dlascl.Run("G", 0, 0, BIGNUM, ANRM, MINMN
                                     , 1, ref S, offset_s, MINMN, ref IERR);
                }
                if (INFO != 0 && ANRM > BIGNUM)
                {
                    this._dlascl.Run("G", 0, 0, BIGNUM, ANRM, MINMN - 1
                                     , 1, ref WORK, 2 + o_work, MINMN, ref IERR);
                }
                if (ANRM < SMLNUM)
                {
                    this._dlascl.Run("G", 0, 0, SMLNUM, ANRM, MINMN
                                     , 1, ref S, offset_s, MINMN, ref IERR);
                }
                if (INFO != 0 && ANRM < SMLNUM)
                {
                    this._dlascl.Run("G", 0, 0, SMLNUM, ANRM, MINMN - 1
                                     , 1, ref WORK, 2 + o_work, MINMN, ref IERR);
                }
            }
            // *
            // *     Return optimal workspace in WORK(1)
            // *
            WORK[1 + o_work] = MAXWRK;
            // *
            return;
            // *
            // *     End of DGESVD
            // *

            #endregion

        }
    }
}
