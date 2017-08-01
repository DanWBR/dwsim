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
    /// DBDSDC computes the singular value decomposition (SVD) of a real
    /// N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
    /// using a divide and conquer method, where S is a diagonal matrix
    /// with non-negative diagonal elements (the singular values of B), and
    /// U and VT are orthogonal matrices of left and right singular vectors,
    /// respectively. DBDSDC can be used to compute all singular values,
    /// and optionally, singular vectors or singular vectors in compact form.
    /// 
    /// This code makes very mild assumptions about floating point
    /// arithmetic. It will work on machines with a guard digit in
    /// add/subtract, or on those binary machines without guard digits
    /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
    /// It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.  See DLASD3 for details.
    /// 
    /// The code currently calls DLASDQ if singular values only are desired.
    /// However, it can be slightly modified to compute singular values
    /// using the divide and conquer method.
    /// 
    ///</summary>
    public class DBDSDC
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DLAMCH _dlamch; DLANST _dlanst; DCOPY _dcopy; DLARTG _dlartg; DLASCL _dlascl; 
        DLASD0 _dlasd0;DLASDA _dlasda; DLASDQ _dlasdq; DLASET _dlaset; DLASR _dlasr; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; 

        #endregion

        public DBDSDC(LSAME lsame, ILAENV ilaenv, DLAMCH dlamch, DLANST dlanst, DCOPY dcopy, DLARTG dlartg, DLASCL dlascl, DLASD0 dlasd0, DLASDA dlasda, DLASDQ dlasdq
                      , DLASET dlaset, DLASR dlasr, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlanst = dlanst; this._dcopy = dcopy; 
            this._dlartg = dlartg;this._dlascl = dlascl; this._dlasd0 = dlasd0; this._dlasda = dlasda; this._dlasdq = dlasdq; 
            this._dlaset = dlaset;this._dlasr = dlasr; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
    
        public DBDSDC()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLAMRG dlamrg = new DLAMRG();
            DLAPY2 dlapy2 = new DLAPY2();
            DROT drot = new DROT();
            DNRM2 dnrm2 = new DNRM2();
            DLASD5 dlasd5 = new DLASD5();
            DLAS2 dlas2 = new DLAS2();
            DLASQ5 dlasq5 = new DLASQ5();
            DLAZQ4 dlazq4 = new DLAZQ4();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DLASDT dlasdt = new DLASDT();
            DDOT ddot = new DDOT();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASET dlaset = new DLASET(lsame);
            DLASD2 dlasd2 = new DLASD2(dlamch, dlapy2, dcopy, dlacpy, dlamrg, dlaset, drot, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);
            DLASD3 dlasd3 = new DLASD3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlascl, dlasd4, xerbla);
            DLASD1 dlasd1 = new DLASD1(dlamrg, dlascl, dlasd2, dlasd3, xerbla);
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
            DLASD0 dlasd0 = new DLASD0(dlasd1, dlasdq, dlasdt, xerbla);
            DLASD7 dlasd7 = new DLASD7(dcopy, dlamrg, drot, xerbla, dlamch, dlapy2);
            DLASD8 dlasd8 = new DLASD8(dcopy, dlascl, dlasd4, dlaset, xerbla, ddot, dlamc3, dnrm2);
            DLASD6 dlasd6 = new DLASD6(dcopy, dlamrg, dlascl, dlasd7, dlasd8, xerbla);
            DLASDA dlasda = new DLASDA(dcopy, dlasd6, dlasdq, dlasdt, dlaset, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlamch = dlamch; this._dlanst = dlanst; this._dcopy = dcopy; 
            this._dlartg = dlartg;this._dlascl = dlascl; this._dlasd0 = dlasd0; this._dlasda = dlasda; this._dlasdq = dlasdq; 
            this._dlaset = dlaset;this._dlasr = dlasr; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DBDSDC computes the singular value decomposition (SVD) of a real
        /// N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
        /// using a divide and conquer method, where S is a diagonal matrix
        /// with non-negative diagonal elements (the singular values of B), and
        /// U and VT are orthogonal matrices of left and right singular vectors,
        /// respectively. DBDSDC can be used to compute all singular values,
        /// and optionally, singular vectors or singular vectors in compact form.
        /// 
        /// This code makes very mild assumptions about floating point
        /// arithmetic. It will work on machines with a guard digit in
        /// add/subtract, or on those binary machines without guard digits
        /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
        /// It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.  See DLASD3 for details.
        /// 
        /// The code currently calls DLASDQ if singular values only are desired.
        /// However, it can be slightly modified to compute singular values
        /// using the divide and conquer method.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  B is upper bidiagonal.
        /// = 'L':  B is lower bidiagonal.
        ///</param>
        /// <param name="COMPQ">
        /// (input) CHARACTER*1
        /// Specifies whether singular vectors are to be computed
        /// as follows:
        /// = 'N':  Compute singular values only;
        /// = 'P':  Compute singular values and compute singular
        /// vectors in compact form;
        /// = 'I':  Compute singular values and singular vectors.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix B.  N .GE. 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the n diagonal elements of the bidiagonal matrix B.
        /// On exit, if INFO=0, the singular values of B.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, the elements of E contain the offdiagonal
        /// elements of the bidiagonal matrix whose SVD is desired.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension (LDU,N)
        /// If  COMPQ = 'I', then:
        /// On exit, if INFO = 0, U contains the left singular vectors
        /// of the bidiagonal matrix.
        /// For other values of COMPQ, U is not referenced.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. 1.
        /// If singular vectors are desired, then LDU .GE. max( 1, N ).
        ///</param>
        /// <param name="VT">
        /// (output) DOUBLE PRECISION array, dimension (LDVT,N)
        /// If  COMPQ = 'I', then:
        /// On exit, if INFO = 0, VT' contains the right singular
        /// vectors of the bidiagonal matrix.
        /// For other values of COMPQ, VT is not referenced.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.  LDVT .GE. 1.
        /// If singular vectors are desired, then LDVT .GE. max( 1, N ).
        ///</param>
        /// <param name="Q">
        /// (output) DOUBLE PRECISION array, dimension (LDQ)
        /// If  COMPQ = 'P', then:
        /// On exit, if INFO = 0, Q and IQ contain the left
        /// and right singular vectors in a compact form,
        /// requiring O(N log N) space instead of 2*N**2.
        /// In particular, Q contains all the DOUBLE PRECISION data in
        /// LDQ .GE. N*(11 + 2*SMLSIZ + 8*INT(LOG_2(N/(SMLSIZ+1))))
        /// words of memory, where SMLSIZ is returned by ILAENV and
        /// is equal to the maximum size of the subproblems at the
        /// bottom of the computation tree (usually about 25).
        /// For other values of COMPQ, Q is not referenced.
        ///</param>
        /// <param name="IQ">
        /// (output) INTEGER array, dimension (LDIQ)
        /// If  COMPQ = 'P', then:
        /// On exit, if INFO = 0, Q and IQ contain the left
        /// and right singular vectors in a compact form,
        /// requiring O(N log N) space instead of 2*N**2.
        /// In particular, IQ contains all INTEGER data in
        /// LDIQ .GE. N*(3 + 3*INT(LOG_2(N/(SMLSIZ+1))))
        /// words of memory, where SMLSIZ is returned by ILAENV and
        /// is equal to the maximum size of the subproblems at the
        /// bottom of the computation tree (usually about 25).
        /// For other values of COMPQ, IQ is not referenced.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// If COMPQ = 'N' then LWORK .GE. (4 * N).
        /// If COMPQ = 'P' then LWORK .GE. (6 * N).
        /// If COMPQ = 'I' then LWORK .GE. (3 * N**2 + 4 * N).
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (8*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  The algorithm failed to compute an singular value.
        /// The update process of divide and conquer failed.
        ///</param>
        public void Run(string UPLO, string COMPQ, int N, ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] U, int offset_u
                         , int LDU, ref double[] VT, int offset_vt, int LDVT, ref double[] Q, int offset_q, ref int[] IQ, int offset_iq, ref double[] WORK, int offset_work
                         , ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int DIFL = 0; int DIFR = 0; int GIVCOL = 0; int GIVNUM = 0; int GIVPTR = 0; int I = 0; int IC = 0; int ICOMPQ = 0; 
            int IERR = 0;int II = 0; int IS = 0; int IU = 0; int IUPLO = 0; int IVT = 0; int J = 0; int K = 0; int KK = 0; 
            int MLVL = 0;int NM1 = 0; int NSIZE = 0; int PERM = 0; int POLES = 0; int QSTART = 0; int SMLSIZ = 0; int SMLSZP = 0; 
            int SQRE = 0;int START = 0; int WSTART = 0; int Z = 0; double CS = 0; double EPS = 0; double ORGNRM = 0; double P = 0; 
            double R = 0;double SN = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_u = -1 - LDU + offset_u;  int o_vt = -1 - LDVT + offset_vt; 
             int o_q = -1 + offset_q; int o_iq = -1 + offset_iq;  int o_work = -1 + offset_work;  int o_iwork = -1 + offset_iwork; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  COMPQ = COMPQ.Substring(0, 1);  

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
            // *  DBDSDC computes the singular value decomposition (SVD) of a real
            // *  N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
            // *  using a divide and conquer method, where S is a diagonal matrix
            // *  with non-negative diagonal elements (the singular values of B), and
            // *  U and VT are orthogonal matrices of left and right singular vectors,
            // *  respectively. DBDSDC can be used to compute all singular values,
            // *  and optionally, singular vectors or singular vectors in compact form.
            // *
            // *  This code makes very mild assumptions about floating point
            // *  arithmetic. It will work on machines with a guard digit in
            // *  add/subtract, or on those binary machines without guard digits
            // *  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
            // *  It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.  See DLASD3 for details.
            // *
            // *  The code currently calls DLASDQ if singular values only are desired.
            // *  However, it can be slightly modified to compute singular values
            // *  using the divide and conquer method.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  B is upper bidiagonal.
            // *          = 'L':  B is lower bidiagonal.
            // *
            // *  COMPQ   (input) CHARACTER*1
            // *          Specifies whether singular vectors are to be computed
            // *          as follows:
            // *          = 'N':  Compute singular values only;
            // *          = 'P':  Compute singular values and compute singular
            // *                  vectors in compact form;
            // *          = 'I':  Compute singular values and singular vectors.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix B.  N >= 0.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the n diagonal elements of the bidiagonal matrix B.
            // *          On exit, if INFO=0, the singular values of B.
            // *
            // *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, the elements of E contain the offdiagonal
            // *          elements of the bidiagonal matrix whose SVD is desired.
            // *          On exit, E has been destroyed.
            // *
            // *  U       (output) DOUBLE PRECISION array, dimension (LDU,N)
            // *          If  COMPQ = 'I', then:
            // *             On exit, if INFO = 0, U contains the left singular vectors
            // *             of the bidiagonal matrix.
            // *          For other values of COMPQ, U is not referenced.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U.  LDU >= 1.
            // *          If singular vectors are desired, then LDU >= max( 1, N ).
            // *
            // *  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
            // *          If  COMPQ = 'I', then:
            // *             On exit, if INFO = 0, VT' contains the right singular
            // *             vectors of the bidiagonal matrix.
            // *          For other values of COMPQ, VT is not referenced.
            // *
            // *  LDVT    (input) INTEGER
            // *          The leading dimension of the array VT.  LDVT >= 1.
            // *          If singular vectors are desired, then LDVT >= max( 1, N ).
            // *
            // *  Q       (output) DOUBLE PRECISION array, dimension (LDQ)
            // *          If  COMPQ = 'P', then:
            // *             On exit, if INFO = 0, Q and IQ contain the left
            // *             and right singular vectors in a compact form,
            // *             requiring O(N log N) space instead of 2*N**2.
            // *             In particular, Q contains all the DOUBLE PRECISION data in
            // *             LDQ >= N*(11 + 2*SMLSIZ + 8*INT(LOG_2(N/(SMLSIZ+1))))
            // *             words of memory, where SMLSIZ is returned by ILAENV and
            // *             is equal to the maximum size of the subproblems at the
            // *             bottom of the computation tree (usually about 25).
            // *          For other values of COMPQ, Q is not referenced.
            // *
            // *  IQ      (output) INTEGER array, dimension (LDIQ)
            // *          If  COMPQ = 'P', then:
            // *             On exit, if INFO = 0, Q and IQ contain the left
            // *             and right singular vectors in a compact form,
            // *             requiring O(N log N) space instead of 2*N**2.
            // *             In particular, IQ contains all INTEGER data in
            // *             LDIQ >= N*(3 + 3*INT(LOG_2(N/(SMLSIZ+1))))
            // *             words of memory, where SMLSIZ is returned by ILAENV and
            // *             is equal to the maximum size of the subproblems at the
            // *             bottom of the computation tree (usually about 25).
            // *          For other values of COMPQ, IQ is not referenced.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          If COMPQ = 'N' then LWORK >= (4 * N).
            // *          If COMPQ = 'P' then LWORK >= (6 * N).
            // *          If COMPQ = 'I' then LWORK >= (3 * N**2 + 4 * N).
            // *
            // *  IWORK   (workspace) INTEGER array, dimension (8*N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  The algorithm failed to compute an singular value.
            // *                The update process of divide and conquer failed.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
            // *
            // *  =====================================================================
            // *  Changed dimension statement in comment describing E from (N) to
            // *  (N-1).  Sven, 17 Feb 05.
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
            IUPLO = 0;
            if (this._lsame.Run(UPLO, "U")) IUPLO = 1;
            if (this._lsame.Run(UPLO, "L")) IUPLO = 2;
            if (this._lsame.Run(COMPQ, "N"))
            {
                ICOMPQ = 0;
            }
            else
            {
                if (this._lsame.Run(COMPQ, "P"))
                {
                    ICOMPQ = 1;
                }
                else
                {
                    if (this._lsame.Run(COMPQ, "I"))
                    {
                        ICOMPQ = 2;
                    }
                    else
                    {
                        ICOMPQ =  - 1;
                    }
                }
            }
            if (IUPLO == 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (ICOMPQ < 0)
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
                        if ((LDU < 1) || ((ICOMPQ == 2) && (LDU < N)))
                        {
                            INFO =  - 7;
                        }
                        else
                        {
                            if ((LDVT < 1) || ((ICOMPQ == 2) && (LDVT < N)))
                            {
                                INFO =  - 9;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DBDSDC",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            SMLSIZ = this._ilaenv.Run(9, "DBDSDC", " ", 0, 0, 0, 0);
            if (N == 1)
            {
                if (ICOMPQ == 1)
                {
                    Q[1 + o_q] = FortranLib.Sign(ONE,D[1 + o_d]);
                    Q[1 + SMLSIZ * N + o_q] = ONE;
                }
                else
                {
                    if (ICOMPQ == 2)
                    {
                        U[1+1 * LDU + o_u] = FortranLib.Sign(ONE,D[1 + o_d]);
                        VT[1+1 * LDVT + o_vt] = ONE;
                    }
                }
                D[1 + o_d] = Math.Abs(D[1 + o_d]);
                return;
            }
            NM1 = N - 1;
            // *
            // *     If matrix lower bidiagonal, rotate to be upper bidiagonal
            // *     by applying Givens rotations on the left
            // *
            WSTART = 1;
            QSTART = 3;
            if (ICOMPQ == 1)
            {
                this._dcopy.Run(N, D, offset_d, 1, ref Q, 1 + o_q, 1);
                this._dcopy.Run(N - 1, E, offset_e, 1, ref Q, N + 1 + o_q, 1);
            }
            if (IUPLO == 2)
            {
                QSTART = 5;
                WSTART = 2 * N - 1;
                for (I = 1; I <= N - 1; I++)
                {
                    this._dlartg.Run(D[I + o_d], E[I + o_e], ref CS, ref SN, ref R);
                    D[I + o_d] = R;
                    E[I + o_e] = SN * D[I + 1 + o_d];
                    D[I + 1 + o_d] *= CS;
                    if (ICOMPQ == 1)
                    {
                        Q[I + 2 * N + o_q] = CS;
                        Q[I + 3 * N + o_q] = SN;
                    }
                    else
                    {
                        if (ICOMPQ == 2)
                        {
                            WORK[I + o_work] = CS;
                            WORK[NM1 + I + o_work] =  - SN;
                        }
                    }
                }
            }
            // *
            // *     If ICOMPQ = 0, use DLASDQ to compute the singular values.
            // *
            if (ICOMPQ == 0)
            {
                this._dlasdq.Run("U", 0, N, 0, 0, 0
                                 , ref D, offset_d, ref E, offset_e, ref VT, offset_vt, LDVT, ref U, offset_u, LDU
                                 , ref U, offset_u, LDU, ref WORK, WSTART + o_work, ref INFO);
                goto LABEL40;
            }
            // *
            // *     If N is smaller than the minimum divide size SMLSIZ, then solve
            // *     the problem with another solver.
            // *
            if (N <= SMLSIZ)
            {
                if (ICOMPQ == 2)
                {
                    this._dlaset.Run("A", N, N, ZERO, ONE, ref U, offset_u
                                     , LDU);
                    this._dlaset.Run("A", N, N, ZERO, ONE, ref VT, offset_vt
                                     , LDVT);
                    this._dlasdq.Run("U", 0, N, N, N, 0
                                     , ref D, offset_d, ref E, offset_e, ref VT, offset_vt, LDVT, ref U, offset_u, LDU
                                     , ref U, offset_u, LDU, ref WORK, WSTART + o_work, ref INFO);
                }
                else
                {
                    if (ICOMPQ == 1)
                    {
                        IU = 1;
                        IVT = IU + N;
                        this._dlaset.Run("A", N, N, ZERO, ONE, ref Q, IU + (QSTART - 1) * N + o_q
                                         , N);
                        this._dlaset.Run("A", N, N, ZERO, ONE, ref Q, IVT + (QSTART - 1) * N + o_q
                                         , N);
                        this._dlasdq.Run("U", 0, N, N, N, 0
                                         , ref D, offset_d, ref E, offset_e, ref Q, IVT + (QSTART - 1) * N + o_q, N, ref Q, IU + (QSTART - 1) * N + o_q, N
                                         , ref Q, IU + (QSTART - 1) * N + o_q, N, ref WORK, WSTART + o_work, ref INFO);
                    }
                }
                goto LABEL40;
            }
            // *
            if (ICOMPQ == 2)
            {
                this._dlaset.Run("A", N, N, ZERO, ONE, ref U, offset_u
                                 , LDU);
                this._dlaset.Run("A", N, N, ZERO, ONE, ref VT, offset_vt
                                 , LDVT);
            }
            // *
            // *     Scale.
            // *
            ORGNRM = this._dlanst.Run("M", N, D, offset_d, E, offset_e);
            if (ORGNRM == ZERO) return;
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, N
                             , 1, ref D, offset_d, N, ref IERR);
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, NM1
                             , 1, ref E, offset_e, NM1, ref IERR);
            // *
            EPS = this._dlamch.Run("Epsilon");
            // *
            MLVL = Convert.ToInt32(Math.Truncate(Math.Log(Convert.ToDouble(N) / Convert.ToDouble(SMLSIZ + 1)) / Math.Log(TWO))) + 1;
            SMLSZP = SMLSIZ + 1;
            // *
            if (ICOMPQ == 1)
            {
                IU = 1;
                IVT = 1 + SMLSIZ;
                DIFL = IVT + SMLSZP;
                DIFR = DIFL + MLVL;
                Z = DIFR + MLVL * 2;
                IC = Z + MLVL;
                IS = IC + 1;
                POLES = IS + 1;
                GIVNUM = POLES + 2 * MLVL;
                // *
                K = 1;
                GIVPTR = 2;
                PERM = 3;
                GIVCOL = PERM + MLVL;
            }
            // *
            for (I = 1; I <= N; I++)
            {
                if (Math.Abs(D[I + o_d]) < EPS)
                {
                    D[I + o_d] = FortranLib.Sign(EPS,D[I + o_d]);
                }
            }
            // *
            START = 1;
            SQRE = 0;
            // *
            for (I = 1; I <= NM1; I++)
            {
                if ((Math.Abs(E[I + o_e]) < EPS) || (I == NM1))
                {
                    // *
                    // *        Subproblem found. First determine its size and then
                    // *        apply divide and conquer on it.
                    // *
                    if (I < NM1)
                    {
                        // *
                        // *        A subproblem with E(I) small for I < NM1.
                        // *
                        NSIZE = I - START + 1;
                    }
                    else
                    {
                        if (Math.Abs(E[I + o_e]) >= EPS)
                        {
                            // *
                            // *        A subproblem with E(NM1) not too small but I = NM1.
                            // *
                            NSIZE = N - START + 1;
                        }
                        else
                        {
                            // *
                            // *        A subproblem with E(NM1) small. This implies an
                            // *        1-by-1 subproblem at D(N). Solve this 1-by-1 problem
                            // *        first.
                            // *
                            NSIZE = I - START + 1;
                            if (ICOMPQ == 2)
                            {
                                U[N+N * LDU + o_u] = FortranLib.Sign(ONE,D[N + o_d]);
                                VT[N+N * LDVT + o_vt] = ONE;
                            }
                            else
                            {
                                if (ICOMPQ == 1)
                                {
                                    Q[N + (QSTART - 1) * N + o_q] = FortranLib.Sign(ONE,D[N + o_d]);
                                    Q[N + (SMLSIZ + QSTART - 1) * N + o_q] = ONE;
                                }
                            }
                            D[N + o_d] = Math.Abs(D[N + o_d]);
                        }
                    }
                    if (ICOMPQ == 2)
                    {
                        this._dlasd0.Run(NSIZE, SQRE, ref D, START + o_d, ref E, START + o_e, ref U, START+START * LDU + o_u, LDU
                                         , ref VT, START+START * LDVT + o_vt, LDVT, SMLSIZ, ref IWORK, offset_iwork, ref WORK, WSTART + o_work, ref INFO);
                    }
                    else
                    {
                        this._dlasda.Run(ICOMPQ, SMLSIZ, NSIZE, SQRE, ref D, START + o_d, ref E, START + o_e
                                         , ref Q, START + (IU + QSTART - 2) * N + o_q, N, ref Q, START + (IVT + QSTART - 2) * N + o_q, ref IQ, START + K * N + o_iq, ref Q, START + (DIFL + QSTART - 2) * N + o_q, ref Q, START + (DIFR + QSTART - 2) * N + o_q
                                         , ref Q, START + (Z + QSTART - 2) * N + o_q, ref Q, START + (POLES + QSTART - 2) * N + o_q, ref IQ, START + GIVPTR * N + o_iq, ref IQ, START + GIVCOL * N + o_iq, N, ref IQ, START + PERM * N + o_iq
                                         , ref Q, START + (GIVNUM + QSTART - 2) * N + o_q, ref Q, START + (IC + QSTART - 2) * N + o_q, ref Q, START + (IS + QSTART - 2) * N + o_q, ref WORK, WSTART + o_work, ref IWORK, offset_iwork, ref INFO);
                        if (INFO != 0)
                        {
                            return;
                        }
                    }
                    START = I + 1;
                }
            }
            // *
            // *     Unscale
            // *
            this._dlascl.Run("G", 0, 0, ONE, ORGNRM, N
                             , 1, ref D, offset_d, N, ref IERR);
        LABEL40:;
            // *
            // *     Use Selection Sort to minimize swaps of singular vectors
            // *
            for (II = 2; II <= N; II++)
            {
                I = II - 1;
                KK = I;
                P = D[I + o_d];
                for (J = II; J <= N; J++)
                {
                    if (D[J + o_d] > P)
                    {
                        KK = J;
                        P = D[J + o_d];
                    }
                }
                if (KK != I)
                {
                    D[KK + o_d] = D[I + o_d];
                    D[I + o_d] = P;
                    if (ICOMPQ == 1)
                    {
                        IQ[I + o_iq] = KK;
                    }
                    else
                    {
                        if (ICOMPQ == 2)
                        {
                            this._dswap.Run(N, ref U, 1+I * LDU + o_u, 1, ref U, 1+KK * LDU + o_u, 1);
                            this._dswap.Run(N, ref VT, I+1 * LDVT + o_vt, LDVT, ref VT, KK+1 * LDVT + o_vt, LDVT);
                        }
                    }
                }
                else
                {
                    if (ICOMPQ == 1)
                    {
                        IQ[I + o_iq] = I;
                    }
                }
            }
            // *
            // *     If ICOMPQ = 1, use IQ(N,1) as the indicator for UPLO
            // *
            if (ICOMPQ == 1)
            {
                if (IUPLO == 1)
                {
                    IQ[N + o_iq] = 1;
                }
                else
                {
                    IQ[N + o_iq] = 0;
                }
            }
            // *
            // *     If B is lower bidiagonal, update U by those Givens rotations
            // *     which rotated B to be upper bidiagonal
            // *
            if ((IUPLO == 2) && (ICOMPQ == 2))
            {
                this._dlasr.Run("L", "V", "B", N, N, WORK, 1 + o_work
                                , WORK, N + o_work, ref U, offset_u, LDU);
            }
            // *
            return;
            // *
            // *     End of DBDSDC
            // *

            #endregion

        }
    }
}
