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
    /// -- LAPACK routine (version 3.1.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// January 2007
    /// Purpose
    /// =======
    /// 
    /// DBDSQR computes the singular values and, optionally, the right and/or
    /// left singular vectors from the singular value decomposition (SVD) of
    /// a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
    /// zero-shift QR algorithm.  The SVD of B has the form
    /// 
    /// B = Q * S * P**T
    /// 
    /// where S is the diagonal matrix of singular values, Q is an orthogonal
    /// matrix of left singular vectors, and P is an orthogonal matrix of
    /// right singular vectors.  If left singular vectors are requested, this
    /// subroutine actually returns U*Q instead of Q, and, if right singular
    /// vectors are requested, this subroutine returns P**T*VT instead of
    /// P**T, for given real input matrices U and VT.  When U and VT are the
    /// orthogonal matrices that reduce a general matrix A to bidiagonal
    /// form:  A = U*B*VT, as computed by DGEBRD, then
    /// 
    /// A = (U*Q) * S * (P**T*VT)
    /// 
    /// is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
    /// for a given real input matrix C.
    /// 
    /// See "Computing  Small Singular Values of Bidiagonal Matrices With
    /// Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
    /// LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
    /// no. 5, pp. 873-912, Sept 1990) and
    /// "Accurate singular values and differential qd algorithms," by
    /// B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
    /// Department, University of California at Berkeley, July 1992
    /// for a detailed description of the algorithm.
    /// 
    ///</summary>
    public class DBDSQR
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; DLARTG _dlartg; DLAS2 _dlas2; DLASQ1 _dlasq1; DLASR _dlasr; DLASV2 _dlasv2; DROT _drot; 
        DSCAL _dscal;DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double NEGONE =  - 1.0E0; const double HNDRTH = 0.01E0; 
        const double TEN = 10.0E0;const double HNDRD = 100.0E0; const double MEIGTH =  - 0.125E0; const int MAXITR = 6; 

        #endregion

        public DBDSQR(LSAME lsame, DLAMCH dlamch, DLARTG dlartg, DLAS2 dlas2, DLASQ1 dlasq1, DLASR dlasr, DLASV2 dlasv2, DROT drot, DSCAL dscal, DSWAP dswap
                      , XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlartg = dlartg; this._dlas2 = dlas2; this._dlasq1 = dlasq1; 
            this._dlasr = dlasr;this._dlasv2 = dlasv2; this._drot = drot; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DBDSQR()
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

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlartg = dlartg; this._dlas2 = dlas2; this._dlasq1 = dlasq1; 
            this._dlasr = dlasr;this._dlasv2 = dlasv2; this._drot = drot; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DBDSQR computes the singular values and, optionally, the right and/or
        /// left singular vectors from the singular value decomposition (SVD) of
        /// a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
        /// zero-shift QR algorithm.  The SVD of B has the form
        /// 
        /// B = Q * S * P**T
        /// 
        /// where S is the diagonal matrix of singular values, Q is an orthogonal
        /// matrix of left singular vectors, and P is an orthogonal matrix of
        /// right singular vectors.  If left singular vectors are requested, this
        /// subroutine actually returns U*Q instead of Q, and, if right singular
        /// vectors are requested, this subroutine returns P**T*VT instead of
        /// P**T, for given real input matrices U and VT.  When U and VT are the
        /// orthogonal matrices that reduce a general matrix A to bidiagonal
        /// form:  A = U*B*VT, as computed by DGEBRD, then
        /// 
        /// A = (U*Q) * S * (P**T*VT)
        /// 
        /// is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
        /// for a given real input matrix C.
        /// 
        /// See "Computing  Small Singular Values of Bidiagonal Matrices With
        /// Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
        /// LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
        /// no. 5, pp. 873-912, Sept 1990) and
        /// "Accurate singular values and differential qd algorithms," by
        /// B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
        /// Department, University of California at Berkeley, July 1992
        /// for a detailed description of the algorithm.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  B is upper bidiagonal;
        /// = 'L':  B is lower bidiagonal.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix B.  N .GE. 0.
        ///</param>
        /// <param name="NCVT">
        /// (input) INTEGER
        /// The number of columns of the matrix VT. NCVT .GE. 0.
        ///</param>
        /// <param name="NRU">
        /// (input) INTEGER
        /// The number of rows of the matrix U. NRU .GE. 0.
        ///</param>
        /// <param name="NCC">
        /// (input) INTEGER
        /// The number of columns of the matrix C. NCC .GE. 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the n diagonal elements of the bidiagonal matrix B.
        /// On exit, if INFO=0, the singular values of B in decreasing
        /// order.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, the N-1 offdiagonal elements of the bidiagonal
        /// matrix B. 
        /// On exit, if INFO = 0, E is destroyed; if INFO .GT. 0, D and E
        /// will contain the diagonal and superdiagonal elements of a
        /// bidiagonal matrix orthogonally equivalent to the one given
        /// as input.
        ///</param>
        /// <param name="VT">
        /// (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
        /// On entry, an N-by-NCVT matrix VT.
        /// On exit, VT is overwritten by P**T * VT.
        /// Not referenced if NCVT = 0.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.
        /// LDVT .GE. max(1,N) if NCVT .GT. 0; LDVT .GE. 1 if NCVT = 0.
        ///</param>
        /// <param name="U">
        /// (input/output) DOUBLE PRECISION array, dimension (LDU, N)
        /// On entry, an NRU-by-N matrix U.
        /// On exit, U is overwritten by U * Q.
        /// Not referenced if NRU = 0.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. max(1,NRU).
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
        /// On entry, an N-by-NCC matrix C.
        /// On exit, C is overwritten by Q**T * C.
        /// Not referenced if NCC = 0.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C.
        /// LDC .GE. max(1,N) if NCC .GT. 0; LDC .GE.1 if NCC = 0.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (2*N)
        /// if NCVT = NRU = NCC = 0, (max(1, 4*N)) otherwise
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  If INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  the algorithm did not converge; D and E contain the
        /// elements of a bidiagonal matrix which is orthogonally
        /// similar to the input matrix B;  if INFO = i, i
        /// elements of E have not converged to zero.
        ///</param>
        public void Run(string UPLO, int N, int NCVT, int NRU, int NCC, ref double[] D, int offset_d
                         , ref double[] E, int offset_e, ref double[] VT, int offset_vt, int LDVT, ref double[] U, int offset_u, int LDU, ref double[] C, int offset_c
                         , int LDC, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool LOWER = false; bool ROTATE = false; int I = 0; int IDIR = 0; int ISUB = 0; int ITER = 0; int J = 0; int LL = 0; 
            int LLL = 0;int M = 0; int MAXIT = 0; int NM1 = 0; int NM12 = 0; int NM13 = 0; int OLDLL = 0; int OLDM = 0; 
            double ABSE = 0;double ABSS = 0; double COSL = 0; double COSR = 0; double CS = 0; double EPS = 0; double F = 0; 
            double G = 0;double H = 0; double MU = 0; double OLDCS = 0; double OLDSN = 0; double R = 0; double SHIFT = 0; 
            double SIGMN = 0;double SIGMX = 0; double SINL = 0; double SINR = 0; double SLL = 0; double SMAX = 0; double SMIN = 0; 
            double SMINL = 0;double SMINOA = 0; double SN = 0; double THRESH = 0; double TOL = 0; double TOLMUL = 0; 
            double UNFL = 0;

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_vt = -1 - LDVT + offset_vt;  int o_u = -1 - LDU + offset_u; 
             int o_c = -1 - LDC + offset_c; int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     January 2007
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DBDSQR computes the singular values and, optionally, the right and/or
            // *  left singular vectors from the singular value decomposition (SVD) of
            // *  a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
            // *  zero-shift QR algorithm.  The SVD of B has the form
            // * 
            // *     B = Q * S * P**T
            // * 
            // *  where S is the diagonal matrix of singular values, Q is an orthogonal
            // *  matrix of left singular vectors, and P is an orthogonal matrix of
            // *  right singular vectors.  If left singular vectors are requested, this
            // *  subroutine actually returns U*Q instead of Q, and, if right singular
            // *  vectors are requested, this subroutine returns P**T*VT instead of
            // *  P**T, for given real input matrices U and VT.  When U and VT are the
            // *  orthogonal matrices that reduce a general matrix A to bidiagonal
            // *  form:  A = U*B*VT, as computed by DGEBRD, then
            // *
            // *     A = (U*Q) * S * (P**T*VT)
            // *
            // *  is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
            // *  for a given real input matrix C.
            // *
            // *  See "Computing  Small Singular Values of Bidiagonal Matrices With
            // *  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
            // *  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
            // *  no. 5, pp. 873-912, Sept 1990) and
            // *  "Accurate singular values and differential qd algorithms," by
            // *  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
            // *  Department, University of California at Berkeley, July 1992
            // *  for a detailed description of the algorithm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  B is upper bidiagonal;
            // *          = 'L':  B is lower bidiagonal.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix B.  N >= 0.
            // *
            // *  NCVT    (input) INTEGER
            // *          The number of columns of the matrix VT. NCVT >= 0.
            // *
            // *  NRU     (input) INTEGER
            // *          The number of rows of the matrix U. NRU >= 0.
            // *
            // *  NCC     (input) INTEGER
            // *          The number of columns of the matrix C. NCC >= 0.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the n diagonal elements of the bidiagonal matrix B.
            // *          On exit, if INFO=0, the singular values of B in decreasing
            // *          order.
            // *
            // *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, the N-1 offdiagonal elements of the bidiagonal
            // *          matrix B. 
            // *          On exit, if INFO = 0, E is destroyed; if INFO > 0, D and E
            // *          will contain the diagonal and superdiagonal elements of a
            // *          bidiagonal matrix orthogonally equivalent to the one given
            // *          as input.
            // *
            // *  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
            // *          On entry, an N-by-NCVT matrix VT.
            // *          On exit, VT is overwritten by P**T * VT.
            // *          Not referenced if NCVT = 0.
            // *
            // *  LDVT    (input) INTEGER
            // *          The leading dimension of the array VT.
            // *          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
            // *
            // *  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
            // *          On entry, an NRU-by-N matrix U.
            // *          On exit, U is overwritten by U * Q.
            // *          Not referenced if NRU = 0.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U.  LDU >= max(1,NRU).
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
            // *          On entry, an N-by-NCC matrix C.
            // *          On exit, C is overwritten by Q**T * C.
            // *          Not referenced if NCC = 0.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C.
            // *          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
            // *          if NCVT = NRU = NCC = 0, (max(1, 4*N)) otherwise
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  If INFO = -i, the i-th argument had an illegal value
            // *          > 0:  the algorithm did not converge; D and E contain the
            // *                elements of a bidiagonal matrix which is orthogonally
            // *                similar to the input matrix B;  if INFO = i, i
            // *                elements of E have not converged to zero.
            // *
            // *  Internal Parameters
            // *  ===================
            // *
            // *  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
            // *          TOLMUL controls the convergence criterion of the QR loop.
            // *          If it is positive, TOLMUL*EPS is the desired relative
            // *             precision in the computed singular values.
            // *          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
            // *             desired absolute accuracy in the computed singular
            // *             values (corresponds to relative accuracy
            // *             abs(TOLMUL*EPS) in the largest singular value.
            // *          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
            // *             between 10 (for fast convergence) and .1/EPS
            // *             (for there to be some accuracy in the results).
            // *          Default is to lose at either one eighth or 2 of the
            // *             available decimal digits in each computed singular value
            // *             (whichever is smaller).
            // *
            // *  MAXITR  INTEGER, default = 6
            // *          MAXITR controls the maximum number of passes of the
            // *          algorithm through its inner loop. The algorithms stops
            // *          (and so fails to converge) if the number of passes
            // *          through the inner loop exceeds MAXITR*N**2.
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
            //      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LOWER = this._lsame.Run(UPLO, "L");
            if (!this._lsame.Run(UPLO, "U") && !LOWER)
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
                    if (NCVT < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (NRU < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (NCC < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if ((NCVT == 0 && LDVT < 1) || (NCVT > 0 && LDVT < Math.Max(1, N)))
                                {
                                    INFO =  - 9;
                                }
                                else
                                {
                                    if (LDU < Math.Max(1, NRU))
                                    {
                                        INFO =  - 11;
                                    }
                                    else
                                    {
                                        if ((NCC == 0 && LDC < 1) || (NCC > 0 && LDC < Math.Max(1, N)))
                                        {
                                            INFO =  - 13;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DBDSQR",  - INFO);
                return;
            }
            if (N == 0) return;
            if (N == 1) goto LABEL160;
            // *
            // *     ROTATE is true if any singular vectors desired, false otherwise
            // *
            ROTATE = (NCVT > 0) || (NRU > 0) || (NCC > 0);
            // *
            // *     If no singular vectors desired, use qd algorithm
            // *
            if (!ROTATE)
            {
                this._dlasq1.Run(N, ref D, offset_d, E, offset_e, ref WORK, offset_work, ref INFO);
                return;
            }
            // *
            NM1 = N - 1;
            NM12 = NM1 + NM1;
            NM13 = NM12 + NM1;
            IDIR = 0;
            // *
            // *     Get machine constants
            // *
            EPS = this._dlamch.Run("Epsilon");
            UNFL = this._dlamch.Run("Safe minimum");
            // *
            // *     If matrix lower bidiagonal, rotate to be upper bidiagonal
            // *     by applying Givens rotations on the left
            // *
            if (LOWER)
            {
                for (I = 1; I <= N - 1; I++)
                {
                    this._dlartg.Run(D[I + o_d], E[I + o_e], ref CS, ref SN, ref R);
                    D[I + o_d] = R;
                    E[I + o_e] = SN * D[I + 1 + o_d];
                    D[I + 1 + o_d] *= CS;
                    WORK[I + o_work] = CS;
                    WORK[NM1 + I + o_work] = SN;
                }
                // *
                // *        Update singular vectors if desired
                // *
                if (NRU > 0)
                {
                    this._dlasr.Run("R", "V", "F", NRU, N, WORK, 1 + o_work
                                    , WORK, N + o_work, ref U, offset_u, LDU);
                }
                if (NCC > 0)
                {
                    this._dlasr.Run("L", "V", "F", N, NCC, WORK, 1 + o_work
                                    , WORK, N + o_work, ref C, offset_c, LDC);
                }
            }
            // *
            // *     Compute singular values to relative accuracy TOL
            // *     (By setting TOL to be negative, algorithm will compute
            // *     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
            // *
            TOLMUL = Math.Max(TEN, Math.Min(HNDRD, Math.Pow(EPS,MEIGTH)));
            TOL = TOLMUL * EPS;
            // *
            // *     Compute approximate maximum, minimum singular values
            // *
            SMAX = ZERO;
            for (I = 1; I <= N; I++)
            {
                SMAX = Math.Max(SMAX, Math.Abs(D[I + o_d]));
            }
            for (I = 1; I <= N - 1; I++)
            {
                SMAX = Math.Max(SMAX, Math.Abs(E[I + o_e]));
            }
            SMINL = ZERO;
            if (TOL >= ZERO)
            {
                // *
                // *        Relative accuracy desired
                // *
                SMINOA = Math.Abs(D[1 + o_d]);
                if (SMINOA == ZERO) goto LABEL50;
                MU = SMINOA;
                for (I = 2; I <= N; I++)
                {
                    MU = Math.Abs(D[I + o_d]) * (MU / (MU + Math.Abs(E[I - 1 + o_e])));
                    SMINOA = Math.Min(SMINOA, MU);
                    if (SMINOA == ZERO) goto LABEL50;
                }
            LABEL50:;
                SMINOA /= Math.Sqrt(Convert.ToDouble(N));
                THRESH = Math.Max(TOL * SMINOA, MAXITR * N * N * UNFL);
            }
            else
            {
                // *
                // *        Absolute accuracy desired
                // *
                THRESH = Math.Max(Math.Abs(TOL) * SMAX, MAXITR * N * N * UNFL);
            }
            // *
            // *     Prepare for main iteration loop for the singular values
            // *     (MAXIT is the maximum number of passes through the inner
            // *     loop permitted before nonconvergence signalled.)
            // *
            MAXIT = MAXITR * N * N;
            ITER = 0;
            OLDLL =  - 1;
            OLDM =  - 1;
            // *
            // *     M points to last element of unconverged part of matrix
            // *
            M = N;
            // *
            // *     Begin main iteration loop
            // *
        LABEL60:;
            // *
            // *     Check for convergence or exceeding iteration count
            // *
            if (M <= 1) goto LABEL160;
            if (ITER > MAXIT) goto LABEL200;
            // *
            // *     Find diagonal block of matrix to work on
            // *
            if (TOL < ZERO && Math.Abs(D[M + o_d]) <= THRESH) D[M + o_d] = ZERO;
            SMAX = Math.Abs(D[M + o_d]);
            SMIN = SMAX;
            for (LLL = 1; LLL <= M - 1; LLL++)
            {
                LL = M - LLL;
                ABSS = Math.Abs(D[LL + o_d]);
                ABSE = Math.Abs(E[LL + o_e]);
                if (TOL < ZERO && ABSS <= THRESH) D[LL + o_d] = ZERO;
                if (ABSE <= THRESH) goto LABEL80;
                SMIN = Math.Min(SMIN, ABSS);
                SMAX = Math.Max(SMAX, Math.Max(ABSS, ABSE));
            }
            LL = 0;
            goto LABEL90;
        LABEL80:;
            E[LL + o_e] = ZERO;
            // *
            // *     Matrix splits since E(LL) = 0
            // *
            if (LL == M - 1)
            {
                // *
                // *        Convergence of bottom singular value, return to top of loop
                // *
                M -= 1;
                goto LABEL60;
            }
        LABEL90:;
            LL += 1;
            // *
            // *     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
            // *
            if (LL == M - 1)
            {
                // *
                // *        2 by 2 block, handle separately
                // *
                this._dlasv2.Run(D[M - 1 + o_d], E[M - 1 + o_e], D[M + o_d], ref SIGMN, ref SIGMX, ref SINR
                                 , ref COSR, ref SINL, ref COSL);
                D[M - 1 + o_d] = SIGMX;
                E[M - 1 + o_e] = ZERO;
                D[M + o_d] = SIGMN;
                // *
                // *        Compute singular vectors, if desired
                // *
                if (NCVT > 0)
                {
                    this._drot.Run(NCVT, ref VT, M - 1+1 * LDVT + o_vt, LDVT, ref VT, M+1 * LDVT + o_vt, LDVT, COSR
                                   , SINR);
                }
                if (NRU > 0)
                {
                    this._drot.Run(NRU, ref U, 1+(M - 1) * LDU + o_u, 1, ref U, 1+M * LDU + o_u, 1, COSL
                                   , SINL);
                }
                if (NCC > 0)
                {
                    this._drot.Run(NCC, ref C, M - 1+1 * LDC + o_c, LDC, ref C, M+1 * LDC + o_c, LDC, COSL
                                   , SINL);
                }
                M -= 2;
                goto LABEL60;
            }
            // *
            // *     If working on new submatrix, choose shift direction
            // *     (from larger end diagonal element towards smaller)
            // *
            if (LL > OLDM || M < OLDLL)
            {
                if (Math.Abs(D[LL + o_d]) >= Math.Abs(D[M + o_d]))
                {
                    // *
                    // *           Chase bulge from top (big end) to bottom (small end)
                    // *
                    IDIR = 1;
                }
                else
                {
                    // *
                    // *           Chase bulge from bottom (big end) to top (small end)
                    // *
                    IDIR = 2;
                }
            }
            // *
            // *     Apply convergence tests
            // *
            if (IDIR == 1)
            {
                // *
                // *        Run convergence test in forward direction
                // *        First apply standard test to bottom of matrix
                // *
                if (Math.Abs(E[M - 1 + o_e]) <= Math.Abs(TOL) * Math.Abs(D[M + o_d]) || (TOL < ZERO && Math.Abs(E[M - 1 + o_e]) <= THRESH))
                {
                    E[M - 1 + o_e] = ZERO;
                    goto LABEL60;
                }
                // *
                if (TOL >= ZERO)
                {
                    // *
                    // *           If relative accuracy desired,
                    // *           apply convergence criterion forward
                    // *
                    MU = Math.Abs(D[LL + o_d]);
                    SMINL = MU;
                    for (LLL = LL; LLL <= M - 1; LLL++)
                    {
                        if (Math.Abs(E[LLL + o_e]) <= TOL * MU)
                        {
                            E[LLL + o_e] = ZERO;
                            goto LABEL60;
                        }
                        MU = Math.Abs(D[LLL + 1 + o_d]) * (MU / (MU + Math.Abs(E[LLL + o_e])));
                        SMINL = Math.Min(SMINL, MU);
                    }
                }
                // *
            }
            else
            {
                // *
                // *        Run convergence test in backward direction
                // *        First apply standard test to top of matrix
                // *
                if (Math.Abs(E[LL + o_e]) <= Math.Abs(TOL) * Math.Abs(D[LL + o_d]) || (TOL < ZERO && Math.Abs(E[LL + o_e]) <= THRESH))
                {
                    E[LL + o_e] = ZERO;
                    goto LABEL60;
                }
                // *
                if (TOL >= ZERO)
                {
                    // *
                    // *           If relative accuracy desired,
                    // *           apply convergence criterion backward
                    // *
                    MU = Math.Abs(D[M + o_d]);
                    SMINL = MU;
                    for (LLL = M - 1; LLL >= LL; LLL +=  - 1)
                    {
                        if (Math.Abs(E[LLL + o_e]) <= TOL * MU)
                        {
                            E[LLL + o_e] = ZERO;
                            goto LABEL60;
                        }
                        MU = Math.Abs(D[LLL + o_d]) * (MU / (MU + Math.Abs(E[LLL + o_e])));
                        SMINL = Math.Min(SMINL, MU);
                    }
                }
            }
            OLDLL = LL;
            OLDM = M;
            // *
            // *     Compute shift.  First, test if shifting would ruin relative
            // *     accuracy, and if so set the shift to zero.
            // *
            if (TOL >= ZERO && N * TOL * (SMINL / SMAX) <= Math.Max(EPS, HNDRTH * TOL))
            {
                // *
                // *        Use a zero shift to avoid loss of relative accuracy
                // *
                SHIFT = ZERO;
            }
            else
            {
                // *
                // *        Compute the shift from 2-by-2 block at end of matrix
                // *
                if (IDIR == 1)
                {
                    SLL = Math.Abs(D[LL + o_d]);
                    this._dlas2.Run(D[M - 1 + o_d], E[M - 1 + o_e], D[M + o_d], ref SHIFT, ref R);
                }
                else
                {
                    SLL = Math.Abs(D[M + o_d]);
                    this._dlas2.Run(D[LL + o_d], E[LL + o_e], D[LL + 1 + o_d], ref SHIFT, ref R);
                }
                // *
                // *        Test if shift negligible, and if so set to zero
                // *
                if (SLL > ZERO)
                {
                    if (Math.Pow(SHIFT / SLL,2) < EPS) SHIFT = ZERO;
                }
            }
            // *
            // *     Increment iteration count
            // *
            ITER += M - LL;
            // *
            // *     If SHIFT = 0, do simplified QR iteration
            // *
            if (SHIFT == ZERO)
            {
                if (IDIR == 1)
                {
                    // *
                    // *           Chase bulge from top to bottom
                    // *           Save cosines and sines for later singular vector updates
                    // *
                    CS = ONE;
                    OLDCS = ONE;
                    for (I = LL; I <= M - 1; I++)
                    {
                        this._dlartg.Run(D[I + o_d] * CS, E[I + o_e], ref CS, ref SN, ref R);
                        if (I > LL) E[I - 1 + o_e] = OLDSN * R;
                        this._dlartg.Run(OLDCS * R, D[I + 1 + o_d] * SN, ref OLDCS, ref OLDSN, ref D[I + o_d]);
                        WORK[I - LL + 1 + o_work] = CS;
                        WORK[I - LL + 1 + NM1 + o_work] = SN;
                        WORK[I - LL + 1 + NM12 + o_work] = OLDCS;
                        WORK[I - LL + 1 + NM13 + o_work] = OLDSN;
                    }
                    H = D[M + o_d] * CS;
                    D[M + o_d] = H * OLDCS;
                    E[M - 1 + o_e] = H * OLDSN;
                    // *
                    // *           Update singular vectors
                    // *
                    if (NCVT > 0)
                    {
                        this._dlasr.Run("L", "V", "F", M - LL + 1, NCVT, WORK, 1 + o_work
                                        , WORK, N + o_work, ref VT, LL+1 * LDVT + o_vt, LDVT);
                    }
                    if (NRU > 0)
                    {
                        this._dlasr.Run("R", "V", "F", NRU, M - LL + 1, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref U, 1+LL * LDU + o_u, LDU);
                    }
                    if (NCC > 0)
                    {
                        this._dlasr.Run("L", "V", "F", M - LL + 1, NCC, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref C, LL+1 * LDC + o_c, LDC);
                    }
                    // *
                    // *           Test convergence
                    // *
                    if (Math.Abs(E[M - 1 + o_e]) <= THRESH) E[M - 1 + o_e] = ZERO;
                    // *
                }
                else
                {
                    // *
                    // *           Chase bulge from bottom to top
                    // *           Save cosines and sines for later singular vector updates
                    // *
                    CS = ONE;
                    OLDCS = ONE;
                    for (I = M; I >= LL + 1; I +=  - 1)
                    {
                        this._dlartg.Run(D[I + o_d] * CS, E[I - 1 + o_e], ref CS, ref SN, ref R);
                        if (I < M) E[I + o_e] = OLDSN * R;
                        this._dlartg.Run(OLDCS * R, D[I - 1 + o_d] * SN, ref OLDCS, ref OLDSN, ref D[I + o_d]);
                        WORK[I - LL + o_work] = CS;
                        WORK[I - LL + NM1 + o_work] =  - SN;
                        WORK[I - LL + NM12 + o_work] = OLDCS;
                        WORK[I - LL + NM13 + o_work] =  - OLDSN;
                    }
                    H = D[LL + o_d] * CS;
                    D[LL + o_d] = H * OLDCS;
                    E[LL + o_e] = H * OLDSN;
                    // *
                    // *           Update singular vectors
                    // *
                    if (NCVT > 0)
                    {
                        this._dlasr.Run("L", "V", "B", M - LL + 1, NCVT, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref VT, LL+1 * LDVT + o_vt, LDVT);
                    }
                    if (NRU > 0)
                    {
                        this._dlasr.Run("R", "V", "B", NRU, M - LL + 1, WORK, 1 + o_work
                                        , WORK, N + o_work, ref U, 1+LL * LDU + o_u, LDU);
                    }
                    if (NCC > 0)
                    {
                        this._dlasr.Run("L", "V", "B", M - LL + 1, NCC, WORK, 1 + o_work
                                        , WORK, N + o_work, ref C, LL+1 * LDC + o_c, LDC);
                    }
                    // *
                    // *           Test convergence
                    // *
                    if (Math.Abs(E[LL + o_e]) <= THRESH) E[LL + o_e] = ZERO;
                }
            }
            else
            {
                // *
                // *        Use nonzero shift
                // *
                if (IDIR == 1)
                {
                    // *
                    // *           Chase bulge from top to bottom
                    // *           Save cosines and sines for later singular vector updates
                    // *
                    F = (Math.Abs(D[LL + o_d]) - SHIFT) * (FortranLib.Sign(ONE,D[LL + o_d]) + SHIFT / D[LL + o_d]);
                    G = E[LL + o_e];
                    for (I = LL; I <= M - 1; I++)
                    {
                        this._dlartg.Run(F, G, ref COSR, ref SINR, ref R);
                        if (I > LL) E[I - 1 + o_e] = R;
                        F = COSR * D[I + o_d] + SINR * E[I + o_e];
                        E[I + o_e] = COSR * E[I + o_e] - SINR * D[I + o_d];
                        G = SINR * D[I + 1 + o_d];
                        D[I + 1 + o_d] *= COSR;
                        this._dlartg.Run(F, G, ref COSL, ref SINL, ref R);
                        D[I + o_d] = R;
                        F = COSL * E[I + o_e] + SINL * D[I + 1 + o_d];
                        D[I + 1 + o_d] = COSL * D[I + 1 + o_d] - SINL * E[I + o_e];
                        if (I < M - 1)
                        {
                            G = SINL * E[I + 1 + o_e];
                            E[I + 1 + o_e] *= COSL;
                        }
                        WORK[I - LL + 1 + o_work] = COSR;
                        WORK[I - LL + 1 + NM1 + o_work] = SINR;
                        WORK[I - LL + 1 + NM12 + o_work] = COSL;
                        WORK[I - LL + 1 + NM13 + o_work] = SINL;
                    }
                    E[M - 1 + o_e] = F;
                    // *
                    // *           Update singular vectors
                    // *
                    if (NCVT > 0)
                    {
                        this._dlasr.Run("L", "V", "F", M - LL + 1, NCVT, WORK, 1 + o_work
                                        , WORK, N + o_work, ref VT, LL+1 * LDVT + o_vt, LDVT);
                    }
                    if (NRU > 0)
                    {
                        this._dlasr.Run("R", "V", "F", NRU, M - LL + 1, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref U, 1+LL * LDU + o_u, LDU);
                    }
                    if (NCC > 0)
                    {
                        this._dlasr.Run("L", "V", "F", M - LL + 1, NCC, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref C, LL+1 * LDC + o_c, LDC);
                    }
                    // *
                    // *           Test convergence
                    // *
                    if (Math.Abs(E[M - 1 + o_e]) <= THRESH) E[M - 1 + o_e] = ZERO;
                    // *
                }
                else
                {
                    // *
                    // *           Chase bulge from bottom to top
                    // *           Save cosines and sines for later singular vector updates
                    // *
                    F = (Math.Abs(D[M + o_d]) - SHIFT) * (FortranLib.Sign(ONE,D[M + o_d]) + SHIFT / D[M + o_d]);
                    G = E[M - 1 + o_e];
                    for (I = M; I >= LL + 1; I +=  - 1)
                    {
                        this._dlartg.Run(F, G, ref COSR, ref SINR, ref R);
                        if (I < M) E[I + o_e] = R;
                        F = COSR * D[I + o_d] + SINR * E[I - 1 + o_e];
                        E[I - 1 + o_e] = COSR * E[I - 1 + o_e] - SINR * D[I + o_d];
                        G = SINR * D[I - 1 + o_d];
                        D[I - 1 + o_d] *= COSR;
                        this._dlartg.Run(F, G, ref COSL, ref SINL, ref R);
                        D[I + o_d] = R;
                        F = COSL * E[I - 1 + o_e] + SINL * D[I - 1 + o_d];
                        D[I - 1 + o_d] = COSL * D[I - 1 + o_d] - SINL * E[I - 1 + o_e];
                        if (I > LL + 1)
                        {
                            G = SINL * E[I - 2 + o_e];
                            E[I - 2 + o_e] *= COSL;
                        }
                        WORK[I - LL + o_work] = COSR;
                        WORK[I - LL + NM1 + o_work] =  - SINR;
                        WORK[I - LL + NM12 + o_work] = COSL;
                        WORK[I - LL + NM13 + o_work] =  - SINL;
                    }
                    E[LL + o_e] = F;
                    // *
                    // *           Test convergence
                    // *
                    if (Math.Abs(E[LL + o_e]) <= THRESH) E[LL + o_e] = ZERO;
                    // *
                    // *           Update singular vectors if desired
                    // *
                    if (NCVT > 0)
                    {
                        this._dlasr.Run("L", "V", "B", M - LL + 1, NCVT, WORK, NM12 + 1 + o_work
                                        , WORK, NM13 + 1 + o_work, ref VT, LL+1 * LDVT + o_vt, LDVT);
                    }
                    if (NRU > 0)
                    {
                        this._dlasr.Run("R", "V", "B", NRU, M - LL + 1, WORK, 1 + o_work
                                        , WORK, N + o_work, ref U, 1+LL * LDU + o_u, LDU);
                    }
                    if (NCC > 0)
                    {
                        this._dlasr.Run("L", "V", "B", M - LL + 1, NCC, WORK, 1 + o_work
                                        , WORK, N + o_work, ref C, LL+1 * LDC + o_c, LDC);
                    }
                }
            }
            // *
            // *     QR iteration finished, go back and check convergence
            // *
            goto LABEL60;
            // *
            // *     All singular values converged, so make them positive
            // *
        LABEL160:;
            for (I = 1; I <= N; I++)
            {
                if (D[I + o_d] < ZERO)
                {
                    D[I + o_d] =  - D[I + o_d];
                    // *
                    // *           Change sign of singular vectors, if desired
                    // *
                    if (NCVT > 0) this._dscal.Run(NCVT, NEGONE, ref VT, I+1 * LDVT + o_vt, LDVT);
                }
            }
            // *
            // *     Sort the singular values into decreasing order (insertion sort on
            // *     singular values, but only one transposition per singular vector)
            // *
            for (I = 1; I <= N - 1; I++)
            {
                // *
                // *        Scan for smallest D(I)
                // *
                ISUB = 1;
                SMIN = D[1 + o_d];
                for (J = 2; J <= N + 1 - I; J++)
                {
                    if (D[J + o_d] <= SMIN)
                    {
                        ISUB = J;
                        SMIN = D[J + o_d];
                    }
                }
                if (ISUB != N + 1 - I)
                {
                    // *
                    // *           Swap singular values and vectors
                    // *
                    D[ISUB + o_d] = D[N + 1 - I + o_d];
                    D[N + 1 - I + o_d] = SMIN;
                    if (NCVT > 0) this._dswap.Run(NCVT, ref VT, ISUB+1 * LDVT + o_vt, LDVT, ref VT, N + 1 - I+1 * LDVT + o_vt, LDVT);
                    if (NRU > 0) this._dswap.Run(NRU, ref U, 1+ISUB * LDU + o_u, 1, ref U, 1+(N + 1 - I) * LDU + o_u, 1);
                    if (NCC > 0) this._dswap.Run(NCC, ref C, ISUB+1 * LDC + o_c, LDC, ref C, N + 1 - I+1 * LDC + o_c, LDC);
                }
            }
            goto LABEL220;
            // *
            // *     Maximum number of iterations exceeded, failure to converge
            // *
        LABEL200:;
            INFO = 0;
            for (I = 1; I <= N - 1; I++)
            {
                if (E[I + o_e] != ZERO) INFO += 1;
            }
        LABEL220:;
            return;
            // *
            // *     End of DBDSQR
            // *

            #endregion

        }
    }
}
