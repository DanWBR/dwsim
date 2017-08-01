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
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLASDQ computes the singular value decomposition (SVD) of a real
    /// (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
    /// E, accumulating the transformations if desired. Letting B denote
    /// the input bidiagonal matrix, the algorithm computes orthogonal
    /// matrices Q and P such that B = Q * S * P' (P' denotes the transpose
    /// of P). The singular values S are overwritten on D.
    /// 
    /// The input matrix U  is changed to U  * Q  if desired.
    /// The input matrix VT is changed to P' * VT if desired.
    /// The input matrix C  is changed to Q' * C  if desired.
    /// 
    /// See "Computing  Small Singular Values of Bidiagonal Matrices With
    /// Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
    /// LAPACK Working Note #3, for a detailed description of the algorithm.
    /// 
    ///</summary>
    public class DLASDQ
    {
    

        #region Dependencies
        
        DBDSQR _dbdsqr; DLARTG _dlartg; DLASR _dlasr; DSWAP _dswap; XERBLA _xerbla; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DLASDQ(DBDSQR dbdsqr, DLARTG dlartg, DLASR dlasr, DSWAP dswap, XERBLA xerbla, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dbdsqr = dbdsqr; this._dlartg = dlartg; this._dlasr = dlasr; this._dswap = dswap; this._xerbla = xerbla; 
            this._lsame = lsame;

            #endregion

        }
    
        public DLASDQ()
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
            DBDSQR dbdsqr = new DBDSQR(lsame, dlamch, dlartg, dlas2, dlasq1, dlasr, dlasv2, drot, dscal, dswap
                                       , xerbla);

            #endregion


            #region Set Dependencies
            
            this._dbdsqr = dbdsqr; this._dlartg = dlartg; this._dlasr = dlasr; this._dswap = dswap; this._xerbla = xerbla; 
            this._lsame = lsame;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASDQ computes the singular value decomposition (SVD) of a real
        /// (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
        /// E, accumulating the transformations if desired. Letting B denote
        /// the input bidiagonal matrix, the algorithm computes orthogonal
        /// matrices Q and P such that B = Q * S * P' (P' denotes the transpose
        /// of P). The singular values S are overwritten on D.
        /// 
        /// The input matrix U  is changed to U  * Q  if desired.
        /// The input matrix VT is changed to P' * VT if desired.
        /// The input matrix C  is changed to Q' * C  if desired.
        /// 
        /// See "Computing  Small Singular Values of Bidiagonal Matrices With
        /// Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
        /// LAPACK Working Note #3, for a detailed description of the algorithm.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// On entry, UPLO specifies whether the input bidiagonal matrix
        /// is upper or lower bidiagonal, and wether it is square are
        /// not.
        /// UPLO = 'U' or 'u'   B is upper bidiagonal.
        /// UPLO = 'L' or 'l'   B is lower bidiagonal.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// = 0: then the input matrix is N-by-N.
        /// = 1: then the input matrix is N-by-(N+1) if UPLU = 'U' and
        /// (N+1)-by-N if UPLU = 'L'.
        /// 
        /// The bidiagonal matrix has
        /// N = NL + NR + 1 rows and
        /// M = N + SQRE .GE. N columns.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// On entry, N specifies the number of rows and columns
        /// in the matrix. N must be at least 0.
        ///</param>
        /// <param name="NCVT">
        /// (input) INTEGER
        /// On entry, NCVT specifies the number of columns of
        /// the matrix VT. NCVT must be at least 0.
        ///</param>
        /// <param name="NRU">
        /// (input) INTEGER
        /// On entry, NRU specifies the number of rows of
        /// the matrix U. NRU must be at least 0.
        ///</param>
        /// <param name="NCC">
        /// (input) INTEGER
        /// On entry, NCC specifies the number of columns of
        /// the matrix C. NCC must be at least 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, D contains the diagonal entries of the
        /// bidiagonal matrix whose SVD is desired. On normal exit,
        /// D contains the singular values in ascending order.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array.
        /// dimension is (N-1) if SQRE = 0 and N if SQRE = 1.
        /// On entry, the entries of E contain the offdiagonal entries
        /// of the bidiagonal matrix whose SVD is desired. On normal
        /// exit, E will contain 0. If the algorithm does not converge,
        /// D and E will contain the diagonal and superdiagonal entries
        /// of a bidiagonal matrix orthogonally equivalent to the one
        /// given as input.
        ///</param>
        /// <param name="VT">
        /// (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
        /// On entry, contains a matrix which on exit has been
        /// premultiplied by P', dimension N-by-NCVT if SQRE = 0
        /// and (N+1)-by-NCVT if SQRE = 1 (not referenced if NCVT=0).
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// On entry, LDVT specifies the leading dimension of VT as
        /// declared in the calling (sub) program. LDVT must be at
        /// least 1. If NCVT is nonzero LDVT must also be at least N.
        ///</param>
        /// <param name="U">
        /// (input/output) DOUBLE PRECISION array, dimension (LDU, N)
        /// On entry, contains a  matrix which on exit has been
        /// postmultiplied by Q, dimension NRU-by-N if SQRE = 0
        /// and NRU-by-(N+1) if SQRE = 1 (not referenced if NRU=0).
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// On entry, LDU  specifies the leading dimension of U as
        /// declared in the calling (sub) program. LDU must be at
        /// least max( 1, NRU ) .
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
        /// On entry, contains an N-by-NCC matrix which on exit
        /// has been premultiplied by Q'  dimension N-by-NCC if SQRE = 0
        /// and (N+1)-by-NCC if SQRE = 1 (not referenced if NCC=0).
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// On entry, LDC  specifies the leading dimension of C as
        /// declared in the calling (sub) program. LDC must be at
        /// least 1. If NCC is nonzero, LDC must also be at least N.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (4*N)
        /// Workspace. Only referenced if one of NCVT, NRU, or NCC is
        /// nonzero, and if N is at least 2.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// On exit, a value of 0 indicates a successful exit.
        /// If INFO .LT. 0, argument number -INFO is illegal.
        /// If INFO .GT. 0, the algorithm did not converge, and INFO
        /// specifies how many superdiagonals did not converge.
        ///</param>
        public void Run(string UPLO, int SQRE, int N, int NCVT, int NRU, int NCC
                         , ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] VT, int offset_vt, int LDVT, ref double[] U, int offset_u, int LDU
                         , ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool ROTATE = false; int I = 0; int ISUB = 0; int IUPLO = 0; int J = 0; int NP1 = 0; int SQRE1 = 0; double CS = 0; 
            double R = 0;double SMIN = 0; double SN = 0; 

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
            // *  -- LAPACK auxiliary routine (version 3.1) --
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
            // *  DLASDQ computes the singular value decomposition (SVD) of a real
            // *  (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
            // *  E, accumulating the transformations if desired. Letting B denote
            // *  the input bidiagonal matrix, the algorithm computes orthogonal
            // *  matrices Q and P such that B = Q * S * P' (P' denotes the transpose
            // *  of P). The singular values S are overwritten on D.
            // *
            // *  The input matrix U  is changed to U  * Q  if desired.
            // *  The input matrix VT is changed to P' * VT if desired.
            // *  The input matrix C  is changed to Q' * C  if desired.
            // *
            // *  See "Computing  Small Singular Values of Bidiagonal Matrices With
            // *  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
            // *  LAPACK Working Note #3, for a detailed description of the algorithm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO  (input) CHARACTER*1
            // *        On entry, UPLO specifies whether the input bidiagonal matrix
            // *        is upper or lower bidiagonal, and wether it is square are
            // *        not.
            // *           UPLO = 'U' or 'u'   B is upper bidiagonal.
            // *           UPLO = 'L' or 'l'   B is lower bidiagonal.
            // *
            // *  SQRE  (input) INTEGER
            // *        = 0: then the input matrix is N-by-N.
            // *        = 1: then the input matrix is N-by-(N+1) if UPLU = 'U' and
            // *             (N+1)-by-N if UPLU = 'L'.
            // *
            // *        The bidiagonal matrix has
            // *        N = NL + NR + 1 rows and
            // *        M = N + SQRE >= N columns.
            // *
            // *  N     (input) INTEGER
            // *        On entry, N specifies the number of rows and columns
            // *        in the matrix. N must be at least 0.
            // *
            // *  NCVT  (input) INTEGER
            // *        On entry, NCVT specifies the number of columns of
            // *        the matrix VT. NCVT must be at least 0.
            // *
            // *  NRU   (input) INTEGER
            // *        On entry, NRU specifies the number of rows of
            // *        the matrix U. NRU must be at least 0.
            // *
            // *  NCC   (input) INTEGER
            // *        On entry, NCC specifies the number of columns of
            // *        the matrix C. NCC must be at least 0.
            // *
            // *  D     (input/output) DOUBLE PRECISION array, dimension (N)
            // *        On entry, D contains the diagonal entries of the
            // *        bidiagonal matrix whose SVD is desired. On normal exit,
            // *        D contains the singular values in ascending order.
            // *
            // *  E     (input/output) DOUBLE PRECISION array.
            // *        dimension is (N-1) if SQRE = 0 and N if SQRE = 1.
            // *        On entry, the entries of E contain the offdiagonal entries
            // *        of the bidiagonal matrix whose SVD is desired. On normal
            // *        exit, E will contain 0. If the algorithm does not converge,
            // *        D and E will contain the diagonal and superdiagonal entries
            // *        of a bidiagonal matrix orthogonally equivalent to the one
            // *        given as input.
            // *
            // *  VT    (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
            // *        On entry, contains a matrix which on exit has been
            // *        premultiplied by P', dimension N-by-NCVT if SQRE = 0
            // *        and (N+1)-by-NCVT if SQRE = 1 (not referenced if NCVT=0).
            // *
            // *  LDVT  (input) INTEGER
            // *        On entry, LDVT specifies the leading dimension of VT as
            // *        declared in the calling (sub) program. LDVT must be at
            // *        least 1. If NCVT is nonzero LDVT must also be at least N.
            // *
            // *  U     (input/output) DOUBLE PRECISION array, dimension (LDU, N)
            // *        On entry, contains a  matrix which on exit has been
            // *        postmultiplied by Q, dimension NRU-by-N if SQRE = 0
            // *        and NRU-by-(N+1) if SQRE = 1 (not referenced if NRU=0).
            // *
            // *  LDU   (input) INTEGER
            // *        On entry, LDU  specifies the leading dimension of U as
            // *        declared in the calling (sub) program. LDU must be at
            // *        least max( 1, NRU ) .
            // *
            // *  C     (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
            // *        On entry, contains an N-by-NCC matrix which on exit
            // *        has been premultiplied by Q'  dimension N-by-NCC if SQRE = 0
            // *        and (N+1)-by-NCC if SQRE = 1 (not referenced if NCC=0).
            // *
            // *  LDC   (input) INTEGER
            // *        On entry, LDC  specifies the leading dimension of C as
            // *        declared in the calling (sub) program. LDC must be at
            // *        least 1. If NCC is nonzero, LDC must also be at least N.
            // *
            // *  WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
            // *        Workspace. Only referenced if one of NCVT, NRU, or NCC is
            // *        nonzero, and if N is at least 2.
            // *
            // *  INFO  (output) INTEGER
            // *        On exit, a value of 0 indicates a successful exit.
            // *        If INFO < 0, argument number -INFO is illegal.
            // *        If INFO > 0, the algorithm did not converge, and INFO
            // *        specifies how many superdiagonals did not converge.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
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
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            IUPLO = 0;
            if (this._lsame.Run(UPLO, "U")) IUPLO = 1;
            if (this._lsame.Run(UPLO, "L")) IUPLO = 2;
            if (IUPLO == 0)
            {
                INFO =  - 1;
            }
            else
            {
                if ((SQRE < 0) || (SQRE > 1))
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
                        if (NCVT < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (NRU < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (NCC < 0)
                                {
                                    INFO =  - 6;
                                }
                                else
                                {
                                    if ((NCVT == 0 && LDVT < 1) || (NCVT > 0 && LDVT < Math.Max(1, N)))
                                    {
                                        INFO =  - 10;
                                    }
                                    else
                                    {
                                        if (LDU < Math.Max(1, NRU))
                                        {
                                            INFO =  - 12;
                                        }
                                        else
                                        {
                                            if ((NCC == 0 && LDC < 1) || (NCC > 0 && LDC < Math.Max(1, N)))
                                            {
                                                INFO =  - 14;
                                            }
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
                this._xerbla.Run("DLASDQ",  - INFO);
                return;
            }
            if (N == 0) return;
            // *
            // *     ROTATE is true if any singular vectors desired, false otherwise
            // *
            ROTATE = (NCVT > 0) || (NRU > 0) || (NCC > 0);
            NP1 = N + 1;
            SQRE1 = SQRE;
            // *
            // *     If matrix non-square upper bidiagonal, rotate to be lower
            // *     bidiagonal.  The rotations are on the right.
            // *
            if ((IUPLO == 1) && (SQRE1 == 1))
            {
                for (I = 1; I <= N - 1; I++)
                {
                    this._dlartg.Run(D[I + o_d], E[I + o_e], ref CS, ref SN, ref R);
                    D[I + o_d] = R;
                    E[I + o_e] = SN * D[I + 1 + o_d];
                    D[I + 1 + o_d] *= CS;
                    if (ROTATE)
                    {
                        WORK[I + o_work] = CS;
                        WORK[N + I + o_work] = SN;
                    }
                }
                this._dlartg.Run(D[N + o_d], E[N + o_e], ref CS, ref SN, ref R);
                D[N + o_d] = R;
                E[N + o_e] = ZERO;
                if (ROTATE)
                {
                    WORK[N + o_work] = CS;
                    WORK[N + N + o_work] = SN;
                }
                IUPLO = 2;
                SQRE1 = 0;
                // *
                // *        Update singular vectors if desired.
                // *
                if (NCVT > 0)
                {
                    this._dlasr.Run("L", "V", "F", NP1, NCVT, WORK, 1 + o_work
                                    , WORK, NP1 + o_work, ref VT, offset_vt, LDVT);
                }
            }
            // *
            // *     If matrix lower bidiagonal, rotate to be upper bidiagonal
            // *     by applying Givens rotations on the left.
            // *
            if (IUPLO == 2)
            {
                for (I = 1; I <= N - 1; I++)
                {
                    this._dlartg.Run(D[I + o_d], E[I + o_e], ref CS, ref SN, ref R);
                    D[I + o_d] = R;
                    E[I + o_e] = SN * D[I + 1 + o_d];
                    D[I + 1 + o_d] *= CS;
                    if (ROTATE)
                    {
                        WORK[I + o_work] = CS;
                        WORK[N + I + o_work] = SN;
                    }
                }
                // *
                // *        If matrix (N+1)-by-N lower bidiagonal, one additional
                // *        rotation is needed.
                // *
                if (SQRE1 == 1)
                {
                    this._dlartg.Run(D[N + o_d], E[N + o_e], ref CS, ref SN, ref R);
                    D[N + o_d] = R;
                    if (ROTATE)
                    {
                        WORK[N + o_work] = CS;
                        WORK[N + N + o_work] = SN;
                    }
                }
                // *
                // *        Update singular vectors if desired.
                // *
                if (NRU > 0)
                {
                    if (SQRE1 == 0)
                    {
                        this._dlasr.Run("R", "V", "F", NRU, N, WORK, 1 + o_work
                                        , WORK, NP1 + o_work, ref U, offset_u, LDU);
                    }
                    else
                    {
                        this._dlasr.Run("R", "V", "F", NRU, NP1, WORK, 1 + o_work
                                        , WORK, NP1 + o_work, ref U, offset_u, LDU);
                    }
                }
                if (NCC > 0)
                {
                    if (SQRE1 == 0)
                    {
                        this._dlasr.Run("L", "V", "F", N, NCC, WORK, 1 + o_work
                                        , WORK, NP1 + o_work, ref C, offset_c, LDC);
                    }
                    else
                    {
                        this._dlasr.Run("L", "V", "F", NP1, NCC, WORK, 1 + o_work
                                        , WORK, NP1 + o_work, ref C, offset_c, LDC);
                    }
                }
            }
            // *
            // *     Call DBDSQR to compute the SVD of the reduced real
            // *     N-by-N upper bidiagonal matrix.
            // *
            this._dbdsqr.Run("U", N, NCVT, NRU, NCC, ref D, offset_d
                             , ref E, offset_e, ref VT, offset_vt, LDVT, ref U, offset_u, LDU, ref C, offset_c
                             , LDC, ref WORK, offset_work, ref INFO);
            // *
            // *     Sort the singular values into ascending order (insertion sort on
            // *     singular values, but only one transposition per singular vector)
            // *
            for (I = 1; I <= N; I++)
            {
                // *
                // *        Scan for smallest D(I).
                // *
                ISUB = I;
                SMIN = D[I + o_d];
                for (J = I + 1; J <= N; J++)
                {
                    if (D[J + o_d] < SMIN)
                    {
                        ISUB = J;
                        SMIN = D[J + o_d];
                    }
                }
                if (ISUB != I)
                {
                    // *
                    // *           Swap singular values and vectors.
                    // *
                    D[ISUB + o_d] = D[I + o_d];
                    D[I + o_d] = SMIN;
                    if (NCVT > 0) this._dswap.Run(NCVT, ref VT, ISUB+1 * LDVT + o_vt, LDVT, ref VT, I+1 * LDVT + o_vt, LDVT);
                    if (NRU > 0) this._dswap.Run(NRU, ref U, 1+ISUB * LDU + o_u, 1, ref U, 1+I * LDU + o_u, 1);
                    if (NCC > 0) this._dswap.Run(NCC, ref C, ISUB+1 * LDC + o_c, LDC, ref C, I+1 * LDC + o_c, LDC);
                }
            }
            // *
            return;
            // *
            // *     End of DLASDQ
            // *

            #endregion

        }
    }
}
