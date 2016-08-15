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
    /// DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
    /// symmetric tridiagonal matrix using the implicit QL or QR method.
    /// The eigenvectors of a full or band symmetric matrix can also be found
    /// if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
    /// tridiagonal form.
    /// 
    ///</summary>
    public class DSTEQR
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; DLANST _dlanst; DLAPY2 _dlapy2; DLAE2 _dlae2; DLAEV2 _dlaev2; DLARTG _dlartg; 
        DLASCL _dlascl;DLASET _dlaset; DLASR _dlasr; DLASRT _dlasrt; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; const double THREE = 3.0E0; 
        const int MAXIT = 30;

        #endregion

        public DSTEQR(LSAME lsame, DLAMCH dlamch, DLANST dlanst, DLAPY2 dlapy2, DLAE2 dlae2, DLAEV2 dlaev2, DLARTG dlartg, DLASCL dlascl, DLASET dlaset, DLASR dlasr
                      , DLASRT dlasrt, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlanst = dlanst; this._dlapy2 = dlapy2; this._dlae2 = dlae2; 
            this._dlaev2 = dlaev2;this._dlartg = dlartg; this._dlascl = dlascl; this._dlaset = dlaset; this._dlasr = dlasr; 
            this._dlasrt = dlasrt;this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
    
        public DSTEQR()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAE2 dlae2 = new DLAE2();
            DLAEV2 dlaev2 = new DLAEV2();
            XERBLA xerbla = new XERBLA();
            DSWAP dswap = new DSWAP();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANST dlanst = new DLANST(lsame, dlassq);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLASET dlaset = new DLASET(lsame);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlanst = dlanst; this._dlapy2 = dlapy2; this._dlae2 = dlae2; 
            this._dlaev2 = dlaev2;this._dlartg = dlartg; this._dlascl = dlascl; this._dlaset = dlaset; this._dlasr = dlasr; 
            this._dlasrt = dlasrt;this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
        /// symmetric tridiagonal matrix using the implicit QL or QR method.
        /// The eigenvectors of a full or band symmetric matrix can also be found
        /// if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
        /// tridiagonal form.
        /// 
        ///</summary>
        /// <param name="COMPZ">
        /// (input) CHARACTER*1
        /// = 'N':  Compute eigenvalues only.
        /// = 'V':  Compute eigenvalues and eigenvectors of the original
        /// symmetric matrix.  On entry, Z must contain the
        /// orthogonal matrix used to reduce the original matrix
        /// to tridiagonal form.
        /// = 'I':  Compute eigenvalues and eigenvectors of the
        /// tridiagonal matrix.  Z is initialized to the identity
        /// matrix.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix.  N .GE. 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the diagonal elements of the tridiagonal matrix.
        /// On exit, if INFO = 0, the eigenvalues in ascending order.
        ///</param>
        /// <param name="E">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
        /// On entry, if  COMPZ = 'V', then Z contains the orthogonal
        /// matrix used in the reduction to tridiagonal form.
        /// On exit, if INFO = 0, then if  COMPZ = 'V', Z contains the
        /// orthonormal eigenvectors of the original symmetric matrix,
        /// and if COMPZ = 'I', Z contains the orthonormal eigenvectors
        /// of the symmetric tridiagonal matrix.
        /// If COMPZ = 'N', then Z is not referenced.
        ///</param>
        /// <param name="LDZ">
        /// (input) INTEGER
        /// The leading dimension of the array Z.  LDZ .GE. 1, and if
        /// eigenvectors are desired, then  LDZ .GE. max(1,N).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
        /// If COMPZ = 'N', then WORK is not referenced.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  the algorithm has failed to find all the eigenvalues in
        /// a total of 30*N iterations; if INFO = i, then i
        /// elements of E have not converged to zero; on exit, D
        /// and E contain the elements of a symmetric tridiagonal
        /// matrix which is orthogonally similar to the original
        /// matrix.
        ///</param>
        public void Run(string COMPZ, int N, ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] Z, int offset_z, int LDZ
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int ICOMPZ = 0; int II = 0; int ISCALE = 0; int J = 0; int JTOT = 0; int K = 0; int L = 0; int L1 = 0; 
            int LEND = 0;int LENDM1 = 0; int LENDP1 = 0; int LENDSV = 0; int LM1 = 0; int LSV = 0; int M = 0; int MM = 0; 
            int MM1 = 0;int NM1 = 0; int NMAXIT = 0; double ANORM = 0; double B = 0; double C = 0; double EPS = 0; 
            double EPS2 = 0;double F = 0; double G = 0; double P = 0; double R = 0; double RT1 = 0; double RT2 = 0; double S = 0; 
            double SAFMAX = 0;double SAFMIN = 0; double SSFMAX = 0; double SSFMIN = 0; double TST = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_z = -1 - LDZ + offset_z;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            COMPZ = COMPZ.Substring(0, 1);  

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
            // *  DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
            // *  symmetric tridiagonal matrix using the implicit QL or QR method.
            // *  The eigenvectors of a full or band symmetric matrix can also be found
            // *  if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
            // *  tridiagonal form.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  COMPZ   (input) CHARACTER*1
            // *          = 'N':  Compute eigenvalues only.
            // *          = 'V':  Compute eigenvalues and eigenvectors of the original
            // *                  symmetric matrix.  On entry, Z must contain the
            // *                  orthogonal matrix used to reduce the original matrix
            // *                  to tridiagonal form.
            // *          = 'I':  Compute eigenvalues and eigenvectors of the
            // *                  tridiagonal matrix.  Z is initialized to the identity
            // *                  matrix.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix.  N >= 0.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the diagonal elements of the tridiagonal matrix.
            // *          On exit, if INFO = 0, the eigenvalues in ascending order.
            // *
            // *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, the (n-1) subdiagonal elements of the tridiagonal
            // *          matrix.
            // *          On exit, E has been destroyed.
            // *
            // *  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
            // *          On entry, if  COMPZ = 'V', then Z contains the orthogonal
            // *          matrix used in the reduction to tridiagonal form.
            // *          On exit, if INFO = 0, then if  COMPZ = 'V', Z contains the
            // *          orthonormal eigenvectors of the original symmetric matrix,
            // *          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
            // *          of the symmetric tridiagonal matrix.
            // *          If COMPZ = 'N', then Z is not referenced.
            // *
            // *  LDZ     (input) INTEGER
            // *          The leading dimension of the array Z.  LDZ >= 1, and if
            // *          eigenvectors are desired, then  LDZ >= max(1,N).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
            // *          If COMPZ = 'N', then WORK is not referenced.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  the algorithm has failed to find all the eigenvalues in
            // *                a total of 30*N iterations; if INFO = i, then i
            // *                elements of E have not converged to zero; on exit, D
            // *                and E contain the elements of a symmetric tridiagonal
            // *                matrix which is orthogonally similar to the original
            // *                matrix.
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
            //      INTRINSIC          ABS, MAX, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
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
            if (INFO != 0)
            {
                this._xerbla.Run("DSTEQR",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            if (N == 1)
            {
                if (ICOMPZ == 2) Z[1+1 * LDZ + o_z] = ONE;
                return;
            }
            // *
            // *     Determine the unit roundoff and over/underflow thresholds.
            // *
            EPS = this._dlamch.Run("E");
            EPS2 = Math.Pow(EPS,2);
            SAFMIN = this._dlamch.Run("S");
            SAFMAX = ONE / SAFMIN;
            SSFMAX = Math.Sqrt(SAFMAX) / THREE;
            SSFMIN = Math.Sqrt(SAFMIN) / EPS2;
            // *
            // *     Compute the eigenvalues and eigenvectors of the tridiagonal
            // *     matrix.
            // *
            if (ICOMPZ == 2)
            {
                this._dlaset.Run("Full", N, N, ZERO, ONE, ref Z, offset_z
                                 , LDZ);
            }
            // *
            NMAXIT = N * MAXIT;
            JTOT = 0;
            // *
            // *     Determine where the matrix splits and choose QL or QR iteration
            // *     for each block, according to whether top or bottom diagonal
            // *     element is smaller.
            // *
            L1 = 1;
            NM1 = N - 1;
            // *
        LABEL10:;
            if (L1 > N) goto LABEL160;
            if (L1 > 1) E[L1 - 1 + o_e] = ZERO;
            if (L1 <= NM1)
            {
                for (M = L1; M <= NM1; M++)
                {
                    TST = Math.Abs(E[M + o_e]);
                    if (TST == ZERO) goto LABEL30;
                    if (TST <= (Math.Sqrt(Math.Abs(D[M + o_d])) * Math.Sqrt(Math.Abs(D[M + 1 + o_d]))) * EPS)
                    {
                        E[M + o_e] = ZERO;
                        goto LABEL30;
                    }
                }
            }
            M = N;
            // *
        LABEL30:;
            L = L1;
            LSV = L;
            LEND = M;
            LENDSV = LEND;
            L1 = M + 1;
            if (LEND == L) goto LABEL10;
            // *
            // *     Scale submatrix in rows and columns L to LEND
            // *
            ANORM = this._dlanst.Run("I", LEND - L + 1, D, L + o_d, E, L + o_e);
            ISCALE = 0;
            if (ANORM == ZERO) goto LABEL10;
            if (ANORM > SSFMAX)
            {
                ISCALE = 1;
                this._dlascl.Run("G", 0, 0, ANORM, SSFMAX, LEND - L + 1
                                 , 1, ref D, L + o_d, N, ref INFO);
                this._dlascl.Run("G", 0, 0, ANORM, SSFMAX, LEND - L
                                 , 1, ref E, L + o_e, N, ref INFO);
            }
            else
            {
                if (ANORM < SSFMIN)
                {
                    ISCALE = 2;
                    this._dlascl.Run("G", 0, 0, ANORM, SSFMIN, LEND - L + 1
                                     , 1, ref D, L + o_d, N, ref INFO);
                    this._dlascl.Run("G", 0, 0, ANORM, SSFMIN, LEND - L
                                     , 1, ref E, L + o_e, N, ref INFO);
                }
            }
            // *
            // *     Choose between QL and QR iteration
            // *
            if (Math.Abs(D[LEND + o_d]) < Math.Abs(D[L + o_d]))
            {
                LEND = LSV;
                L = LENDSV;
            }
            // *
            if (LEND > L)
            {
                // *
                // *        QL Iteration
                // *
                // *        Look for small subdiagonal element.
                // *
            LABEL40:;
                if (L != LEND)
                {
                    LENDM1 = LEND - 1;
                    for (M = L; M <= LENDM1; M++)
                    {
                        TST = Math.Pow(Math.Abs(E[M + o_e]),2);
                        if (TST <= (EPS2 * Math.Abs(D[M + o_d])) * Math.Abs(D[M + 1 + o_d]) + SAFMIN) goto LABEL60;
                    }
                }
                // *
                M = LEND;
                // *
            LABEL60:;
                if (M < LEND) E[M + o_e] = ZERO;
                P = D[L + o_d];
                if (M == L) goto LABEL80;
                // *
                // *        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                // *        to compute its eigensystem.
                // *
                if (M == L + 1)
                {
                    if (ICOMPZ > 0)
                    {
                        this._dlaev2.Run(D[L + o_d], E[L + o_e], D[L + 1 + o_d], ref RT1, ref RT2, ref C
                                         , ref S);
                        WORK[L + o_work] = C;
                        WORK[N - 1 + L + o_work] = S;
                        this._dlasr.Run("R", "V", "B", N, 2, WORK, L + o_work
                                        , WORK, N - 1 + L + o_work, ref Z, 1+L * LDZ + o_z, LDZ);
                    }
                    else
                    {
                        this._dlae2.Run(D[L + o_d], E[L + o_e], D[L + 1 + o_d], ref RT1, ref RT2);
                    }
                    D[L + o_d] = RT1;
                    D[L + 1 + o_d] = RT2;
                    E[L + o_e] = ZERO;
                    L += 2;
                    if (L <= LEND) goto LABEL40;
                    goto LABEL140;
                }
                // *
                if (JTOT == NMAXIT) goto LABEL140;
                JTOT += 1;
                // *
                // *        Form shift.
                // *
                G = (D[L + 1 + o_d] - P) / (TWO * E[L + o_e]);
                R = this._dlapy2.Run(G, ONE);
                G = D[M + o_d] - P + (E[L + o_e] / (G + FortranLib.Sign(R,G)));
                // *
                S = ONE;
                C = ONE;
                P = ZERO;
                // *
                // *        Inner loop
                // *
                MM1 = M - 1;
                for (I = MM1; I >= L; I +=  - 1)
                {
                    F = S * E[I + o_e];
                    B = C * E[I + o_e];
                    this._dlartg.Run(G, F, ref C, ref S, ref R);
                    if (I != M - 1) E[I + 1 + o_e] = R;
                    G = D[I + 1 + o_d] - P;
                    R = (D[I + o_d] - G) * S + TWO * C * B;
                    P = S * R;
                    D[I + 1 + o_d] = G + P;
                    G = C * R - B;
                    // *
                    // *           If eigenvectors are desired, then save rotations.
                    // *
                    if (ICOMPZ > 0)
                    {
                        WORK[I + o_work] = C;
                        WORK[N - 1 + I + o_work] =  - S;
                    }
                    // *
                }
                // *
                // *        If eigenvectors are desired, then apply saved rotations.
                // *
                if (ICOMPZ > 0)
                {
                    MM = M - L + 1;
                    this._dlasr.Run("R", "V", "B", N, MM, WORK, L + o_work
                                    , WORK, N - 1 + L + o_work, ref Z, 1+L * LDZ + o_z, LDZ);
                }
                // *
                D[L + o_d] -= P;
                E[L + o_e] = G;
                goto LABEL40;
                // *
                // *        Eigenvalue found.
                // *
            LABEL80:;
                D[L + o_d] = P;
                // *
                L += 1;
                if (L <= LEND) goto LABEL40;
                goto LABEL140;
                // *
            }
            else
            {
                // *
                // *        QR Iteration
                // *
                // *        Look for small superdiagonal element.
                // *
            LABEL90:;
                if (L != LEND)
                {
                    LENDP1 = LEND + 1;
                    for (M = L; M >= LENDP1; M +=  - 1)
                    {
                        TST = Math.Pow(Math.Abs(E[M - 1 + o_e]),2);
                        if (TST <= (EPS2 * Math.Abs(D[M + o_d])) * Math.Abs(D[M - 1 + o_d]) + SAFMIN) goto LABEL110;
                    }
                }
                // *
                M = LEND;
                // *
            LABEL110:;
                if (M > LEND) E[M - 1 + o_e] = ZERO;
                P = D[L + o_d];
                if (M == L) goto LABEL130;
                // *
                // *        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                // *        to compute its eigensystem.
                // *
                if (M == L - 1)
                {
                    if (ICOMPZ > 0)
                    {
                        this._dlaev2.Run(D[L - 1 + o_d], E[L - 1 + o_e], D[L + o_d], ref RT1, ref RT2, ref C
                                         , ref S);
                        WORK[M + o_work] = C;
                        WORK[N - 1 + M + o_work] = S;
                        this._dlasr.Run("R", "V", "F", N, 2, WORK, M + o_work
                                        , WORK, N - 1 + M + o_work, ref Z, 1+(L - 1) * LDZ + o_z, LDZ);
                    }
                    else
                    {
                        this._dlae2.Run(D[L - 1 + o_d], E[L - 1 + o_e], D[L + o_d], ref RT1, ref RT2);
                    }
                    D[L - 1 + o_d] = RT1;
                    D[L + o_d] = RT2;
                    E[L - 1 + o_e] = ZERO;
                    L -= 2;
                    if (L >= LEND) goto LABEL90;
                    goto LABEL140;
                }
                // *
                if (JTOT == NMAXIT) goto LABEL140;
                JTOT += 1;
                // *
                // *        Form shift.
                // *
                G = (D[L - 1 + o_d] - P) / (TWO * E[L - 1 + o_e]);
                R = this._dlapy2.Run(G, ONE);
                G = D[M + o_d] - P + (E[L - 1 + o_e] / (G + FortranLib.Sign(R,G)));
                // *
                S = ONE;
                C = ONE;
                P = ZERO;
                // *
                // *        Inner loop
                // *
                LM1 = L - 1;
                for (I = M; I <= LM1; I++)
                {
                    F = S * E[I + o_e];
                    B = C * E[I + o_e];
                    this._dlartg.Run(G, F, ref C, ref S, ref R);
                    if (I != M) E[I - 1 + o_e] = R;
                    G = D[I + o_d] - P;
                    R = (D[I + 1 + o_d] - G) * S + TWO * C * B;
                    P = S * R;
                    D[I + o_d] = G + P;
                    G = C * R - B;
                    // *
                    // *           If eigenvectors are desired, then save rotations.
                    // *
                    if (ICOMPZ > 0)
                    {
                        WORK[I + o_work] = C;
                        WORK[N - 1 + I + o_work] = S;
                    }
                    // *
                }
                // *
                // *        If eigenvectors are desired, then apply saved rotations.
                // *
                if (ICOMPZ > 0)
                {
                    MM = L - M + 1;
                    this._dlasr.Run("R", "V", "F", N, MM, WORK, M + o_work
                                    , WORK, N - 1 + M + o_work, ref Z, 1+M * LDZ + o_z, LDZ);
                }
                // *
                D[L + o_d] -= P;
                E[LM1 + o_e] = G;
                goto LABEL90;
                // *
                // *        Eigenvalue found.
                // *
            LABEL130:;
                D[L + o_d] = P;
                // *
                L -= 1;
                if (L >= LEND) goto LABEL90;
                goto LABEL140;
                // *
            }
            // *
            // *     Undo scaling if necessary
            // *
        LABEL140:;
            if (ISCALE == 1)
            {
                this._dlascl.Run("G", 0, 0, SSFMAX, ANORM, LENDSV - LSV + 1
                                 , 1, ref D, LSV + o_d, N, ref INFO);
                this._dlascl.Run("G", 0, 0, SSFMAX, ANORM, LENDSV - LSV
                                 , 1, ref E, LSV + o_e, N, ref INFO);
            }
            else
            {
                if (ISCALE == 2)
                {
                    this._dlascl.Run("G", 0, 0, SSFMIN, ANORM, LENDSV - LSV + 1
                                     , 1, ref D, LSV + o_d, N, ref INFO);
                    this._dlascl.Run("G", 0, 0, SSFMIN, ANORM, LENDSV - LSV
                                     , 1, ref E, LSV + o_e, N, ref INFO);
                }
            }
            // *
            // *     Check for no convergence to an eigenvalue after a total
            // *     of N*MAXIT iterations.
            // *
            if (JTOT < NMAXIT) goto LABEL10;
            for (I = 1; I <= N - 1; I++)
            {
                if (E[I + o_e] != ZERO) INFO += 1;
            }
            goto LABEL190;
            // *
            // *     Order eigenvalues and eigenvectors.
            // *
        LABEL160:;
            if (ICOMPZ == 0)
            {
                // *
                // *        Use Quick Sort
                // *
                this._dlasrt.Run("I", N, ref D, offset_d, ref INFO);
                // *
            }
            else
            {
                // *
                // *        Use Selection Sort to minimize swaps of eigenvectors
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
            // *
        LABEL190:;
            return;
            // *
            // *     End of DSTEQR
            // *

            #endregion

        }
    }
}
