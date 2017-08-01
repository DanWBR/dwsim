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
    /// DLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
    /// an upper quasi-triangular matrix T by an orthogonal similarity
    /// transformation.
    /// 
    /// T must be in Schur canonical form, that is, block upper triangular
    /// with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
    /// has its diagonal elemnts equal and its off-diagonal elements of
    /// opposite sign.
    /// 
    ///</summary>
    public class DLAEXC
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLANGE _dlange; DLACPY _dlacpy; DLANV2 _dlanv2; DLARFG _dlarfg; DLARFX _dlarfx; DLARTG _dlartg; 
        DLASY2 _dlasy2;DROT _drot; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TEN = 1.0E+1; const int LDD = 4; const int LDX = 2; 
        double[] D = new double[LDD * 4];double[] U = new double[3]; double[] U1 = new double[3]; double[] U2 = new double[3]; 
        double[] X = new double[LDX * 2];

        #endregion

        public DLAEXC(DLAMCH dlamch, DLANGE dlange, DLACPY dlacpy, DLANV2 dlanv2, DLARFG dlarfg, DLARFX dlarfx, DLARTG dlartg, DLASY2 dlasy2, DROT drot)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlange = dlange; this._dlacpy = dlacpy; this._dlanv2 = dlanv2; this._dlarfg = dlarfg; 
            this._dlarfx = dlarfx;this._dlartg = dlartg; this._dlasy2 = dlasy2; this._drot = drot; 

            #endregion

        }
    
        public DLAEXC()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            IDAMAX idamax = new IDAMAX();
            DCOPY dcopy = new DCOPY();
            DSWAP dswap = new DSWAP();
            DROT drot = new DROT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANGE dlange = new DLANGE(dlassq, lsame);
            DLACPY dlacpy = new DLACPY(lsame);
            DLANV2 dlanv2 = new DLANV2(dlamch, dlapy2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARFX dlarfx = new DLARFX(lsame, dgemv, dger);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASY2 dlasy2 = new DLASY2(idamax, dlamch, dcopy, dswap);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlange = dlange; this._dlacpy = dlacpy; this._dlanv2 = dlanv2; this._dlarfg = dlarfg; 
            this._dlarfx = dlarfx;this._dlartg = dlartg; this._dlasy2 = dlasy2; this._drot = drot; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
        /// an upper quasi-triangular matrix T by an orthogonal similarity
        /// transformation.
        /// 
        /// T must be in Schur canonical form, that is, block upper triangular
        /// with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
        /// has its diagonal elemnts equal and its off-diagonal elements of
        /// opposite sign.
        /// 
        ///</summary>
        /// <param name="WANTQ">
        /// (input) LOGICAL
        /// = .TRUE. : accumulate the transformation in the matrix Q;
        /// = .FALSE.: do not accumulate the transformation.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix T. N .GE. 0.
        ///</param>
        /// <param name="T">
        /// must be in Schur canonical form, that is, block upper triangular
        ///</param>
        /// <param name="LDT">
        /// (input)  INTEGER
        /// The leading dimension of the array T. LDT .GE. max(1,N).
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// On entry, if WANTQ is .TRUE., the orthogonal matrix Q.
        /// On exit, if WANTQ is .TRUE., the updated matrix Q.
        /// If WANTQ is .FALSE., Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.
        /// LDQ .GE. 1; and if WANTQ is .TRUE., LDQ .GE. N.
        ///</param>
        /// <param name="J1">
        /// (input) INTEGER
        /// The index of the first row of the first block T11.
        ///</param>
        /// <param name="N1">
        /// (input) INTEGER
        /// The order of the first block T11. N1 = 0, 1 or 2.
        ///</param>
        /// <param name="N2">
        /// (input) INTEGER
        /// The order of the second block T22. N2 = 0, 1 or 2.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// = 1: the transformed matrix T would be too far from Schur
        /// form; the blocks are not swapped and T and Q are
        /// unchanged.
        ///</param>
        public void Run(bool WANTQ, int N, ref double[] T, int offset_t, int LDT, ref double[] Q, int offset_q, int LDQ
                         , int J1, int N1, int N2, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int IERR = 0; int J2 = 0; int J3 = 0; int J4 = 0; int K = 0; int ND = 0; double CS = 0; double DNORM = 0; 
            double EPS = 0;double SCALE = 0; double SMLNUM = 0; double SN = 0; double T11 = 0; double T22 = 0; double T33 = 0; 
            double TAU = 0;double TAU1 = 0; double TAU2 = 0; double TEMP = 0; double THRESH = 0; double WI1 = 0; double WI2 = 0; 
            double WR1 = 0;double WR2 = 0; double XNORM = 0; int offset_d = 0; int o_d = -1 - LDD; int offset_u = 0; int o_u = -1; 
            int offset_u1 = 0; int o_u1 = -1;int offset_u2 = 0; int o_u2 = -1; int offset_x = 0; int o_x = -1 - LDX; 

            #endregion


            #region Array Index Correction
            
             int o_t = -1 - LDT + offset_t;  int o_q = -1 - LDQ + offset_q;  int o_work = -1 + offset_work; 

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
            // *  DLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
            // *  an upper quasi-triangular matrix T by an orthogonal similarity
            // *  transformation.
            // *
            // *  T must be in Schur canonical form, that is, block upper triangular
            // *  with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
            // *  has its diagonal elemnts equal and its off-diagonal elements of
            // *  opposite sign.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  WANTQ   (input) LOGICAL
            // *          = .TRUE. : accumulate the transformation in the matrix Q;
            // *          = .FALSE.: do not accumulate the transformation.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix T. N >= 0.
            // *
            // *  T       (input/output) DOUBLE PRECISION array, dimension (LDT,N)
            // *          On entry, the upper quasi-triangular matrix T, in Schur
            // *          canonical form.
            // *          On exit, the updated matrix T, again in Schur canonical form.
            // *
            // *  LDT     (input)  INTEGER
            // *          The leading dimension of the array T. LDT >= max(1,N).
            // *
            // *  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          On entry, if WANTQ is .TRUE., the orthogonal matrix Q.
            // *          On exit, if WANTQ is .TRUE., the updated matrix Q.
            // *          If WANTQ is .FALSE., Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q.
            // *          LDQ >= 1; and if WANTQ is .TRUE., LDQ >= N.
            // *
            // *  J1      (input) INTEGER
            // *          The index of the first row of the first block T11.
            // *
            // *  N1      (input) INTEGER
            // *          The order of the first block T11. N1 = 0, 1 or 2.
            // *
            // *  N2      (input) INTEGER
            // *          The order of the second block T22. N2 = 0, 1 or 2.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          = 1: the transformed matrix T would be too far from Schur
            // *               form; the blocks are not swapped and T and Q are
            // *               unchanged.
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
            //      INTRINSIC          ABS, MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            // *     Quick return if possible
            // *
            if (N == 0 || N1 == 0 || N2 == 0) return;
            if (J1 + N1 > N) return;
            // *
            J2 = J1 + 1;
            J3 = J1 + 2;
            J4 = J1 + 3;
            // *
            if (N1 == 1 && N2 == 1)
            {
                // *
                // *        Swap two 1-by-1 blocks.
                // *
                T11 = T[J1+J1 * LDT + o_t];
                T22 = T[J2+J2 * LDT + o_t];
                // *
                // *        Determine the transformation to perform the interchange.
                // *
                this._dlartg.Run(T[J1+J2 * LDT + o_t], T22 - T11, ref CS, ref SN, ref TEMP);
                // *
                // *        Apply transformation to the matrix T.
                // *
                if (J3 <= N)
                {
                    this._drot.Run(N - J1 - 1, ref T, J1+J3 * LDT + o_t, LDT, ref T, J2+J3 * LDT + o_t, LDT, CS
                                   , SN);
                }
                this._drot.Run(J1 - 1, ref T, 1+J1 * LDT + o_t, 1, ref T, 1+J2 * LDT + o_t, 1, CS
                               , SN);
                // *
                T[J1+J1 * LDT + o_t] = T22;
                T[J2+J2 * LDT + o_t] = T11;
                // *
                if (WANTQ)
                {
                    // *
                    // *           Accumulate transformation in the matrix Q.
                    // *
                    this._drot.Run(N, ref Q, 1+J1 * LDQ + o_q, 1, ref Q, 1+J2 * LDQ + o_q, 1, CS
                                   , SN);
                }
                // *
            }
            else
            {
                // *
                // *        Swapping involves at least one 2-by-2 block.
                // *
                // *        Copy the diagonal block of order N1+N2 to the local array D
                // *        and compute its norm.
                // *
                ND = N1 + N2;
                this._dlacpy.Run("Full", ND, ND, T, J1+J1 * LDT + o_t, LDT, ref D, offset_d
                                 , LDD);
                DNORM = this._dlange.Run("Max", ND, ND, D, offset_d, LDD, ref WORK, offset_work);
                // *
                // *        Compute machine-dependent threshold for test for accepting
                // *        swap.
                // *
                EPS = this._dlamch.Run("P");
                SMLNUM = this._dlamch.Run("S") / EPS;
                THRESH = Math.Max(TEN * EPS * DNORM, SMLNUM);
                // *
                // *        Solve T11*X - X*T22 = scale*T12 for X.
                // *
                this._dlasy2.Run(false, false,  - 1, N1, N2, D, offset_d
                                 , LDD, D, N1 + 1+(N1 + 1) * LDD + o_d, LDD, D, 1+(N1 + 1) * LDD + o_d, LDD, ref SCALE
                                 , ref X, offset_x, LDX, ref XNORM, ref IERR);
                // *
                // *        Swap the adjacent diagonal blocks.
                // *
                K = N1 + N1 + N2 - 3;
                switch (K)
                {
                    case 1: goto LABEL10;
                    case 2: goto LABEL20;
                    case 3: goto LABEL30;
                }
                // *
            LABEL10:;
                // *
                // *        N1 = 1, N2 = 2: generate elementary reflector H so that:
                // *
                // *        ( scale, X11, X12 ) H = ( 0, 0, * )
                // *
                U[1 + o_u] = SCALE;
                U[2 + o_u] = X[1+1 * LDX + o_x];
                U[3 + o_u] = X[1+2 * LDX + o_x];
                this._dlarfg.Run(3, ref U[3 + o_u], ref U, offset_u, 1, ref TAU);
                U[3 + o_u] = ONE;
                T11 = T[J1+J1 * LDT + o_t];
                // *
                // *        Perform swap provisionally on diagonal block in D.
                // *
                this._dlarfx.Run("L", 3, 3, U, offset_u, TAU, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                this._dlarfx.Run("R", 3, 3, U, offset_u, TAU, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                // *
                // *        Test whether to reject swap.
                // *
                if (Math.Max(Math.Abs(D[3+1 * LDD + o_d]), Math.Max(Math.Abs(D[3+2 * LDD + o_d]), Math.Abs(D[3+3 * LDD + o_d] - T11))) > THRESH) goto LABEL50;
                // *
                // *        Accept swap: apply transformation to the entire matrix T.
                // *
                this._dlarfx.Run("L", 3, N - J1 + 1, U, offset_u, TAU, ref T, J1+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                this._dlarfx.Run("R", J2, 3, U, offset_u, TAU, ref T, 1+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                // *
                T[J3+J1 * LDT + o_t] = ZERO;
                T[J3+J2 * LDT + o_t] = ZERO;
                T[J3+J3 * LDT + o_t] = T11;
                // *
                if (WANTQ)
                {
                    // *
                    // *           Accumulate transformation in the matrix Q.
                    // *
                    this._dlarfx.Run("R", N, 3, U, offset_u, TAU, ref Q, 1+J1 * LDQ + o_q
                                     , LDQ, ref WORK, offset_work);
                }
                goto LABEL40;
                // *
            LABEL20:;
                // *
                // *        N1 = 2, N2 = 1: generate elementary reflector H so that:
                // *
                // *        H (  -X11 ) = ( * )
                // *          (  -X21 ) = ( 0 )
                // *          ( scale ) = ( 0 )
                // *
                U[1 + o_u] =  - X[1+1 * LDX + o_x];
                U[2 + o_u] =  - X[2+1 * LDX + o_x];
                U[3 + o_u] = SCALE;
                this._dlarfg.Run(3, ref U[1 + o_u], ref U, 2 + o_u, 1, ref TAU);
                U[1 + o_u] = ONE;
                T33 = T[J3+J3 * LDT + o_t];
                // *
                // *        Perform swap provisionally on diagonal block in D.
                // *
                this._dlarfx.Run("L", 3, 3, U, offset_u, TAU, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                this._dlarfx.Run("R", 3, 3, U, offset_u, TAU, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                // *
                // *        Test whether to reject swap.
                // *
                if (Math.Max(Math.Abs(D[2+1 * LDD + o_d]), Math.Max(Math.Abs(D[3+1 * LDD + o_d]), Math.Abs(D[1+1 * LDD + o_d] - T33))) > THRESH) goto LABEL50;
                // *
                // *        Accept swap: apply transformation to the entire matrix T.
                // *
                this._dlarfx.Run("R", J3, 3, U, offset_u, TAU, ref T, 1+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                this._dlarfx.Run("L", 3, N - J1, U, offset_u, TAU, ref T, J1+J2 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                // *
                T[J1+J1 * LDT + o_t] = T33;
                T[J2+J1 * LDT + o_t] = ZERO;
                T[J3+J1 * LDT + o_t] = ZERO;
                // *
                if (WANTQ)
                {
                    // *
                    // *           Accumulate transformation in the matrix Q.
                    // *
                    this._dlarfx.Run("R", N, 3, U, offset_u, TAU, ref Q, 1+J1 * LDQ + o_q
                                     , LDQ, ref WORK, offset_work);
                }
                goto LABEL40;
                // *
            LABEL30:;
                // *
                // *        N1 = 2, N2 = 2: generate elementary reflectors H(1) and H(2) so
                // *        that:
                // *
                // *        H(2) H(1) (  -X11  -X12 ) = (  *  * )
                // *                  (  -X21  -X22 )   (  0  * )
                // *                  ( scale    0  )   (  0  0 )
                // *                  (    0  scale )   (  0  0 )
                // *
                U1[1 + o_u1] =  - X[1+1 * LDX + o_x];
                U1[2 + o_u1] =  - X[2+1 * LDX + o_x];
                U1[3 + o_u1] = SCALE;
                this._dlarfg.Run(3, ref U1[1 + o_u1], ref U1, 2 + o_u1, 1, ref TAU1);
                U1[1 + o_u1] = ONE;
                // *
                TEMP =  - TAU1 * (X[1+2 * LDX + o_x] + U1[2 + o_u1] * X[2+2 * LDX + o_x]);
                U2[1 + o_u2] =  - TEMP * U1[2 + o_u1] - X[2+2 * LDX + o_x];
                U2[2 + o_u2] =  - TEMP * U1[3 + o_u1];
                U2[3 + o_u2] = SCALE;
                this._dlarfg.Run(3, ref U2[1 + o_u2], ref U2, 2 + o_u2, 1, ref TAU2);
                U2[1 + o_u2] = ONE;
                // *
                // *        Perform swap provisionally on diagonal block in D.
                // *
                this._dlarfx.Run("L", 3, 4, U1, offset_u1, TAU1, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                this._dlarfx.Run("R", 4, 3, U1, offset_u1, TAU1, ref D, offset_d
                                 , LDD, ref WORK, offset_work);
                this._dlarfx.Run("L", 3, 4, U2, offset_u2, TAU2, ref D, 2+1 * LDD + o_d
                                 , LDD, ref WORK, offset_work);
                this._dlarfx.Run("R", 4, 3, U2, offset_u2, TAU2, ref D, 1+2 * LDD + o_d
                                 , LDD, ref WORK, offset_work);
                // *
                // *        Test whether to reject swap.
                // *
                if (Math.Max(Math.Abs(D[3+1 * LDD + o_d]), Math.Max(Math.Abs(D[3+2 * LDD + o_d]), Math.Max(Math.Abs(D[4+1 * LDD + o_d]), Math.Abs(D[4+2 * LDD + o_d])))) > THRESH) goto LABEL50;
                // *
                // *        Accept swap: apply transformation to the entire matrix T.
                // *
                this._dlarfx.Run("L", 3, N - J1 + 1, U1, offset_u1, TAU1, ref T, J1+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                this._dlarfx.Run("R", J4, 3, U1, offset_u1, TAU1, ref T, 1+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                this._dlarfx.Run("L", 3, N - J1 + 1, U2, offset_u2, TAU2, ref T, J2+J1 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                this._dlarfx.Run("R", J4, 3, U2, offset_u2, TAU2, ref T, 1+J2 * LDT + o_t
                                 , LDT, ref WORK, offset_work);
                // *
                T[J3+J1 * LDT + o_t] = ZERO;
                T[J3+J2 * LDT + o_t] = ZERO;
                T[J4+J1 * LDT + o_t] = ZERO;
                T[J4+J2 * LDT + o_t] = ZERO;
                // *
                if (WANTQ)
                {
                    // *
                    // *           Accumulate transformation in the matrix Q.
                    // *
                    this._dlarfx.Run("R", N, 3, U1, offset_u1, TAU1, ref Q, 1+J1 * LDQ + o_q
                                     , LDQ, ref WORK, offset_work);
                    this._dlarfx.Run("R", N, 3, U2, offset_u2, TAU2, ref Q, 1+J2 * LDQ + o_q
                                     , LDQ, ref WORK, offset_work);
                }
                // *
            LABEL40:;
                // *
                if (N2 == 2)
                {
                    // *
                    // *           Standardize new 2-by-2 block T11
                    // *
                    this._dlanv2.Run(ref T[J1+J1 * LDT + o_t], ref T[J1+J2 * LDT + o_t], ref T[J2+J1 * LDT + o_t], ref T[J2+J2 * LDT + o_t], ref WR1, ref WI1
                                     , ref WR2, ref WI2, ref CS, ref SN);
                    this._drot.Run(N - J1 - 1, ref T, J1+(J1 + 2) * LDT + o_t, LDT, ref T, J2+(J1 + 2) * LDT + o_t, LDT, CS
                                   , SN);
                    this._drot.Run(J1 - 1, ref T, 1+J1 * LDT + o_t, 1, ref T, 1+J2 * LDT + o_t, 1, CS
                                   , SN);
                    if (WANTQ)
                    {
                        this._drot.Run(N, ref Q, 1+J1 * LDQ + o_q, 1, ref Q, 1+J2 * LDQ + o_q, 1, CS
                                       , SN);
                    }
                }
                // *
                if (N1 == 2)
                {
                    // *
                    // *           Standardize new 2-by-2 block T22
                    // *
                    J3 = J1 + N2;
                    J4 = J3 + 1;
                    this._dlanv2.Run(ref T[J3+J3 * LDT + o_t], ref T[J3+J4 * LDT + o_t], ref T[J4+J3 * LDT + o_t], ref T[J4+J4 * LDT + o_t], ref WR1, ref WI1
                                     , ref WR2, ref WI2, ref CS, ref SN);
                    if (J3 + 2 <= N)
                    {
                        this._drot.Run(N - J3 - 1, ref T, J3+(J3 + 2) * LDT + o_t, LDT, ref T, J4+(J3 + 2) * LDT + o_t, LDT, CS
                                       , SN);
                    }
                    this._drot.Run(J3 - 1, ref T, 1+J3 * LDT + o_t, 1, ref T, 1+J4 * LDT + o_t, 1, CS
                                   , SN);
                    if (WANTQ)
                    {
                        this._drot.Run(N, ref Q, 1+J3 * LDQ + o_q, 1, ref Q, 1+J4 * LDQ + o_q, 1, CS
                                       , SN);
                    }
                }
                // *
            }
            return;
            // *
            // *     Exit with INFO = 1 if swap was rejected.
            // *
        LABEL50:;
            INFO = 1;
            return;
            // *
            // *     End of DLAEXC
            // *

            #endregion

        }
    }
}
