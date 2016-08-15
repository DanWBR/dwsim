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
    /// DLAED2 merges the two sets of eigenvalues together into a single
    /// sorted set.  Then it tries to deflate the size of the problem.
    /// There are two ways in which deflation can occur:  when two or more
    /// eigenvalues are close together or if there is a tiny entry in the
    /// Z vector.  For each such occurrence the order of the related secular
    /// equation problem is reduced by one.
    /// 
    ///</summary>
    public class DLAED2
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DLAMCH _dlamch; DLAPY2 _dlapy2; DCOPY _dcopy; DLACPY _dlacpy; DLAMRG _dlamrg; DROT _drot; DSCAL _dscal; 
        XERBLA _xerbla;

        #endregion


        #region Variables
        
        const double MONE =  - 1.0E0; const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double EIGHT = 8.0E0;int[] CTOT = new int[4]; int[] PSM = new int[4]; 

        #endregion

        public DLAED2(IDAMAX idamax, DLAMCH dlamch, DLAPY2 dlapy2, DCOPY dcopy, DLACPY dlacpy, DLAMRG dlamrg, DROT drot, DSCAL dscal, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dcopy = dcopy; this._dlacpy = dlacpy; 
            this._dlamrg = dlamrg;this._drot = drot; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAED2()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DCOPY dcopy = new DCOPY();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLACPY dlacpy = new DLACPY(lsame);

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dcopy = dcopy; this._dlacpy = dlacpy; 
            this._dlamrg = dlamrg;this._drot = drot; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED2 merges the two sets of eigenvalues together into a single
        /// sorted set.  Then it tries to deflate the size of the problem.
        /// There are two ways in which deflation can occur:  when two or more
        /// eigenvalues are close together or if there is a tiny entry in the
        /// Z vector.  For each such occurrence the order of the related secular
        /// equation problem is reduced by one.
        /// 
        ///</summary>
        /// <param name="K">
        /// (output) INTEGER
        /// The number of non-deflated eigenvalues, and the order of the
        /// related secular equation. 0 .LE. K .LE.N.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the symmetric tridiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="N1">
        /// (input) INTEGER
        /// The location of the last eigenvalue in the leading sub-matrix.
        /// min(1,N) .LE. N1 .LE. N/2.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, D contains the eigenvalues of the two submatrices to
        /// be combined.
        /// On exit, D contains the trailing (N-K) updated eigenvalues
        /// (those which were deflated) sorted into increasing order.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
        /// On entry, Q contains the eigenvectors of two submatrices in
        /// the two square blocks with corners at (1,1), (N1,N1)
        /// and (N1+1, N1+1), (N,N).
        /// On exit, Q contains the trailing (N-K) updated eigenvectors
        /// (those which were deflated) in its last N-K columns.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="INDXQ">
        /// (input/output) INTEGER array, dimension (N)
        /// The permutation which separately sorts the two sub-problems
        /// in D into ascending order.  Note that elements in the second
        /// half of this permutation must first have N1 added to their
        /// values. Destroyed on exit.
        ///</param>
        /// <param name="RHO">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the off-diagonal element associated with the rank-1
        /// cut which originally split the two submatrices which are now
        /// being recombined.
        /// On exit, RHO has been modified to the value required by
        /// DLAED3.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// On entry, Z contains the updating vector (the last
        /// row of the first sub-eigenvector matrix and the first row of
        /// the second sub-eigenvector matrix).
        /// On exit, the contents of Z have been destroyed by the updating
        /// process.
        ///</param>
        /// <param name="DLAMDA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// A copy of the first K eigenvalues which will be used by
        /// DLAED3 to form the secular equation.
        ///</param>
        /// <param name="W">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The first k values of the final deflation-altered z-vector
        /// which will be passed to DLAED3.
        ///</param>
        /// <param name="Q2">
        /// (output) DOUBLE PRECISION array, dimension (N1**2+(N-N1)**2)
        /// A copy of the first K eigenvectors which will be used by
        /// DLAED3 in a matrix multiply (DGEMM) to solve for the new
        /// eigenvectors.
        ///</param>
        /// <param name="INDX">
        /// (workspace) INTEGER array, dimension (N)
        /// The permutation used to sort the contents of DLAMDA into
        /// ascending order.
        ///</param>
        /// <param name="INDXC">
        /// (output) INTEGER array, dimension (N)
        /// The permutation used to arrange the columns of the deflated
        /// Q matrix into three groups:  the first group contains non-zero
        /// elements only at and above N1, the second contains
        /// non-zero elements only below N1, and the third is dense.
        ///</param>
        /// <param name="INDXP">
        /// (workspace) INTEGER array, dimension (N)
        /// The permutation used to place deflated values of D at the end
        /// of the array.  INDXP(1:K) points to the nondeflated D-values
        /// and INDXP(K+1:N) points to the deflated eigenvalues.
        ///</param>
        /// <param name="COLTYP">
        /// (workspace/output) INTEGER array, dimension (N)
        /// During execution, a label which will indicate which of the
        /// following types a column in the Q2 matrix is:
        /// 1 : non-zero in the upper half only;
        /// 2 : dense;
        /// 3 : non-zero in the lower half only;
        /// 4 : deflated.
        /// On exit, COLTYP(i) is the number of columns of type i,
        /// for i=1 to 4 only.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(ref int K, int N, int N1, ref double[] D, int offset_d, ref double[] Q, int offset_q, int LDQ
                         , ref int[] INDXQ, int offset_indxq, ref double RHO, ref double[] Z, int offset_z, ref double[] DLAMDA, int offset_dlamda, ref double[] W, int offset_w, ref double[] Q2, int offset_q2
                         , ref int[] INDX, int offset_indx, ref int[] INDXC, int offset_indxc, ref int[] INDXP, int offset_indxp, ref int[] COLTYP, int offset_coltyp, ref int INFO)
        {

            #region Variables
            
            int o_ctot = -1;int o_psm = -1; int CT = 0; int I = 0; int IMAX = 0; 
            int IQ1 = 0;int IQ2 = 0; int J = 0; int JMAX = 0; int JS = 0; int K2 = 0; int N1P1 = 0; int N2 = 0; int NJ = 0; 
            int PJ = 0;double C = 0; double EPS = 0; double S = 0; double T = 0; double TAU = 0; double TOL = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_indxq = -1 + offset_indxq;  int o_z = -1 + offset_z; 
             int o_dlamda = -1 + offset_dlamda; int o_w = -1 + offset_w;  int o_q2 = -1 + offset_q2; 
             int o_indx = -1 + offset_indx; int o_indxc = -1 + offset_indxc;  int o_indxp = -1 + offset_indxp; 
             int o_coltyp = -1 + offset_coltyp;

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
            // *  DLAED2 merges the two sets of eigenvalues together into a single
            // *  sorted set.  Then it tries to deflate the size of the problem.
            // *  There are two ways in which deflation can occur:  when two or more
            // *  eigenvalues are close together or if there is a tiny entry in the
            // *  Z vector.  For each such occurrence the order of the related secular
            // *  equation problem is reduced by one.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  K      (output) INTEGER
            // *         The number of non-deflated eigenvalues, and the order of the
            // *         related secular equation. 0 <= K <=N.
            // *
            // *  N      (input) INTEGER
            // *         The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  N1     (input) INTEGER
            // *         The location of the last eigenvalue in the leading sub-matrix.
            // *         min(1,N) <= N1 <= N/2.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry, D contains the eigenvalues of the two submatrices to
            // *         be combined.
            // *         On exit, D contains the trailing (N-K) updated eigenvalues
            // *         (those which were deflated) sorted into increasing order.
            // *
            // *  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
            // *         On entry, Q contains the eigenvectors of two submatrices in
            // *         the two square blocks with corners at (1,1), (N1,N1)
            // *         and (N1+1, N1+1), (N,N).
            // *         On exit, Q contains the trailing (N-K) updated eigenvectors
            // *         (those which were deflated) in its last N-K columns.
            // *
            // *  LDQ    (input) INTEGER
            // *         The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  INDXQ  (input/output) INTEGER array, dimension (N)
            // *         The permutation which separately sorts the two sub-problems
            // *         in D into ascending order.  Note that elements in the second
            // *         half of this permutation must first have N1 added to their
            // *         values. Destroyed on exit.
            // *
            // *  RHO    (input/output) DOUBLE PRECISION
            // *         On entry, the off-diagonal element associated with the rank-1
            // *         cut which originally split the two submatrices which are now
            // *         being recombined.
            // *         On exit, RHO has been modified to the value required by
            // *         DLAED3.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension (N)
            // *         On entry, Z contains the updating vector (the last
            // *         row of the first sub-eigenvector matrix and the first row of
            // *         the second sub-eigenvector matrix).
            // *         On exit, the contents of Z have been destroyed by the updating
            // *         process.
            // *
            // *  DLAMDA (output) DOUBLE PRECISION array, dimension (N)
            // *         A copy of the first K eigenvalues which will be used by
            // *         DLAED3 to form the secular equation.
            // *
            // *  W      (output) DOUBLE PRECISION array, dimension (N)
            // *         The first k values of the final deflation-altered z-vector
            // *         which will be passed to DLAED3.
            // *
            // *  Q2     (output) DOUBLE PRECISION array, dimension (N1**2+(N-N1)**2)
            // *         A copy of the first K eigenvectors which will be used by
            // *         DLAED3 in a matrix multiply (DGEMM) to solve for the new
            // *         eigenvectors.
            // *
            // *  INDX   (workspace) INTEGER array, dimension (N)
            // *         The permutation used to sort the contents of DLAMDA into
            // *         ascending order.
            // *
            // *  INDXC  (output) INTEGER array, dimension (N)
            // *         The permutation used to arrange the columns of the deflated
            // *         Q matrix into three groups:  the first group contains non-zero
            // *         elements only at and above N1, the second contains
            // *         non-zero elements only below N1, and the third is dense.
            // *
            // *  INDXP  (workspace) INTEGER array, dimension (N)
            // *         The permutation used to place deflated values of D at the end
            // *         of the array.  INDXP(1:K) points to the nondeflated D-values
            // *         and INDXP(K+1:N) points to the deflated eigenvalues.
            // *
            // *  COLTYP (workspace/output) INTEGER array, dimension (N)
            // *         During execution, a label which will indicate which of the
            // *         following types a column in the Q2 matrix is:
            // *         1 : non-zero in the upper half only;
            // *         2 : dense;
            // *         3 : non-zero in the lower half only;
            // *         4 : deflated.
            // *         On exit, COLTYP(i) is the number of columns of type i,
            // *         for i=1 to 4 only.
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
            // *     .. Local Arrays ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
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
                INFO =  - 2;
            }
            else
            {
                if (LDQ < Math.Max(1, N))
                {
                    INFO =  - 6;
                }
                else
                {
                    if (Math.Min(1, (N / 2)) > N1 || (N / 2) < N1)
                    {
                        INFO =  - 3;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED2",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            N2 = N - N1;
            N1P1 = N1 + 1;
            // *
            if (RHO < ZERO)
            {
                this._dscal.Run(N2, MONE, ref Z, N1P1 + o_z, 1);
            }
            // *
            // *     Normalize z so that norm(z) = 1.  Since z is the concatenation of
            // *     two normalized vectors, norm2(z) = sqrt(2).
            // *
            T = ONE / Math.Sqrt(TWO);
            this._dscal.Run(N, T, ref Z, offset_z, 1);
            // *
            // *     RHO = ABS( norm(z)**2 * RHO )
            // *
            RHO = Math.Abs(TWO * RHO);
            // *
            // *     Sort the eigenvalues into increasing order
            // *
            for (I = N1P1; I <= N; I++)
            {
                INDXQ[I + o_indxq] += N1;
            }
            // *
            // *     re-integrate the deflated parts from the last pass
            // *
            for (I = 1; I <= N; I++)
            {
                DLAMDA[I + o_dlamda] = D[INDXQ[I + o_indxq] + o_d];
            }
            this._dlamrg.Run(N1, N2, DLAMDA, offset_dlamda, 1, 1, ref INDXC, offset_indxc);
            for (I = 1; I <= N; I++)
            {
                INDX[I + o_indx] = INDXQ[INDXC[I + o_indxc] + o_indxq];
            }
            // *
            // *     Calculate the allowable deflation tolerance
            // *
            IMAX = this._idamax.Run(N, Z, offset_z, 1);
            JMAX = this._idamax.Run(N, D, offset_d, 1);
            EPS = this._dlamch.Run("Epsilon");
            TOL = EIGHT * EPS * Math.Max(Math.Abs(D[JMAX + o_d]), Math.Abs(Z[IMAX + o_z]));
            // *
            // *     If the rank-1 modifier is small enough, no more needs to be done
            // *     except to reorganize Q so that its columns correspond with the
            // *     elements in D.
            // *
            if (RHO * Math.Abs(Z[IMAX + o_z]) <= TOL)
            {
                K = 0;
                IQ2 = 1;
                for (J = 1; J <= N; J++)
                {
                    I = INDX[J + o_indx];
                    this._dcopy.Run(N, Q, 1+I * LDQ + o_q, 1, ref Q2, IQ2 + o_q2, 1);
                    DLAMDA[J + o_dlamda] = D[I + o_d];
                    IQ2 += N;
                }
                this._dlacpy.Run("A", N, N, Q2, offset_q2, N, ref Q, offset_q
                                 , LDQ);
                this._dcopy.Run(N, DLAMDA, offset_dlamda, 1, ref D, offset_d, 1);
                goto LABEL190;
            }
            // *
            // *     If there are multiple eigenvalues then the problem deflates.  Here
            // *     the number of equal eigenvalues are found.  As each equal
            // *     eigenvalue is found, an elementary reflector is computed to rotate
            // *     the corresponding eigensubspace so that the corresponding
            // *     components of Z are zero in this new basis.
            // *
            for (I = 1; I <= N1; I++)
            {
                COLTYP[I + o_coltyp] = 1;
            }
            for (I = N1P1; I <= N; I++)
            {
                COLTYP[I + o_coltyp] = 3;
            }
            // *
            // *
            K = 0;
            K2 = N + 1;
            for (J = 1; J <= N; J++)
            {
                NJ = INDX[J + o_indx];
                if (RHO * Math.Abs(Z[NJ + o_z]) <= TOL)
                {
                    // *
                    // *           Deflate due to small z component.
                    // *
                    K2 -= 1;
                    COLTYP[NJ + o_coltyp] = 4;
                    INDXP[K2 + o_indxp] = NJ;
                    if (J == N) goto LABEL100;
                }
                else
                {
                    PJ = NJ;
                    goto LABEL80;
                }
            }
        LABEL80:;
            J += 1;
            NJ = INDX[J + o_indx];
            if (J > N) goto LABEL100;
            if (RHO * Math.Abs(Z[NJ + o_z]) <= TOL)
            {
                // *
                // *        Deflate due to small z component.
                // *
                K2 -= 1;
                COLTYP[NJ + o_coltyp] = 4;
                INDXP[K2 + o_indxp] = NJ;
            }
            else
            {
                // *
                // *        Check if eigenvalues are close enough to allow deflation.
                // *
                S = Z[PJ + o_z];
                C = Z[NJ + o_z];
                // *
                // *        Find sqrt(a**2+b**2) without overflow or
                // *        destructive underflow.
                // *
                TAU = this._dlapy2.Run(C, S);
                T = D[NJ + o_d] - D[PJ + o_d];
                C /= TAU;
                S =  - S / TAU;
                if (Math.Abs(T * C * S) <= TOL)
                {
                    // *
                    // *           Deflation is possible.
                    // *
                    Z[NJ + o_z] = TAU;
                    Z[PJ + o_z] = ZERO;
                    if (COLTYP[NJ + o_coltyp] != COLTYP[PJ + o_coltyp]) COLTYP[NJ + o_coltyp] = 2;
                    COLTYP[PJ + o_coltyp] = 4;
                    this._drot.Run(N, ref Q, 1+PJ * LDQ + o_q, 1, ref Q, 1+NJ * LDQ + o_q, 1, C
                                   , S);
                    T = D[PJ + o_d] * Math.Pow(C,2) + D[NJ + o_d] * Math.Pow(S,2);
                    D[NJ + o_d] = D[PJ + o_d] * Math.Pow(S,2) + D[NJ + o_d] * Math.Pow(C,2);
                    D[PJ + o_d] = T;
                    K2 -= 1;
                    I = 1;
                LABEL90:;
                    if (K2 + I <= N)
                    {
                        if (D[PJ + o_d] < D[INDXP[K2 + I + o_indxp] + o_d])
                        {
                            INDXP[K2 + I - 1 + o_indxp] = INDXP[K2 + I + o_indxp];
                            INDXP[K2 + I + o_indxp] = PJ;
                            I += 1;
                            goto LABEL90;
                        }
                        else
                        {
                            INDXP[K2 + I - 1 + o_indxp] = PJ;
                        }
                    }
                    else
                    {
                        INDXP[K2 + I - 1 + o_indxp] = PJ;
                    }
                    PJ = NJ;
                }
                else
                {
                    K += 1;
                    DLAMDA[K + o_dlamda] = D[PJ + o_d];
                    W[K + o_w] = Z[PJ + o_z];
                    INDXP[K + o_indxp] = PJ;
                    PJ = NJ;
                }
            }
            goto LABEL80;
        LABEL100:;
            // *
            // *     Record the last eigenvalue.
            // *
            K += 1;
            DLAMDA[K + o_dlamda] = D[PJ + o_d];
            W[K + o_w] = Z[PJ + o_z];
            INDXP[K + o_indxp] = PJ;
            // *
            // *     Count up the total number of the various types of columns, then
            // *     form a permutation which positions the four column types into
            // *     four uniform groups (although one or more of these groups may be
            // *     empty).
            // *
            for (J = 1; J <= 4; J++)
            {
                CTOT[J + o_ctot] = 0;
            }
            for (J = 1; J <= N; J++)
            {
                CT = COLTYP[J + o_coltyp];
                CTOT[CT + o_ctot] += 1;
            }
            // *
            // *     PSM(*) = Position in SubMatrix (of types 1 through 4)
            // *
            PSM[1 + o_psm] = 1;
            PSM[2 + o_psm] = 1 + CTOT[1 + o_ctot];
            PSM[3 + o_psm] = PSM[2 + o_psm] + CTOT[2 + o_ctot];
            PSM[4 + o_psm] = PSM[3 + o_psm] + CTOT[3 + o_ctot];
            K = N - CTOT[4 + o_ctot];
            // *
            // *     Fill out the INDXC array so that the permutation which it induces
            // *     will place all type-1 columns first, all type-2 columns next,
            // *     then all type-3's, and finally all type-4's.
            // *
            for (J = 1; J <= N; J++)
            {
                JS = INDXP[J + o_indxp];
                CT = COLTYP[JS + o_coltyp];
                INDX[PSM[CT + o_psm] + o_indx] = JS;
                INDXC[PSM[CT + o_psm] + o_indxc] = J;
                PSM[CT + o_psm] += 1;
            }
            // *
            // *     Sort the eigenvalues and corresponding eigenvectors into DLAMDA
            // *     and Q2 respectively.  The eigenvalues/vectors which were not
            // *     deflated go into the first K slots of DLAMDA and Q2 respectively,
            // *     while those which were deflated go into the last N - K slots.
            // *
            I = 1;
            IQ1 = 1;
            IQ2 = 1 + (CTOT[1 + o_ctot] + CTOT[2 + o_ctot]) * N1;
            for (J = 1; J <= CTOT[1 + o_ctot]; J++)
            {
                JS = INDX[I + o_indx];
                this._dcopy.Run(N1, Q, 1+JS * LDQ + o_q, 1, ref Q2, IQ1 + o_q2, 1);
                Z[I + o_z] = D[JS + o_d];
                I += 1;
                IQ1 += N1;
            }
            // *
            for (J = 1; J <= CTOT[2 + o_ctot]; J++)
            {
                JS = INDX[I + o_indx];
                this._dcopy.Run(N1, Q, 1+JS * LDQ + o_q, 1, ref Q2, IQ1 + o_q2, 1);
                this._dcopy.Run(N2, Q, N1 + 1+JS * LDQ + o_q, 1, ref Q2, IQ2 + o_q2, 1);
                Z[I + o_z] = D[JS + o_d];
                I += 1;
                IQ1 += N1;
                IQ2 += N2;
            }
            // *
            for (J = 1; J <= CTOT[3 + o_ctot]; J++)
            {
                JS = INDX[I + o_indx];
                this._dcopy.Run(N2, Q, N1 + 1+JS * LDQ + o_q, 1, ref Q2, IQ2 + o_q2, 1);
                Z[I + o_z] = D[JS + o_d];
                I += 1;
                IQ2 += N2;
            }
            // *
            IQ1 = IQ2;
            for (J = 1; J <= CTOT[4 + o_ctot]; J++)
            {
                JS = INDX[I + o_indx];
                this._dcopy.Run(N, Q, 1+JS * LDQ + o_q, 1, ref Q2, IQ2 + o_q2, 1);
                IQ2 += N;
                Z[I + o_z] = D[JS + o_d];
                I += 1;
            }
            // *
            // *     The deflated eigenvalues and their corresponding vectors go back
            // *     into the last N - K slots of D and Q respectively.
            // *
            this._dlacpy.Run("A", N, CTOT[4 + o_ctot], Q2, IQ1 + o_q2, N, ref Q, 1+(K + 1) * LDQ + o_q
                             , LDQ);
            this._dcopy.Run(N - K, Z, K + 1 + o_z, 1, ref D, K + 1 + o_d, 1);
            // *
            // *     Copy CTOT into COLTYP for referencing in DLAED3.
            // *
            for (J = 1; J <= 4; J++)
            {
                COLTYP[J + o_coltyp] = CTOT[J + o_ctot];
            }
            // *
        LABEL190:;
            return;
            // *
            // *     End of DLAED2
            // *

            #endregion

        }
    }
}
