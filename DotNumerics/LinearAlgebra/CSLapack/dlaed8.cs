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
    /// DLAED8 merges the two sets of eigenvalues together into a single
    /// sorted set.  Then it tries to deflate the size of the problem.
    /// There are two ways in which deflation can occur:  when two or more
    /// eigenvalues are close together or if there is a tiny element in the
    /// Z vector.  For each such occurrence the order of the related secular
    /// equation problem is reduced by one.
    /// 
    ///</summary>
    public class DLAED8
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DLAMCH _dlamch; DLAPY2 _dlapy2; DCOPY _dcopy; DLACPY _dlacpy; DLAMRG _dlamrg; DROT _drot; DSCAL _dscal; 
        XERBLA _xerbla;

        #endregion


        #region Variables
        
        const double MONE =  - 1.0E0; const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double EIGHT = 8.0E0;

        #endregion

        public DLAED8(IDAMAX idamax, DLAMCH dlamch, DLAPY2 dlapy2, DCOPY dcopy, DLACPY dlacpy, DLAMRG dlamrg, DROT drot, DSCAL dscal, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dcopy = dcopy; this._dlacpy = dlacpy; 
            this._dlamrg = dlamrg;this._drot = drot; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAED8()
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
        /// DLAED8 merges the two sets of eigenvalues together into a single
        /// sorted set.  Then it tries to deflate the size of the problem.
        /// There are two ways in which deflation can occur:  when two or more
        /// eigenvalues are close together or if there is a tiny element in the
        /// Z vector.  For each such occurrence the order of the related secular
        /// equation problem is reduced by one.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// = 0:  Compute eigenvalues only.
        /// = 1:  Compute eigenvectors of original dense symmetric matrix
        /// also.  On entry, Q contains the orthogonal matrix used
        /// to reduce the original matrix to tridiagonal form.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER
        /// The number of non-deflated eigenvalues, and the order of the
        /// related secular equation.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the symmetric tridiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="QSIZ">
        /// (input) INTEGER
        /// The dimension of the orthogonal matrix used to reduce
        /// the full matrix to tridiagonal form.  QSIZ .GE. N if ICOMPQ = 1.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the eigenvalues of the two submatrices to be
        /// combined.  On exit, the trailing (N-K) updated eigenvalues
        /// (those which were deflated) sorted into increasing order.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// If ICOMPQ = 0, Q is not referenced.  Otherwise,
        /// on entry, Q contains the eigenvectors of the partially solved
        /// system which has been previously updated in matrix
        /// multiplies with other partially solved eigensystems.
        /// On exit, Q contains the trailing (N-K) updated eigenvectors
        /// (those which were deflated) in its last N-K columns.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="INDXQ">
        /// (input) INTEGER array, dimension (N)
        /// The permutation which separately sorts the two sub-problems
        /// in D into ascending order.  Note that elements in the second
        /// half of this permutation must first have CUTPNT added to
        /// their values in order to be accurate.
        ///</param>
        /// <param name="RHO">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the off-diagonal element associated with the rank-1
        /// cut which originally split the two submatrices which are now
        /// being recombined.
        /// On exit, RHO has been modified to the value required by
        /// DLAED3.
        ///</param>
        /// <param name="CUTPNT">
        /// (input) INTEGER
        /// The location of the last eigenvalue in the leading
        /// sub-matrix.  min(1,N) .LE. CUTPNT .LE. N.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// On entry, Z contains the updating vector (the last row of
        /// the first sub-eigenvector matrix and the first row of the
        /// second sub-eigenvector matrix).
        /// On exit, the contents of Z are destroyed by the updating
        /// process.
        ///</param>
        /// <param name="DLAMDA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// A copy of the first K eigenvalues which will be used by
        /// DLAED3 to form the secular equation.
        ///</param>
        /// <param name="Q2">
        /// (output) DOUBLE PRECISION array, dimension (LDQ2,N)
        /// If ICOMPQ = 0, Q2 is not referenced.  Otherwise,
        /// a copy of the first K eigenvectors which will be used by
        /// DLAED7 in a matrix multiply (DGEMM) to update the new
        /// eigenvectors.
        ///</param>
        /// <param name="LDQ2">
        /// (input) INTEGER
        /// The leading dimension of the array Q2.  LDQ2 .GE. max(1,N).
        ///</param>
        /// <param name="W">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The first k values of the final deflation-altered z-vector and
        /// will be passed to DLAED3.
        ///</param>
        /// <param name="PERM">
        /// (output) INTEGER array, dimension (N)
        /// The permutations (from deflation and sorting) to be applied
        /// to each eigenblock.
        ///</param>
        /// <param name="GIVPTR">
        /// (output) INTEGER
        /// The number of Givens rotations which took place in this
        /// subproblem.
        ///</param>
        /// <param name="GIVCOL">
        /// (output) INTEGER array, dimension (2, N)
        /// Each pair of numbers indicates a pair of columns to take place
        /// in a Givens rotation.
        ///</param>
        /// <param name="GIVNUM">
        /// (output) DOUBLE PRECISION array, dimension (2, N)
        /// Each number indicates the S value to be used in the
        /// corresponding Givens rotation.
        ///</param>
        /// <param name="INDXP">
        /// (workspace) INTEGER array, dimension (N)
        /// The permutation used to place deflated values of D at the end
        /// of the array.  INDXP(1:K) points to the nondeflated D-values
        /// and INDXP(K+1:N) points to the deflated eigenvalues.
        ///</param>
        /// <param name="INDX">
        /// (workspace) INTEGER array, dimension (N)
        /// The permutation used to sort the contents of D into ascending
        /// order.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int ICOMPQ, ref int K, int N, int QSIZ, ref double[] D, int offset_d, ref double[] Q, int offset_q
                         , int LDQ, ref int[] INDXQ, int offset_indxq, ref double RHO, int CUTPNT, ref double[] Z, int offset_z, ref double[] DLAMDA, int offset_dlamda
                         , ref double[] Q2, int offset_q2, int LDQ2, ref double[] W, int offset_w, ref int[] PERM, int offset_perm, ref int GIVPTR, ref int[] GIVCOL, int offset_givcol
                         , ref double[] GIVNUM, int offset_givnum, ref int[] INDXP, int offset_indxp, ref int[] INDX, int offset_indx, ref int INFO)
        {

            #region Variables
            
            int I = 0; int IMAX = 0; int J = 0; int JLAM = 0; int JMAX = 0; int JP = 0; int K2 = 0; int N1 = 0; int N1P1 = 0; 
            int N2 = 0;double C = 0; double EPS = 0; double S = 0; double T = 0; double TAU = 0; double TOL = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_indxq = -1 + offset_indxq;  int o_z = -1 + offset_z; 
             int o_dlamda = -1 + offset_dlamda; int o_q2 = -1 - LDQ2 + offset_q2;  int o_w = -1 + offset_w; 
             int o_perm = -1 + offset_perm; int o_givcol = -3 + offset_givcol;  int o_givnum = -3 + offset_givnum; 
             int o_indxp = -1 + offset_indxp; int o_indx = -1 + offset_indx; 

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
            // *  DLAED8 merges the two sets of eigenvalues together into a single
            // *  sorted set.  Then it tries to deflate the size of the problem.
            // *  There are two ways in which deflation can occur:  when two or more
            // *  eigenvalues are close together or if there is a tiny element in the
            // *  Z vector.  For each such occurrence the order of the related secular
            // *  equation problem is reduced by one.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ  (input) INTEGER
            // *          = 0:  Compute eigenvalues only.
            // *          = 1:  Compute eigenvectors of original dense symmetric matrix
            // *                also.  On entry, Q contains the orthogonal matrix used
            // *                to reduce the original matrix to tridiagonal form.
            // *
            // *  K      (output) INTEGER
            // *         The number of non-deflated eigenvalues, and the order of the
            // *         related secular equation.
            // *
            // *  N      (input) INTEGER
            // *         The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  QSIZ   (input) INTEGER
            // *         The dimension of the orthogonal matrix used to reduce
            // *         the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry, the eigenvalues of the two submatrices to be
            // *         combined.  On exit, the trailing (N-K) updated eigenvalues
            // *         (those which were deflated) sorted into increasing order.
            // *
            // *  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *         If ICOMPQ = 0, Q is not referenced.  Otherwise,
            // *         on entry, Q contains the eigenvectors of the partially solved
            // *         system which has been previously updated in matrix
            // *         multiplies with other partially solved eigensystems.
            // *         On exit, Q contains the trailing (N-K) updated eigenvectors
            // *         (those which were deflated) in its last N-K columns.
            // *
            // *  LDQ    (input) INTEGER
            // *         The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  INDXQ  (input) INTEGER array, dimension (N)
            // *         The permutation which separately sorts the two sub-problems
            // *         in D into ascending order.  Note that elements in the second
            // *         half of this permutation must first have CUTPNT added to
            // *         their values in order to be accurate.
            // *
            // *  RHO    (input/output) DOUBLE PRECISION
            // *         On entry, the off-diagonal element associated with the rank-1
            // *         cut which originally split the two submatrices which are now
            // *         being recombined.
            // *         On exit, RHO has been modified to the value required by
            // *         DLAED3.
            // *
            // *  CUTPNT (input) INTEGER
            // *         The location of the last eigenvalue in the leading
            // *         sub-matrix.  min(1,N) <= CUTPNT <= N.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension (N)
            // *         On entry, Z contains the updating vector (the last row of
            // *         the first sub-eigenvector matrix and the first row of the
            // *         second sub-eigenvector matrix).
            // *         On exit, the contents of Z are destroyed by the updating
            // *         process.
            // *
            // *  DLAMDA (output) DOUBLE PRECISION array, dimension (N)
            // *         A copy of the first K eigenvalues which will be used by
            // *         DLAED3 to form the secular equation.
            // *
            // *  Q2     (output) DOUBLE PRECISION array, dimension (LDQ2,N)
            // *         If ICOMPQ = 0, Q2 is not referenced.  Otherwise,
            // *         a copy of the first K eigenvectors which will be used by
            // *         DLAED7 in a matrix multiply (DGEMM) to update the new
            // *         eigenvectors.
            // *
            // *  LDQ2   (input) INTEGER
            // *         The leading dimension of the array Q2.  LDQ2 >= max(1,N).
            // *
            // *  W      (output) DOUBLE PRECISION array, dimension (N)
            // *         The first k values of the final deflation-altered z-vector and
            // *         will be passed to DLAED3.
            // *
            // *  PERM   (output) INTEGER array, dimension (N)
            // *         The permutations (from deflation and sorting) to be applied
            // *         to each eigenblock.
            // *
            // *  GIVPTR (output) INTEGER
            // *         The number of Givens rotations which took place in this
            // *         subproblem.
            // *
            // *  GIVCOL (output) INTEGER array, dimension (2, N)
            // *         Each pair of numbers indicates a pair of columns to take place
            // *         in a Givens rotation.
            // *
            // *  GIVNUM (output) DOUBLE PRECISION array, dimension (2, N)
            // *         Each number indicates the S value to be used in the
            // *         corresponding Givens rotation.
            // *
            // *  INDXP  (workspace) INTEGER array, dimension (N)
            // *         The permutation used to place deflated values of D at the end
            // *         of the array.  INDXP(1:K) points to the nondeflated D-values
            // *         and INDXP(K+1:N) points to the deflated eigenvalues.
            // *
            // *  INDX   (workspace) INTEGER array, dimension (N)
            // *         The permutation used to sort the contents of D into ascending
            // *         order.
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
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *
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
            if (ICOMPQ < 0 || ICOMPQ > 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 3;
                }
                else
                {
                    if (ICOMPQ == 1 && QSIZ < N)
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LDQ < Math.Max(1, N))
                        {
                            INFO =  - 7;
                        }
                        else
                        {
                            if (CUTPNT < Math.Min(1, N) || CUTPNT > N)
                            {
                                INFO =  - 10;
                            }
                            else
                            {
                                if (LDQ2 < Math.Max(1, N))
                                {
                                    INFO =  - 14;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED8",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            N1 = CUTPNT;
            N2 = N - N1;
            N1P1 = N1 + 1;
            // *
            if (RHO < ZERO)
            {
                this._dscal.Run(N2, MONE, ref Z, N1P1 + o_z, 1);
            }
            // *
            // *     Normalize z so that norm(z) = 1
            // *
            T = ONE / Math.Sqrt(TWO);
            for (J = 1; J <= N; J++)
            {
                INDX[J + o_indx] = J;
            }
            this._dscal.Run(N, T, ref Z, offset_z, 1);
            RHO = Math.Abs(TWO * RHO);
            // *
            // *     Sort the eigenvalues into increasing order
            // *
            for (I = CUTPNT + 1; I <= N; I++)
            {
                INDXQ[I + o_indxq] += CUTPNT;
            }
            for (I = 1; I <= N; I++)
            {
                DLAMDA[I + o_dlamda] = D[INDXQ[I + o_indxq] + o_d];
                W[I + o_w] = Z[INDXQ[I + o_indxq] + o_z];
            }
            I = 1;
            J = CUTPNT + 1;
            this._dlamrg.Run(N1, N2, DLAMDA, offset_dlamda, 1, 1, ref INDX, offset_indx);
            for (I = 1; I <= N; I++)
            {
                D[I + o_d] = DLAMDA[INDX[I + o_indx] + o_dlamda];
                Z[I + o_z] = W[INDX[I + o_indx] + o_w];
            }
            // *
            // *     Calculate the allowable deflation tolerence
            // *
            IMAX = this._idamax.Run(N, Z, offset_z, 1);
            JMAX = this._idamax.Run(N, D, offset_d, 1);
            EPS = this._dlamch.Run("Epsilon");
            TOL = EIGHT * EPS * Math.Abs(D[JMAX + o_d]);
            // *
            // *     If the rank-1 modifier is small enough, no more needs to be done
            // *     except to reorganize Q so that its columns correspond with the
            // *     elements in D.
            // *
            if (RHO * Math.Abs(Z[IMAX + o_z]) <= TOL)
            {
                K = 0;
                if (ICOMPQ == 0)
                {
                    for (J = 1; J <= N; J++)
                    {
                        PERM[J + o_perm] = INDXQ[INDX[J + o_indx] + o_indxq];
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        PERM[J + o_perm] = INDXQ[INDX[J + o_indx] + o_indxq];
                        this._dcopy.Run(QSIZ, Q, 1+(PERM[J + o_perm]) * LDQ + o_q, 1, ref Q2, 1+J * LDQ2 + o_q2, 1);
                    }
                    this._dlacpy.Run("A", QSIZ, N, Q2, 1+1 * LDQ2 + o_q2, LDQ2, ref Q, 1+1 * LDQ + o_q
                                     , LDQ);
                }
                return;
            }
            // *
            // *     If there are multiple eigenvalues then the problem deflates.  Here
            // *     the number of equal eigenvalues are found.  As each equal
            // *     eigenvalue is found, an elementary reflector is computed to rotate
            // *     the corresponding eigensubspace so that the corresponding
            // *     components of Z are zero in this new basis.
            // *
            K = 0;
            GIVPTR = 0;
            K2 = N + 1;
            for (J = 1; J <= N; J++)
            {
                if (RHO * Math.Abs(Z[J + o_z]) <= TOL)
                {
                    // *
                    // *           Deflate due to small z component.
                    // *
                    K2 -= 1;
                    INDXP[K2 + o_indxp] = J;
                    if (J == N) goto LABEL110;
                }
                else
                {
                    JLAM = J;
                    goto LABEL80;
                }
            }
        LABEL80:;
            J += 1;
            if (J > N) goto LABEL100;
            if (RHO * Math.Abs(Z[J + o_z]) <= TOL)
            {
                // *
                // *        Deflate due to small z component.
                // *
                K2 -= 1;
                INDXP[K2 + o_indxp] = J;
            }
            else
            {
                // *
                // *        Check if eigenvalues are close enough to allow deflation.
                // *
                S = Z[JLAM + o_z];
                C = Z[J + o_z];
                // *
                // *        Find sqrt(a**2+b**2) without overflow or
                // *        destructive underflow.
                // *
                TAU = this._dlapy2.Run(C, S);
                T = D[J + o_d] - D[JLAM + o_d];
                C /= TAU;
                S =  - S / TAU;
                if (Math.Abs(T * C * S) <= TOL)
                {
                    // *
                    // *           Deflation is possible.
                    // *
                    Z[J + o_z] = TAU;
                    Z[JLAM + o_z] = ZERO;
                    // *
                    // *           Record the appropriate Givens rotation
                    // *
                    GIVPTR += 1;
                    GIVCOL[1+GIVPTR * 2 + o_givcol] = INDXQ[INDX[JLAM + o_indx] + o_indxq];
                    GIVCOL[2+GIVPTR * 2 + o_givcol] = INDXQ[INDX[J + o_indx] + o_indxq];
                    GIVNUM[1+GIVPTR * 2 + o_givnum] = C;
                    GIVNUM[2+GIVPTR * 2 + o_givnum] = S;
                    if (ICOMPQ == 1)
                    {
                        this._drot.Run(QSIZ, ref Q, 1+(INDXQ[INDX[JLAM + o_indx] + o_indxq]) * LDQ + o_q, 1, ref Q, 1+(INDXQ[INDX[J + o_indx] + o_indxq]) * LDQ + o_q, 1, C
                                       , S);
                    }
                    T = D[JLAM + o_d] * C * C + D[J + o_d] * S * S;
                    D[J + o_d] = D[JLAM + o_d] * S * S + D[J + o_d] * C * C;
                    D[JLAM + o_d] = T;
                    K2 -= 1;
                    I = 1;
                LABEL90:;
                    if (K2 + I <= N)
                    {
                        if (D[JLAM + o_d] < D[INDXP[K2 + I + o_indxp] + o_d])
                        {
                            INDXP[K2 + I - 1 + o_indxp] = INDXP[K2 + I + o_indxp];
                            INDXP[K2 + I + o_indxp] = JLAM;
                            I += 1;
                            goto LABEL90;
                        }
                        else
                        {
                            INDXP[K2 + I - 1 + o_indxp] = JLAM;
                        }
                    }
                    else
                    {
                        INDXP[K2 + I - 1 + o_indxp] = JLAM;
                    }
                    JLAM = J;
                }
                else
                {
                    K += 1;
                    W[K + o_w] = Z[JLAM + o_z];
                    DLAMDA[K + o_dlamda] = D[JLAM + o_d];
                    INDXP[K + o_indxp] = JLAM;
                    JLAM = J;
                }
            }
            goto LABEL80;
        LABEL100:;
            // *
            // *     Record the last eigenvalue.
            // *
            K += 1;
            W[K + o_w] = Z[JLAM + o_z];
            DLAMDA[K + o_dlamda] = D[JLAM + o_d];
            INDXP[K + o_indxp] = JLAM;
            // *
        LABEL110:;
            // *
            // *     Sort the eigenvalues and corresponding eigenvectors into DLAMDA
            // *     and Q2 respectively.  The eigenvalues/vectors which were not
            // *     deflated go into the first K slots of DLAMDA and Q2 respectively,
            // *     while those which were deflated go into the last N - K slots.
            // *
            if (ICOMPQ == 0)
            {
                for (J = 1; J <= N; J++)
                {
                    JP = INDXP[J + o_indxp];
                    DLAMDA[J + o_dlamda] = D[JP + o_d];
                    PERM[J + o_perm] = INDXQ[INDX[JP + o_indx] + o_indxq];
                }
            }
            else
            {
                for (J = 1; J <= N; J++)
                {
                    JP = INDXP[J + o_indxp];
                    DLAMDA[J + o_dlamda] = D[JP + o_d];
                    PERM[J + o_perm] = INDXQ[INDX[JP + o_indx] + o_indxq];
                    this._dcopy.Run(QSIZ, Q, 1+(PERM[J + o_perm]) * LDQ + o_q, 1, ref Q2, 1+J * LDQ2 + o_q2, 1);
                }
            }
            // *
            // *     The deflated eigenvalues and their corresponding vectors go back
            // *     into the last N - K slots of D and Q respectively.
            // *
            if (K < N)
            {
                if (ICOMPQ == 0)
                {
                    this._dcopy.Run(N - K, DLAMDA, K + 1 + o_dlamda, 1, ref D, K + 1 + o_d, 1);
                }
                else
                {
                    this._dcopy.Run(N - K, DLAMDA, K + 1 + o_dlamda, 1, ref D, K + 1 + o_d, 1);
                    this._dlacpy.Run("A", QSIZ, N - K, Q2, 1+(K + 1) * LDQ2 + o_q2, LDQ2, ref Q, 1+(K + 1) * LDQ + o_q
                                     , LDQ);
                }
            }
            // *
            return;
            // *
            // *     End of DLAED8
            // *

            #endregion

        }
    }
}
