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
    /// DLAED7 computes the updated eigensystem of a diagonal
    /// matrix after modification by a rank-one symmetric matrix. This
    /// routine is used only for the eigenproblem which requires all
    /// eigenvalues and optionally eigenvectors of a dense symmetric matrix
    /// that has been reduced to tridiagonal form.  DLAED1 handles
    /// the case in which all eigenvalues and eigenvectors of a symmetric
    /// tridiagonal matrix are desired.
    /// 
    /// T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
    /// 
    /// where Z = Q'u, u is a vector of length N with ones in the
    /// CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
    /// 
    /// The eigenvectors of the original matrix are stored in Q, and the
    /// eigenvalues are in D.  The algorithm consists of three stages:
    /// 
    /// The first stage consists of deflating the size of the problem
    /// when there are multiple eigenvalues or if there is a zero in
    /// the Z vector.  For each such occurence the dimension of the
    /// secular equation problem is reduced by one.  This stage is
    /// performed by the routine DLAED8.
    /// 
    /// The second stage consists of calculating the updated
    /// eigenvalues. This is done by finding the roots of the secular
    /// equation via the routine DLAED4 (as called by DLAED9).
    /// This routine also calculates the eigenvectors of the current
    /// problem.
    /// 
    /// The final stage consists of computing the updated eigenvectors
    /// directly using the updated eigenvalues.  The eigenvectors for
    /// the current problem are multiplied with the eigenvectors from
    /// the overall problem.
    /// 
    ///</summary>
    public class DLAED7
    {
    

        #region Dependencies
        
        DGEMM _dgemm; DLAED8 _dlaed8; DLAED9 _dlaed9; DLAEDA _dlaeda; DLAMRG _dlamrg; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public DLAED7(DGEMM dgemm, DLAED8 dlaed8, DLAED9 dlaed9, DLAEDA dlaeda, DLAMRG dlamrg, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dgemm = dgemm; this._dlaed8 = dlaed8; this._dlaed9 = dlaed9; this._dlaeda = dlaeda; this._dlamrg = dlamrg; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DLAED7()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            IDAMAX idamax = new IDAMAX();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DCOPY dcopy = new DCOPY();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DNRM2 dnrm2 = new DNRM2();
            DLAED5 dlaed5 = new DLAED5();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAED8 dlaed8 = new DLAED8(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLAED9 dlaed9 = new DLAED9(dlamc3, dnrm2, dcopy, dlaed4, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAEDA dlaeda = new DLAEDA(dcopy, dgemv, drot, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgemm = dgemm; this._dlaed8 = dlaed8; this._dlaed9 = dlaed9; this._dlaeda = dlaeda; this._dlamrg = dlamrg; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED7 computes the updated eigensystem of a diagonal
        /// matrix after modification by a rank-one symmetric matrix. This
        /// routine is used only for the eigenproblem which requires all
        /// eigenvalues and optionally eigenvectors of a dense symmetric matrix
        /// that has been reduced to tridiagonal form.  DLAED1 handles
        /// the case in which all eigenvalues and eigenvectors of a symmetric
        /// tridiagonal matrix are desired.
        /// 
        /// T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
        /// 
        /// where Z = Q'u, u is a vector of length N with ones in the
        /// CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
        /// 
        /// The eigenvectors of the original matrix are stored in Q, and the
        /// eigenvalues are in D.  The algorithm consists of three stages:
        /// 
        /// The first stage consists of deflating the size of the problem
        /// when there are multiple eigenvalues or if there is a zero in
        /// the Z vector.  For each such occurence the dimension of the
        /// secular equation problem is reduced by one.  This stage is
        /// performed by the routine DLAED8.
        /// 
        /// The second stage consists of calculating the updated
        /// eigenvalues. This is done by finding the roots of the secular
        /// equation via the routine DLAED4 (as called by DLAED9).
        /// This routine also calculates the eigenvectors of the current
        /// problem.
        /// 
        /// The final stage consists of computing the updated eigenvectors
        /// directly using the updated eigenvalues.  The eigenvectors for
        /// the current problem are multiplied with the eigenvectors from
        /// the overall problem.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// = 0:  Compute eigenvalues only.
        /// = 1:  Compute eigenvectors of original dense symmetric matrix
        /// also.  On entry, Q contains the orthogonal matrix used
        /// to reduce the original matrix to tridiagonal form.
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
        /// <param name="TLVLS">
        /// (input) INTEGER
        /// The total number of merging levels in the overall divide and
        /// conquer tree.
        ///</param>
        /// <param name="CURLVL">
        /// (input) INTEGER
        /// The current level in the overall merge routine,
        /// 0 .LE. CURLVL .LE. TLVLS.
        ///</param>
        /// <param name="CURPBM">
        /// (input) INTEGER
        /// The current problem in the current level in the overall
        /// merge routine (counting from upper left to lower right).
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the eigenvalues of the rank-1-perturbed matrix.
        /// On exit, the eigenvalues of the repaired matrix.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
        /// On entry, the eigenvectors of the rank-1-perturbed matrix.
        /// On exit, the eigenvectors of the repaired tridiagonal matrix.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="INDXQ">
        /// (output) INTEGER array, dimension (N)
        /// The permutation which will reintegrate the subproblem just
        /// solved back into sorted order, i.e., D( INDXQ( I = 1, N ) )
        /// will be in ascending order.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The subdiagonal element used to create the rank-1
        /// modification.
        ///</param>
        /// <param name="CUTPNT">
        /// (input) INTEGER
        /// Contains the location of the last eigenvalue in the leading
        /// sub-matrix.  min(1,N) .LE. CUTPNT .LE. N.
        ///</param>
        /// <param name="QSTORE">
        /// (input/output) DOUBLE PRECISION array, dimension (N**2+1)
        /// Stores eigenvectors of submatrices encountered during
        /// divide and conquer, packed together. QPTR points to
        /// beginning of the submatrices.
        ///</param>
        /// <param name="QPTR">
        /// (input/output) INTEGER array, dimension (N+2)
        /// List of indices pointing to beginning of submatrices stored
        /// in QSTORE. The submatrices are numbered starting at the
        /// bottom left of the divide and conquer tree, from left to
        /// right and bottom to top.
        ///</param>
        /// <param name="PRMPTR">
        /// (input) INTEGER array, dimension (N lg N)
        /// Contains a list of pointers which indicate where in PERM a
        /// level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
        /// indicates the size of the permutation and also the size of
        /// the full, non-deflated problem.
        ///</param>
        /// <param name="PERM">
        /// (input) INTEGER array, dimension (N lg N)
        /// Contains the permutations (from deflation and sorting) to be
        /// applied to each eigenblock.
        ///</param>
        /// <param name="GIVPTR">
        /// (input) INTEGER array, dimension (N lg N)
        /// Contains a list of pointers which indicate where in GIVCOL a
        /// level's Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
        /// indicates the number of Givens rotations.
        ///</param>
        /// <param name="GIVCOL">
        /// (input) INTEGER array, dimension (2, N lg N)
        /// Each pair of numbers indicates a pair of columns to take place
        /// in a Givens rotation.
        ///</param>
        /// <param name="GIVNUM">
        /// (input) DOUBLE PRECISION array, dimension (2, N lg N)
        /// Each number indicates the S value to be used in the
        /// corresponding Givens rotation.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (3*N+QSIZ*N)
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (4*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an eigenvalue did not converge
        ///</param>
        public void Run(int ICOMPQ, int N, int QSIZ, int TLVLS, int CURLVL, int CURPBM
                         , ref double[] D, int offset_d, ref double[] Q, int offset_q, int LDQ, ref int[] INDXQ, int offset_indxq, ref double RHO, int CUTPNT
                         , ref double[] QSTORE, int offset_qstore, ref int[] QPTR, int offset_qptr, ref int[] PRMPTR, int offset_prmptr, ref int[] PERM, int offset_perm, ref int[] GIVPTR, int offset_givptr, ref int[] GIVCOL, int offset_givcol
                         , ref double[] GIVNUM, int offset_givnum, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int COLTYP = 0; int CURR = 0; int I = 0; int IDLMDA = 0; int INDX = 0; int INDXC = 0; int INDXP = 0; int IQ2 = 0; 
            int IS = 0;int IW = 0; int IZ = 0; int K = 0; int LDQ2 = 0; int N1 = 0; int N2 = 0; int PTR = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_indxq = -1 + offset_indxq; 
             int o_qstore = -1 + offset_qstore; int o_qptr = -1 + offset_qptr;  int o_prmptr = -1 + offset_prmptr; 
             int o_perm = -1 + offset_perm; int o_givptr = -1 + offset_givptr;  int o_givcol = -3 + offset_givcol; 
             int o_givnum = -3 + offset_givnum; int o_work = -1 + offset_work;  int o_iwork = -1 + offset_iwork; 

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
            // *  DLAED7 computes the updated eigensystem of a diagonal
            // *  matrix after modification by a rank-one symmetric matrix. This
            // *  routine is used only for the eigenproblem which requires all
            // *  eigenvalues and optionally eigenvectors of a dense symmetric matrix
            // *  that has been reduced to tridiagonal form.  DLAED1 handles
            // *  the case in which all eigenvalues and eigenvectors of a symmetric
            // *  tridiagonal matrix are desired.
            // *
            // *    T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
            // *
            // *     where Z = Q'u, u is a vector of length N with ones in the
            // *     CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
            // *
            // *     The eigenvectors of the original matrix are stored in Q, and the
            // *     eigenvalues are in D.  The algorithm consists of three stages:
            // *
            // *        The first stage consists of deflating the size of the problem
            // *        when there are multiple eigenvalues or if there is a zero in
            // *        the Z vector.  For each such occurence the dimension of the
            // *        secular equation problem is reduced by one.  This stage is
            // *        performed by the routine DLAED8.
            // *
            // *        The second stage consists of calculating the updated
            // *        eigenvalues. This is done by finding the roots of the secular
            // *        equation via the routine DLAED4 (as called by DLAED9).
            // *        This routine also calculates the eigenvectors of the current
            // *        problem.
            // *
            // *        The final stage consists of computing the updated eigenvectors
            // *        directly using the updated eigenvalues.  The eigenvectors for
            // *        the current problem are multiplied with the eigenvectors from
            // *        the overall problem.
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
            // *  N      (input) INTEGER
            // *         The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  QSIZ   (input) INTEGER
            // *         The dimension of the orthogonal matrix used to reduce
            // *         the full matrix to tridiagonal form.  QSIZ >= N if ICOMPQ = 1.
            // *
            // *  TLVLS  (input) INTEGER
            // *         The total number of merging levels in the overall divide and
            // *         conquer tree.
            // *
            // *  CURLVL (input) INTEGER
            // *         The current level in the overall merge routine,
            // *         0 <= CURLVL <= TLVLS.
            // *
            // *  CURPBM (input) INTEGER
            // *         The current problem in the current level in the overall
            // *         merge routine (counting from upper left to lower right).
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry, the eigenvalues of the rank-1-perturbed matrix.
            // *         On exit, the eigenvalues of the repaired matrix.
            // *
            // *  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
            // *         On entry, the eigenvectors of the rank-1-perturbed matrix.
            // *         On exit, the eigenvectors of the repaired tridiagonal matrix.
            // *
            // *  LDQ    (input) INTEGER
            // *         The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  INDXQ  (output) INTEGER array, dimension (N)
            // *         The permutation which will reintegrate the subproblem just
            // *         solved back into sorted order, i.e., D( INDXQ( I = 1, N ) )
            // *         will be in ascending order.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The subdiagonal element used to create the rank-1
            // *         modification.
            // *
            // *  CUTPNT (input) INTEGER
            // *         Contains the location of the last eigenvalue in the leading
            // *         sub-matrix.  min(1,N) <= CUTPNT <= N.
            // *
            // *  QSTORE (input/output) DOUBLE PRECISION array, dimension (N**2+1)
            // *         Stores eigenvectors of submatrices encountered during
            // *         divide and conquer, packed together. QPTR points to
            // *         beginning of the submatrices.
            // *
            // *  QPTR   (input/output) INTEGER array, dimension (N+2)
            // *         List of indices pointing to beginning of submatrices stored
            // *         in QSTORE. The submatrices are numbered starting at the
            // *         bottom left of the divide and conquer tree, from left to
            // *         right and bottom to top.
            // *
            // *  PRMPTR (input) INTEGER array, dimension (N lg N)
            // *         Contains a list of pointers which indicate where in PERM a
            // *         level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
            // *         indicates the size of the permutation and also the size of
            // *         the full, non-deflated problem.
            // *
            // *  PERM   (input) INTEGER array, dimension (N lg N)
            // *         Contains the permutations (from deflation and sorting) to be
            // *         applied to each eigenblock.
            // *
            // *  GIVPTR (input) INTEGER array, dimension (N lg N)
            // *         Contains a list of pointers which indicate where in GIVCOL a
            // *         level's Givens rotations are stored.  GIVPTR(i+1) - GIVPTR(i)
            // *         indicates the number of Givens rotations.
            // *
            // *  GIVCOL (input) INTEGER array, dimension (2, N lg N)
            // *         Each pair of numbers indicates a pair of columns to take place
            // *         in a Givens rotation.
            // *
            // *  GIVNUM (input) DOUBLE PRECISION array, dimension (2, N lg N)
            // *         Each number indicates the S value to be used in the
            // *         corresponding Givens rotation.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension (3*N+QSIZ*N)
            // *
            // *  IWORK  (workspace) INTEGER array, dimension (4*N)
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, an eigenvalue did not converge
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
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
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
                    INFO =  - 2;
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
                            INFO =  - 9;
                        }
                        else
                        {
                            if (Math.Min(1, N) > CUTPNT || N < CUTPNT)
                            {
                                INFO =  - 12;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED7",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     The following values are for bookkeeping purposes only.  They are
            // *     integer pointers which indicate the portion of the workspace
            // *     used by a particular array in DLAED8 and DLAED9.
            // *
            if (ICOMPQ == 1)
            {
                LDQ2 = QSIZ;
            }
            else
            {
                LDQ2 = N;
            }
            // *
            IZ = 1;
            IDLMDA = IZ + N;
            IW = IDLMDA + N;
            IQ2 = IW + N;
            IS = IQ2 + N * LDQ2;
            // *
            INDX = 1;
            INDXC = INDX + N;
            COLTYP = INDXC + N;
            INDXP = COLTYP + N;
            // *
            // *     Form the z-vector which consists of the last row of Q_1 and the
            // *     first row of Q_2.
            // *
            PTR = 1 + (int)Math.Pow(2, TLVLS);
            for (I = 1; I <= CURLVL - 1; I++)
            {
                PTR += (int)Math.Pow(2, TLVLS - I);
            }
            CURR = PTR + CURPBM;
            this._dlaeda.Run(N, TLVLS, CURLVL, CURPBM, PRMPTR, offset_prmptr, PERM, offset_perm
                             , GIVPTR, offset_givptr, GIVCOL, offset_givcol, GIVNUM, offset_givnum, QSTORE, offset_qstore, QPTR, offset_qptr, ref WORK, IZ + o_work
                             , ref WORK, IZ + N + o_work, ref INFO);
            // *
            // *     When solving the final problem, we no longer need the stored data,
            // *     so we will overwrite the data from this level onto the previously
            // *     used storage space.
            // *
            if (CURLVL == TLVLS)
            {
                QPTR[CURR + o_qptr] = 1;
                PRMPTR[CURR + o_prmptr] = 1;
                GIVPTR[CURR + o_givptr] = 1;
            }
            // *
            // *     Sort and Deflate eigenvalues.
            // *
            this._dlaed8.Run(ICOMPQ, ref K, N, QSIZ, ref D, offset_d, ref Q, offset_q
                             , LDQ, ref INDXQ, offset_indxq, ref RHO, CUTPNT, ref WORK, IZ + o_work, ref WORK, IDLMDA + o_work
                             , ref WORK, IQ2 + o_work, LDQ2, ref WORK, IW + o_work, ref PERM, PRMPTR[CURR + o_prmptr] + o_perm, ref GIVPTR[CURR + 1 + o_givptr], ref GIVCOL, 1+(GIVPTR[CURR + o_givptr]) * 2 + o_givcol
                             , ref GIVNUM, 1+(GIVPTR[CURR + o_givptr]) * 2 + o_givnum, ref IWORK, INDXP + o_iwork, ref IWORK, INDX + o_iwork, ref INFO);
            PRMPTR[CURR + 1 + o_prmptr] = PRMPTR[CURR + o_prmptr] + N;
            GIVPTR[CURR + 1 + o_givptr] += GIVPTR[CURR + o_givptr];
            // *
            // *     Solve Secular Equation.
            // *
            if (K != 0)
            {
                this._dlaed9.Run(K, 1, K, N, ref D, offset_d, ref WORK, IS + o_work
                                 , K, RHO, ref WORK, IDLMDA + o_work, ref WORK, IW + o_work, ref QSTORE, QPTR[CURR + o_qptr] + o_qstore, K
                                 , ref INFO);
                if (INFO != 0) goto LABEL30;
                if (ICOMPQ == 1)
                {
                    this._dgemm.Run("N", "N", QSIZ, K, K, ONE
                                    , WORK, IQ2 + o_work, LDQ2, QSTORE, QPTR[CURR + o_qptr] + o_qstore, K, ZERO, ref Q, offset_q
                                    , LDQ);
                }
                QPTR[CURR + 1 + o_qptr] = QPTR[CURR + o_qptr] + (int)Math.Pow(K, 2);
                // *
                // *     Prepare the INDXQ sorting permutation.
                // *
                N1 = K;
                N2 = N - K;
                this._dlamrg.Run(N1, N2, D, offset_d, 1,  - 1, ref INDXQ, offset_indxq);
            }
            else
            {
                QPTR[CURR + 1 + o_qptr] = QPTR[CURR + o_qptr];
                for (I = 1; I <= N; I++)
                {
                    INDXQ[I + o_indxq] = I;
                }
            }
            // *
        LABEL30:;
            return;
            // *
            // *     End of DLAED7
            // *

            #endregion

        }
    }
}
