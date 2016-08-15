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
    /// DLASD2 merges the two sets of singular values together into a single
    /// sorted set.  Then it tries to deflate the size of the problem.
    /// There are two ways in which deflation can occur:  when two or more
    /// singular values are close together or if there is a tiny entry in the
    /// Z vector.  For each such occurrence the order of the related secular
    /// equation problem is reduced by one.
    /// 
    /// DLASD2 is called from DLASD1.
    /// 
    ///</summary>
    public class DLASD2
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLAPY2 _dlapy2; DCOPY _dcopy; DLACPY _dlacpy; DLAMRG _dlamrg; DLASET _dlaset; DROT _drot; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; const double EIGHT = 8.0E+0; 
        int[] CTOT = new int[4];int[] PSM = new int[4]; 

        #endregion

        public DLASD2(DLAMCH dlamch, DLAPY2 dlapy2, DCOPY dcopy, DLACPY dlacpy, DLAMRG dlamrg, DLASET dlaset, DROT drot, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dcopy = dcopy; this._dlacpy = dlacpy; this._dlamrg = dlamrg; 
            this._dlaset = dlaset;this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASD2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DCOPY dcopy = new DCOPY();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASET dlaset = new DLASET(lsame);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dcopy = dcopy; this._dlacpy = dlacpy; this._dlamrg = dlamrg; 
            this._dlaset = dlaset;this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD2 merges the two sets of singular values together into a single
        /// sorted set.  Then it tries to deflate the size of the problem.
        /// There are two ways in which deflation can occur:  when two or more
        /// singular values are close together or if there is a tiny entry in the
        /// Z vector.  For each such occurrence the order of the related secular
        /// equation problem is reduced by one.
        /// 
        /// DLASD2 is called from DLASD1.
        /// 
        ///</summary>
        /// <param name="NL">
        /// (input) INTEGER
        /// The row dimension of the upper block.  NL .GE. 1.
        ///</param>
        /// <param name="NR">
        /// (input) INTEGER
        /// The row dimension of the lower block.  NR .GE. 1.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// = 0: the lower block is an NR-by-NR square matrix.
        /// = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
        /// 
        /// The bidiagonal matrix has N = NL + NR + 1 rows and
        /// M = N + SQRE .GE. N columns.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER
        /// Contains the dimension of the non-deflated matrix,
        /// This is the order of the related secular equation. 1 .LE. K .LE.N.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension(N)
        /// On entry D contains the singular values of the two submatrices
        /// to be combined.  On exit D contains the trailing (N-K) updated
        /// singular values (those which were deflated) sorted into
        /// increasing order.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array, dimension(N)
        /// On exit Z contains the updating row vector in the secular
        /// equation.
        ///</param>
        /// <param name="ALPHA">
        /// (input) DOUBLE PRECISION
        /// Contains the diagonal element associated with the added row.
        ///</param>
        /// <param name="BETA">
        /// (input) DOUBLE PRECISION
        /// Contains the off-diagonal element associated with the added
        /// row.
        ///</param>
        /// <param name="U">
        /// (input/output) DOUBLE PRECISION array, dimension(LDU,N)
        /// On entry U contains the left singular vectors of two
        /// submatrices in the two square blocks with corners at (1,1),
        /// (NL, NL), and (NL+2, NL+2), (N,N).
        /// On exit U contains the trailing (N-K) updated left singular
        /// vectors (those which were deflated) in its last N-K columns.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. N.
        ///</param>
        /// <param name="VT">
        /// (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
        /// On entry VT' contains the right singular vectors of two
        /// submatrices in the two square blocks with corners at (1,1),
        /// (NL+1, NL+1), and (NL+2, NL+2), (M,M).
        /// On exit VT' contains the trailing (N-K) updated right singular
        /// vectors (those which were deflated) in its last N-K columns.
        /// In case SQRE =1, the last row of VT spans the right null
        /// space.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.  LDVT .GE. M.
        ///</param>
        /// <param name="DSIGMA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// Contains a copy of the diagonal elements (K-1 singular values
        /// and one zero) in the secular equation.
        ///</param>
        /// <param name="U2">
        /// (output) DOUBLE PRECISION array, dimension(LDU2,N)
        /// Contains a copy of the first K-1 left singular vectors which
        /// will be used by DLASD3 in a matrix multiply (DGEMM) to solve
        /// for the new left singular vectors. U2 is arranged into four
        /// blocks. The first block contains a column with 1 at NL+1 and
        /// zero everywhere else; the second block contains non-zero
        /// entries only at and above NL; the third contains non-zero
        /// entries only below NL+1; and the fourth is dense.
        ///</param>
        /// <param name="LDU2">
        /// (input) INTEGER
        /// The leading dimension of the array U2.  LDU2 .GE. N.
        ///</param>
        /// <param name="VT2">
        /// (output) DOUBLE PRECISION array, dimension(LDVT2,N)
        /// VT2' contains a copy of the first K right singular vectors
        /// which will be used by DLASD3 in a matrix multiply (DGEMM) to
        /// solve for the new right singular vectors. VT2 is arranged into
        /// three blocks. The first block contains a row that corresponds
        /// to the special 0 diagonal element in SIGMA; the second block
        /// contains non-zeros only at and before NL +1; the third block
        /// contains non-zeros only at and after  NL +2.
        ///</param>
        /// <param name="LDVT2">
        /// (input) INTEGER
        /// The leading dimension of the array VT2.  LDVT2 .GE. M.
        ///</param>
        /// <param name="IDXP">
        /// (workspace) INTEGER array dimension(N)
        /// This will contain the permutation used to place deflated
        /// values of D at the end of the array. On output IDXP(2:K)
        /// points to the nondeflated D-values and IDXP(K+1:N)
        /// points to the deflated singular values.
        ///</param>
        /// <param name="IDX">
        /// (workspace) INTEGER array dimension(N)
        /// This will contain the permutation used to sort the contents of
        /// D into ascending order.
        ///</param>
        /// <param name="IDXC">
        /// (output) INTEGER array dimension(N)
        /// This will contain the permutation used to arrange the columns
        /// of the deflated U matrix into three groups:  the first group
        /// contains non-zero entries only at and above NL, the second
        /// contains non-zero entries only below NL+2, and the third is
        /// dense.
        ///</param>
        /// <param name="IDXQ">
        /// (input/output) INTEGER array dimension(N)
        /// This contains the permutation which separately sorts the two
        /// sub-problems in D into ascending order.  Note that entries in
        /// the first hlaf of this permutation must first be moved one
        /// position backward; and entries in the second half
        /// must first have NL+1 added to their values.
        ///</param>
        /// <param name="COLTYP">
        /// (workspace/output) INTEGER array dimension(N)
        /// As workspace, this will contain a label which will indicate
        /// which of the following types a column in the U2 matrix or a
        /// row in the VT2 matrix is:
        /// 1 : non-zero in the upper half only
        /// 2 : non-zero in the lower half only
        /// 3 : dense
        /// 4 : deflated
        /// 
        /// On exit, it is an array of dimension 4, with COLTYP(I) being
        /// the dimension of the I-th type columns.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int NL, int NR, int SQRE, ref int K, ref double[] D, int offset_d, ref double[] Z, int offset_z
                         , double ALPHA, double BETA, ref double[] U, int offset_u, int LDU, ref double[] VT, int offset_vt, int LDVT
                         , ref double[] DSIGMA, int offset_dsigma, ref double[] U2, int offset_u2, int LDU2, ref double[] VT2, int offset_vt2, int LDVT2, ref int[] IDXP, int offset_idxp
                         , ref int[] IDX, int offset_idx, ref int[] IDXC, int offset_idxc, ref int[] IDXQ, int offset_idxq, ref int[] COLTYP, int offset_coltyp, ref int INFO)
        {

            #region Variables
            
            int o_ctot = -1; int o_psm = -1; int CT = 0; int I = 0; int IDXI = 0; 
            int IDXJ = 0;int IDXJP = 0; int J = 0; int JP = 0; int JPREV = 0; int K2 = 0; int M = 0; int N = 0; int NLP1 = 0; 
            int NLP2 = 0;double C = 0; double EPS = 0; double HLFTOL = 0; double S = 0; double TAU = 0; double TOL = 0; 
            double Z1 = 0;

            #endregion


            #region Implicit Variables
            
            int VT_NLP1 = 0; int VT_NLP2 = 0; int U2_1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_u = -1 - LDU + offset_u;  int o_vt = -1 - LDVT + offset_vt; 
             int o_dsigma = -1 + offset_dsigma; int o_u2 = -1 - LDU2 + offset_u2;  int o_vt2 = -1 - LDVT2 + offset_vt2; 
             int o_idxp = -1 + offset_idxp; int o_idx = -1 + offset_idx;  int o_idxc = -1 + offset_idxc; 
             int o_idxq = -1 + offset_idxq; int o_coltyp = -1 + offset_coltyp; 

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
            // *  DLASD2 merges the two sets of singular values together into a single
            // *  sorted set.  Then it tries to deflate the size of the problem.
            // *  There are two ways in which deflation can occur:  when two or more
            // *  singular values are close together or if there is a tiny entry in the
            // *  Z vector.  For each such occurrence the order of the related secular
            // *  equation problem is reduced by one.
            // *
            // *  DLASD2 is called from DLASD1.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NL     (input) INTEGER
            // *         The row dimension of the upper block.  NL >= 1.
            // *
            // *  NR     (input) INTEGER
            // *         The row dimension of the lower block.  NR >= 1.
            // *
            // *  SQRE   (input) INTEGER
            // *         = 0: the lower block is an NR-by-NR square matrix.
            // *         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
            // *
            // *         The bidiagonal matrix has N = NL + NR + 1 rows and
            // *         M = N + SQRE >= N columns.
            // *
            // *  K      (output) INTEGER
            // *         Contains the dimension of the non-deflated matrix,
            // *         This is the order of the related secular equation. 1 <= K <=N.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension(N)
            // *         On entry D contains the singular values of the two submatrices
            // *         to be combined.  On exit D contains the trailing (N-K) updated
            // *         singular values (those which were deflated) sorted into
            // *         increasing order.
            // *
            // *  Z      (output) DOUBLE PRECISION array, dimension(N)
            // *         On exit Z contains the updating row vector in the secular
            // *         equation.
            // *
            // *  ALPHA  (input) DOUBLE PRECISION
            // *         Contains the diagonal element associated with the added row.
            // *
            // *  BETA   (input) DOUBLE PRECISION
            // *         Contains the off-diagonal element associated with the added
            // *         row.
            // *
            // *  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
            // *         On entry U contains the left singular vectors of two
            // *         submatrices in the two square blocks with corners at (1,1),
            // *         (NL, NL), and (NL+2, NL+2), (N,N).
            // *         On exit U contains the trailing (N-K) updated left singular
            // *         vectors (those which were deflated) in its last N-K columns.
            // *
            // *  LDU    (input) INTEGER
            // *         The leading dimension of the array U.  LDU >= N.
            // *
            // *  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
            // *         On entry VT' contains the right singular vectors of two
            // *         submatrices in the two square blocks with corners at (1,1),
            // *         (NL+1, NL+1), and (NL+2, NL+2), (M,M).
            // *         On exit VT' contains the trailing (N-K) updated right singular
            // *         vectors (those which were deflated) in its last N-K columns.
            // *         In case SQRE =1, the last row of VT spans the right null
            // *         space.
            // *
            // *  LDVT   (input) INTEGER
            // *         The leading dimension of the array VT.  LDVT >= M.
            // *
            // *  DSIGMA (output) DOUBLE PRECISION array, dimension (N)
            // *         Contains a copy of the diagonal elements (K-1 singular values
            // *         and one zero) in the secular equation.
            // *
            // *  U2     (output) DOUBLE PRECISION array, dimension(LDU2,N)
            // *         Contains a copy of the first K-1 left singular vectors which
            // *         will be used by DLASD3 in a matrix multiply (DGEMM) to solve
            // *         for the new left singular vectors. U2 is arranged into four
            // *         blocks. The first block contains a column with 1 at NL+1 and
            // *         zero everywhere else; the second block contains non-zero
            // *         entries only at and above NL; the third contains non-zero
            // *         entries only below NL+1; and the fourth is dense.
            // *
            // *  LDU2   (input) INTEGER
            // *         The leading dimension of the array U2.  LDU2 >= N.
            // *
            // *  VT2    (output) DOUBLE PRECISION array, dimension(LDVT2,N)
            // *         VT2' contains a copy of the first K right singular vectors
            // *         which will be used by DLASD3 in a matrix multiply (DGEMM) to
            // *         solve for the new right singular vectors. VT2 is arranged into
            // *         three blocks. The first block contains a row that corresponds
            // *         to the special 0 diagonal element in SIGMA; the second block
            // *         contains non-zeros only at and before NL +1; the third block
            // *         contains non-zeros only at and after  NL +2.
            // *
            // *  LDVT2  (input) INTEGER
            // *         The leading dimension of the array VT2.  LDVT2 >= M.
            // *
            // *  IDXP   (workspace) INTEGER array dimension(N)
            // *         This will contain the permutation used to place deflated
            // *         values of D at the end of the array. On output IDXP(2:K)
            // *         points to the nondeflated D-values and IDXP(K+1:N)
            // *         points to the deflated singular values.
            // *
            // *  IDX    (workspace) INTEGER array dimension(N)
            // *         This will contain the permutation used to sort the contents of
            // *         D into ascending order.
            // *
            // *  IDXC   (output) INTEGER array dimension(N)
            // *         This will contain the permutation used to arrange the columns
            // *         of the deflated U matrix into three groups:  the first group
            // *         contains non-zero entries only at and above NL, the second
            // *         contains non-zero entries only below NL+2, and the third is
            // *         dense.
            // *
            // *  IDXQ   (input/output) INTEGER array dimension(N)
            // *         This contains the permutation which separately sorts the two
            // *         sub-problems in D into ascending order.  Note that entries in
            // *         the first hlaf of this permutation must first be moved one
            // *         position backward; and entries in the second half
            // *         must first have NL+1 added to their values.
            // *
            // *  COLTYP (workspace/output) INTEGER array dimension(N)
            // *         As workspace, this will contain a label which will indicate
            // *         which of the following types a column in the U2 matrix or a
            // *         row in the VT2 matrix is:
            // *         1 : non-zero in the upper half only
            // *         2 : non-zero in the lower half only
            // *         3 : dense
            // *         4 : deflated
            // *
            // *         On exit, it is an array of dimension 4, with COLTYP(I) being
            // *         the dimension of the I-th type columns.
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
            // *     .. Local Arrays ..
            // *     ..
            // *     .. Local Scalars ..
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
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (NL < 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (NR < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if ((SQRE != 1) && (SQRE != 0))
                    {
                        INFO =  - 3;
                    }
                }
            }
            // *
            N = NL + NR + 1;
            M = N + SQRE;
            // *
            if (LDU < N)
            {
                INFO =  - 10;
            }
            else
            {
                if (LDVT < M)
                {
                    INFO =  - 12;
                }
                else
                {
                    if (LDU2 < N)
                    {
                        INFO =  - 15;
                    }
                    else
                    {
                        if (LDVT2 < M)
                        {
                            INFO =  - 17;
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD2",  - INFO);
                return;
            }
            // *
            NLP1 = NL + 1;
            NLP2 = NL + 2;
            // *
            // *     Generate the first part of the vector Z; and move the singular
            // *     values in the first part of D one position backward.
            // *
            Z1 = ALPHA * VT[NLP1+NLP1 * LDVT + o_vt];
            Z[1 + o_z] = Z1;
            VT_NLP1 = NLP1 * LDVT + o_vt;
            for (I = NL; I >= 1; I +=  - 1)
            {
                Z[I + 1 + o_z] = ALPHA * VT[I + VT_NLP1];
                D[I + 1 + o_d] = D[I + o_d];
                IDXQ[I + 1 + o_idxq] = IDXQ[I + o_idxq] + 1;
            }
            // *
            // *     Generate the second part of the vector Z.
            // *
            VT_NLP2 = NLP2 * LDVT + o_vt;
            for (I = NLP2; I <= M; I++)
            {
                Z[I + o_z] = BETA * VT[I + VT_NLP2];
            }
            // *
            // *     Initialize some reference arrays.
            // *
            for (I = 2; I <= NLP1; I++)
            {
                COLTYP[I + o_coltyp] = 1;
            }
            for (I = NLP2; I <= N; I++)
            {
                COLTYP[I + o_coltyp] = 2;
            }
            // *
            // *     Sort the singular values into increasing order
            // *
            for (I = NLP2; I <= N; I++)
            {
                IDXQ[I + o_idxq] += NLP1;
            }
            // *
            // *     DSIGMA, IDXC, IDXC, and the first column of U2
            // *     are used as storage space.
            // *
            U2_1 = 1 * LDU2 + o_u2;
            for (I = 2; I <= N; I++)
            {
                DSIGMA[I + o_dsigma] = D[IDXQ[I + o_idxq] + o_d];
                U2[I + U2_1] = Z[IDXQ[I + o_idxq] + o_z];
                IDXC[I + o_idxc] = COLTYP[IDXQ[I + o_idxq] + o_coltyp];
            }
            // *
            this._dlamrg.Run(NL, NR, DSIGMA, 2 + o_dsigma, 1, 1, ref IDX, 2 + o_idx);
            // *
            for (I = 2; I <= N; I++)
            {
                IDXI = 1 + IDX[I + o_idx];
                D[I + o_d] = DSIGMA[IDXI + o_dsigma];
                Z[I + o_z] = U2[IDXI+1 * LDU2 + o_u2];
                COLTYP[I + o_coltyp] = IDXC[IDXI + o_idxc];
            }
            // *
            // *     Calculate the allowable deflation tolerance
            // *
            EPS = this._dlamch.Run("Epsilon");
            TOL = Math.Max(Math.Abs(ALPHA), Math.Abs(BETA));
            TOL = EIGHT * EPS * Math.Max(Math.Abs(D[N + o_d]), TOL);
            // *
            // *     There are 2 kinds of deflation -- first a value in the z-vector
            // *     is small, second two (or more) singular values are very close
            // *     together (their difference is small).
            // *
            // *     If the value in the z-vector is small, we simply permute the
            // *     array so that the corresponding singular value is moved to the
            // *     end.
            // *
            // *     If two values in the D-vector are close, we perform a two-sided
            // *     rotation designed to make one of the corresponding z-vector
            // *     entries zero, and then permute the array so that the deflated
            // *     singular value is moved to the end.
            // *
            // *     If there are multiple singular values then the problem deflates.
            // *     Here the number of equal singular values are found.  As each equal
            // *     singular value is found, an elementary reflector is computed to
            // *     rotate the corresponding singular subspace so that the
            // *     corresponding components of Z are zero in this new basis.
            // *
            K = 1;
            K2 = N + 1;
            for (J = 2; J <= N; J++)
            {
                if (Math.Abs(Z[J + o_z]) <= TOL)
                {
                    // *
                    // *           Deflate due to small z component.
                    // *
                    K2 -= 1;
                    IDXP[K2 + o_idxp] = J;
                    COLTYP[J + o_coltyp] = 4;
                    if (J == N) goto LABEL120;
                }
                else
                {
                    JPREV = J;
                    goto LABEL90;
                }
            }
        LABEL90:;
            J = JPREV;
        LABEL100:;
            J += 1;
            if (J > N) goto LABEL110;
            if (Math.Abs(Z[J + o_z]) <= TOL)
            {
                // *
                // *        Deflate due to small z component.
                // *
                K2 -= 1;
                IDXP[K2 + o_idxp] = J;
                COLTYP[J + o_coltyp] = 4;
            }
            else
            {
                // *
                // *        Check if singular values are close enough to allow deflation.
                // *
                if (Math.Abs(D[J + o_d] - D[JPREV + o_d]) <= TOL)
                {
                    // *
                    // *           Deflation is possible.
                    // *
                    S = Z[JPREV + o_z];
                    C = Z[J + o_z];
                    // *
                    // *           Find sqrt(a**2+b**2) without overflow or
                    // *           destructive underflow.
                    // *
                    TAU = this._dlapy2.Run(C, S);
                    C /= TAU;
                    S =  - S / TAU;
                    Z[J + o_z] = TAU;
                    Z[JPREV + o_z] = ZERO;
                    // *
                    // *           Apply back the Givens rotation to the left and right
                    // *           singular vector matrices.
                    // *
                    IDXJP = IDXQ[IDX[JPREV + o_idx] + 1 + o_idxq];
                    IDXJ = IDXQ[IDX[J + o_idx] + 1 + o_idxq];
                    if (IDXJP <= NLP1)
                    {
                        IDXJP -= 1;
                    }
                    if (IDXJ <= NLP1)
                    {
                        IDXJ -= 1;
                    }
                    this._drot.Run(N, ref U, 1+IDXJP * LDU + o_u, 1, ref U, 1+IDXJ * LDU + o_u, 1, C
                                   , S);
                    this._drot.Run(M, ref VT, IDXJP+1 * LDVT + o_vt, LDVT, ref VT, IDXJ+1 * LDVT + o_vt, LDVT, C
                                   , S);
                    if (COLTYP[J + o_coltyp] != COLTYP[JPREV + o_coltyp])
                    {
                        COLTYP[J + o_coltyp] = 3;
                    }
                    COLTYP[JPREV + o_coltyp] = 4;
                    K2 -= 1;
                    IDXP[K2 + o_idxp] = JPREV;
                    JPREV = J;
                }
                else
                {
                    K += 1;
                    U2[K+1 * LDU2 + o_u2] = Z[JPREV + o_z];
                    DSIGMA[K + o_dsigma] = D[JPREV + o_d];
                    IDXP[K + o_idxp] = JPREV;
                    JPREV = J;
                }
            }
            goto LABEL100;
        LABEL110:;
            // *
            // *     Record the last singular value.
            // *
            K += 1;
            U2[K+1 * LDU2 + o_u2] = Z[JPREV + o_z];
            DSIGMA[K + o_dsigma] = D[JPREV + o_d];
            IDXP[K + o_idxp] = JPREV;
            // *
        LABEL120:;
            // *
            // *     Count up the total number of the various types of columns, then
            // *     form a permutation which positions the four column types into
            // *     four groups of uniform structure (although one or more of these
            // *     groups may be empty).
            // *
            for (J = 1; J <= 4; J++)
            {
                CTOT[J + o_ctot] = 0;
            }
            for (J = 2; J <= N; J++)
            {
                CT = COLTYP[J + o_coltyp];
                CTOT[CT + o_ctot] += 1;
            }
            // *
            // *     PSM(*) = Position in SubMatrix (of types 1 through 4)
            // *
            PSM[1 + o_psm] = 2;
            PSM[2 + o_psm] = 2 + CTOT[1 + o_ctot];
            PSM[3 + o_psm] = PSM[2 + o_psm] + CTOT[2 + o_ctot];
            PSM[4 + o_psm] = PSM[3 + o_psm] + CTOT[3 + o_ctot];
            // *
            // *     Fill out the IDXC array so that the permutation which it induces
            // *     will place all type-1 columns first, all type-2 columns next,
            // *     then all type-3's, and finally all type-4's, starting from the
            // *     second column. This applies similarly to the rows of VT.
            // *
            for (J = 2; J <= N; J++)
            {
                JP = IDXP[J + o_idxp];
                CT = COLTYP[JP + o_coltyp];
                IDXC[PSM[CT + o_psm] + o_idxc] = J;
                PSM[CT + o_psm] += 1;
            }
            // *
            // *     Sort the singular values and corresponding singular vectors into
            // *     DSIGMA, U2, and VT2 respectively.  The singular values/vectors
            // *     which were not deflated go into the first K slots of DSIGMA, U2,
            // *     and VT2 respectively, while those which were deflated go into the
            // *     last N - K slots, except that the first column/row will be treated
            // *     separately.
            // *
            for (J = 2; J <= N; J++)
            {
                JP = IDXP[J + o_idxp];
                DSIGMA[J + o_dsigma] = D[JP + o_d];
                IDXJ = IDXQ[IDX[IDXP[IDXC[J + o_idxc] + o_idxp] + o_idx] + 1 + o_idxq];
                if (IDXJ <= NLP1)
                {
                    IDXJ -= 1;
                }
                this._dcopy.Run(N, U, 1+IDXJ * LDU + o_u, 1, ref U2, 1+J * LDU2 + o_u2, 1);
                this._dcopy.Run(M, VT, IDXJ+1 * LDVT + o_vt, LDVT, ref VT2, J+1 * LDVT2 + o_vt2, LDVT2);
            }
            // *
            // *     Determine DSIGMA(1), DSIGMA(2) and Z(1)
            // *
            DSIGMA[1 + o_dsigma] = ZERO;
            HLFTOL = TOL / TWO;
            if (Math.Abs(DSIGMA[2 + o_dsigma]) <= HLFTOL) DSIGMA[2 + o_dsigma] = HLFTOL;
            if (M > N)
            {
                Z[1 + o_z] = this._dlapy2.Run(Z1, Z[M + o_z]);
                if (Z[1 + o_z] <= TOL)
                {
                    C = ONE;
                    S = ZERO;
                    Z[1 + o_z] = TOL;
                }
                else
                {
                    C = Z1 / Z[1 + o_z];
                    S = Z[M + o_z] / Z[1 + o_z];
                }
            }
            else
            {
                if (Math.Abs(Z1) <= TOL)
                {
                    Z[1 + o_z] = TOL;
                }
                else
                {
                    Z[1 + o_z] = Z1;
                }
            }
            // *
            // *     Move the rest of the updating row to Z.
            // *
            this._dcopy.Run(K - 1, U2, 2+1 * LDU2 + o_u2, 1, ref Z, 2 + o_z, 1);
            // *
            // *     Determine the first column of U2, the first row of VT2 and the
            // *     last row of VT.
            // *
            this._dlaset.Run("A", N, 1, ZERO, ZERO, ref U2, offset_u2
                             , LDU2);
            U2[NLP1+1 * LDU2 + o_u2] = ONE;
            if (M > N)
            {
                for (I = 1; I <= NLP1; I++)
                {
                    VT[M+I * LDVT + o_vt] =  - S * VT[NLP1+I * LDVT + o_vt];
                    VT2[1+I * LDVT2 + o_vt2] = C * VT[NLP1+I * LDVT + o_vt];
                }
                for (I = NLP2; I <= M; I++)
                {
                    VT2[1+I * LDVT2 + o_vt2] = S * VT[M+I * LDVT + o_vt];
                    VT[M+I * LDVT + o_vt] *= C;
                }
            }
            else
            {
                this._dcopy.Run(M, VT, NLP1+1 * LDVT + o_vt, LDVT, ref VT2, 1+1 * LDVT2 + o_vt2, LDVT2);
            }
            if (M > N)
            {
                this._dcopy.Run(M, VT, M+1 * LDVT + o_vt, LDVT, ref VT2, M+1 * LDVT2 + o_vt2, LDVT2);
            }
            // *
            // *     The deflated singular values and their corresponding vectors go
            // *     into the back of D, U, and V respectively.
            // *
            if (N > K)
            {
                this._dcopy.Run(N - K, DSIGMA, K + 1 + o_dsigma, 1, ref D, K + 1 + o_d, 1);
                this._dlacpy.Run("A", N, N - K, U2, 1+(K + 1) * LDU2 + o_u2, LDU2, ref U, 1+(K + 1) * LDU + o_u
                                 , LDU);
                this._dlacpy.Run("A", N - K, M, VT2, K + 1+1 * LDVT2 + o_vt2, LDVT2, ref VT, K + 1+1 * LDVT + o_vt
                                 , LDVT);
            }
            // *
            // *     Copy CTOT into COLTYP for referencing in DLASD3.
            // *
            for (J = 1; J <= 4; J++)
            {
                COLTYP[J + o_coltyp] = CTOT[J + o_ctot];
            }
            // *
            return;
            // *
            // *     End of DLASD2
            // *

            #endregion

        }
    }
}
