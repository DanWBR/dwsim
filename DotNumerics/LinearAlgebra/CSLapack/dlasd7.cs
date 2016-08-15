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
    /// DLASD7 merges the two sets of singular values together into a single
    /// sorted set. Then it tries to deflate the size of the problem. There
    /// are two ways in which deflation can occur:  when two or more singular
    /// values are close together or if there is a tiny entry in the Z
    /// vector. For each such occurrence the order of the related
    /// secular equation problem is reduced by one.
    /// 
    /// DLASD7 is called from DLASD6.
    /// 
    ///</summary>
    public class DLASD7
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DLAMRG _dlamrg; DROT _drot; XERBLA _xerbla; DLAMCH _dlamch; DLAPY2 _dlapy2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; const double EIGHT = 8.0E+0; 

        #endregion

        public DLASD7(DCOPY dcopy, DLAMRG dlamrg, DROT drot, XERBLA xerbla, DLAMCH dlamch, DLAPY2 dlapy2)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlamrg = dlamrg; this._drot = drot; this._xerbla = xerbla; this._dlamch = dlamch; 
            this._dlapy2 = dlapy2;

            #endregion

        }
    
        public DLASD7()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            XERBLA xerbla = new XERBLA();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlamrg = dlamrg; this._drot = drot; this._xerbla = xerbla; this._dlamch = dlamch; 
            this._dlapy2 = dlapy2;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD7 merges the two sets of singular values together into a single
        /// sorted set. Then it tries to deflate the size of the problem. There
        /// are two ways in which deflation can occur:  when two or more singular
        /// values are close together or if there is a tiny entry in the Z
        /// vector. For each such occurrence the order of the related
        /// secular equation problem is reduced by one.
        /// 
        /// DLASD7 is called from DLASD6.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether singular vectors are to be computed
        /// in compact form, as follows:
        /// = 0: Compute singular values only.
        /// = 1: Compute singular vectors of upper
        /// bidiagonal matrix in compact form.
        ///</param>
        /// <param name="NL">
        /// (input) INTEGER
        /// The row dimension of the upper block. NL .GE. 1.
        ///</param>
        /// <param name="NR">
        /// (input) INTEGER
        /// The row dimension of the lower block. NR .GE. 1.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// = 0: the lower block is an NR-by-NR square matrix.
        /// = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
        /// 
        /// The bidiagonal matrix has
        /// N = NL + NR + 1 rows and
        /// M = N + SQRE .GE. N columns.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER
        /// Contains the dimension of the non-deflated matrix, this is
        /// the order of the related secular equation. 1 .LE. K .LE.N.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension ( N )
        /// On entry D contains the singular values of the two submatrices
        /// to be combined. On exit D contains the trailing (N-K) updated
        /// singular values (those which were deflated) sorted into
        /// increasing order.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array, dimension ( M )
        /// On exit Z contains the updating row vector in the secular
        /// equation.
        ///</param>
        /// <param name="ZW">
        /// (workspace) DOUBLE PRECISION array, dimension ( M )
        /// Workspace for Z.
        ///</param>
        /// <param name="VF">
        /// (input/output) DOUBLE PRECISION array, dimension ( M )
        /// On entry, VF(1:NL+1) contains the first components of all
        /// right singular vectors of the upper block; and VF(NL+2:M)
        /// contains the first components of all right singular vectors
        /// of the lower block. On exit, VF contains the first components
        /// of all right singular vectors of the bidiagonal matrix.
        ///</param>
        /// <param name="VFW">
        /// (workspace) DOUBLE PRECISION array, dimension ( M )
        /// Workspace for VF.
        ///</param>
        /// <param name="VL">
        /// (input/output) DOUBLE PRECISION array, dimension ( M )
        /// On entry, VL(1:NL+1) contains the  last components of all
        /// right singular vectors of the upper block; and VL(NL+2:M)
        /// contains the last components of all right singular vectors
        /// of the lower block. On exit, VL contains the last components
        /// of all right singular vectors of the bidiagonal matrix.
        ///</param>
        /// <param name="VLW">
        /// (workspace) DOUBLE PRECISION array, dimension ( M )
        /// Workspace for VL.
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
        /// <param name="DSIGMA">
        /// (output) DOUBLE PRECISION array, dimension ( N )
        /// Contains a copy of the diagonal elements (K-1 singular values
        /// and one zero) in the secular equation.
        ///</param>
        /// <param name="IDX">
        /// (workspace) INTEGER array, dimension ( N )
        /// This will contain the permutation used to sort the contents of
        /// D into ascending order.
        ///</param>
        /// <param name="IDXP">
        /// (workspace) INTEGER array, dimension ( N )
        /// This will contain the permutation used to place deflated
        /// values of D at the end of the array. On output IDXP(2:K)
        /// points to the nondeflated D-values and IDXP(K+1:N)
        /// points to the deflated singular values.
        ///</param>
        /// <param name="IDXQ">
        /// (input) INTEGER array, dimension ( N )
        /// This contains the permutation which separately sorts the two
        /// sub-problems in D into ascending order.  Note that entries in
        /// the first half of this permutation must first be moved one
        /// position backward; and entries in the second half
        /// must first have NL+1 added to their values.
        ///</param>
        /// <param name="PERM">
        /// (output) INTEGER array, dimension ( N )
        /// The permutations (from deflation and sorting) to be applied
        /// to each singular block. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="GIVPTR">
        /// (output) INTEGER
        /// The number of Givens rotations which took place in this
        /// subproblem. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="GIVCOL">
        /// (output) INTEGER array, dimension ( LDGCOL, 2 )
        /// Each pair of numbers indicates a pair of columns to take place
        /// in a Givens rotation. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="LDGCOL">
        /// (input) INTEGER
        /// The leading dimension of GIVCOL, must be at least N.
        ///</param>
        /// <param name="GIVNUM">
        /// (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
        /// Each number indicates the C or S value to be used in the
        /// corresponding Givens rotation. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="LDGNUM">
        /// (input) INTEGER
        /// The leading dimension of GIVNUM, must be at least N.
        ///</param>
        /// <param name="C">
        /// (output) DOUBLE PRECISION
        /// C contains garbage if SQRE =0 and the C-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION
        /// S contains garbage if SQRE =0 and the S-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int ICOMPQ, int NL, int NR, int SQRE, ref int K, ref double[] D, int offset_d
                         , ref double[] Z, int offset_z, ref double[] ZW, int offset_zw, ref double[] VF, int offset_vf, ref double[] VFW, int offset_vfw, ref double[] VL, int offset_vl, ref double[] VLW, int offset_vlw
                         , double ALPHA, double BETA, ref double[] DSIGMA, int offset_dsigma, ref int[] IDX, int offset_idx, ref int[] IDXP, int offset_idxp, ref int[] IDXQ, int offset_idxq
                         , ref int[] PERM, int offset_perm, ref int GIVPTR, ref int[] GIVCOL, int offset_givcol, int LDGCOL, ref double[] GIVNUM, int offset_givnum, int LDGNUM
                         , ref double C, ref double S, ref int INFO)
        {

            #region Variables
            
            int I = 0; int IDXI = 0; int IDXJ = 0; int IDXJP = 0; int J = 0; int JP = 0; int JPREV = 0; int K2 = 0; int M = 0; 
            int N = 0;int NLP1 = 0; int NLP2 = 0; double EPS = 0; double HLFTOL = 0; double TAU = 0; double TOL = 0; 
            double Z1 = 0;

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_zw = -1 + offset_zw;  int o_vf = -1 + offset_vf; 
             int o_vfw = -1 + offset_vfw; int o_vl = -1 + offset_vl;  int o_vlw = -1 + offset_vlw; 
             int o_dsigma = -1 + offset_dsigma; int o_idx = -1 + offset_idx;  int o_idxp = -1 + offset_idxp; 
             int o_idxq = -1 + offset_idxq; int o_perm = -1 + offset_perm;  int o_givcol = -1 - LDGCOL + offset_givcol; 
             int o_givnum = -1 - LDGNUM + offset_givnum;

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
            // *  DLASD7 merges the two sets of singular values together into a single
            // *  sorted set. Then it tries to deflate the size of the problem. There
            // *  are two ways in which deflation can occur:  when two or more singular
            // *  values are close together or if there is a tiny entry in the Z
            // *  vector. For each such occurrence the order of the related
            // *  secular equation problem is reduced by one.
            // *
            // *  DLASD7 is called from DLASD6.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ  (input) INTEGER
            // *          Specifies whether singular vectors are to be computed
            // *          in compact form, as follows:
            // *          = 0: Compute singular values only.
            // *          = 1: Compute singular vectors of upper
            // *               bidiagonal matrix in compact form.
            // *
            // *  NL     (input) INTEGER
            // *         The row dimension of the upper block. NL >= 1.
            // *
            // *  NR     (input) INTEGER
            // *         The row dimension of the lower block. NR >= 1.
            // *
            // *  SQRE   (input) INTEGER
            // *         = 0: the lower block is an NR-by-NR square matrix.
            // *         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
            // *
            // *         The bidiagonal matrix has
            // *         N = NL + NR + 1 rows and
            // *         M = N + SQRE >= N columns.
            // *
            // *  K      (output) INTEGER
            // *         Contains the dimension of the non-deflated matrix, this is
            // *         the order of the related secular equation. 1 <= K <=N.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension ( N )
            // *         On entry D contains the singular values of the two submatrices
            // *         to be combined. On exit D contains the trailing (N-K) updated
            // *         singular values (those which were deflated) sorted into
            // *         increasing order.
            // *
            // *  Z      (output) DOUBLE PRECISION array, dimension ( M )
            // *         On exit Z contains the updating row vector in the secular
            // *         equation.
            // *
            // *  ZW     (workspace) DOUBLE PRECISION array, dimension ( M )
            // *         Workspace for Z.
            // *
            // *  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
            // *         On entry, VF(1:NL+1) contains the first components of all
            // *         right singular vectors of the upper block; and VF(NL+2:M)
            // *         contains the first components of all right singular vectors
            // *         of the lower block. On exit, VF contains the first components
            // *         of all right singular vectors of the bidiagonal matrix.
            // *
            // *  VFW    (workspace) DOUBLE PRECISION array, dimension ( M )
            // *         Workspace for VF.
            // *
            // *  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
            // *         On entry, VL(1:NL+1) contains the  last components of all
            // *         right singular vectors of the upper block; and VL(NL+2:M)
            // *         contains the last components of all right singular vectors
            // *         of the lower block. On exit, VL contains the last components
            // *         of all right singular vectors of the bidiagonal matrix.
            // *
            // *  VLW    (workspace) DOUBLE PRECISION array, dimension ( M )
            // *         Workspace for VL.
            // *
            // *  ALPHA  (input) DOUBLE PRECISION
            // *         Contains the diagonal element associated with the added row.
            // *
            // *  BETA   (input) DOUBLE PRECISION
            // *         Contains the off-diagonal element associated with the added
            // *         row.
            // *
            // *  DSIGMA (output) DOUBLE PRECISION array, dimension ( N )
            // *         Contains a copy of the diagonal elements (K-1 singular values
            // *         and one zero) in the secular equation.
            // *
            // *  IDX    (workspace) INTEGER array, dimension ( N )
            // *         This will contain the permutation used to sort the contents of
            // *         D into ascending order.
            // *
            // *  IDXP   (workspace) INTEGER array, dimension ( N )
            // *         This will contain the permutation used to place deflated
            // *         values of D at the end of the array. On output IDXP(2:K)
            // *         points to the nondeflated D-values and IDXP(K+1:N)
            // *         points to the deflated singular values.
            // *
            // *  IDXQ   (input) INTEGER array, dimension ( N )
            // *         This contains the permutation which separately sorts the two
            // *         sub-problems in D into ascending order.  Note that entries in
            // *         the first half of this permutation must first be moved one
            // *         position backward; and entries in the second half
            // *         must first have NL+1 added to their values.
            // *
            // *  PERM   (output) INTEGER array, dimension ( N )
            // *         The permutations (from deflation and sorting) to be applied
            // *         to each singular block. Not referenced if ICOMPQ = 0.
            // *
            // *  GIVPTR (output) INTEGER
            // *         The number of Givens rotations which took place in this
            // *         subproblem. Not referenced if ICOMPQ = 0.
            // *
            // *  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
            // *         Each pair of numbers indicates a pair of columns to take place
            // *         in a Givens rotation. Not referenced if ICOMPQ = 0.
            // *
            // *  LDGCOL (input) INTEGER
            // *         The leading dimension of GIVCOL, must be at least N.
            // *
            // *  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
            // *         Each number indicates the C or S value to be used in the
            // *         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
            // *
            // *  LDGNUM (input) INTEGER
            // *         The leading dimension of GIVNUM, must be at least N.
            // *
            // *  C      (output) DOUBLE PRECISION
            // *         C contains garbage if SQRE =0 and the C-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  S      (output) DOUBLE PRECISION
            // *         S contains garbage if SQRE =0 and the S-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  INFO   (output) INTEGER
            // *         = 0:  successful exit.
            // *         < 0:  if INFO = -i, the i-th argument had an illegal value.
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
            // *
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
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
            N = NL + NR + 1;
            M = N + SQRE;
            // *
            if ((ICOMPQ < 0) || (ICOMPQ > 1))
            {
                INFO =  - 1;
            }
            else
            {
                if (NL < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (NR < 1)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if ((SQRE < 0) || (SQRE > 1))
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDGCOL < N)
                            {
                                INFO =  - 22;
                            }
                            else
                            {
                                if (LDGNUM < N)
                                {
                                    INFO =  - 24;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD7",  - INFO);
                return;
            }
            // *
            NLP1 = NL + 1;
            NLP2 = NL + 2;
            if (ICOMPQ == 1)
            {
                GIVPTR = 0;
            }
            // *
            // *     Generate the first part of the vector Z and move the singular
            // *     values in the first part of D one position backward.
            // *
            Z1 = ALPHA * VL[NLP1 + o_vl];
            VL[NLP1 + o_vl] = ZERO;
            TAU = VF[NLP1 + o_vf];
            for (I = NL; I >= 1; I +=  - 1)
            {
                Z[I + 1 + o_z] = ALPHA * VL[I + o_vl];
                VL[I + o_vl] = ZERO;
                VF[I + 1 + o_vf] = VF[I + o_vf];
                D[I + 1 + o_d] = D[I + o_d];
                IDXQ[I + 1 + o_idxq] = IDXQ[I + o_idxq] + 1;
            }
            VF[1 + o_vf] = TAU;
            // *
            // *     Generate the second part of the vector Z.
            // *
            for (I = NLP2; I <= M; I++)
            {
                Z[I + o_z] = BETA * VF[I + o_vf];
                VF[I + o_vf] = ZERO;
            }
            // *
            // *     Sort the singular values into increasing order
            // *
            for (I = NLP2; I <= N; I++)
            {
                IDXQ[I + o_idxq] += NLP1;
            }
            // *
            // *     DSIGMA, IDXC, IDXC, and ZW are used as storage space.
            // *
            for (I = 2; I <= N; I++)
            {
                DSIGMA[I + o_dsigma] = D[IDXQ[I + o_idxq] + o_d];
                ZW[I + o_zw] = Z[IDXQ[I + o_idxq] + o_z];
                VFW[I + o_vfw] = VF[IDXQ[I + o_idxq] + o_vf];
                VLW[I + o_vlw] = VL[IDXQ[I + o_idxq] + o_vl];
            }
            // *
            this._dlamrg.Run(NL, NR, DSIGMA, 2 + o_dsigma, 1, 1, ref IDX, 2 + o_idx);
            // *
            for (I = 2; I <= N; I++)
            {
                IDXI = 1 + IDX[I + o_idx];
                D[I + o_d] = DSIGMA[IDXI + o_dsigma];
                Z[I + o_z] = ZW[IDXI + o_zw];
                VF[I + o_vf] = VFW[IDXI + o_vfw];
                VL[I + o_vl] = VLW[IDXI + o_vlw];
            }
            // *
            // *     Calculate the allowable deflation tolerence
            // *
            EPS = this._dlamch.Run("Epsilon");
            TOL = Math.Max(Math.Abs(ALPHA), Math.Abs(BETA));
            TOL = EIGHT * EIGHT * EPS * Math.Max(Math.Abs(D[N + o_d]), TOL);
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
                    if (J == N) goto LABEL100;
                }
                else
                {
                    JPREV = J;
                    goto LABEL70;
                }
            }
        LABEL70:;
            J = JPREV;
        LABEL80:;
            J += 1;
            if (J > N) goto LABEL90;
            if (Math.Abs(Z[J + o_z]) <= TOL)
            {
                // *
                // *        Deflate due to small z component.
                // *
                K2 -= 1;
                IDXP[K2 + o_idxp] = J;
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
                    Z[J + o_z] = TAU;
                    Z[JPREV + o_z] = ZERO;
                    C /= TAU;
                    S =  - S / TAU;
                    // *
                    // *           Record the appropriate Givens rotation
                    // *
                    if (ICOMPQ == 1)
                    {
                        GIVPTR += 1;
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
                        GIVCOL[GIVPTR+2 * LDGCOL + o_givcol] = IDXJP;
                        GIVCOL[GIVPTR+1 * LDGCOL + o_givcol] = IDXJ;
                        GIVNUM[GIVPTR+2 * LDGNUM + o_givnum] = C;
                        GIVNUM[GIVPTR+1 * LDGNUM + o_givnum] = S;
                    }
                    this._drot.Run(1, ref VF, JPREV + o_vf, 1, ref VF, J + o_vf, 1, C
                                   , S);
                    this._drot.Run(1, ref VL, JPREV + o_vl, 1, ref VL, J + o_vl, 1, C
                                   , S);
                    K2 -= 1;
                    IDXP[K2 + o_idxp] = JPREV;
                    JPREV = J;
                }
                else
                {
                    K += 1;
                    ZW[K + o_zw] = Z[JPREV + o_z];
                    DSIGMA[K + o_dsigma] = D[JPREV + o_d];
                    IDXP[K + o_idxp] = JPREV;
                    JPREV = J;
                }
            }
            goto LABEL80;
        LABEL90:;
            // *
            // *     Record the last singular value.
            // *
            K += 1;
            ZW[K + o_zw] = Z[JPREV + o_z];
            DSIGMA[K + o_dsigma] = D[JPREV + o_d];
            IDXP[K + o_idxp] = JPREV;
            // *
        LABEL100:;
            // *
            // *     Sort the singular values into DSIGMA. The singular values which
            // *     were not deflated go into the first K slots of DSIGMA, except
            // *     that DSIGMA(1) is treated separately.
            // *
            for (J = 2; J <= N; J++)
            {
                JP = IDXP[J + o_idxp];
                DSIGMA[J + o_dsigma] = D[JP + o_d];
                VFW[J + o_vfw] = VF[JP + o_vf];
                VLW[J + o_vlw] = VL[JP + o_vl];
            }
            if (ICOMPQ == 1)
            {
                for (J = 2; J <= N; J++)
                {
                    JP = IDXP[J + o_idxp];
                    PERM[J + o_perm] = IDXQ[IDX[JP + o_idx] + 1 + o_idxq];
                    if (PERM[J + o_perm] <= NLP1)
                    {
                        PERM[J + o_perm] -= 1;
                    }
                }
            }
            // *
            // *     The deflated singular values go back into the last N - K slots of
            // *     D.
            // *
            this._dcopy.Run(N - K, DSIGMA, K + 1 + o_dsigma, 1, ref D, K + 1 + o_d, 1);
            // *
            // *     Determine DSIGMA(1), DSIGMA(2), Z(1), VF(1), VL(1), VF(M), and
            // *     VL(M).
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
                    S =  - Z[M + o_z] / Z[1 + o_z];
                }
                this._drot.Run(1, ref VF, M + o_vf, 1, ref VF, 1 + o_vf, 1, C
                               , S);
                this._drot.Run(1, ref VL, M + o_vl, 1, ref VL, 1 + o_vl, 1, C
                               , S);
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
            // *     Restore Z, VF, and VL.
            // *
            this._dcopy.Run(K - 1, ZW, 2 + o_zw, 1, ref Z, 2 + o_z, 1);
            this._dcopy.Run(N - 1, VFW, 2 + o_vfw, 1, ref VF, 2 + o_vf, 1);
            this._dcopy.Run(N - 1, VLW, 2 + o_vlw, 1, ref VL, 2 + o_vl, 1);
            // *
            return;
            // *
            // *     End of DLASD7
            // *

            #endregion

        }
    }
}
