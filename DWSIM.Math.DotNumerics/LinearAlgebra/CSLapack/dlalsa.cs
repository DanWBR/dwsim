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
    /// DLALSA is an itermediate step in solving the least squares problem
    /// by computing the SVD of the coefficient matrix in compact form (The
    /// singular vectors are computed as products of simple orthorgonal
    /// matrices.).
    /// 
    /// If ICOMPQ = 0, DLALSA applies the inverse of the left singular vector
    /// matrix of an upper bidiagonal matrix to the right hand side; and if
    /// ICOMPQ = 1, DLALSA applies the right singular vector matrix to the
    /// right hand side. The singular vector matrices were generated in
    /// compact form by DLALSA.
    /// 
    ///</summary>
    public class DLALSA
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DGEMM _dgemm; DLALS0 _dlals0; DLASDT _dlasdt; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DLALSA(DCOPY dcopy, DGEMM dgemm, DLALS0 dlals0, DLASDT dlasdt, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemm = dgemm; this._dlals0 = dlals0; this._dlasdt = dlasdt; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLALSA()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DNRM2 dnrm2 = new DNRM2();
            DLASDT dlasdt = new DLASDT();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLALS0 dlals0 = new DLALS0(dcopy, dgemv, dlacpy, dlascl, drot, dscal, xerbla, dlamc3, dnrm2);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemm = dgemm; this._dlals0 = dlals0; this._dlasdt = dlasdt; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLALSA is an itermediate step in solving the least squares problem
        /// by computing the SVD of the coefficient matrix in compact form (The
        /// singular vectors are computed as products of simple orthorgonal
        /// matrices.).
        /// 
        /// If ICOMPQ = 0, DLALSA applies the inverse of the left singular vector
        /// matrix of an upper bidiagonal matrix to the right hand side; and if
        /// ICOMPQ = 1, DLALSA applies the right singular vector matrix to the
        /// right hand side. The singular vector matrices were generated in
        /// compact form by DLALSA.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether the left or the right singular vector
        /// matrix is involved.
        /// = 0: Left singular vector matrix
        /// = 1: Right singular vector matrix
        ///</param>
        /// <param name="SMLSIZ">
        /// (input) INTEGER
        /// The maximum size of the subproblems at the bottom of the
        /// computation tree.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The row and column dimensions of the upper bidiagonal matrix.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of columns of B and BX. NRHS must be at least 1.
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension ( LDB, NRHS )
        /// On input, B contains the right hand sides of the least
        /// squares problem in rows 1 through M.
        /// On output, B contains the solution X in rows 1 through N.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of B in the calling subprogram.
        /// LDB must be at least max(1,MAX( M, N ) ).
        ///</param>
        /// <param name="BX">
        /// (output) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
        /// On exit, the result of applying the left or right singular
        /// vector matrix to B.
        ///</param>
        /// <param name="LDBX">
        /// (input) INTEGER
        /// The leading dimension of BX.
        ///</param>
        /// <param name="U">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ ).
        /// On entry, U contains the left singular vector matrices of all
        /// subproblems at the bottom level.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER, LDU = .GT. N.
        /// The leading dimension of arrays U, VT, DIFL, DIFR,
        /// POLES, GIVNUM, and Z.
        ///</param>
        /// <param name="VT">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ+1 ).
        /// On entry, VT' contains the right singular vector matrices of
        /// all subproblems at the bottom level.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER array, dimension ( N ).
        ///</param>
        /// <param name="DIFL">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
        /// where NLVL = INT(log_2 (N/(SMLSIZ+1))) + 1.
        ///</param>
        /// <param name="DIFR">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
        /// On entry, DIFL(*, I) and DIFR(*, 2 * I -1) record
        /// distances between singular values on the I-th level and
        /// singular values on the (I -1)-th level, and DIFR(*, 2 * I)
        /// record the normalizing factors of the right singular vectors
        /// matrices of subproblems on I-th level.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
        /// On entry, Z(1, I) contains the components of the deflation-
        /// adjusted updating row vector for subproblems on the I-th
        /// level.
        ///</param>
        /// <param name="POLES">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
        /// On entry, POLES(*, 2 * I -1: 2 * I) contains the new and old
        /// singular values involved in the secular equations on the I-th
        /// level.
        ///</param>
        /// <param name="GIVPTR">
        /// (input) INTEGER array, dimension ( N ).
        /// On entry, GIVPTR( I ) records the number of Givens
        /// rotations performed on the I-th problem on the computation
        /// tree.
        ///</param>
        /// <param name="GIVCOL">
        /// (input) INTEGER array, dimension ( LDGCOL, 2 * NLVL ).
        /// On entry, for each I, GIVCOL(*, 2 * I - 1: 2 * I) records the
        /// locations of Givens rotations performed on the I-th level on
        /// the computation tree.
        ///</param>
        /// <param name="LDGCOL">
        /// (input) INTEGER, LDGCOL = .GT. N.
        /// The leading dimension of arrays GIVCOL and PERM.
        ///</param>
        /// <param name="PERM">
        /// (input) INTEGER array, dimension ( LDGCOL, NLVL ).
        /// On entry, PERM(*, I) records permutations done on the I-th
        /// level of the computation tree.
        ///</param>
        /// <param name="GIVNUM">
        /// (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
        /// On entry, GIVNUM(*, 2 *I -1 : 2 * I) records the C- and S-
        /// values of Givens rotations performed on the I-th level on the
        /// computation tree.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION array, dimension ( N ).
        /// On entry, if the I-th subproblem is not square,
        /// C( I ) contains the C-value of a Givens rotation related to
        /// the right null space of the I-th subproblem.
        ///</param>
        /// <param name="S">
        /// (input) DOUBLE PRECISION array, dimension ( N ).
        /// On entry, if the I-th subproblem is not square,
        /// S( I ) contains the S-value of a Givens rotation related to
        /// the right null space of the I-th subproblem.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array.
        /// The dimension must be at least N.
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array.
        /// The dimension must be at least 3 * N
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int ICOMPQ, int SMLSIZ, int N, int NRHS, ref double[] B, int offset_b, int LDB
                         , ref double[] BX, int offset_bx, int LDBX, double[] U, int offset_u, int LDU, double[] VT, int offset_vt, int[] K, int offset_k
                         , double[] DIFL, int offset_difl, double[] DIFR, int offset_difr, double[] Z, int offset_z, double[] POLES, int offset_poles, int[] GIVPTR, int offset_givptr, int[] GIVCOL, int offset_givcol
                         , int LDGCOL, int[] PERM, int offset_perm, double[] GIVNUM, int offset_givnum, double[] C, int offset_c, double[] S, int offset_s, ref double[] WORK, int offset_work
                         , ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int I = 0; int I1 = 0; int IC = 0; int IM1 = 0; int INODE = 0; int J = 0; int LF = 0; int LL = 0; int LVL = 0; 
            int LVL2 = 0;int ND = 0; int NDB1 = 0; int NDIML = 0; int NDIMR = 0; int NL = 0; int NLF = 0; int NLP1 = 0; 
            int NLVL = 0;int NR = 0; int NRF = 0; int NRP1 = 0; int SQRE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_b = -1 - LDB + offset_b;  int o_bx = -1 - LDBX + offset_bx;  int o_u = -1 - LDU + offset_u; 
             int o_vt = -1 - LDU + offset_vt; int o_k = -1 + offset_k;  int o_difl = -1 - LDU + offset_difl; 
             int o_difr = -1 - LDU + offset_difr; int o_z = -1 - LDU + offset_z;  int o_poles = -1 - LDU + offset_poles; 
             int o_givptr = -1 + offset_givptr; int o_givcol = -1 - LDGCOL + offset_givcol; 
             int o_perm = -1 - LDGCOL + offset_perm; int o_givnum = -1 - LDU + offset_givnum;  int o_c = -1 + offset_c; 
             int o_s = -1 + offset_s; int o_work = -1 + offset_work;  int o_iwork = -1 + offset_iwork; 

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
            // *  DLALSA is an itermediate step in solving the least squares problem
            // *  by computing the SVD of the coefficient matrix in compact form (The
            // *  singular vectors are computed as products of simple orthorgonal
            // *  matrices.).
            // *
            // *  If ICOMPQ = 0, DLALSA applies the inverse of the left singular vector
            // *  matrix of an upper bidiagonal matrix to the right hand side; and if
            // *  ICOMPQ = 1, DLALSA applies the right singular vector matrix to the
            // *  right hand side. The singular vector matrices were generated in
            // *  compact form by DLALSA.
            // *
            // *  Arguments
            // *  =========
            // *
            // *
            // *  ICOMPQ (input) INTEGER
            // *         Specifies whether the left or the right singular vector
            // *         matrix is involved.
            // *         = 0: Left singular vector matrix
            // *         = 1: Right singular vector matrix
            // *
            // *  SMLSIZ (input) INTEGER
            // *         The maximum size of the subproblems at the bottom of the
            // *         computation tree.
            // *
            // *  N      (input) INTEGER
            // *         The row and column dimensions of the upper bidiagonal matrix.
            // *
            // *  NRHS   (input) INTEGER
            // *         The number of columns of B and BX. NRHS must be at least 1.
            // *
            // *  B      (input/output) DOUBLE PRECISION array, dimension ( LDB, NRHS )
            // *         On input, B contains the right hand sides of the least
            // *         squares problem in rows 1 through M.
            // *         On output, B contains the solution X in rows 1 through N.
            // *
            // *  LDB    (input) INTEGER
            // *         The leading dimension of B in the calling subprogram.
            // *         LDB must be at least max(1,MAX( M, N ) ).
            // *
            // *  BX     (output) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
            // *         On exit, the result of applying the left or right singular
            // *         vector matrix to B.
            // *
            // *  LDBX   (input) INTEGER
            // *         The leading dimension of BX.
            // *
            // *  U      (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ ).
            // *         On entry, U contains the left singular vector matrices of all
            // *         subproblems at the bottom level.
            // *
            // *  LDU    (input) INTEGER, LDU = > N.
            // *         The leading dimension of arrays U, VT, DIFL, DIFR,
            // *         POLES, GIVNUM, and Z.
            // *
            // *  VT     (input) DOUBLE PRECISION array, dimension ( LDU, SMLSIZ+1 ).
            // *         On entry, VT' contains the right singular vector matrices of
            // *         all subproblems at the bottom level.
            // *
            // *  K      (input) INTEGER array, dimension ( N ).
            // *
            // *  DIFL   (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
            // *         where NLVL = INT(log_2 (N/(SMLSIZ+1))) + 1.
            // *
            // *  DIFR   (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
            // *         On entry, DIFL(*, I) and DIFR(*, 2 * I -1) record
            // *         distances between singular values on the I-th level and
            // *         singular values on the (I -1)-th level, and DIFR(*, 2 * I)
            // *         record the normalizing factors of the right singular vectors
            // *         matrices of subproblems on I-th level.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension ( LDU, NLVL ).
            // *         On entry, Z(1, I) contains the components of the deflation-
            // *         adjusted updating row vector for subproblems on the I-th
            // *         level.
            // *
            // *  POLES  (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
            // *         On entry, POLES(*, 2 * I -1: 2 * I) contains the new and old
            // *         singular values involved in the secular equations on the I-th
            // *         level.
            // *
            // *  GIVPTR (input) INTEGER array, dimension ( N ).
            // *         On entry, GIVPTR( I ) records the number of Givens
            // *         rotations performed on the I-th problem on the computation
            // *         tree.
            // *
            // *  GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 * NLVL ).
            // *         On entry, for each I, GIVCOL(*, 2 * I - 1: 2 * I) records the
            // *         locations of Givens rotations performed on the I-th level on
            // *         the computation tree.
            // *
            // *  LDGCOL (input) INTEGER, LDGCOL = > N.
            // *         The leading dimension of arrays GIVCOL and PERM.
            // *
            // *  PERM   (input) INTEGER array, dimension ( LDGCOL, NLVL ).
            // *         On entry, PERM(*, I) records permutations done on the I-th
            // *         level of the computation tree.
            // *
            // *  GIVNUM (input) DOUBLE PRECISION array, dimension ( LDU, 2 * NLVL ).
            // *         On entry, GIVNUM(*, 2 *I -1 : 2 * I) records the C- and S-
            // *         values of Givens rotations performed on the I-th level on the
            // *         computation tree.
            // *
            // *  C      (input) DOUBLE PRECISION array, dimension ( N ).
            // *         On entry, if the I-th subproblem is not square,
            // *         C( I ) contains the C-value of a Givens rotation related to
            // *         the right null space of the I-th subproblem.
            // *
            // *  S      (input) DOUBLE PRECISION array, dimension ( N ).
            // *         On entry, if the I-th subproblem is not square,
            // *         S( I ) contains the S-value of a Givens rotation related to
            // *         the right null space of the I-th subproblem.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array.
            // *         The dimension must be at least N.
            // *
            // *  IWORK  (workspace) INTEGER array.
            // *         The dimension must be at least 3 * N
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Ren-Cang Li, Computer Science Division, University of
            // *       California at Berkeley, USA
            // *     Osni Marques, LBNL/NERSC, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if ((ICOMPQ < 0) || (ICOMPQ > 1))
            {
                INFO =  - 1;
            }
            else
            {
                if (SMLSIZ < 3)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < SMLSIZ)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (NRHS < 1)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDB < N)
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LDBX < N)
                                {
                                    INFO =  - 8;
                                }
                                else
                                {
                                    if (LDU < N)
                                    {
                                        INFO =  - 10;
                                    }
                                    else
                                    {
                                        if (LDGCOL < N)
                                        {
                                            INFO =  - 19;
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
                this._xerbla.Run("DLALSA",  - INFO);
                return;
            }
            // *
            // *     Book-keeping and  setting up the computation tree.
            // *
            INODE = 1;
            NDIML = INODE + N;
            NDIMR = NDIML + N;
            // *
            this._dlasdt.Run(N, ref NLVL, ref ND, ref IWORK, INODE + o_iwork, ref IWORK, NDIML + o_iwork, ref IWORK, NDIMR + o_iwork
                             , SMLSIZ);
            // *
            // *     The following code applies back the left singular vector factors.
            // *     For applying back the right singular vector factors, go to 50.
            // *
            if (ICOMPQ == 1)
            {
                goto LABEL50;
            }
            // *
            // *     The nodes on the bottom level of the tree were solved
            // *     by DLASDQ. The corresponding left and right singular vector
            // *     matrices are in explicit form. First apply back the left
            // *     singular vector matrices.
            // *
            NDB1 = (ND + 1) / 2;
            for (I = NDB1; I <= ND; I++)
            {
                // *
                // *        IC : center row of each node
                // *        NL : number of rows of left  subproblem
                // *        NR : number of rows of right subproblem
                // *        NLF: starting row of the left   subproblem
                // *        NRF: starting row of the right  subproblem
                // *
                I1 = I - 1;
                IC = IWORK[INODE + I1 + o_iwork];
                NL = IWORK[NDIML + I1 + o_iwork];
                NR = IWORK[NDIMR + I1 + o_iwork];
                NLF = IC - NL;
                NRF = IC + 1;
                this._dgemm.Run("T", "N", NL, NRHS, NL, ONE
                                , U, NLF+1 * LDU + o_u, LDU, B, NLF+1 * LDB + o_b, LDB, ZERO, ref BX, NLF+1 * LDBX + o_bx
                                , LDBX);
                this._dgemm.Run("T", "N", NR, NRHS, NR, ONE
                                , U, NRF+1 * LDU + o_u, LDU, B, NRF+1 * LDB + o_b, LDB, ZERO, ref BX, NRF+1 * LDBX + o_bx
                                , LDBX);
            }
            // *
            // *     Next copy the rows of B that correspond to unchanged rows
            // *     in the bidiagonal matrix to BX.
            // *
            for (I = 1; I <= ND; I++)
            {
                IC = IWORK[INODE + I - 1 + o_iwork];
                this._dcopy.Run(NRHS, B, IC+1 * LDB + o_b, LDB, ref BX, IC+1 * LDBX + o_bx, LDBX);
            }
            // *
            // *     Finally go through the left singular vector matrices of all
            // *     the other subproblems bottom-up on the tree.
            // *
            J = (int)Math.Pow(2, NLVL);
            SQRE = 0;
            // *
            for (LVL = NLVL; LVL >= 1; LVL +=  - 1)
            {
                LVL2 = 2 * LVL - 1;
                // *
                // *        find the first node LF and last node LL on
                // *        the current level LVL
                // *
                if (LVL == 1)
                {
                    LF = 1;
                    LL = 1;
                }
                else
                {
                    LF = (int)Math.Pow(2, LVL - 1);
                    LL = 2 * LF - 1;
                }
                for (I = LF; I <= LL; I++)
                {
                    IM1 = I - 1;
                    IC = IWORK[INODE + IM1 + o_iwork];
                    NL = IWORK[NDIML + IM1 + o_iwork];
                    NR = IWORK[NDIMR + IM1 + o_iwork];
                    NLF = IC - NL;
                    NRF = IC + 1;
                    J -= 1;
                    this._dlals0.Run(ICOMPQ, NL, NR, SQRE, NRHS, ref BX, NLF+1 * LDBX + o_bx
                                     , LDBX, ref B, NLF+1 * LDB + o_b, LDB, PERM, NLF+LVL * LDGCOL + o_perm, GIVPTR[J + o_givptr], GIVCOL, NLF+LVL2 * LDGCOL + o_givcol
                                     , LDGCOL, GIVNUM, NLF+LVL2 * LDU + o_givnum, LDU, POLES, NLF+LVL2 * LDU + o_poles, DIFL, NLF+LVL * LDU + o_difl, DIFR, NLF+LVL2 * LDU + o_difr
                                     , Z, NLF+LVL * LDU + o_z, K[J + o_k], C[J + o_c], S[J + o_s], ref WORK, offset_work, ref INFO);
                }
            }
            goto LABEL90;
            // *
            // *     ICOMPQ = 1: applying back the right singular vector factors.
            // *
        LABEL50:;
            // *
            // *     First now go through the right singular vector matrices of all
            // *     the tree nodes top-down.
            // *
            J = 0;
            for (LVL = 1; LVL <= NLVL; LVL++)
            {
                LVL2 = 2 * LVL - 1;
                // *
                // *        Find the first node LF and last node LL on
                // *        the current level LVL.
                // *
                if (LVL == 1)
                {
                    LF = 1;
                    LL = 1;
                }
                else
                {
                    LF = (int)Math.Pow(2, LVL - 1);
                    LL = 2 * LF - 1;
                }
                for (I = LL; I >= LF; I +=  - 1)
                {
                    IM1 = I - 1;
                    IC = IWORK[INODE + IM1 + o_iwork];
                    NL = IWORK[NDIML + IM1 + o_iwork];
                    NR = IWORK[NDIMR + IM1 + o_iwork];
                    NLF = IC - NL;
                    NRF = IC + 1;
                    if (I == LL)
                    {
                        SQRE = 0;
                    }
                    else
                    {
                        SQRE = 1;
                    }
                    J += 1;
                    this._dlals0.Run(ICOMPQ, NL, NR, SQRE, NRHS, ref B, NLF+1 * LDB + o_b
                                     , LDB, ref BX, NLF+1 * LDBX + o_bx, LDBX, PERM, NLF+LVL * LDGCOL + o_perm, GIVPTR[J + o_givptr], GIVCOL, NLF+LVL2 * LDGCOL + o_givcol
                                     , LDGCOL, GIVNUM, NLF+LVL2 * LDU + o_givnum, LDU, POLES, NLF+LVL2 * LDU + o_poles, DIFL, NLF+LVL * LDU + o_difl, DIFR, NLF+LVL2 * LDU + o_difr
                                     , Z, NLF+LVL * LDU + o_z, K[J + o_k], C[J + o_c], S[J + o_s], ref WORK, offset_work, ref INFO);
                }
            }
            // *
            // *     The nodes on the bottom level of the tree were solved
            // *     by DLASDQ. The corresponding right singular vector
            // *     matrices are in explicit form. Apply them back.
            // *
            NDB1 = (ND + 1) / 2;
            for (I = NDB1; I <= ND; I++)
            {
                I1 = I - 1;
                IC = IWORK[INODE + I1 + o_iwork];
                NL = IWORK[NDIML + I1 + o_iwork];
                NR = IWORK[NDIMR + I1 + o_iwork];
                NLP1 = NL + 1;
                if (I == ND)
                {
                    NRP1 = NR;
                }
                else
                {
                    NRP1 = NR + 1;
                }
                NLF = IC - NL;
                NRF = IC + 1;
                this._dgemm.Run("T", "N", NLP1, NRHS, NLP1, ONE
                                , VT, NLF+1 * LDU + o_vt, LDU, B, NLF+1 * LDB + o_b, LDB, ZERO, ref BX, NLF+1 * LDBX + o_bx
                                , LDBX);
                this._dgemm.Run("T", "N", NRP1, NRHS, NRP1, ONE
                                , VT, NRF+1 * LDU + o_vt, LDU, B, NRF+1 * LDB + o_b, LDB, ZERO, ref BX, NRF+1 * LDBX + o_bx
                                , LDBX);
            }
            // *
        LABEL90:;
            // *
            return;
            // *
            // *     End of DLALSA
            // *

            #endregion

        }
    }
}
