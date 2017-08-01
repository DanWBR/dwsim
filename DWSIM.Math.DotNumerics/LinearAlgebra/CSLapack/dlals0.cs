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
    /// DLALS0 applies back the multiplying factors of either the left or the
    /// right singular vector matrix of a diagonal matrix appended by a row
    /// to the right hand side matrix B in solving the least squares problem
    /// using the divide-and-conquer SVD approach.
    /// 
    /// For the left singular vector matrix, three types of orthogonal
    /// matrices are involved:
    /// 
    /// (1L) Givens rotations: the number of such rotations is GIVPTR; the
    /// pairs of columns/rows they were applied to are stored in GIVCOL;
    /// and the C- and S-values of these rotations are stored in GIVNUM.
    /// 
    /// (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
    /// row, and for J=2:N, PERM(J)-th row of B is to be moved to the
    /// J-th row.
    /// 
    /// (3L) The left singular vector matrix of the remaining matrix.
    /// 
    /// For the right singular vector matrix, four types of orthogonal
    /// matrices are involved:
    /// 
    /// (1R) The right singular vector matrix of the remaining matrix.
    /// 
    /// (2R) If SQRE = 1, one extra Givens rotation to generate the right
    /// null space.
    /// 
    /// (3R) The inverse transformation of (2L).
    /// 
    /// (4R) The inverse transformation of (1L).
    /// 
    ///</summary>
    public class DLALS0
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DGEMV _dgemv; DLACPY _dlacpy; DLASCL _dlascl; DROT _drot; DSCAL _dscal; XERBLA _xerbla; DLAMC3 _dlamc3; 
        DNRM2 _dnrm2;

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; const double NEGONE =  - 1.0E0; 

        #endregion

        public DLALS0(DCOPY dcopy, DGEMV dgemv, DLACPY dlacpy, DLASCL dlascl, DROT drot, DSCAL dscal, XERBLA xerbla, DLAMC3 dlamc3, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._dlacpy = dlacpy; this._dlascl = dlascl; this._drot = drot; 
            this._dscal = dscal;this._xerbla = xerbla; this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; 

            #endregion

        }
    
        public DLALS0()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            DNRM2 dnrm2 = new DNRM2();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._dlacpy = dlacpy; this._dlascl = dlascl; this._drot = drot; 
            this._dscal = dscal;this._xerbla = xerbla; this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLALS0 applies back the multiplying factors of either the left or the
        /// right singular vector matrix of a diagonal matrix appended by a row
        /// to the right hand side matrix B in solving the least squares problem
        /// using the divide-and-conquer SVD approach.
        /// 
        /// For the left singular vector matrix, three types of orthogonal
        /// matrices are involved:
        /// 
        /// (1L) Givens rotations: the number of such rotations is GIVPTR; the
        /// pairs of columns/rows they were applied to are stored in GIVCOL;
        /// and the C- and S-values of these rotations are stored in GIVNUM.
        /// 
        /// (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
        /// row, and for J=2:N, PERM(J)-th row of B is to be moved to the
        /// J-th row.
        /// 
        /// (3L) The left singular vector matrix of the remaining matrix.
        /// 
        /// For the right singular vector matrix, four types of orthogonal
        /// matrices are involved:
        /// 
        /// (1R) The right singular vector matrix of the remaining matrix.
        /// 
        /// (2R) If SQRE = 1, one extra Givens rotation to generate the right
        /// null space.
        /// 
        /// (3R) The inverse transformation of (2L).
        /// 
        /// (4R) The inverse transformation of (1L).
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether singular vectors are to be computed in
        /// factored form:
        /// = 0: Left singular vector matrix.
        /// = 1: Right singular vector matrix.
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
        /// The bidiagonal matrix has row dimension N = NL + NR + 1,
        /// and column dimension M = N + SQRE.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of columns of B and BX. NRHS must be at least 1.
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension ( LDB, NRHS )
        /// On input, B contains the right hand sides of the least
        /// squares problem in rows 1 through M. On output, B contains
        /// the solution X in rows 1 through N.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of B. LDB must be at least
        /// max(1,MAX( M, N ) ).
        ///</param>
        /// <param name="BX">
        /// (workspace) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
        ///</param>
        /// <param name="LDBX">
        /// (input) INTEGER
        /// The leading dimension of BX.
        ///</param>
        /// <param name="PERM">
        /// (input) INTEGER array, dimension ( N )
        /// The permutations (from deflation and sorting) applied
        /// to the two blocks.
        ///</param>
        /// <param name="GIVPTR">
        /// (input) INTEGER
        /// The number of Givens rotations which took place in this
        /// subproblem.
        ///</param>
        /// <param name="GIVCOL">
        /// (input) INTEGER array, dimension ( LDGCOL, 2 )
        /// Each pair of numbers indicates a pair of rows/columns
        /// involved in a Givens rotation.
        ///</param>
        /// <param name="LDGCOL">
        /// (input) INTEGER
        /// The leading dimension of GIVCOL, must be at least N.
        ///</param>
        /// <param name="GIVNUM">
        /// (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
        /// Each number indicates the C or S value used in the
        /// corresponding Givens rotation.
        ///</param>
        /// <param name="LDGNUM">
        /// (input) INTEGER
        /// The leading dimension of arrays DIFR, POLES and
        /// GIVNUM, must be at least K.
        ///</param>
        /// <param name="POLES">
        /// (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
        /// On entry, POLES(1:K, 1) contains the new singular
        /// values obtained from solving the secular equation, and
        /// POLES(1:K, 2) is an array containing the poles in the secular
        /// equation.
        ///</param>
        /// <param name="DIFL">
        /// (input) DOUBLE PRECISION array, dimension ( K ).
        /// On entry, DIFL(I) is the distance between I-th updated
        /// (undeflated) singular value and the I-th (undeflated) old
        /// singular value.
        ///</param>
        /// <param name="DIFR">
        /// (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 ).
        /// On entry, DIFR(I, 1) contains the distances between I-th
        /// updated (undeflated) singular value and the I+1-th
        /// (undeflated) old singular value. And DIFR(I, 2) is the
        /// normalizing factor for the I-th right singular vector.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( K )
        /// Contain the components of the deflation-adjusted updating row
        /// vector.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// Contains the dimension of the non-deflated matrix,
        /// This is the order of the related secular equation. 1 .LE. K .LE.N.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION
        /// C contains garbage if SQRE =0 and the C-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="S">
        /// (input) DOUBLE PRECISION
        /// S contains garbage if SQRE =0 and the S-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension ( K )
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int ICOMPQ, int NL, int NR, int SQRE, int NRHS, ref double[] B, int offset_b
                         , int LDB, ref double[] BX, int offset_bx, int LDBX, int[] PERM, int offset_perm, int GIVPTR, int[] GIVCOL, int offset_givcol
                         , int LDGCOL, double[] GIVNUM, int offset_givnum, int LDGNUM, double[] POLES, int offset_poles, double[] DIFL, int offset_difl, double[] DIFR, int offset_difr
                         , double[] Z, int offset_z, int K, double C, double S, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; int M = 0; int N = 0; int NLP1 = 0; double DIFLJ = 0; double DIFRJ = 0; double DJ = 0; 
            double DSIGJ = 0;double DSIGJP = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int POLES_1 = 0; int POLES_2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_b = -1 - LDB + offset_b;  int o_bx = -1 - LDBX + offset_bx;  int o_perm = -1 + offset_perm; 
             int o_givcol = -1 - LDGCOL + offset_givcol; int o_givnum = -1 - LDGNUM + offset_givnum; 
             int o_poles = -1 - LDGNUM + offset_poles; int o_difl = -1 + offset_difl;  int o_difr = -1 - LDGNUM + offset_difr; 
             int o_z = -1 + offset_z; int o_work = -1 + offset_work; 

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
            // *  DLALS0 applies back the multiplying factors of either the left or the
            // *  right singular vector matrix of a diagonal matrix appended by a row
            // *  to the right hand side matrix B in solving the least squares problem
            // *  using the divide-and-conquer SVD approach.
            // *
            // *  For the left singular vector matrix, three types of orthogonal
            // *  matrices are involved:
            // *
            // *  (1L) Givens rotations: the number of such rotations is GIVPTR; the
            // *       pairs of columns/rows they were applied to are stored in GIVCOL;
            // *       and the C- and S-values of these rotations are stored in GIVNUM.
            // *
            // *  (2L) Permutation. The (NL+1)-st row of B is to be moved to the first
            // *       row, and for J=2:N, PERM(J)-th row of B is to be moved to the
            // *       J-th row.
            // *
            // *  (3L) The left singular vector matrix of the remaining matrix.
            // *
            // *  For the right singular vector matrix, four types of orthogonal
            // *  matrices are involved:
            // *
            // *  (1R) The right singular vector matrix of the remaining matrix.
            // *
            // *  (2R) If SQRE = 1, one extra Givens rotation to generate the right
            // *       null space.
            // *
            // *  (3R) The inverse transformation of (2L).
            // *
            // *  (4R) The inverse transformation of (1L).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ (input) INTEGER
            // *         Specifies whether singular vectors are to be computed in
            // *         factored form:
            // *         = 0: Left singular vector matrix.
            // *         = 1: Right singular vector matrix.
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
            // *         The bidiagonal matrix has row dimension N = NL + NR + 1,
            // *         and column dimension M = N + SQRE.
            // *
            // *  NRHS   (input) INTEGER
            // *         The number of columns of B and BX. NRHS must be at least 1.
            // *
            // *  B      (input/output) DOUBLE PRECISION array, dimension ( LDB, NRHS )
            // *         On input, B contains the right hand sides of the least
            // *         squares problem in rows 1 through M. On output, B contains
            // *         the solution X in rows 1 through N.
            // *
            // *  LDB    (input) INTEGER
            // *         The leading dimension of B. LDB must be at least
            // *         max(1,MAX( M, N ) ).
            // *
            // *  BX     (workspace) DOUBLE PRECISION array, dimension ( LDBX, NRHS )
            // *
            // *  LDBX   (input) INTEGER
            // *         The leading dimension of BX.
            // *
            // *  PERM   (input) INTEGER array, dimension ( N )
            // *         The permutations (from deflation and sorting) applied
            // *         to the two blocks.
            // *
            // *  GIVPTR (input) INTEGER
            // *         The number of Givens rotations which took place in this
            // *         subproblem.
            // *
            // *  GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 )
            // *         Each pair of numbers indicates a pair of rows/columns
            // *         involved in a Givens rotation.
            // *
            // *  LDGCOL (input) INTEGER
            // *         The leading dimension of GIVCOL, must be at least N.
            // *
            // *  GIVNUM (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
            // *         Each number indicates the C or S value used in the
            // *         corresponding Givens rotation.
            // *
            // *  LDGNUM (input) INTEGER
            // *         The leading dimension of arrays DIFR, POLES and
            // *         GIVNUM, must be at least K.
            // *
            // *  POLES  (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
            // *         On entry, POLES(1:K, 1) contains the new singular
            // *         values obtained from solving the secular equation, and
            // *         POLES(1:K, 2) is an array containing the poles in the secular
            // *         equation.
            // *
            // *  DIFL   (input) DOUBLE PRECISION array, dimension ( K ).
            // *         On entry, DIFL(I) is the distance between I-th updated
            // *         (undeflated) singular value and the I-th (undeflated) old
            // *         singular value.
            // *
            // *  DIFR   (input) DOUBLE PRECISION array, dimension ( LDGNUM, 2 ).
            // *         On entry, DIFR(I, 1) contains the distances between I-th
            // *         updated (undeflated) singular value and the I+1-th
            // *         (undeflated) old singular value. And DIFR(I, 2) is the
            // *         normalizing factor for the I-th right singular vector.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension ( K )
            // *         Contain the components of the deflation-adjusted updating row
            // *         vector.
            // *
            // *  K      (input) INTEGER
            // *         Contains the dimension of the non-deflated matrix,
            // *         This is the order of the related secular equation. 1 <= K <=N.
            // *
            // *  C      (input) DOUBLE PRECISION
            // *         C contains garbage if SQRE =0 and the C-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  S      (input) DOUBLE PRECISION
            // *         S contains garbage if SQRE =0 and the S-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension ( K )
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
                    }
                }
            }
            // *
            N = NL + NR + 1;
            // *
            if (NRHS < 1)
            {
                INFO =  - 5;
            }
            else
            {
                if (LDB < N)
                {
                    INFO =  - 7;
                }
                else
                {
                    if (LDBX < N)
                    {
                        INFO =  - 9;
                    }
                    else
                    {
                        if (GIVPTR < 0)
                        {
                            INFO =  - 11;
                        }
                        else
                        {
                            if (LDGCOL < N)
                            {
                                INFO =  - 13;
                            }
                            else
                            {
                                if (LDGNUM < N)
                                {
                                    INFO =  - 15;
                                }
                                else
                                {
                                    if (K < 1)
                                    {
                                        INFO =  - 20;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLALS0",  - INFO);
                return;
            }
            // *
            M = N + SQRE;
            NLP1 = NL + 1;
            // *
            if (ICOMPQ == 0)
            {
                // *
                // *        Apply back orthogonal transformations from the left.
                // *
                // *        Step (1L): apply back the Givens rotations performed.
                // *
                for (I = 1; I <= GIVPTR; I++)
                {
                    this._drot.Run(NRHS, ref B, GIVCOL[I+2 * LDGCOL + o_givcol]+1 * LDB + o_b, LDB, ref B, GIVCOL[I+1 * LDGCOL + o_givcol]+1 * LDB + o_b, LDB, GIVNUM[I+2 * LDGNUM + o_givnum]
                                   , GIVNUM[I+1 * LDGNUM + o_givnum]);
                }
                // *
                // *        Step (2L): permute rows of B.
                // *
                this._dcopy.Run(NRHS, B, NLP1+1 * LDB + o_b, LDB, ref BX, 1+1 * LDBX + o_bx, LDBX);
                for (I = 2; I <= N; I++)
                {
                    this._dcopy.Run(NRHS, B, PERM[I + o_perm]+1 * LDB + o_b, LDB, ref BX, I+1 * LDBX + o_bx, LDBX);
                }
                // *
                // *        Step (3L): apply the inverse of the left singular vector
                // *        matrix to BX.
                // *
                if (K == 1)
                {
                    this._dcopy.Run(NRHS, BX, offset_bx, LDBX, ref B, offset_b, LDB);
                    if (Z[1 + o_z] < ZERO)
                    {
                        this._dscal.Run(NRHS, NEGONE, ref B, offset_b, LDB);
                    }
                }
                else
                {
                    POLES_1 = 1 * LDGNUM + o_poles;
                    POLES_2 = 2 * LDGNUM + o_poles;
                    for (J = 1; J <= K; J++)
                    {
                        DIFLJ = DIFL[J + o_difl];
                        DJ = POLES[J + POLES_1];
                        DSIGJ =  - POLES[J + POLES_2];
                        if (J < K)
                        {
                            DIFRJ =  - DIFR[J+1 * LDGNUM + o_difr];
                            DSIGJP =  - POLES[J + 1+2 * LDGNUM + o_poles];
                        }
                        if ((Z[J + o_z] == ZERO) || (POLES[J+2 * LDGNUM + o_poles] == ZERO))
                        {
                            WORK[J + o_work] = ZERO;
                        }
                        else
                        {
                            WORK[J + o_work] =  - POLES[J+2 * LDGNUM + o_poles] * Z[J + o_z] / DIFLJ / (POLES[J+2 * LDGNUM + o_poles] + DJ);
                        }
                        for (I = 1; I <= J - 1; I++)
                        {
                            if ((Z[I + o_z] == ZERO) || (POLES[I+2 * LDGNUM + o_poles] == ZERO))
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            else
                            {
                                WORK[I + o_work] = POLES[I+2 * LDGNUM + o_poles] * Z[I + o_z] / (this._dlamc3.Run(POLES[I+2 * LDGNUM + o_poles], DSIGJ) - DIFLJ) / (POLES[I+2 * LDGNUM + o_poles] + DJ);
                            }
                        }
                        for (I = J + 1; I <= K; I++)
                        {
                            if ((Z[I + o_z] == ZERO) || (POLES[I+2 * LDGNUM + o_poles] == ZERO))
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            else
                            {
                                WORK[I + o_work] = POLES[I+2 * LDGNUM + o_poles] * Z[I + o_z] / (this._dlamc3.Run(POLES[I+2 * LDGNUM + o_poles], DSIGJP) + DIFRJ) / (POLES[I+2 * LDGNUM + o_poles] + DJ);
                            }
                        }
                        WORK[1 + o_work] = NEGONE;
                        TEMP = this._dnrm2.Run(K, WORK, offset_work, 1);
                        this._dgemv.Run("T", K, NRHS, ONE, BX, offset_bx, LDBX
                                        , WORK, offset_work, 1, ZERO, ref B, J+1 * LDB + o_b, LDB);
                        this._dlascl.Run("G", 0, 0, TEMP, ONE, 1
                                         , NRHS, ref B, J+1 * LDB + o_b, LDB, ref INFO);
                    }
                }
                // *
                // *        Move the deflated rows of BX to B also.
                // *
                if (K < Math.Max(M, N))
                {
                    this._dlacpy.Run("A", N - K, NRHS, BX, K + 1+1 * LDBX + o_bx, LDBX, ref B, K + 1+1 * LDB + o_b
                                     , LDB);
                }
            }
            else
            {
                // *
                // *        Apply back the right orthogonal transformations.
                // *
                // *        Step (1R): apply back the new right singular vector matrix
                // *        to B.
                // *
                if (K == 1)
                {
                    this._dcopy.Run(NRHS, B, offset_b, LDB, ref BX, offset_bx, LDBX);
                }
                else
                {
                    POLES_2 = 2 * LDGNUM + o_poles;
                    for (J = 1; J <= K; J++)
                    {
                        DSIGJ = POLES[J + POLES_2];
                        if (Z[J + o_z] == ZERO)
                        {
                            WORK[J + o_work] = ZERO;
                        }
                        else
                        {
                            WORK[J + o_work] =  - Z[J + o_z] / DIFL[J + o_difl] / (DSIGJ + POLES[J+1 * LDGNUM + o_poles]) / DIFR[J+2 * LDGNUM + o_difr];
                        }
                        for (I = 1; I <= J - 1; I++)
                        {
                            if (Z[J + o_z] == ZERO)
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            else
                            {
                                WORK[I + o_work] = Z[J + o_z] / (this._dlamc3.Run(DSIGJ,  - POLES[I + 1+2 * LDGNUM + o_poles]) - DIFR[I+1 * LDGNUM + o_difr]) / (DSIGJ + POLES[I+1 * LDGNUM + o_poles]) / DIFR[I+2 * LDGNUM + o_difr];
                            }
                        }
                        for (I = J + 1; I <= K; I++)
                        {
                            if (Z[J + o_z] == ZERO)
                            {
                                WORK[I + o_work] = ZERO;
                            }
                            else
                            {
                                WORK[I + o_work] = Z[J + o_z] / (this._dlamc3.Run(DSIGJ,  - POLES[I+2 * LDGNUM + o_poles]) - DIFL[I + o_difl]) / (DSIGJ + POLES[I+1 * LDGNUM + o_poles]) / DIFR[I+2 * LDGNUM + o_difr];
                            }
                        }
                        this._dgemv.Run("T", K, NRHS, ONE, B, offset_b, LDB
                                        , WORK, offset_work, 1, ZERO, ref BX, J+1 * LDBX + o_bx, LDBX);
                    }
                }
                // *
                // *        Step (2R): if SQRE = 1, apply back the rotation that is
                // *        related to the right null space of the subproblem.
                // *
                if (SQRE == 1)
                {
                    this._dcopy.Run(NRHS, B, M+1 * LDB + o_b, LDB, ref BX, M+1 * LDBX + o_bx, LDBX);
                    this._drot.Run(NRHS, ref BX, 1+1 * LDBX + o_bx, LDBX, ref BX, M+1 * LDBX + o_bx, LDBX, C
                                   , S);
                }
                if (K < Math.Max(M, N))
                {
                    this._dlacpy.Run("A", N - K, NRHS, B, K + 1+1 * LDB + o_b, LDB, ref BX, K + 1+1 * LDBX + o_bx
                                     , LDBX);
                }
                // *
                // *        Step (3R): permute rows of B.
                // *
                this._dcopy.Run(NRHS, BX, 1+1 * LDBX + o_bx, LDBX, ref B, NLP1+1 * LDB + o_b, LDB);
                if (SQRE == 1)
                {
                    this._dcopy.Run(NRHS, BX, M+1 * LDBX + o_bx, LDBX, ref B, M+1 * LDB + o_b, LDB);
                }
                for (I = 2; I <= N; I++)
                {
                    this._dcopy.Run(NRHS, BX, I+1 * LDBX + o_bx, LDBX, ref B, PERM[I + o_perm]+1 * LDB + o_b, LDB);
                }
                // *
                // *        Step (4R): apply back the Givens rotations performed.
                // *
                for (I = GIVPTR; I >= 1; I +=  - 1)
                {
                    this._drot.Run(NRHS, ref B, GIVCOL[I+2 * LDGCOL + o_givcol]+1 * LDB + o_b, LDB, ref B, GIVCOL[I+1 * LDGCOL + o_givcol]+1 * LDB + o_b, LDB, GIVNUM[I+2 * LDGNUM + o_givnum]
                                   ,  - GIVNUM[I+1 * LDGNUM + o_givnum]);
                }
            }
            // *
            return;
            // *
            // *     End of DLALS0
            // *

            #endregion

        }
    }
}
