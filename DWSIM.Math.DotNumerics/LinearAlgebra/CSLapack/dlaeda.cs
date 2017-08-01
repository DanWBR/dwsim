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
    /// DLAEDA computes the Z vector corresponding to the merge step in the
    /// CURLVLth step of the merge process with TLVLS steps for the CURPBMth
    /// problem.
    /// 
    ///</summary>
    public class DLAEDA
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DGEMV _dgemv; DROT _drot; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double HALF = 0.5E0; const double ONE = 1.0E0; 

        #endregion

        public DLAEDA(DCOPY dcopy, DGEMV dgemv, DROT drot, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAEDA()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DROT drot = new DROT();
            DGEMV dgemv = new DGEMV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._drot = drot; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAEDA computes the Z vector corresponding to the merge step in the
        /// CURLVLth step of the merge process with TLVLS steps for the CURPBMth
        /// problem.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the symmetric tridiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="TLVLS">
        /// (input) INTEGER
        /// The total number of merging levels in the overall divide and
        /// conquer tree.
        ///</param>
        /// <param name="CURLVL">
        /// (input) INTEGER
        /// The current level in the overall merge routine,
        /// 0 .LE. curlvl .LE. tlvls.
        ///</param>
        /// <param name="CURPBM">
        /// (input) INTEGER
        /// The current problem in the current level in the overall
        /// merge routine (counting from upper left to lower right).
        ///</param>
        /// <param name="PRMPTR">
        /// (input) INTEGER array, dimension (N lg N)
        /// Contains a list of pointers which indicate where in PERM a
        /// level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
        /// indicates the size of the permutation and incidentally the
        /// size of the full, non-deflated problem.
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
        /// <param name="Q">
        /// (input) DOUBLE PRECISION array, dimension (N**2)
        /// Contains the square eigenblocks from previous levels, the
        /// starting positions for blocks are given by QPTR.
        ///</param>
        /// <param name="QPTR">
        /// (input) INTEGER array, dimension (N+2)
        /// Contains a list of pointers which indicate where in Q an
        /// eigenblock is stored.  SQRT( QPTR(i+1) - QPTR(i) ) indicates
        /// the size of the block.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// On output this vector contains the updating vector (the last
        /// row of the first sub-eigenvector matrix and the first row of
        /// the second sub-eigenvector matrix).
        ///</param>
        /// <param name="ZTEMP">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int N, int TLVLS, int CURLVL, int CURPBM, int[] PRMPTR, int offset_prmptr, int[] PERM, int offset_perm
                         , int[] GIVPTR, int offset_givptr, int[] GIVCOL, int offset_givcol, double[] GIVNUM, int offset_givnum, double[] Q, int offset_q, int[] QPTR, int offset_qptr, ref double[] Z, int offset_z
                         , ref double[] ZTEMP, int offset_ztemp, ref int INFO)
        {

            #region Variables
            
            int BSIZ1 = 0; int BSIZ2 = 0; int CURR = 0; int I = 0; int K = 0; int MID = 0; int PSIZ1 = 0; int PSIZ2 = 0; 
            int PTR = 0;int ZPTR1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_prmptr = -1 + offset_prmptr;  int o_perm = -1 + offset_perm;  int o_givptr = -1 + offset_givptr; 
             int o_givcol = -3 + offset_givcol; int o_givnum = -3 + offset_givnum;  int o_q = -1 + offset_q; 
             int o_qptr = -1 + offset_qptr; int o_z = -1 + offset_z;  int o_ztemp = -1 + offset_ztemp; 

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
            // *  DLAEDA computes the Z vector corresponding to the merge step in the
            // *  CURLVLth step of the merge process with TLVLS steps for the CURPBMth
            // *  problem.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N      (input) INTEGER
            // *         The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  TLVLS  (input) INTEGER
            // *         The total number of merging levels in the overall divide and
            // *         conquer tree.
            // *
            // *  CURLVL (input) INTEGER
            // *         The current level in the overall merge routine,
            // *         0 <= curlvl <= tlvls.
            // *
            // *  CURPBM (input) INTEGER
            // *         The current problem in the current level in the overall
            // *         merge routine (counting from upper left to lower right).
            // *
            // *  PRMPTR (input) INTEGER array, dimension (N lg N)
            // *         Contains a list of pointers which indicate where in PERM a
            // *         level's permutation is stored.  PRMPTR(i+1) - PRMPTR(i)
            // *         indicates the size of the permutation and incidentally the
            // *         size of the full, non-deflated problem.
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
            // *  Q      (input) DOUBLE PRECISION array, dimension (N**2)
            // *         Contains the square eigenblocks from previous levels, the
            // *         starting positions for blocks are given by QPTR.
            // *
            // *  QPTR   (input) INTEGER array, dimension (N+2)
            // *         Contains a list of pointers which indicate where in Q an
            // *         eigenblock is stored.  SQRT( QPTR(i+1) - QPTR(i) ) indicates
            // *         the size of the block.
            // *
            // *  Z      (output) DOUBLE PRECISION array, dimension (N)
            // *         On output this vector contains the updating vector (the last
            // *         row of the first sub-eigenvector matrix and the first row of
            // *         the second sub-eigenvector matrix).
            // *
            // *  ZTEMP  (workspace) DOUBLE PRECISION array, dimension (N)
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
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          DBLE, INT, SQRT;
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
                INFO =  - 1;
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAEDA",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Determine location of first number in second half.
            // *
            MID = N / 2 + 1;
            // *
            // *     Gather last/first rows of appropriate eigenblocks into center of Z
            // *
            PTR = 1;
            // *
            // *     Determine location of lowest level subproblem in the full storage
            // *     scheme
            // *
            CURR = (int)(PTR + CURPBM * Math.Pow(2,CURLVL) + Math.Pow(2,CURLVL - 1) - 1);
            // *
            // *     Determine size of these matrices.  We add HALF to the value of
            // *     the SQRT in case the machine underestimates one of these square
            // *     roots.
            // *
            BSIZ1 = Convert.ToInt32(Math.Truncate(HALF + Math.Sqrt(Convert.ToDouble(QPTR[CURR + 1 + o_qptr] - QPTR[CURR + o_qptr]))));
            BSIZ2 = Convert.ToInt32(Math.Truncate(HALF + Math.Sqrt(Convert.ToDouble(QPTR[CURR + 2 + o_qptr] - QPTR[CURR + 1 + o_qptr]))));
            for (K = 1; K <= MID - BSIZ1 - 1; K++)
            {
                Z[K + o_z] = ZERO;
            }
            this._dcopy.Run(BSIZ1, Q, QPTR[CURR + o_qptr] + BSIZ1 - 1 + o_q, BSIZ1, ref Z, MID - BSIZ1 + o_z, 1);
            this._dcopy.Run(BSIZ2, Q, QPTR[CURR + 1 + o_qptr] + o_q, BSIZ2, ref Z, MID + o_z, 1);
            for (K = MID + BSIZ2; K <= N; K++)
            {
                Z[K + o_z] = ZERO;
            }
            // *
            // *     Loop thru remaining levels 1 -> CURLVL applying the Givens
            // *     rotations and permutation and then multiplying the center matrices
            // *     against the current Z.
            // *
            PTR = (int)Math.Pow(2, TLVLS) + 1;
            for (K = 1; K <= CURLVL - 1; K++)
            {
                CURR = (int)(PTR + CURPBM * Math.Pow(2, CURLVL - K) + Math.Pow(2, CURLVL - K - 1) - 1);
                PSIZ1 = PRMPTR[CURR + 1 + o_prmptr] - PRMPTR[CURR + o_prmptr];
                PSIZ2 = PRMPTR[CURR + 2 + o_prmptr] - PRMPTR[CURR + 1 + o_prmptr];
                ZPTR1 = MID - PSIZ1;
                // *
                // *       Apply Givens at CURR and CURR+1
                // *
                for (I = GIVPTR[CURR + o_givptr]; I <= GIVPTR[CURR + 1 + o_givptr] - 1; I++)
                {
                    this._drot.Run(1, ref Z, ZPTR1 + GIVCOL[1+I * 2 + o_givcol] - 1 + o_z, 1, ref Z, ZPTR1 + GIVCOL[2+I * 2 + o_givcol] - 1 + o_z, 1, GIVNUM[1+I * 2 + o_givnum]
                                   , GIVNUM[2+I * 2 + o_givnum]);
                }
                for (I = GIVPTR[CURR + 1 + o_givptr]; I <= GIVPTR[CURR + 2 + o_givptr] - 1; I++)
                {
                    this._drot.Run(1, ref Z, MID - 1 + GIVCOL[1+I * 2 + o_givcol] + o_z, 1, ref Z, MID - 1 + GIVCOL[2+I * 2 + o_givcol] + o_z, 1, GIVNUM[1+I * 2 + o_givnum]
                                   , GIVNUM[2+I * 2 + o_givnum]);
                }
                PSIZ1 = PRMPTR[CURR + 1 + o_prmptr] - PRMPTR[CURR + o_prmptr];
                PSIZ2 = PRMPTR[CURR + 2 + o_prmptr] - PRMPTR[CURR + 1 + o_prmptr];
                for (I = 0; I <= PSIZ1 - 1; I++)
                {
                    ZTEMP[I + 1 + o_ztemp] = Z[ZPTR1 + PERM[PRMPTR[CURR + o_prmptr] + I + o_perm] - 1 + o_z];
                }
                for (I = 0; I <= PSIZ2 - 1; I++)
                {
                    ZTEMP[PSIZ1 + I + 1 + o_ztemp] = Z[MID + PERM[PRMPTR[CURR + 1 + o_prmptr] + I + o_perm] - 1 + o_z];
                }
                // *
                // *        Multiply Blocks at CURR and CURR+1
                // *
                // *        Determine size of these matrices.  We add HALF to the value of
                // *        the SQRT in case the machine underestimates one of these
                // *        square roots.
                // *
                BSIZ1 = Convert.ToInt32(Math.Truncate(HALF + Math.Sqrt(Convert.ToDouble(QPTR[CURR + 1 + o_qptr] - QPTR[CURR + o_qptr]))));
                BSIZ2 = Convert.ToInt32(Math.Truncate(HALF + Math.Sqrt(Convert.ToDouble(QPTR[CURR + 2 + o_qptr] - QPTR[CURR + 1 + o_qptr]))));
                if (BSIZ1 > 0)
                {
                    this._dgemv.Run("T", BSIZ1, BSIZ1, ONE, Q, QPTR[CURR + o_qptr] + o_q, BSIZ1
                                    , ZTEMP, 1 + o_ztemp, 1, ZERO, ref Z, ZPTR1 + o_z, 1);
                }
                this._dcopy.Run(PSIZ1 - BSIZ1, ZTEMP, BSIZ1 + 1 + o_ztemp, 1, ref Z, ZPTR1 + BSIZ1 + o_z, 1);
                if (BSIZ2 > 0)
                {
                    this._dgemv.Run("T", BSIZ2, BSIZ2, ONE, Q, QPTR[CURR + 1 + o_qptr] + o_q, BSIZ2
                                    , ZTEMP, PSIZ1 + 1 + o_ztemp, 1, ZERO, ref Z, MID + o_z, 1);
                }
                this._dcopy.Run(PSIZ2 - BSIZ2, ZTEMP, PSIZ1 + BSIZ2 + 1 + o_ztemp, 1, ref Z, MID + BSIZ2 + o_z, 1);
                // *
                PTR += (int)Math.Pow(2, TLVLS - K);
            }
            // *
            return;
            // *
            // *     End of DLAEDA
            // *

            #endregion

        }
    }
}
