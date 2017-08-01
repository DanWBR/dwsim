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
    /// DLASDT creates a tree of subproblems for bidiagonal divide and
    /// conquer.
    /// 
    ///</summary>
    public class DLASDT
    {
    

        #region Variables
        
        const double TWO = 2.0E+0; 

        #endregion

        public DLASDT()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASDT creates a tree of subproblems for bidiagonal divide and
        /// conquer.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// On entry, the number of diagonal elements of the
        /// bidiagonal matrix.
        ///</param>
        /// <param name="LVL">
        /// (output) INTEGER
        /// On exit, the number of levels on the computation tree.
        ///</param>
        /// <param name="ND">
        /// (output) INTEGER
        /// On exit, the number of nodes on the tree.
        ///</param>
        /// <param name="INODE">
        /// (output) INTEGER array, dimension ( N )
        /// On exit, centers of subproblems.
        ///</param>
        /// <param name="NDIML">
        /// (output) INTEGER array, dimension ( N )
        /// On exit, row dimensions of left children.
        ///</param>
        /// <param name="NDIMR">
        /// (output) INTEGER array, dimension ( N )
        /// On exit, row dimensions of right children.
        ///</param>
        /// <param name="MSUB">
        /// (input) INTEGER.
        /// On entry, the maximum row dimension each subproblem at the
        /// bottom of the tree can be of.
        ///</param>
        public void Run(int N, ref int LVL, ref int ND, ref int[] INODE, int offset_inode, ref int[] NDIML, int offset_ndiml, ref int[] NDIMR, int offset_ndimr
                         , int MSUB)
        {

            #region Variables
            
            int I = 0; int IL = 0; int IR = 0; int LLST = 0; int MAXN = 0; int NCRNT = 0; int NLVL = 0; double TEMP = 0; 

            #endregion


            #region Array Index Correction
            
             int o_inode = -1 + offset_inode;  int o_ndiml = -1 + offset_ndiml;  int o_ndimr = -1 + offset_ndimr; 

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
            // *  DLASDT creates a tree of subproblems for bidiagonal divide and
            // *  conquer.
            // *
            // *  Arguments
            // *  =========
            // *
            // *   N      (input) INTEGER
            // *          On entry, the number of diagonal elements of the
            // *          bidiagonal matrix.
            // *
            // *   LVL    (output) INTEGER
            // *          On exit, the number of levels on the computation tree.
            // *
            // *   ND     (output) INTEGER
            // *          On exit, the number of nodes on the tree.
            // *
            // *   INODE  (output) INTEGER array, dimension ( N )
            // *          On exit, centers of subproblems.
            // *
            // *   NDIML  (output) INTEGER array, dimension ( N )
            // *          On exit, row dimensions of left children.
            // *
            // *   NDIMR  (output) INTEGER array, dimension ( N )
            // *          On exit, row dimensions of right children.
            // *
            // *   MSUB   (input) INTEGER.
            // *          On entry, the maximum row dimension each subproblem at the
            // *          bottom of the tree can be of.
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
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          DBLE, INT, LOG, MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Find the number of levels on the tree.
            // *

            #endregion


            #region Body
            
            MAXN = Math.Max(1, N);
            TEMP = Math.Log(Convert.ToDouble(MAXN) / Convert.ToDouble(MSUB + 1)) / Math.Log(TWO);
            LVL = Convert.ToInt32(Math.Truncate(TEMP)) + 1;
            // *
            I = N / 2;
            INODE[1 + o_inode] = I + 1;
            NDIML[1 + o_ndiml] = I;
            NDIMR[1 + o_ndimr] = N - I - 1;
            IL = 0;
            IR = 1;
            LLST = 1;
            for (NLVL = 1; NLVL <= LVL - 1; NLVL++)
            {
                // *
                // *        Constructing the tree at (NLVL+1)-st level. The number of
                // *        nodes created on this level is LLST * 2.
                // *
                for (I = 0; I <= LLST - 1; I++)
                {
                    IL += 2;
                    IR += 2;
                    NCRNT = LLST + I;
                    NDIML[IL + o_ndiml] = NDIML[NCRNT + o_ndiml] / 2;
                    NDIMR[IL + o_ndimr] = NDIML[NCRNT + o_ndiml] - NDIML[IL + o_ndiml] - 1;
                    INODE[IL + o_inode] = INODE[NCRNT + o_inode] - NDIMR[IL + o_ndimr] - 1;
                    NDIML[IR + o_ndiml] = NDIMR[NCRNT + o_ndimr] / 2;
                    NDIMR[IR + o_ndimr] = NDIMR[NCRNT + o_ndimr] - NDIML[IR + o_ndiml] - 1;
                    INODE[IR + o_inode] = INODE[NCRNT + o_inode] + NDIML[IR + o_ndiml] + 1;
                }
                LLST *= 2;
            }
            ND = LLST * 2 - 1;
            // *
            return;
            // *
            // *     End of DLASDT
            // *

            #endregion

        }
    }
}
