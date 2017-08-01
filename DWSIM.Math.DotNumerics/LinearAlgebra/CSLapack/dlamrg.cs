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
    /// DLAMRG will create a permutation list which will merge the elements
    /// of A (which is composed of two independently sorted sets) into a
    /// single set which is sorted in ascending order.
    /// 
    ///</summary>
    public class DLAMRG
    {
    
        public DLAMRG()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMRG will create a permutation list which will merge the elements
        /// of A (which is composed of two independently sorted sets) into a
        /// single set which is sorted in ascending order.
        /// 
        ///</summary>
        /// <param name="N1">
        /// (input) INTEGER
        ///</param>
        /// <param name="N2">
        /// (input) INTEGER
        /// These arguements contain the respective lengths of the two
        /// sorted lists to be merged.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension (N1+N2)
        /// The first N1 elements of A contain a list of numbers which
        /// are sorted in either ascending or descending order.  Likewise
        /// for the final N2 elements.
        ///</param>
        /// <param name="DTRD1">
        /// (input) INTEGER
        ///</param>
        /// <param name="DTRD2">
        /// (input) INTEGER
        /// These are the strides to be taken through the array A.
        /// Allowable strides are 1 and -1.  They indicate whether a
        /// subset of A is sorted in ascending (DTRDx = 1) or descending
        /// (DTRDx = -1) order.
        ///</param>
        /// <param name="INDEX">
        /// (output) INTEGER array, dimension (N1+N2)
        /// On exit this array will contain a permutation such that
        /// if B( I ) = A( INDEX( I ) ) for I=1,N1+N2, then B will be
        /// sorted in ascending order.
        ///</param>
        public void Run(int N1, int N2, double[] A, int offset_a, int DTRD1, int DTRD2, ref int[] INDEX, int offset_index)
        {

            #region Variables
            
            int I = 0; int IND1 = 0; int IND2 = 0; int N1SV = 0; int N2SV = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 + offset_a;  int o_index = -1 + offset_index; 

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
            // *  DLAMRG will create a permutation list which will merge the elements
            // *  of A (which is composed of two independently sorted sets) into a
            // *  single set which is sorted in ascending order.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N1     (input) INTEGER
            // *  N2     (input) INTEGER
            // *         These arguements contain the respective lengths of the two
            // *         sorted lists to be merged.
            // *
            // *  A      (input) DOUBLE PRECISION array, dimension (N1+N2)
            // *         The first N1 elements of A contain a list of numbers which
            // *         are sorted in either ascending or descending order.  Likewise
            // *         for the final N2 elements.
            // *
            // *  DTRD1  (input) INTEGER
            // *  DTRD2  (input) INTEGER
            // *         These are the strides to be taken through the array A.
            // *         Allowable strides are 1 and -1.  They indicate whether a
            // *         subset of A is sorted in ascending (DTRDx = 1) or descending
            // *         (DTRDx = -1) order.
            // *
            // *  INDEX  (output) INTEGER array, dimension (N1+N2)
            // *         On exit this array will contain a permutation such that
            // *         if B( I ) = A( INDEX( I ) ) for I=1,N1+N2, then B will be
            // *         sorted in ascending order.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            N1SV = N1;
            N2SV = N2;
            if (DTRD1 > 0)
            {
                IND1 = 1;
            }
            else
            {
                IND1 = N1;
            }
            if (DTRD2 > 0)
            {
                IND2 = 1 + N1;
            }
            else
            {
                IND2 = N1 + N2;
            }
            I = 1;
            // *     while ( (N1SV > 0) & (N2SV > 0) )
        LABEL10:;
            if (N1SV > 0 && N2SV > 0)
            {
                if (A[IND1 + o_a] <= A[IND2 + o_a])
                {
                    INDEX[I + o_index] = IND1;
                    I += 1;
                    IND1 += DTRD1;
                    N1SV -= 1;
                }
                else
                {
                    INDEX[I + o_index] = IND2;
                    I += 1;
                    IND2 += DTRD2;
                    N2SV -= 1;
                }
                goto LABEL10;
            }
            // *     end while
            if (N1SV == 0)
            {
                for (N1SV = 1; N1SV <= N2SV; N1SV++)
                {
                    INDEX[I + o_index] = IND2;
                    I += 1;
                    IND2 += DTRD2;
                }
            }
            else
            {
                // *     N2SV .EQ. 0
                for (N2SV = 1; N2SV <= N1SV; N2SV++)
                {
                    INDEX[I + o_index] = IND1;
                    I += 1;
                    IND1 += DTRD1;
                }
            }
            // *
            return;
            // *
            // *     End of DLAMRG
            // *

            #endregion

        }
    }
}
