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
    /// DLASWP performs a series of row interchanges on the matrix A.
    /// One row interchange is initiated for each of rows K1 through K2 of A.
    /// 
    ///</summary>
    public class DLASWP
    {
    
        public DLASWP()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASWP performs a series of row interchanges on the matrix A.
        /// One row interchange is initiated for each of rows K1 through K2 of A.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the matrix of column dimension N to which the row
        /// interchanges will be applied.
        /// On exit, the permuted matrix.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.
        ///</param>
        /// <param name="K1">
        /// (input) INTEGER
        /// The first element of IPIV for which a row interchange will
        /// be done.
        ///</param>
        /// <param name="K2">
        /// (input) INTEGER
        /// The last element of IPIV for which a row interchange will
        /// be done.
        ///</param>
        /// <param name="IPIV">
        /// (input) INTEGER array, dimension (K2*abs(INCX))
        /// The vector of pivot indices.  Only the elements in positions
        /// K1 through K2 of IPIV are accessed.
        /// IPIV(K) = L implies rows K and L are to be interchanged.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between successive values of IPIV.  If IPIV
        /// is negative, the pivots are applied in reverse order.
        ///</param>
        public void Run(int N, ref double[] A, int offset_a, int LDA, int K1, int K2, int[] IPIV, int offset_ipiv
                         , int INCX)
        {

            #region Variables
            
            int I = 0; int I1 = 0; int I2 = 0; int INC = 0; int IP = 0; int IX = 0; int IX0 = 0; int J = 0; int K = 0; 
            int N32 = 0;double TEMP = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipiv = -1 + offset_ipiv; 

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
            // *  DLASWP performs a series of row interchanges on the matrix A.
            // *  One row interchange is initiated for each of rows K1 through K2 of A.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the matrix of column dimension N to which the row
            // *          interchanges will be applied.
            // *          On exit, the permuted matrix.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.
            // *
            // *  K1      (input) INTEGER
            // *          The first element of IPIV for which a row interchange will
            // *          be done.
            // *
            // *  K2      (input) INTEGER
            // *          The last element of IPIV for which a row interchange will
            // *          be done.
            // *
            // *  IPIV    (input) INTEGER array, dimension (K2*abs(INCX))
            // *          The vector of pivot indices.  Only the elements in positions
            // *          K1 through K2 of IPIV are accessed.
            // *          IPIV(K) = L implies rows K and L are to be interchanged.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between successive values of IPIV.  If IPIV
            // *          is negative, the pivots are applied in reverse order.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Modified by
            // *   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // * =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Interchange row I with row IPIV(I) for each of rows K1 through K2.
            // *

            #endregion


            #region Body
            
            if (INCX > 0)
            {
                IX0 = K1;
                I1 = K1;
                I2 = K2;
                INC = 1;
            }
            else
            {
                if (INCX < 0)
                {
                    IX0 = 1 + (1 - K2) * INCX;
                    I1 = K2;
                    I2 = K1;
                    INC =  - 1;
                }
                else
                {
                    return;
                }
            }
            // *
            N32 = (N / 32) * 32;
            if (N32 != 0)
            {
                for (J = 1; J <= N32; J += 32)
                {
                    IX = IX0;
                    for (I = I1; (INC >= 0) ? (I <= I2) : (I >= I2); I += INC)
                    {
                        IP = IPIV[IX + o_ipiv];
                        if (IP != I)
                        {
                            for (K = J; K <= J + 31; K++)
                            {
                                TEMP = A[I+K * LDA + o_a];
                                A[I+K * LDA + o_a] = A[IP+K * LDA + o_a];
                                A[IP+K * LDA + o_a] = TEMP;
                            }
                        }
                        IX += INCX;
                    }
                }
            }
            if (N32 != N)
            {
                N32 += 1;
                IX = IX0;
                for (I = I1; (INC >= 0) ? (I <= I2) : (I >= I2); I += INC)
                {
                    IP = IPIV[IX + o_ipiv];
                    if (IP != I)
                    {
                        for (K = N32; K <= N; K++)
                        {
                            TEMP = A[I+K * LDA + o_a];
                            A[I+K * LDA + o_a] = A[IP+K * LDA + o_a];
                            A[IP+K * LDA + o_a] = TEMP;
                        }
                    }
                    IX += INCX;
                }
            }
            // *
            return;
            // *
            // *     End of DLASWP
            // *

            #endregion

        }
    }
}
