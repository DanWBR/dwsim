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
    /// Sort the numbers in D in increasing order (if ID = 'I') or
    /// in decreasing order (if ID = 'D' ).
    /// 
    /// Use Quick Sort, reverting to Insertion sort on arrays of
    /// size .LE. 20. Dimension of STACK limits N to about 2**32.
    /// 
    ///</summary>
    public class DLASRT
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const int SELECT = 20; int[] STACK = new int[2 * 32]; 

        #endregion

        public DLASRT(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASRT()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// Sort the numbers in D in increasing order (if ID = 'I') or
        /// in decreasing order (if ID = 'D' ).
        /// 
        /// Use Quick Sort, reverting to Insertion sort on arrays of
        /// size .LE. 20. Dimension of STACK limits N to about 2**32.
        /// 
        ///</summary>
        /// <param name="ID">
        /// (input) CHARACTER*1
        /// = 'I': sort D in increasing order;
        /// = 'D': sort D in decreasing order.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The length of the array D.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the array to be sorted.
        /// On exit, D has been sorted into increasing order
        /// (D(1) .LE. ... .LE. D(N) ) or into decreasing order
        /// (D(1) .GE. ... .GE. D(N) ), depending on ID.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string ID, int N, ref double[] D, int offset_d, ref int INFO)
        {

            #region Variables
            
            int DIR = 0; int ENDD = 0; int I = 0; int J = 0; int START = 0; int STKPNT = 0; double D1 = 0; double D2 = 0; 
            double D3 = 0;double DMNMX = 0; double TMP = 0; int o_stack = -3; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d; 

            #endregion


            #region Strings
            
            ID = ID.Substring(0, 1);  

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
            // *  Sort the numbers in D in increasing order (if ID = 'I') or
            // *  in decreasing order (if ID = 'D' ).
            // *
            // *  Use Quick Sort, reverting to Insertion sort on arrays of
            // *  size <= 20. Dimension of STACK limits N to about 2**32.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ID      (input) CHARACTER*1
            // *          = 'I': sort D in increasing order;
            // *          = 'D': sort D in decreasing order.
            // *
            // *  N       (input) INTEGER
            // *          The length of the array D.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the array to be sorted.
            // *          On exit, D has been sorted into increasing order
            // *          (D(1) <= ... <= D(N) ) or into decreasing order
            // *          (D(1) >= ... >= D(N) ), depending on ID.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input paramters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            DIR =  - 1;
            if (this._lsame.Run(ID, "D"))
            {
                DIR = 0;
            }
            else
            {
                if (this._lsame.Run(ID, "I"))
                {
                    DIR = 1;
                }
            }
            if (DIR ==  - 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASRT",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N <= 1) return;
            // *
            STKPNT = 1;
            STACK[1+1 * 2 + o_stack] = 1;
            STACK[2+1 * 2 + o_stack] = N;
        LABEL10:;
            START = STACK[1+STKPNT * 2 + o_stack];
            ENDD = STACK[2+STKPNT * 2 + o_stack];
            STKPNT -= 1;
            if (ENDD - START <= SELECT && ENDD - START > 0)
            {
                // *
                // *        Do Insertion sort on D( START:ENDD )
                // *
                if (DIR == 0)
                {
                    // *
                    // *           Sort into decreasing order
                    // *
                    for (I = START + 1; I <= ENDD; I++)
                    {
                        for (J = I; J >= START + 1; J +=  - 1)
                        {
                            if (D[J + o_d] > D[J - 1 + o_d])
                            {
                                DMNMX = D[J + o_d];
                                D[J + o_d] = D[J - 1 + o_d];
                                D[J - 1 + o_d] = DMNMX;
                            }
                            else
                            {
                                goto LABEL30;
                            }
                        }
                    LABEL30:;
                    }
                    // *
                }
                else
                {
                    // *
                    // *           Sort into increasing order
                    // *
                    for (I = START + 1; I <= ENDD; I++)
                    {
                        for (J = I; J >= START + 1; J +=  - 1)
                        {
                            if (D[J + o_d] < D[J - 1 + o_d])
                            {
                                DMNMX = D[J + o_d];
                                D[J + o_d] = D[J - 1 + o_d];
                                D[J - 1 + o_d] = DMNMX;
                            }
                            else
                            {
                                goto LABEL50;
                            }
                        }
                    LABEL50:;
                    }
                    // *
                }
                // *
            }
            else
            {
                if (ENDD - START > SELECT)
                {
                    // *
                    // *        Partition D( START:ENDD ) and stack parts, largest one first
                    // *
                    // *        Choose partition entry as median of 3
                    // *
                    D1 = D[START + o_d];
                    D2 = D[ENDD + o_d];
                    I = (START + ENDD) / 2;
                    D3 = D[I + o_d];
                    if (D1 < D2)
                    {
                        if (D3 < D1)
                        {
                            DMNMX = D1;
                        }
                        else
                        {
                            if (D3 < D2)
                            {
                                DMNMX = D3;
                            }
                            else
                            {
                                DMNMX = D2;
                            }
                        }
                    }
                    else
                    {
                        if (D3 < D2)
                        {
                            DMNMX = D2;
                        }
                        else
                        {
                            if (D3 < D1)
                            {
                                DMNMX = D3;
                            }
                            else
                            {
                                DMNMX = D1;
                            }
                        }
                    }
                    // *
                    if (DIR == 0)
                    {
                        // *
                        // *           Sort into decreasing order
                        // *
                        I = START - 1;
                        J = ENDD + 1;
                    LABEL60:;
                    LABEL70:;
                        J -= 1;
                        if (D[J + o_d] < DMNMX) goto LABEL70;
                    LABEL80:;
                        I += 1;
                        if (D[I + o_d] > DMNMX) goto LABEL80;
                        if (I < J)
                        {
                            TMP = D[I + o_d];
                            D[I + o_d] = D[J + o_d];
                            D[J + o_d] = TMP;
                            goto LABEL60;
                        }
                        if (J - START > ENDD - J - 1)
                        {
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = START;
                            STACK[2+STKPNT * 2 + o_stack] = J;
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = J + 1;
                            STACK[2+STKPNT * 2 + o_stack] = ENDD;
                        }
                        else
                        {
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = J + 1;
                            STACK[2+STKPNT * 2 + o_stack] = ENDD;
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = START;
                            STACK[2+STKPNT * 2 + o_stack] = J;
                        }
                    }
                    else
                    {
                        // *
                        // *           Sort into increasing order
                        // *
                        I = START - 1;
                        J = ENDD + 1;
                    LABEL90:;
                    LABEL100:;
                        J -= 1;
                        if (D[J + o_d] > DMNMX) goto LABEL100;
                    LABEL110:;
                        I += 1;
                        if (D[I + o_d] < DMNMX) goto LABEL110;
                        if (I < J)
                        {
                            TMP = D[I + o_d];
                            D[I + o_d] = D[J + o_d];
                            D[J + o_d] = TMP;
                            goto LABEL90;
                        }
                        if (J - START > ENDD - J - 1)
                        {
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = START;
                            STACK[2+STKPNT * 2 + o_stack] = J;
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = J + 1;
                            STACK[2+STKPNT * 2 + o_stack] = ENDD;
                        }
                        else
                        {
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = J + 1;
                            STACK[2+STKPNT * 2 + o_stack] = ENDD;
                            STKPNT += 1;
                            STACK[1+STKPNT * 2 + o_stack] = START;
                            STACK[2+STKPNT * 2 + o_stack] = J;
                        }
                    }
                }
            }
            if (STKPNT > 0) goto LABEL10;
            return;
            // *
            // *     End of DLASRT
            // *

            #endregion

        }
    }
}
