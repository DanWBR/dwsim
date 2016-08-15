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
    /// DGTSV  solves the equation
    /// 
    /// A*X = B,
    /// 
    /// where A is an n by n tridiagonal matrix, by Gaussian elimination with
    /// partial pivoting.
    /// 
    /// Note that the equation  A'*X = B  may be solved by interchanging the
    /// order of the arguments DU and DL.
    /// 
    ///</summary>
    public class DGTSV
    {
    

        #region Dependencies
        
        XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DGTSV(XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._xerbla = xerbla; 

            #endregion

        }
    
        public DGTSV()
        {
    

            #region Dependencies (Initialization)
            
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGTSV  solves the equation
        /// 
        /// A*X = B,
        /// 
        /// where A is an n by n tridiagonal matrix, by Gaussian elimination with
        /// partial pivoting.
        /// 
        /// Note that the equation  A'*X = B  may be solved by interchanging the
        /// order of the arguments DU and DL.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS .GE. 0.
        ///</param>
        /// <param name="DL">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, DL must contain the (n-1) sub-diagonal elements of
        /// A.
        /// 
        /// On exit, DL is overwritten by the (n-2) elements of the
        /// second super-diagonal of the upper triangular matrix U from
        /// the LU factorization of A, in DL(1), ..., DL(n-2).
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, D must contain the diagonal elements of A.
        /// 
        /// On exit, D is overwritten by the n diagonal elements of U.
        ///</param>
        /// <param name="DU">
        /// (input/output) DOUBLE PRECISION array, dimension (N-1)
        /// On entry, DU must contain the (n-1) super-diagonal elements
        /// of A.
        /// 
        /// On exit, DU is overwritten by the (n-1) elements of the first
        /// super-diagonal of U.
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the N by NRHS matrix of right hand side matrix B.
        /// On exit, if INFO = 0, the N by NRHS solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0: if INFO = i, U(i,i) is exactly zero, and the solution
        /// has not been computed.  The factorization has not been
        /// completed unless i = N.
        ///</param>
        public void Run(int N, int NRHS, ref double[] DL, int offset_dl, ref double[] D, int offset_d, ref double[] DU, int offset_du, ref double[] B, int offset_b
                         , int LDB, ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; double FACT = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int B_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_dl = -1 + offset_dl;  int o_d = -1 + offset_d;  int o_du = -1 + offset_du;  int o_b = -1 - LDB + offset_b; 

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
            // *  DGTSV  solves the equation
            // *
            // *     A*X = B,
            // *
            // *  where A is an n by n tridiagonal matrix, by Gaussian elimination with
            // *  partial pivoting.
            // *
            // *  Note that the equation  A'*X = B  may be solved by interchanging the
            // *  order of the arguments DU and DL.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrix B.  NRHS >= 0.
            // *
            // *  DL      (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, DL must contain the (n-1) sub-diagonal elements of
            // *          A.
            // *
            // *          On exit, DL is overwritten by the (n-2) elements of the
            // *          second super-diagonal of the upper triangular matrix U from
            // *          the LU factorization of A, in DL(1), ..., DL(n-2).
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, D must contain the diagonal elements of A.
            // *
            // *          On exit, D is overwritten by the n diagonal elements of U.
            // *
            // *  DU      (input/output) DOUBLE PRECISION array, dimension (N-1)
            // *          On entry, DU must contain the (n-1) super-diagonal elements
            // *          of A.
            // *
            // *          On exit, DU is overwritten by the (n-1) elements of the first
            // *          super-diagonal of U.
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the N by NRHS matrix of right hand side matrix B.
            // *          On exit, if INFO = 0, the N by NRHS solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *          > 0: if INFO = i, U(i,i) is exactly zero, and the solution
            // *               has not been computed.  The factorization has not been
            // *               completed unless i = N.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX;
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (NRHS < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDB < Math.Max(1, N))
                    {
                        INFO =  - 7;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGTSV ",  - INFO);
                return;
            }
            // *
            if (N == 0) return;
            // *
            if (NRHS == 1)
            {
                for (I = 1; I <= N - 2; I++)
                {
                    if (Math.Abs(D[I + o_d]) >= Math.Abs(DL[I + o_dl]))
                    {
                        // *
                        // *              No row interchange required
                        // *
                        if (D[I + o_d] != ZERO)
                        {
                            FACT = DL[I + o_dl] / D[I + o_d];
                            D[I + 1 + o_d] +=  - FACT * DU[I + o_du];
                            B[I + 1+1 * LDB + o_b] +=  - FACT * B[I+1 * LDB + o_b];
                        }
                        else
                        {
                            INFO = I;
                            return;
                        }
                        DL[I + o_dl] = ZERO;
                    }
                    else
                    {
                        // *
                        // *              Interchange rows I and I+1
                        // *
                        FACT = D[I + o_d] / DL[I + o_dl];
                        D[I + o_d] = DL[I + o_dl];
                        TEMP = D[I + 1 + o_d];
                        D[I + 1 + o_d] = DU[I + o_du] - FACT * TEMP;
                        DL[I + o_dl] = DU[I + 1 + o_du];
                        DU[I + 1 + o_du] =  - FACT * DL[I + o_dl];
                        DU[I + o_du] = TEMP;
                        TEMP = B[I+1 * LDB + o_b];
                        B[I+1 * LDB + o_b] = B[I + 1+1 * LDB + o_b];
                        B[I + 1+1 * LDB + o_b] = TEMP - FACT * B[I + 1+1 * LDB + o_b];
                    }
                }
                if (N > 1)
                {
                    I = N - 1;
                    if (Math.Abs(D[I + o_d]) >= Math.Abs(DL[I + o_dl]))
                    {
                        if (D[I + o_d] != ZERO)
                        {
                            FACT = DL[I + o_dl] / D[I + o_d];
                            D[I + 1 + o_d] +=  - FACT * DU[I + o_du];
                            B[I + 1+1 * LDB + o_b] +=  - FACT * B[I+1 * LDB + o_b];
                        }
                        else
                        {
                            INFO = I;
                            return;
                        }
                    }
                    else
                    {
                        FACT = D[I + o_d] / DL[I + o_dl];
                        D[I + o_d] = DL[I + o_dl];
                        TEMP = D[I + 1 + o_d];
                        D[I + 1 + o_d] = DU[I + o_du] - FACT * TEMP;
                        DU[I + o_du] = TEMP;
                        TEMP = B[I+1 * LDB + o_b];
                        B[I+1 * LDB + o_b] = B[I + 1+1 * LDB + o_b];
                        B[I + 1+1 * LDB + o_b] = TEMP - FACT * B[I + 1+1 * LDB + o_b];
                    }
                }
                if (D[N + o_d] == ZERO)
                {
                    INFO = N;
                    return;
                }
            }
            else
            {
                for (I = 1; I <= N - 2; I++)
                {
                    if (Math.Abs(D[I + o_d]) >= Math.Abs(DL[I + o_dl]))
                    {
                        // *
                        // *              No row interchange required
                        // *
                        if (D[I + o_d] != ZERO)
                        {
                            FACT = DL[I + o_dl] / D[I + o_d];
                            D[I + 1 + o_d] +=  - FACT * DU[I + o_du];
                            for (J = 1; J <= NRHS; J++)
                            {
                                B[I + 1+J * LDB + o_b] +=  - FACT * B[I+J * LDB + o_b];
                            }
                        }
                        else
                        {
                            INFO = I;
                            return;
                        }
                        DL[I + o_dl] = ZERO;
                    }
                    else
                    {
                        // *
                        // *              Interchange rows I and I+1
                        // *
                        FACT = D[I + o_d] / DL[I + o_dl];
                        D[I + o_d] = DL[I + o_dl];
                        TEMP = D[I + 1 + o_d];
                        D[I + 1 + o_d] = DU[I + o_du] - FACT * TEMP;
                        DL[I + o_dl] = DU[I + 1 + o_du];
                        DU[I + 1 + o_du] =  - FACT * DL[I + o_dl];
                        DU[I + o_du] = TEMP;
                        for (J = 1; J <= NRHS; J++)
                        {
                            TEMP = B[I+J * LDB + o_b];
                            B[I+J * LDB + o_b] = B[I + 1+J * LDB + o_b];
                            B[I + 1+J * LDB + o_b] = TEMP - FACT * B[I + 1+J * LDB + o_b];
                        }
                    }
                }
                if (N > 1)
                {
                    I = N - 1;
                    if (Math.Abs(D[I + o_d]) >= Math.Abs(DL[I + o_dl]))
                    {
                        if (D[I + o_d] != ZERO)
                        {
                            FACT = DL[I + o_dl] / D[I + o_d];
                            D[I + 1 + o_d] +=  - FACT * DU[I + o_du];
                            for (J = 1; J <= NRHS; J++)
                            {
                                B[I + 1+J * LDB + o_b] +=  - FACT * B[I+J * LDB + o_b];
                            }
                        }
                        else
                        {
                            INFO = I;
                            return;
                        }
                    }
                    else
                    {
                        FACT = D[I + o_d] / DL[I + o_dl];
                        D[I + o_d] = DL[I + o_dl];
                        TEMP = D[I + 1 + o_d];
                        D[I + 1 + o_d] = DU[I + o_du] - FACT * TEMP;
                        DU[I + o_du] = TEMP;
                        for (J = 1; J <= NRHS; J++)
                        {
                            TEMP = B[I+J * LDB + o_b];
                            B[I+J * LDB + o_b] = B[I + 1+J * LDB + o_b];
                            B[I + 1+J * LDB + o_b] = TEMP - FACT * B[I + 1+J * LDB + o_b];
                        }
                    }
                }
                if (D[N + o_d] == ZERO)
                {
                    INFO = N;
                    return;
                }
            }
            // *
            // *     Back solve with the matrix U from the factorization.
            // *
            if (NRHS <= 2)
            {
                J = 1;
            LABEL70:;
                B[N+J * LDB + o_b] /= D[N + o_d];
                if (N > 1) B[N - 1+J * LDB + o_b] = (B[N - 1+J * LDB + o_b] - DU[N - 1 + o_du] * B[N+J * LDB + o_b]) / D[N - 1 + o_d];
                B_J = J * LDB + o_b;
                for (I = N - 2; I >= 1; I +=  - 1)
                {
                    B[I + B_J] = (B[I + B_J] - DU[I + o_du] * B[I + 1 + B_J] - DL[I + o_dl] * B[I + 2 + B_J]) / D[I + o_d];
                }
                if (J < NRHS)
                {
                    J += 1;
                    goto LABEL70;
                }
            }
            else
            {
                for (J = 1; J <= NRHS; J++)
                {
                    B[N+J * LDB + o_b] /= D[N + o_d];
                    if (N > 1) B[N - 1+J * LDB + o_b] = (B[N - 1+J * LDB + o_b] - DU[N - 1 + o_du] * B[N+J * LDB + o_b]) / D[N - 1 + o_d];
                    B_J = J * LDB + o_b;
                    for (I = N - 2; I >= 1; I +=  - 1)
                    {
                        B[I + B_J] = (B[I + B_J] - DU[I + o_du] * B[I + 1 + B_J] - DL[I + o_dl] * B[I + 2 + B_J]) / D[I + o_d];
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DGTSV
            // *

            #endregion

        }
    }
}
