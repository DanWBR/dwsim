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
    /// DLASCL multiplies the M by N real matrix A by the real scalar
    /// CTO/CFROM.  This is done without over/underflow as long as the final
    /// result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
    /// A may be full, upper triangular, lower triangular, upper Hessenberg,
    /// or banded.
    /// 
    ///</summary>
    public class DLASCL
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DLASCL(LSAME lsame, DLAMCH dlamch, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASCL()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASCL multiplies the M by N real matrix A by the real scalar
        /// CTO/CFROM.  This is done without over/underflow as long as the final
        /// result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
        /// A may be full, upper triangular, lower triangular, upper Hessenberg,
        /// or banded.
        /// 
        ///</summary>
        /// <param name="TYPE">
        /// (input) CHARACTER*1
        /// TYPE indices the storage type of the input matrix.
        /// = 'G':  A is a full matrix.
        /// = 'L':  A is a lower triangular matrix.
        /// = 'U':  A is an upper triangular matrix.
        /// = 'H':  A is an upper Hessenberg matrix.
        /// = 'B':  A is a symmetric band matrix with lower bandwidth KL
        /// and upper bandwidth KU and with the only the lower
        /// half stored.
        /// = 'Q':  A is a symmetric band matrix with lower bandwidth KL
        /// and upper bandwidth KU and with the only the upper
        /// half stored.
        /// = 'Z':  A is a band matrix with lower bandwidth KL and upper
        /// bandwidth KU.
        ///</param>
        /// <param name="KL">
        /// (input) INTEGER
        /// The lower bandwidth of A.  Referenced only if TYPE = 'B',
        /// 'Q' or 'Z'.
        ///</param>
        /// <param name="KU">
        /// (input) INTEGER
        /// The upper bandwidth of A.  Referenced only if TYPE = 'B',
        /// 'Q' or 'Z'.
        ///</param>
        /// <param name="CFROM">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="CTO">
        /// (input) DOUBLE PRECISION
        /// The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
        /// without over/underflow if the final result CTO*A(I,J)/CFROM
        /// can be represented without over/underflow.  CFROM must be
        /// nonzero.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// The matrix to be multiplied by CTO/CFROM.  See TYPE for the
        /// storage type.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// 0  - successful exit
        /// .LT.0 - if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(string TYPE, int KL, int KU, double CFROM, double CTO, int M
                         , int N, ref double[] A, int offset_a, int LDA, ref int INFO)
        {

            #region Variables
            
            bool DONE = false; int I = 0; int ITYPE = 0; int J = 0; int K1 = 0; int K2 = 0; int K3 = 0; int K4 = 0; 
            double BIGNUM = 0;double CFROM1 = 0; double CFROMC = 0; double CTO1 = 0; double CTOC = 0; double MUL = 0; 
            double SMLNUM = 0;

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a; 

            #endregion


            #region Strings
            
            TYPE = TYPE.Substring(0, 1);  

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
            // *  DLASCL multiplies the M by N real matrix A by the real scalar
            // *  CTO/CFROM.  This is done without over/underflow as long as the final
            // *  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
            // *  A may be full, upper triangular, lower triangular, upper Hessenberg,
            // *  or banded.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  TYPE    (input) CHARACTER*1
            // *          TYPE indices the storage type of the input matrix.
            // *          = 'G':  A is a full matrix.
            // *          = 'L':  A is a lower triangular matrix.
            // *          = 'U':  A is an upper triangular matrix.
            // *          = 'H':  A is an upper Hessenberg matrix.
            // *          = 'B':  A is a symmetric band matrix with lower bandwidth KL
            // *                  and upper bandwidth KU and with the only the lower
            // *                  half stored.
            // *          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
            // *                  and upper bandwidth KU and with the only the upper
            // *                  half stored.
            // *          = 'Z':  A is a band matrix with lower bandwidth KL and upper
            // *                  bandwidth KU.
            // *
            // *  KL      (input) INTEGER
            // *          The lower bandwidth of A.  Referenced only if TYPE = 'B',
            // *          'Q' or 'Z'.
            // *
            // *  KU      (input) INTEGER
            // *          The upper bandwidth of A.  Referenced only if TYPE = 'B',
            // *          'Q' or 'Z'.
            // *
            // *  CFROM   (input) DOUBLE PRECISION
            // *  CTO     (input) DOUBLE PRECISION
            // *          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
            // *          without over/underflow if the final result CTO*A(I,J)/CFROM
            // *          can be represented without over/underflow.  CFROM must be
            // *          nonzero.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
            // *          storage type.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  INFO    (output) INTEGER
            // *          0  - successful exit
            // *          <0 - if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (this._lsame.Run(TYPE, "G"))
            {
                ITYPE = 0;
            }
            else
            {
                if (this._lsame.Run(TYPE, "L"))
                {
                    ITYPE = 1;
                }
                else
                {
                    if (this._lsame.Run(TYPE, "U"))
                    {
                        ITYPE = 2;
                    }
                    else
                    {
                        if (this._lsame.Run(TYPE, "H"))
                        {
                            ITYPE = 3;
                        }
                        else
                        {
                            if (this._lsame.Run(TYPE, "B"))
                            {
                                ITYPE = 4;
                            }
                            else
                            {
                                if (this._lsame.Run(TYPE, "Q"))
                                {
                                    ITYPE = 5;
                                }
                                else
                                {
                                    if (this._lsame.Run(TYPE, "Z"))
                                    {
                                        ITYPE = 6;
                                    }
                                    else
                                    {
                                        ITYPE =  - 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // *
            if (ITYPE ==  - 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (CFROM == ZERO)
                {
                    INFO =  - 4;
                }
                else
                {
                    if (M < 0)
                    {
                        INFO =  - 6;
                    }
                    else
                    {
                        if (N < 0 || (ITYPE == 4 && N != M) || (ITYPE == 5 && N != M))
                        {
                            INFO =  - 7;
                        }
                        else
                        {
                            if (ITYPE <= 3 && LDA < Math.Max(1, M))
                            {
                                INFO =  - 9;
                            }
                            else
                            {
                                if (ITYPE >= 4)
                                {
                                    if (KL < 0 || KL > Math.Max(M - 1, 0))
                                    {
                                        INFO =  - 2;
                                    }
                                    else
                                    {
                                        if (KU < 0 || KU > Math.Max(N - 1, 0) || ((ITYPE == 4 || ITYPE == 5) && KL != KU))
                                        {
                                            INFO =  - 3;
                                        }
                                        else
                                        {
                                            if ((ITYPE == 4 && LDA < KL + 1) || (ITYPE == 5 && LDA < KU + 1) || (ITYPE == 6 && LDA < 2 * KL + KU + 1))
                                            {
                                                INFO =  - 9;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DLASCL",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0 || M == 0) return;
            // *
            // *     Get machine parameters
            // *
            SMLNUM = this._dlamch.Run("S");
            BIGNUM = ONE / SMLNUM;
            // *
            CFROMC = CFROM;
            CTOC = CTO;
            // *
        LABEL10:;
            CFROM1 = CFROMC * SMLNUM;
            CTO1 = CTOC / BIGNUM;
            if (Math.Abs(CFROM1) > Math.Abs(CTOC) && CTOC != ZERO)
            {
                MUL = SMLNUM;
                DONE = false;
                CFROMC = CFROM1;
            }
            else
            {
                if (Math.Abs(CTO1) > Math.Abs(CFROMC))
                {
                    MUL = BIGNUM;
                    DONE = false;
                    CTOC = CTO1;
                }
                else
                {
                    MUL = CTOC / CFROMC;
                    DONE = true;
                }
            }
            // *
            if (ITYPE == 0)
            {
                // *
                // *        Full matrix
                // *
                for (J = 1; J <= N; J++)
                {
                    A_J = J * LDA + o_a;
                    for (I = 1; I <= M; I++)
                    {
                        A[I + A_J] *= MUL;
                    }
                }
                // *
            }
            else
            {
                if (ITYPE == 1)
                {
                    // *
                    // *        Lower triangular matrix
                    // *
                    for (J = 1; J <= N; J++)
                    {
                        A_J = J * LDA + o_a;
                        for (I = J; I <= M; I++)
                        {
                            A[I + A_J] *= MUL;
                        }
                    }
                    // *
                }
                else
                {
                    if (ITYPE == 2)
                    {
                        // *
                        // *        Upper triangular matrix
                        // *
                        for (J = 1; J <= N; J++)
                        {
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= Math.Min(J, M); I++)
                            {
                                A[I + A_J] *= MUL;
                            }
                        }
                        // *
                    }
                    else
                    {
                        if (ITYPE == 3)
                        {
                            // *
                            // *        Upper Hessenberg matrix
                            // *
                            for (J = 1; J <= N; J++)
                            {
                                A_J = J * LDA + o_a;
                                for (I = 1; I <= Math.Min(J + 1, M); I++)
                                {
                                    A[I + A_J] *= MUL;
                                }
                            }
                            // *
                        }
                        else
                        {
                            if (ITYPE == 4)
                            {
                                // *
                                // *        Lower half of a symmetric band matrix
                                // *
                                K3 = KL + 1;
                                K4 = N + 1;
                                for (J = 1; J <= N; J++)
                                {
                                    A_J = J * LDA + o_a;
                                    for (I = 1; I <= Math.Min(K3, K4 - J); I++)
                                    {
                                        A[I + A_J] *= MUL;
                                    }
                                }
                                // *
                            }
                            else
                            {
                                if (ITYPE == 5)
                                {
                                    // *
                                    // *        Upper half of a symmetric band matrix
                                    // *
                                    K1 = KU + 2;
                                    K3 = KU + 1;
                                    for (J = 1; J <= N; J++)
                                    {
                                        A_J = J * LDA + o_a;
                                        for (I = Math.Max(K1 - J, 1); I <= K3; I++)
                                        {
                                            A[I + A_J] *= MUL;
                                        }
                                    }
                                    // *
                                }
                                else
                                {
                                    if (ITYPE == 6)
                                    {
                                        // *
                                        // *        Band matrix
                                        // *
                                        K1 = KL + KU + 2;
                                        K2 = KL + 1;
                                        K3 = 2 * KL + KU + 1;
                                        K4 = KL + KU + 1 + M;
                                        for (J = 1; J <= N; J++)
                                        {
                                            A_J = J * LDA + o_a;
                                            for (I = Math.Max(K1 - J, K2); I <= Math.Min(K3, K4 - J); I++)
                                            {
                                                A[I + A_J] *= MUL;
                                            }
                                        }
                                        // *
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // *
            if (!DONE) goto LABEL10;
            // *
            return;
            // *
            // *     End of DLASCL
            // *

            #endregion

        }
    }
}
