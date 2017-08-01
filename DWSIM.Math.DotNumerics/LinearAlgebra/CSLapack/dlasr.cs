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
    /// DLASR applies a sequence of plane rotations to a real matrix A,
    /// from either the left or the right.
    /// 
    /// When SIDE = 'L', the transformation takes the form
    /// 
    /// A := P*A
    /// 
    /// and when SIDE = 'R', the transformation takes the form
    /// 
    /// A := A*P**T
    /// 
    /// where P is an orthogonal matrix consisting of a sequence of z plane
    /// rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
    /// and P**T is the transpose of P.
    /// 
    /// When DIRECT = 'F' (Forward sequence), then
    /// 
    /// P = P(z-1) * ... * P(2) * P(1)
    /// 
    /// and when DIRECT = 'B' (Backward sequence), then
    /// 
    /// P = P(1) * P(2) * ... * P(z-1)
    /// 
    /// where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
    /// 
    /// R(k) = (  c(k)  s(k) )
    /// = ( -s(k)  c(k) ).
    /// 
    /// When PIVOT = 'V' (Variable pivot), the rotation is performed
    /// for the plane (k,k+1), i.e., P(k) has the form
    /// 
    /// P(k) = (  1                                            )
    /// (       ...                                     )
    /// (              1                                )
    /// (                   c(k)  s(k)                  )
    /// (                  -s(k)  c(k)                  )
    /// (                                1              )
    /// (                                     ...       )
    /// (                                            1  )
    /// 
    /// where R(k) appears as a rank-2 modification to the identity matrix in
    /// rows and columns k and k+1.
    /// 
    /// When PIVOT = 'T' (Top pivot), the rotation is performed for the
    /// plane (1,k+1), so P(k) has the form
    /// 
    /// P(k) = (  c(k)                    s(k)                 )
    /// (         1                                     )
    /// (              ...                              )
    /// (                     1                         )
    /// ( -s(k)                    c(k)                 )
    /// (                                 1             )
    /// (                                      ...      )
    /// (                                             1 )
    /// 
    /// where R(k) appears in rows and columns 1 and k+1.
    /// 
    /// Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
    /// performed for the plane (k,z), giving P(k) the form
    /// 
    /// P(k) = ( 1                                             )
    /// (      ...                                      )
    /// (             1                                 )
    /// (                  c(k)                    s(k) )
    /// (                         1                     )
    /// (                              ...              )
    /// (                                     1         )
    /// (                 -s(k)                    c(k) )
    /// 
    /// where R(k) appears in rows and columns k and z.  The rotations are
    /// performed without ever forming P(k) explicitly.
    /// 
    ///</summary>
    public class DLASR
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLASR(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASR()
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
        /// DLASR applies a sequence of plane rotations to a real matrix A,
        /// from either the left or the right.
        /// 
        /// When SIDE = 'L', the transformation takes the form
        /// 
        /// A := P*A
        /// 
        /// and when SIDE = 'R', the transformation takes the form
        /// 
        /// A := A*P**T
        /// 
        /// where P is an orthogonal matrix consisting of a sequence of z plane
        /// rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
        /// and P**T is the transpose of P.
        /// 
        /// When DIRECT = 'F' (Forward sequence), then
        /// 
        /// P = P(z-1) * ... * P(2) * P(1)
        /// 
        /// and when DIRECT = 'B' (Backward sequence), then
        /// 
        /// P = P(1) * P(2) * ... * P(z-1)
        /// 
        /// where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
        /// 
        /// R(k) = (  c(k)  s(k) )
        /// = ( -s(k)  c(k) ).
        /// 
        /// When PIVOT = 'V' (Variable pivot), the rotation is performed
        /// for the plane (k,k+1), i.e., P(k) has the form
        /// 
        /// P(k) = (  1                                            )
        /// (       ...                                     )
        /// (              1                                )
        /// (                   c(k)  s(k)                  )
        /// (                  -s(k)  c(k)                  )
        /// (                                1              )
        /// (                                     ...       )
        /// (                                            1  )
        /// 
        /// where R(k) appears as a rank-2 modification to the identity matrix in
        /// rows and columns k and k+1.
        /// 
        /// When PIVOT = 'T' (Top pivot), the rotation is performed for the
        /// plane (1,k+1), so P(k) has the form
        /// 
        /// P(k) = (  c(k)                    s(k)                 )
        /// (         1                                     )
        /// (              ...                              )
        /// (                     1                         )
        /// ( -s(k)                    c(k)                 )
        /// (                                 1             )
        /// (                                      ...      )
        /// (                                             1 )
        /// 
        /// where R(k) appears in rows and columns 1 and k+1.
        /// 
        /// Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
        /// performed for the plane (k,z), giving P(k) the form
        /// 
        /// P(k) = ( 1                                             )
        /// (      ...                                      )
        /// (             1                                 )
        /// (                  c(k)                    s(k) )
        /// (                         1                     )
        /// (                              ...              )
        /// (                                     1         )
        /// (                 -s(k)                    c(k) )
        /// 
        /// where R(k) appears in rows and columns k and z.  The rotations are
        /// performed without ever forming P(k) explicitly.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// Specifies whether the plane rotation matrix P is applied to
        /// A on the left or the right.
        /// = 'L':  Left, compute A := P*A
        /// = 'R':  Right, compute A:= A*P**T
        ///</param>
        /// <param name="PIVOT">
        /// (input) CHARACTER*1
        /// Specifies the plane for which P(k) is a plane rotation
        /// matrix.
        /// = 'V':  Variable pivot, the plane (k,k+1)
        /// = 'T':  Top pivot, the plane (1,k+1)
        /// = 'B':  Bottom pivot, the plane (k,z)
        ///</param>
        /// <param name="DIRECT">
        /// (input) CHARACTER*1
        /// Specifies whether P is a forward or backward sequence of
        /// plane rotations.
        /// = 'F':  Forward, P = P(z-1)*...*P(2)*P(1)
        /// = 'B':  Backward, P = P(1)*P(2)*...*P(z-1)
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  If m .LE. 1, an immediate
        /// return is effected.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  If n .LE. 1, an
        /// immediate return is effected.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION array, dimension
        /// (M-1) if SIDE = 'L'
        /// (N-1) if SIDE = 'R'
        /// The cosines c(k) of the plane rotations.
        ///</param>
        /// <param name="S">
        /// (input) DOUBLE PRECISION array, dimension
        /// (M-1) if SIDE = 'L'
        /// (N-1) if SIDE = 'R'
        /// The sines s(k) of the plane rotations.  The 2-by-2 plane
        /// rotation part of the matrix P(k), R(k), has the form
        /// R(k) = (  c(k)  s(k) )
        /// ( -s(k)  c(k) ).
        ///</param>
        /// <param name="A">
        /// := P*A
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        public void Run(string SIDE, string PIVOT, string DIRECT, int M, int N, double[] C, int offset_c
                         , double[] S, int offset_s, ref double[] A, int offset_a, int LDA)
        {

            #region Variables
            
            int I = 0; int INFO = 0; int J = 0; double CTEMP = 0; double STEMP = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int A_0 = 0; int A_1 = 0; int A_J = 0; int A_2 = 0; int A_3 = 0; int A_N = 0; 

            #endregion


            #region Array Index Correction
            
             int o_c = -1 + offset_c;  int o_s = -1 + offset_s;  int o_a = -1 - LDA + offset_a; 

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  PIVOT = PIVOT.Substring(0, 1);  DIRECT = DIRECT.Substring(0, 1);  

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
            // *  DLASR applies a sequence of plane rotations to a real matrix A,
            // *  from either the left or the right.
            // *  
            // *  When SIDE = 'L', the transformation takes the form
            // *  
            // *     A := P*A
            // *  
            // *  and when SIDE = 'R', the transformation takes the form
            // *  
            // *     A := A*P**T
            // *  
            // *  where P is an orthogonal matrix consisting of a sequence of z plane
            // *  rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
            // *  and P**T is the transpose of P.
            // *  
            // *  When DIRECT = 'F' (Forward sequence), then
            // *  
            // *     P = P(z-1) * ... * P(2) * P(1)
            // *  
            // *  and when DIRECT = 'B' (Backward sequence), then
            // *  
            // *     P = P(1) * P(2) * ... * P(z-1)
            // *  
            // *  where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
            // *  
            // *     R(k) = (  c(k)  s(k) )
            // *          = ( -s(k)  c(k) ).
            // *  
            // *  When PIVOT = 'V' (Variable pivot), the rotation is performed
            // *  for the plane (k,k+1), i.e., P(k) has the form
            // *  
            // *     P(k) = (  1                                            )
            // *            (       ...                                     )
            // *            (              1                                )
            // *            (                   c(k)  s(k)                  )
            // *            (                  -s(k)  c(k)                  )
            // *            (                                1              )
            // *            (                                     ...       )
            // *            (                                            1  )
            // *  
            // *  where R(k) appears as a rank-2 modification to the identity matrix in
            // *  rows and columns k and k+1.
            // *  
            // *  When PIVOT = 'T' (Top pivot), the rotation is performed for the
            // *  plane (1,k+1), so P(k) has the form
            // *  
            // *     P(k) = (  c(k)                    s(k)                 )
            // *            (         1                                     )
            // *            (              ...                              )
            // *            (                     1                         )
            // *            ( -s(k)                    c(k)                 )
            // *            (                                 1             )
            // *            (                                      ...      )
            // *            (                                             1 )
            // *  
            // *  where R(k) appears in rows and columns 1 and k+1.
            // *  
            // *  Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
            // *  performed for the plane (k,z), giving P(k) the form
            // *  
            // *     P(k) = ( 1                                             )
            // *            (      ...                                      )
            // *            (             1                                 )
            // *            (                  c(k)                    s(k) )
            // *            (                         1                     )
            // *            (                              ...              )
            // *            (                                     1         )
            // *            (                 -s(k)                    c(k) )
            // *  
            // *  where R(k) appears in rows and columns k and z.  The rotations are
            // *  performed without ever forming P(k) explicitly.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          Specifies whether the plane rotation matrix P is applied to
            // *          A on the left or the right.
            // *          = 'L':  Left, compute A := P*A
            // *          = 'R':  Right, compute A:= A*P**T
            // *
            // *  PIVOT   (input) CHARACTER*1
            // *          Specifies the plane for which P(k) is a plane rotation
            // *          matrix.
            // *          = 'V':  Variable pivot, the plane (k,k+1)
            // *          = 'T':  Top pivot, the plane (1,k+1)
            // *          = 'B':  Bottom pivot, the plane (k,z)
            // *
            // *  DIRECT  (input) CHARACTER*1
            // *          Specifies whether P is a forward or backward sequence of
            // *          plane rotations.
            // *          = 'F':  Forward, P = P(z-1)*...*P(2)*P(1)
            // *          = 'B':  Backward, P = P(1)*P(2)*...*P(z-1)
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  If m <= 1, an immediate
            // *          return is effected.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  If n <= 1, an
            // *          immediate return is effected.
            // *
            // *  C       (input) DOUBLE PRECISION array, dimension
            // *                  (M-1) if SIDE = 'L'
            // *                  (N-1) if SIDE = 'R'
            // *          The cosines c(k) of the plane rotations.
            // *
            // *  S       (input) DOUBLE PRECISION array, dimension
            // *                  (M-1) if SIDE = 'L'
            // *                  (N-1) if SIDE = 'R'
            // *          The sines s(k) of the plane rotations.  The 2-by-2 plane
            // *          rotation part of the matrix P(k), R(k), has the form
            // *          R(k) = (  c(k)  s(k) )
            // *                 ( -s(k)  c(k) ).
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The M-by-N matrix A.  On exit, A is overwritten by P*A if
            // *          SIDE = 'R' or by A*P**T if SIDE = 'L'.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (!(this._lsame.Run(SIDE, "L") || this._lsame.Run(SIDE, "R")))
            {
                INFO = 1;
            }
            else
            {
                if (!(this._lsame.Run(PIVOT, "V") || this._lsame.Run(PIVOT, "T") || this._lsame.Run(PIVOT, "B")))
                {
                    INFO = 2;
                }
                else
                {
                    if (!(this._lsame.Run(DIRECT, "F") || this._lsame.Run(DIRECT, "B")))
                    {
                        INFO = 3;
                    }
                    else
                    {
                        if (M < 0)
                        {
                            INFO = 4;
                        }
                        else
                        {
                            if (N < 0)
                            {
                                INFO = 5;
                            }
                            else
                            {
                                if (LDA < Math.Max(1, M))
                                {
                                    INFO = 9;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASR ", INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if ((M == 0) || (N == 0)) return;
            if (this._lsame.Run(SIDE, "L"))
            {
                // *
                // *        Form  P * A
                // *
                if (this._lsame.Run(PIVOT, "V"))
                {
                    if (this._lsame.Run(DIRECT, "F"))
                    {
                        for (J = 1; J <= M - 1; J++)
                        {
                            CTEMP = C[J + o_c];
                            STEMP = S[J + o_s];
                            if ((CTEMP != ONE) || (STEMP != ZERO))
                            {
                                for (I = 1; I <= N; I++)
                                {
                                    TEMP = A[J + 1+I * LDA + o_a];
                                    A[J + 1+I * LDA + o_a] = CTEMP * TEMP - STEMP * A[J+I * LDA + o_a];
                                    A[J+I * LDA + o_a] = STEMP * TEMP + CTEMP * A[J+I * LDA + o_a];
                                }
                            }
                        }
                    }
                    else
                    {
                        if (this._lsame.Run(DIRECT, "B"))
                        {
                            for (J = M - 1; J >= 1; J +=  - 1)
                            {
                                CTEMP = C[J + o_c];
                                STEMP = S[J + o_s];
                                if ((CTEMP != ONE) || (STEMP != ZERO))
                                {
                                    for (I = 1; I <= N; I++)
                                    {
                                        TEMP = A[J + 1+I * LDA + o_a];
                                        A[J + 1+I * LDA + o_a] = CTEMP * TEMP - STEMP * A[J+I * LDA + o_a];
                                        A[J+I * LDA + o_a] = STEMP * TEMP + CTEMP * A[J+I * LDA + o_a];
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    if (this._lsame.Run(PIVOT, "T"))
                    {
                        if (this._lsame.Run(DIRECT, "F"))
                        {
                            for (J = 2; J <= M; J++)
                            {
                                CTEMP = C[J - 1 + o_c];
                                STEMP = S[J - 1 + o_s];
                                if ((CTEMP != ONE) || (STEMP != ZERO))
                                {
                                    for (I = 1; I <= N; I++)
                                    {
                                        TEMP = A[J+I * LDA + o_a];
                                        A[J+I * LDA + o_a] = CTEMP * TEMP - STEMP * A[1+I * LDA + o_a];
                                        A[1+I * LDA + o_a] = STEMP * TEMP + CTEMP * A[1+I * LDA + o_a];
                                    }
                                }
                            }
                        }
                        else
                        {
                            if (this._lsame.Run(DIRECT, "B"))
                            {
                                for (J = M; J >= 2; J +=  - 1)
                                {
                                    CTEMP = C[J - 1 + o_c];
                                    STEMP = S[J - 1 + o_s];
                                    if ((CTEMP != ONE) || (STEMP != ZERO))
                                    {
                                        for (I = 1; I <= N; I++)
                                        {
                                            TEMP = A[J+I * LDA + o_a];
                                            A[J+I * LDA + o_a] = CTEMP * TEMP - STEMP * A[1+I * LDA + o_a];
                                            A[1+I * LDA + o_a] = STEMP * TEMP + CTEMP * A[1+I * LDA + o_a];
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        if (this._lsame.Run(PIVOT, "B"))
                        {
                            if (this._lsame.Run(DIRECT, "F"))
                            {
                                for (J = 1; J <= M - 1; J++)
                                {
                                    CTEMP = C[J + o_c];
                                    STEMP = S[J + o_s];
                                    if ((CTEMP != ONE) || (STEMP != ZERO))
                                    {
                                        for (I = 1; I <= N; I++)
                                        {
                                            TEMP = A[J+I * LDA + o_a];
                                            A[J+I * LDA + o_a] = STEMP * A[M+I * LDA + o_a] + CTEMP * TEMP;
                                            A[M+I * LDA + o_a] = CTEMP * A[M+I * LDA + o_a] - STEMP * TEMP;
                                        }
                                    }
                                }
                            }
                            else
                            {
                                if (this._lsame.Run(DIRECT, "B"))
                                {
                                    for (J = M - 1; J >= 1; J +=  - 1)
                                    {
                                        CTEMP = C[J + o_c];
                                        STEMP = S[J + o_s];
                                        if ((CTEMP != ONE) || (STEMP != ZERO))
                                        {
                                            for (I = 1; I <= N; I++)
                                            {
                                                TEMP = A[J+I * LDA + o_a];
                                                A[J+I * LDA + o_a] = STEMP * A[M+I * LDA + o_a] + CTEMP * TEMP;
                                                A[M+I * LDA + o_a] = CTEMP * A[M+I * LDA + o_a] - STEMP * TEMP;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                if (this._lsame.Run(SIDE, "R"))
                {
                    // *
                    // *        Form A * P'
                    // *
                    if (this._lsame.Run(PIVOT, "V"))
                    {
                        if (this._lsame.Run(DIRECT, "F"))
                        {
                            for (J = 1; J <= N - 1; J++)
                            {
                                CTEMP = C[J + o_c];
                                STEMP = S[J + o_s];
                                if ((CTEMP != ONE) || (STEMP != ZERO))
                                {
                                    A_0 = (J + 1) * LDA + o_a;
                                    A_1 = (J + 1) * LDA + o_a;
                                    A_J = J * LDA + o_a;
                                    for (I = 1; I <= M; I++)
                                    {
                                        TEMP = A[I + A_0];
                                        A[I + A_1] = CTEMP * TEMP - STEMP * A[I + A_J];
                                        A[I + A_J] = STEMP * TEMP + CTEMP * A[I + A_J];
                                    }
                                }
                            }
                        }
                        else
                        {
                            if (this._lsame.Run(DIRECT, "B"))
                            {
                                for (J = N - 1; J >= 1; J +=  - 1)
                                {
                                    CTEMP = C[J + o_c];
                                    STEMP = S[J + o_s];
                                    if ((CTEMP != ONE) || (STEMP != ZERO))
                                    {
                                        A_2 = (J + 1) * LDA + o_a;
                                        A_3 = (J + 1) * LDA + o_a;
                                        A_J = J * LDA + o_a;
                                        for (I = 1; I <= M; I++)
                                        {
                                            TEMP = A[I + A_2];
                                            A[I + A_3] = CTEMP * TEMP - STEMP * A[I + A_J];
                                            A[I + A_J] = STEMP * TEMP + CTEMP * A[I + A_J];
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        if (this._lsame.Run(PIVOT, "T"))
                        {
                            if (this._lsame.Run(DIRECT, "F"))
                            {
                                for (J = 2; J <= N; J++)
                                {
                                    CTEMP = C[J - 1 + o_c];
                                    STEMP = S[J - 1 + o_s];
                                    if ((CTEMP != ONE) || (STEMP != ZERO))
                                    {
                                        A_J = J * LDA + o_a;
                                        A_1 = 1 * LDA + o_a;
                                        for (I = 1; I <= M; I++)
                                        {
                                            TEMP = A[I + A_J];
                                            A[I + A_J] = CTEMP * TEMP - STEMP * A[I + A_1];
                                            A[I + A_1] = STEMP * TEMP + CTEMP * A[I + A_1];
                                        }
                                    }
                                }
                            }
                            else
                            {
                                if (this._lsame.Run(DIRECT, "B"))
                                {
                                    for (J = N; J >= 2; J +=  - 1)
                                    {
                                        CTEMP = C[J - 1 + o_c];
                                        STEMP = S[J - 1 + o_s];
                                        if ((CTEMP != ONE) || (STEMP != ZERO))
                                        {
                                            A_J = J * LDA + o_a;
                                            A_1 = 1 * LDA + o_a;
                                            for (I = 1; I <= M; I++)
                                            {
                                                TEMP = A[I + A_J];
                                                A[I + A_J] = CTEMP * TEMP - STEMP * A[I + A_1];
                                                A[I + A_1] = STEMP * TEMP + CTEMP * A[I + A_1];
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        else
                        {
                            if (this._lsame.Run(PIVOT, "B"))
                            {
                                if (this._lsame.Run(DIRECT, "F"))
                                {
                                    for (J = 1; J <= N - 1; J++)
                                    {
                                        CTEMP = C[J + o_c];
                                        STEMP = S[J + o_s];
                                        if ((CTEMP != ONE) || (STEMP != ZERO))
                                        {
                                            A_J = J * LDA + o_a;
                                            A_N = N * LDA + o_a;
                                            for (I = 1; I <= M; I++)
                                            {
                                                TEMP = A[I + A_J];
                                                A[I + A_J] = STEMP * A[I + A_N] + CTEMP * TEMP;
                                                A[I + A_N] = CTEMP * A[I + A_N] - STEMP * TEMP;
                                            }
                                        }
                                    }
                                }
                                else
                                {
                                    if (this._lsame.Run(DIRECT, "B"))
                                    {
                                        for (J = N - 1; J >= 1; J +=  - 1)
                                        {
                                            CTEMP = C[J + o_c];
                                            STEMP = S[J + o_s];
                                            if ((CTEMP != ONE) || (STEMP != ZERO))
                                            {
                                                A_J = J * LDA + o_a;
                                                A_N = N * LDA + o_a;
                                                for (I = 1; I <= M; I++)
                                                {
                                                    TEMP = A[I + A_J];
                                                    A[I + A_J] = STEMP * A[I + A_N] + CTEMP * TEMP;
                                                    A[I + A_N] = CTEMP * A[I + A_N] - STEMP * TEMP;
                                                }
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
            return;
            // *
            // *     End of DLASR
            // *

            #endregion

        }
    }
}
