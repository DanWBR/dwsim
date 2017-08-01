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
    /// DLAED1 computes the updated eigensystem of a diagonal
    /// matrix after modification by a rank-one symmetric matrix.  This
    /// routine is used only for the eigenproblem which requires all
    /// eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
    /// the case in which eigenvalues only or eigenvalues and eigenvectors
    /// of a full symmetric matrix (which was reduced to tridiagonal form)
    /// are desired.
    /// 
    /// T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
    /// 
    /// where Z = Q'u, u is a vector of length N with ones in the
    /// CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
    /// 
    /// The eigenvectors of the original matrix are stored in Q, and the
    /// eigenvalues are in D.  The algorithm consists of three stages:
    /// 
    /// The first stage consists of deflating the size of the problem
    /// when there are multiple eigenvalues or if there is a zero in
    /// the Z vector.  For each such occurence the dimension of the
    /// secular equation problem is reduced by one.  This stage is
    /// performed by the routine DLAED2.
    /// 
    /// The second stage consists of calculating the updated
    /// eigenvalues. This is done by finding the roots of the secular
    /// equation via the routine DLAED4 (as called by DLAED3).
    /// This routine also calculates the eigenvectors of the current
    /// problem.
    /// 
    /// The final stage consists of computing the updated eigenvectors
    /// directly using the updated eigenvalues.  The eigenvectors for
    /// the current problem are multiplied with the eigenvectors from
    /// the overall problem.
    /// 
    ///</summary>
    public class DLAED1
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DLAED2 _dlaed2; DLAED3 _dlaed3; DLAMRG _dlamrg; XERBLA _xerbla; 

        #endregion

        public DLAED1(DCOPY dcopy, DLAED2 dlaed2, DLAED3 dlaed3, DLAMRG dlamrg, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlaed2 = dlaed2; this._dlaed3 = dlaed3; this._dlamrg = dlamrg; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAED1()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            IDAMAX idamax = new IDAMAX();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DLAMRG dlamrg = new DLAMRG();
            DROT drot = new DROT();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DNRM2 dnrm2 = new DNRM2();
            DLAED5 dlaed5 = new DLAED5();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAED2 dlaed2 = new DLAED2(idamax, dlamch, dlapy2, dcopy, dlacpy, dlamrg, drot, dscal, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLASET dlaset = new DLASET(lsame);
            DLAED3 dlaed3 = new DLAED3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlaed4, dlaset, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlaed2 = dlaed2; this._dlaed3 = dlaed3; this._dlamrg = dlamrg; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED1 computes the updated eigensystem of a diagonal
        /// matrix after modification by a rank-one symmetric matrix.  This
        /// routine is used only for the eigenproblem which requires all
        /// eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
        /// the case in which eigenvalues only or eigenvalues and eigenvectors
        /// of a full symmetric matrix (which was reduced to tridiagonal form)
        /// are desired.
        /// 
        /// T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
        /// 
        /// where Z = Q'u, u is a vector of length N with ones in the
        /// CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
        /// 
        /// The eigenvectors of the original matrix are stored in Q, and the
        /// eigenvalues are in D.  The algorithm consists of three stages:
        /// 
        /// The first stage consists of deflating the size of the problem
        /// when there are multiple eigenvalues or if there is a zero in
        /// the Z vector.  For each such occurence the dimension of the
        /// secular equation problem is reduced by one.  This stage is
        /// performed by the routine DLAED2.
        /// 
        /// The second stage consists of calculating the updated
        /// eigenvalues. This is done by finding the roots of the secular
        /// equation via the routine DLAED4 (as called by DLAED3).
        /// This routine also calculates the eigenvectors of the current
        /// problem.
        /// 
        /// The final stage consists of computing the updated eigenvectors
        /// directly using the updated eigenvalues.  The eigenvectors for
        /// the current problem are multiplied with the eigenvectors from
        /// the overall problem.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The dimension of the symmetric tridiagonal matrix.  N .GE. 0.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the eigenvalues of the rank-1-perturbed matrix.
        /// On exit, the eigenvalues of the repaired matrix.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// On entry, the eigenvectors of the rank-1-perturbed matrix.
        /// On exit, the eigenvectors of the repaired tridiagonal matrix.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="INDXQ">
        /// (input/output) INTEGER array, dimension (N)
        /// On entry, the permutation which separately sorts the two
        /// subproblems in D into ascending order.
        /// On exit, the permutation which will reintegrate the
        /// subproblems back into sorted order,
        /// i.e. D( INDXQ( I = 1, N ) ) will be in ascending order.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The subdiagonal entry used to create the rank-1 modification.
        ///</param>
        /// <param name="CUTPNT">
        /// (input) INTEGER
        /// The location of the last eigenvalue in the leading sub-matrix.
        /// min(1,N) .LE. CUTPNT .LE. N/2.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (4*N + N**2)
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (4*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an eigenvalue did not converge
        ///</param>
        public void Run(int N, ref double[] D, int offset_d, ref double[] Q, int offset_q, int LDQ, ref int[] INDXQ, int offset_indxq, ref double RHO
                         , int CUTPNT, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int COLTYP = 0; int I = 0; int IDLMDA = 0; int INDX = 0; int INDXC = 0; int INDXP = 0; int IQ2 = 0; int IS = 0; 
            int IW = 0;int IZ = 0; int K = 0; int N1 = 0; int N2 = 0; int ZPP1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_indxq = -1 + offset_indxq; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork; 

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
            // *  DLAED1 computes the updated eigensystem of a diagonal
            // *  matrix after modification by a rank-one symmetric matrix.  This
            // *  routine is used only for the eigenproblem which requires all
            // *  eigenvalues and eigenvectors of a tridiagonal matrix.  DLAED7 handles
            // *  the case in which eigenvalues only or eigenvalues and eigenvectors
            // *  of a full symmetric matrix (which was reduced to tridiagonal form)
            // *  are desired.
            // *
            // *    T = Q(in) ( D(in) + RHO * Z*Z' ) Q'(in) = Q(out) * D(out) * Q'(out)
            // *
            // *     where Z = Q'u, u is a vector of length N with ones in the
            // *     CUTPNT and CUTPNT + 1 th elements and zeros elsewhere.
            // *
            // *     The eigenvectors of the original matrix are stored in Q, and the
            // *     eigenvalues are in D.  The algorithm consists of three stages:
            // *
            // *        The first stage consists of deflating the size of the problem
            // *        when there are multiple eigenvalues or if there is a zero in
            // *        the Z vector.  For each such occurence the dimension of the
            // *        secular equation problem is reduced by one.  This stage is
            // *        performed by the routine DLAED2.
            // *
            // *        The second stage consists of calculating the updated
            // *        eigenvalues. This is done by finding the roots of the secular
            // *        equation via the routine DLAED4 (as called by DLAED3).
            // *        This routine also calculates the eigenvectors of the current
            // *        problem.
            // *
            // *        The final stage consists of computing the updated eigenvectors
            // *        directly using the updated eigenvalues.  The eigenvectors for
            // *        the current problem are multiplied with the eigenvectors from
            // *        the overall problem.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N      (input) INTEGER
            // *         The dimension of the symmetric tridiagonal matrix.  N >= 0.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry, the eigenvalues of the rank-1-perturbed matrix.
            // *         On exit, the eigenvalues of the repaired matrix.
            // *
            // *  Q      (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *         On entry, the eigenvectors of the rank-1-perturbed matrix.
            // *         On exit, the eigenvectors of the repaired tridiagonal matrix.
            // *
            // *  LDQ    (input) INTEGER
            // *         The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  INDXQ  (input/output) INTEGER array, dimension (N)
            // *         On entry, the permutation which separately sorts the two
            // *         subproblems in D into ascending order.
            // *         On exit, the permutation which will reintegrate the
            // *         subproblems back into sorted order,
            // *         i.e. D( INDXQ( I = 1, N ) ) will be in ascending order.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The subdiagonal entry used to create the rank-1 modification.
            // *
            // *  CUTPNT (input) INTEGER
            // *         The location of the last eigenvalue in the leading sub-matrix.
            // *         min(1,N) <= CUTPNT <= N/2.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension (4*N + N**2)
            // *
            // *  IWORK  (workspace) INTEGER array, dimension (4*N)
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, an eigenvalue did not converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Jeff Rutter, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *  Modified by Francoise Tisseur, University of Tennessee.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
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
            else
            {
                if (LDQ < Math.Max(1, N))
                {
                    INFO =  - 4;
                }
                else
                {
                    if (Math.Min(1, N / 2) > CUTPNT || (N / 2) < CUTPNT)
                    {
                        INFO =  - 7;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED1",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     The following values are integer pointers which indicate
            // *     the portion of the workspace
            // *     used by a particular array in DLAED2 and DLAED3.
            // *
            IZ = 1;
            IDLMDA = IZ + N;
            IW = IDLMDA + N;
            IQ2 = IW + N;
            // *
            INDX = 1;
            INDXC = INDX + N;
            COLTYP = INDXC + N;
            INDXP = COLTYP + N;
            // *
            // *
            // *     Form the z-vector which consists of the last row of Q_1 and the
            // *     first row of Q_2.
            // *
            this._dcopy.Run(CUTPNT, Q, CUTPNT+1 * LDQ + o_q, LDQ, ref WORK, IZ + o_work, 1);
            ZPP1 = CUTPNT + 1;
            this._dcopy.Run(N - CUTPNT, Q, ZPP1+ZPP1 * LDQ + o_q, LDQ, ref WORK, IZ + CUTPNT + o_work, 1);
            // *
            // *     Deflate eigenvalues.
            // *
            this._dlaed2.Run(ref K, N, CUTPNT, ref D, offset_d, ref Q, offset_q, LDQ
                             , ref INDXQ, offset_indxq, ref RHO, ref WORK, IZ + o_work, ref WORK, IDLMDA + o_work, ref WORK, IW + o_work, ref WORK, IQ2 + o_work
                             , ref IWORK, INDX + o_iwork, ref IWORK, INDXC + o_iwork, ref IWORK, INDXP + o_iwork, ref IWORK, COLTYP + o_iwork, ref INFO);
            // *
            if (INFO != 0) goto LABEL20;
            // *
            // *     Solve Secular Equation.
            // *
            if (K != 0)
            {
                IS = (IWORK[COLTYP + o_iwork] + IWORK[COLTYP + 1 + o_iwork]) * CUTPNT + (IWORK[COLTYP + 1 + o_iwork] + IWORK[COLTYP + 2 + o_iwork]) * (N - CUTPNT) + IQ2;
                this._dlaed3.Run(K, N, CUTPNT, ref D, offset_d, ref Q, offset_q, LDQ
                                 , RHO, ref WORK, IDLMDA + o_work, WORK, IQ2 + o_work, IWORK, INDXC + o_iwork, IWORK, COLTYP + o_iwork, ref WORK, IW + o_work
                                 , ref WORK, IS + o_work, ref INFO);
                if (INFO != 0) goto LABEL20;
                // *
                // *     Prepare the INDXQ sorting permutation.
                // *
                N1 = K;
                N2 = N - K;
                this._dlamrg.Run(N1, N2, D, offset_d, 1,  - 1, ref INDXQ, offset_indxq);
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    INDXQ[I + o_indxq] = I;
                }
            }
            // *
        LABEL20:;
            return;
            // *
            // *     End of DLAED1
            // *

            #endregion

        }
    }
}
