#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using DotNumerics.LinearAlgebra.CSLapack;

namespace DotNumerics.LinearAlgebra
{


    /// <summary>
    /// Computes the minimum-norm solution to a real linear least squares problem: minimize 2-norm(|| A*X - B||)
    /// involving an M-by-N matrix A. The problem can be solved using: 1) A QR or LQ
    /// factorization, 2) Complete orthogonal factorization, 3) Using singular value decomposition (SVD).  
    /// </summary>
    public sealed class LinearLeastSquares 
    {

        #region Fields

        DGELS _dgels;
        DGELSD _dgelsd;
        DGELSY _dgelsy;

        #endregion

        /// <summary>
        ///Initializes a new instance of the LinearLeastSquares class.
        /// </summary>
        public LinearLeastSquares()
        {

        }

        #region Public Methods

        //public Matrix Solve(Matrix A, Matrix B)
        //{
        //    return this.SVDdcSolve(A, B);
        //}

        //public Matrix Solve(Matrix A, Matrix B, LLSMethod method)
        //{
        //    switch (method)
        //    {
        //        case LLSMethod.QRorLQ:
        //            return this.QRorLQSolve(A, B);
        //            break;
        //        case LLSMethod.COF:
        //            return this.COFSolve(A, B);
        //            break;
        //        case LLSMethod.SVD:
        //            return this.SVDdcSolve(A, B);
        //            break;
        //    }
        //}

        #endregion

        #region Private Metods

        #region QR or LQ factorization



        /// <summary>
        /// Solves overdetermined or underdetermined real linear systems
        /// involving an M-by-N matrix A, using a QR or LQ
        /// factorization of A.  It is assumed that A has full rank.
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution.</returns>
        public Matrix QRorLQSolve(Matrix A, Matrix B)
        {

            if (this._dgels == null) this._dgels = new DGELS();

            Matrix AClon = A.Clone();
            double[] AClonData = AClon.Data;

            Matrix ClonB = new Matrix(Math.Max(A.RowCount, A.ColumnCount), B.ColumnCount);

            double[] ClonData = ClonB.Data;

            for (int j = 0; j < ClonB.ColumnCount; j++)
            {
                for (int i = 0; i < B.RowCount; i++)
                {
                    ClonB[i, j] = B[i, j];
                }
            }


            int Info = 0;
            //int LWork
            double[] Work = new double[1];
            int LWork = -1;
            //Calculamos LWORK ideal
            _dgels.Run("N", A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref Work, 0, LWork, ref Info);
            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dgels.Run("N", A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }


            #region Error
            // = 0:  successful exit
            // .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            // .GT. 0:  if INFO =  i, the i-th diagonal element of the
            // triangular factor of A is zero, so that A does not have
            // full rank; the least squares solution could not be
            // computed.
            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The matrix A does not have full rank; the least squares solution could not be computed. You can use SVDSolve or SVDdcSolve");
            }

            #endregion

            Matrix Solution = new Matrix(A.ColumnCount, B.ColumnCount);

            for (int j = 0; j < Solution.ColumnCount; j++)
            {
                for (int i = 0; i < Solution.RowCount; i++)
                {
                    Solution[i, j] = ClonB[i, j];
                }
            }

            return Solution ;
        }

        #endregion

        #region Complete orthogonal factorization


        /// <summary>
        /// Computes the minimum-norm solution to a real linear least squares problem: minimize 2-norm(|| A*X - B||) 
        /// using a complete orthogonal factorization of A.
        /// The matrix A can be rank-deficient.
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution.</returns>
        public Matrix COFSolve(Matrix A, Matrix B)
        {
            //Fortran 95 Interface Notes
            //From http://www.intel.com/software/products/mkl/docs/WebHelp/lse/functn_gelsy.html
            //rcond Default value for this element is rcond = 100*EPSILON(1.0_WP).
            DLAMCH dch= new DLAMCH();
            double RCOND = 100*dch.Run("Epsilon");

            return this.COFSolve(A, B, RCOND);

        }

        /// <summary>
        /// Computes the minimum-norm solution to a real linear least squares problem: minimize 2-norm(|| A*X - B||) 
        /// using a complete orthogonal factorization of A.
        /// The matrix A can be rank-deficient.
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <param name="rcond">
        /// The parameter rcond is used to determine the effective rank of A, which
        /// is defined as the order of the largest leading triangular
        /// submatrix R11 in the QR factorization with pivoting of A,
        /// whose estimated condition number .LT. 1/rcond.
        /// </param>
        /// <returns>A matrix containing the solution.</returns>
        public Matrix COFSolve(Matrix A, Matrix B, double rcond)
        {

            if (this._dgelsy == null) this._dgelsy = new DGELSY();

            Matrix AClon = A.Clone();
            double[] AClonData = AClon.Data;

            Matrix ClonB = new Matrix(Math.Max(A.RowCount, A.ColumnCount), B.ColumnCount);

            double[] ClonData = ClonB.Data;

            for (int j = 0; j < ClonB.ColumnCount; j++)
            {
                for (int i = 0; i < B.RowCount; i++)
                {
                    ClonB[i, j] = B[i, j];
                }
            }

            int Info = 0;
            //int LWork
            double[] Work = new double[1];
            int[] JPVT = new int[A.ColumnCount];

            int RANK = 1;
            int LWork = -1;
            //Calculamos LWORK ideal
            this._dgelsy.Run(A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref JPVT, 0, rcond, ref RANK, ref Work, 0, LWork, ref Info);
            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                this._dgelsy.Run(A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref JPVT, 0, rcond, ref RANK, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }


            #region Error
            //  INFO    (output) INTEGER
            //  = 0: successful exit
            //  < 0: If INFO = -i, the i-th argument had an illegal value.
            // 
            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                //Aqui no se espqcifica nungun error, asi que en principio este valor no es posible, de cualquier forma se lo
                //pongo por si las dudas
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("Error The algorithm ... ");
            }
            #endregion

            Matrix Solution = new Matrix(A.ColumnCount, B.ColumnCount);

            for (int j = 0; j < Solution.ColumnCount; j++)
            {
                for (int i = 0; i < Solution.RowCount; i++)
                {
                    Solution[i, j] = ClonB[i, j];
                }
            }

            return Solution;


        }

        #endregion

        #region Singular value decomposition (SVD) of A.


        /// <summary>
        /// Computes the minimum-norm solution to a real linear least squares problem: minimize 2-norm(|| A*X - B||) 
        /// using the singular value decomposition (SVD) of A.
        /// The matrix A can be rank-deficient.
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution.</returns>
        public Matrix SVDdcSolve(Matrix A, Matrix B)
        {

            // (input) DOUBLE PRECISION
            // RCOND is used to determine the effective rank of A.
            // Singular values S(i) .LE. RCOND*S(1) are treated as zero.
            // If RCOND .LT. 0, machine precision is used instead.            
            double RCond = -1.0;

            return this.SVDdcSolve(A, B, RCond);
        }


        /// <summary>
        /// Computes the minimum-norm solution to a real linear least squares problem: minimize 2-norm(|| A*X - B||) 
        /// using the singular value decomposition (SVD) of A.
        /// The matrix A can be rank-deficient.
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <param name="rcond">
        /// rcond is used to determine the effective rank of A.
        /// Singular values S(i) .LE. rcond*S(1) are treated as zero.
        /// </param>
        /// <returns>A matrix containing the solution.</returns>
        public Matrix SVDdcSolve(Matrix A, Matrix B, double rcond)
        {

            if (this._dgelsd == null) this._dgelsd = new DGELSD();

            Matrix AClon = A.Clone();
            double[] AClonData = AClon.Data;
            Matrix ClonB = new Matrix(Math.Max(A.RowCount, A.ColumnCount), B.ColumnCount);
            double[] ClonData = ClonB.Data;

            for (int j = 0; j < ClonB.ColumnCount; j++)
            {
                for (int i = 0; i < B.RowCount; i++)
                {
                    ClonB[i, j] = B[i, j];
                }
            }

            // (output) DOUBLE PRECISION array, dimension (min(M,N))
            //The singular values of A in decreasing order.
            //The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            double[] S = new double[Math.Min(A.RowCount, A.ColumnCount)];

            //// (input) DOUBLE PRECISION
            //// RCOND is used to determine the effective rank of A.
            //// Singular values S(i) .LE. RCOND*S(1) are treated as zero.
            //// If RCOND .LT. 0, machine precision is used instead.            
            //double RCond = -1.0;
            // (output) INTEGER
            // The effective rank of A, i.e., the number of singular values
            // which are greater than RCOND*S(1).
            int Rank = 0;
            int Info = 0;
            //int LWork
            double[] Work = new double[1];
            int LWork = -1;
            int LIWork = this.CalculateLIWORK(A.RowCount, A.ColumnCount);
            int[] IWork = new int[LIWork];

            //Calculamos LWORK ideal
            this._dgelsd.Run(A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref S, 0, rcond, ref Rank, ref Work, 0, LWork, ref IWork, 0, ref Info);
            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                this._dgelsd.Run(A.RowCount, A.ColumnCount, B.ColumnCount, ref AClonData, 0, A.RowCount, ref ClonData, 0, ClonB.RowCount, ref S, 0, rcond, ref Rank, ref Work, 0, LWork, ref IWork, 0, ref Info);
            }
            else
            {

                //Error
            }


            #region Error
            // = 0:  successful exit
            // .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
            // .GT. 0:  the algorithm for computing the SVD failed to converge;
            // if INFO = i, i off-diagonal elements of an intermediate
            // bidiagonal form did not converge to zero.
            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The algorithm for computing the SVD failed to converge.");
            }

            #endregion




            Matrix Solution = new Matrix(A.ColumnCount, B.ColumnCount);

            for (int j = 0; j < Solution.ColumnCount; j++)
            {
                for (int i = 0; i < Solution.RowCount; i++)
                {
                    Solution[i, j] = ClonB[i, j];
                }
            }

            return Solution;

        }



        /// <summary>
        /// Calcula LIWORK para ser usado en IWORK. Este programa se encuentra en el foro de preguntas de Lapack
        /// </summary>
        /// <param name="ARows">Number of A rows </param>
        /// <param name="AColumns">Number of A columns </param>
        /// <returns>LIWORK </returns>
        private int CalculateLIWORK(int ARows, int AColumns)
        {
            int LIwork = 0;

            double TWO = 2.0;
            ILAENV SubILAENV = new ILAENV();

            int MinMN = Math.Min(ARows, AColumns);
            MinMN = Math.Min(1, MinMN);

            int SMLSIZ = SubILAENV.Run(9, "DGELSD", " ", 0, 0, 0, 0);
            //NLVL=MAX(INT(LOG(DBLE(MinMN)/DBLE(SMLSIZ+1))/LOG(TWO))+ 1, 0)  CODIGO FORTRAN
            int NLVL = Math.Max(Convert.ToInt32(Math.Truncate(Math.Log(Convert.ToDouble(MinMN) / Convert.ToDouble(SMLSIZ + 1)) / Math.Log(TWO))) + 1, 0);

            LIwork = 3 * MinMN * NLVL + 11 + MinMN;

            return LIwork;
        }


        #endregion




        #endregion

    }


}
