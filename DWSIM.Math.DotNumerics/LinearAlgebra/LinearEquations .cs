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
using System.Drawing;
using DotNumerics.LinearAlgebra.CSLapack;

namespace DotNumerics.LinearAlgebra
{

    /// <summary>
    /// Computes the solution to a system of linear equations.
    /// </summary>
    public sealed class LinearEquations
    {

        #region Fields

        DGESV _dgesv;
        DGBSV _dgbsv;
        DGTSV _dgtsv;

        #endregion


        /// <summary>
        /// Initializes a new instance of the LinearEquations class.
        /// </summary>
        public LinearEquations()
        {

        }

        #region Public LU Solver


        #region General Matrix



        /// <summary>
        /// Computes the solution to a real system of linear equations A * X = B, where A is a general matrix. 
        /// </summary>
        /// <param name="A">The square matrix.</param>
        /// <param name="B">The vector containing the right-hand side of the linear system.</param>
        /// <returns>A vector containing the solution to the linear system of equations.</returns>
        public Vector Solve(Matrix A, Vector B)
        {
            this.CheckDimensions(A, B);

            Matrix Solution = B.Clone();
            Matrix AClon = A.Clone();

            this.SolveInplace(AClon, Solution);

            return Solution.GetColumnVector(0);

        }

        /// <summary>
        /// Computes the solution to a real system of linear equations A * X = B, where A is a general matrix. 
        /// </summary>
        /// <param name="A">The square matrix.</param>
        /// <param name="B">The vector containing the right-hand side of the linear system.</param>
        /// <returns>A vector containing the solution to the linear system of equations.</returns>
        public double[] Solve(double[,] A, double[] B)
        {

            Vector Solution = new Vector(B);
            Matrix AClon = new Matrix(A);

            this.CheckDimensions(AClon, Solution);


            this.SolveInplace(AClon, Solution);

            double[] solutionDate = Solution.Data;

            return solutionDate;

        }


        /// <summary>
        /// Computes the solution to a real system of linear equations A * X = B, where A is a general matrix. 
        /// </summary>
        /// <param name="A">The square matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution to the linear system of equations.</returns>
        public Matrix Solve(Matrix A, Matrix B)
        {
            this.CheckDimensions(A, B);

            Matrix Solution = B.Clone();
            Matrix AClon = A.Clone();

            this.SolveInplace(AClon, Solution);
            

            return Solution;
        }


        /// <summary>
        /// In place, Computes the solution to a real system of linear equations A * X = B
        /// </summary>
        /// <param name="A">The square matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution to the linear system of equations.</returns>
        private void SolveInplace(Matrix A, Matrix B)
        {

            if (this._dgesv == null) this._dgesv = new DGESV();

            this.CheckDimensions(A, B);

            int numberEquations = A.RowCount;
            int numberSolutions = B.ColumnCount;
            double[] AMatrix = A.Data;
            double[] BMatrix = B.Data;

            int[] IPIV = new int[numberEquations];
            int Info = 0;

            this._dgesv.Run(numberEquations, numberSolutions, ref AMatrix, 0, numberEquations, ref IPIV, 0, ref BMatrix, 0, numberEquations, ref Info);


            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
            /// has been completed, but the factor U is exactly
            /// singular, so the solution could not be computed.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("U(" + infoSTg + "," + infoSTg + ") is exactly zero.  The factorization has been completed, but the factor U is exactly singular, so the solution could not be computed.");
            }

            #endregion

        }






        #endregion


        #region BandMatrix


        /// <summary>
        /// Computes the solution to a real system of linear equations
        /// A * X = B, where A is a band matrix.
        /// </summary>
        /// <param name="A">The band matrix.</param>
        /// <param name="B">The vector containing the right-hand side of the linear system.</param>
        /// <returns>A vector containing the solution to the linear system of equations.</returns>
        public Vector Solve(BandMatrix A, Vector B)
        {
            Matrix myB = B;
            Vector solution = this.Solve(A, myB).GetColumnVector(0);
            return this.Solve(A, B);
        }


        /// <summary>
        /// Computes the solution to a real system of linear equations
        /// A * X = B, where A is a band matrix.
        /// </summary>
        /// <param name="A">The band matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution to the linear system of equations.</returns>
        public Matrix Solve(BandMatrix A, Matrix B)
        {
            if (this._dgbsv == null) this._dgbsv = new DGBSV();

            this.CheckDimensions(A, B);

            Matrix Solution = B.Clone();
            Matrix ExtendedMatrix = A.GetBandPackedMatrix();
            double[] BAData = ExtendedMatrix.Data;
            double[] SolutionData = Solution.Data;

            int[] IPIV = new int[A.RowCount];
            int Info = 0;

            this._dgbsv.Run(A.RowCount, A.LowerBandWidth, A.UpperBandWidth, B.ColumnCount, ref BAData, 0, ExtendedMatrix.RowCount, ref IPIV, 0, ref SolutionData, 0, Solution.RowCount, ref Info);


            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
            /// has been completed, but the factor U is exactly
            /// singular, and the solution has not been computed.
            ///</param>

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("U(" + infoSTg + "," + infoSTg + ") is exactly zero.  The factorization has been completed, but the factor U is exactly singular, so the solution could not be computed.");
            }

            #endregion




            return Solution;
        }


        #endregion


        /// <summary>
        /// Computes the solution to a real system of linear equations
        /// A * X = B, where A is a tridiagonal matrix.
        /// </summary>
        /// <param name="A">The tridiagonal matrix.</param>
        /// <param name="B">The matrix containing the right-hand side of the linear system.</param>
        /// <returns>A matrix containing the solution to the linear system of equations.</returns>
        public Matrix Solve(TridiagonalMatrix A, Matrix B)
        {

            if (this._dgtsv == null) this._dgtsv = new DGTSV();

            this.CheckDimensions(A, B);

            Matrix Solution = B.Clone();
            double[] SolutionData = Solution.Data;
            double[] Diagonal;
            double[] SubDiagonal;
            double[] SuperDiagonal;
            A.GetPackedMatrix(out SubDiagonal, out SuperDiagonal, out Diagonal);

            int[] IPIV = new int[A.RowCount];
            int Info = 0;

            this._dgtsv.Run(A.RowCount, B.ColumnCount, ref SubDiagonal, 0, ref Diagonal, 0, ref SuperDiagonal, 0, ref SolutionData, 0, Solution.RowCount, ref Info);


            #region Error
            /// = 0: successful exit
            /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0: if INFO = i, U(i,i) is exactly zero, and the solution
            /// has not been computed.  The factorization has not been
            /// completed unless i = N.
            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("U(" + infoSTg + "," + infoSTg + ") is exactly zero.  and the solution has not been computed.  The factorization has not been completed unless i = N.");
            }

            #endregion

            return Solution;
        }



        private void CheckDimensions(BaseMatrix matrixA, BaseMatrix matrixB)
        {
            if (matrixA.IsSquare!= true)
            {
                throw new System.ArgumentException("Matrix A is not a square matrix.");
            }


            if (matrixA.RowCount != matrixB.RowCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }
        }

        private void CheckDimensions(BaseMatrix matrixA, Vector vectorB)
        {
            if (matrixA.IsSquare != true)
            {
                throw new System.ArgumentException("Matrix A is not a square matrix.");
            }

            if (vectorB.Type != VectorType.Column)
            {
                throw new System.ArgumentException("The vector should be a column vector.");
            }

            if (matrixA.RowCount != vectorB.Length)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }
        }


        #endregion



    }
}
