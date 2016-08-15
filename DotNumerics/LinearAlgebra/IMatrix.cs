#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Interface for a Matrix
    /// </summary>
    public interface IMatrix<T>
    {

        /// <summary>
        /// Returns the number of rows.
        /// </summary>
        int RowCount
        {
            get;
        }

        /// <summary>
        /// Returns the number of columns.
        /// </summary>
        int ColumnCount
        {
            get;
        }

        /// <summary>
        /// Gets or set the value of a element of this Matrix.
        /// </summary>
        /// <param name="row">The row value (zero-based).</param>
        /// <param name="column">The column value (zero-based).</param>
        /// <returns>The Matrix element at (row, column).</returns>
        T this[int row, int column]
        {
            get;
            set;
        }

        /// <summary>
        ///  Copy the elements of this matrix to a rectangular array.
        /// </summary>
        /// <returns>A rectangular array.</returns>
        T[,] CopyToArray();

        /// <summary>
        ///  Copy the elements of this matrix to a jagged array.
        /// </summary>
        /// <returns>A jagged array</returns>
        T[][] CopyToJaggedArray();
    }
}
