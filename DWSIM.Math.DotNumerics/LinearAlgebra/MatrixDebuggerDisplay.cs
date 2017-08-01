#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Esta clase manda la informacion de cada elemento de la matriz para ser visualizada
    /// </summary>
    internal class MatrixDebuggerDisplay
    {
        private BaseMatrix MeMatrix;
        public MatrixDebuggerDisplay(BaseMatrix matrix)
        {
            this.MeMatrix = matrix;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public double[,] ElementList
        {
            get
            {
                double[,] TempElementList = new double[this.MeMatrix.RowCount, this.MeMatrix.ColumnCount];

                for (int j = 0; j < this.MeMatrix.ColumnCount; j++)
                {
                    for (int i = 0; i < this.MeMatrix.RowCount; i++)
                    {
                        TempElementList[i, j] = this.MeMatrix[i, j];
                    }
                }
                return TempElementList;
            }
        }

    }


    ///// <summary>
    ///// Esta clase es para mostrar los elementos de la materiz, cada 
    ///// objeto almacena los indices y el valor de un elemento de la matriz
    ///// </summary>
    //[DebuggerDisplay("{MeValue}", Name = "[{MeRowIndex}, {MeColumnIndex}]")]
    //internal class MatrixElement
    //{
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private int MeRowIndex;
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private int MeColumnIndex;
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private double MeValue;
    //    /// <summary>
    //    /// Inicializa un  objeto que representa un elemento de la matiz 
    //    /// </summary>
    //    /// <param name="rowIndex">El renglon</param>
    //    /// <param name="columnIndex">la columna</param>
    //    /// <param name="value">El valor del elemento</param>
    //    public MatrixElement(int rowIndex, int columnIndex, double value)
    //    {
    //        this.MeValue = value;
    //        this.MeRowIndex = rowIndex;
    //        this.MeColumnIndex = columnIndex;
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public int RowIndex
    //    {
    //        get { return MeRowIndex; }
    //        set
    //        {
    //            MeRowIndex = value;
    //        }
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public int ColumnIndex
    //    {
    //        get { return MeColumnIndex; }
    //        set
    //        {
    //            MeColumnIndex = value;
    //        }
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public double Value
    //    {
    //        get { return this.MeValue; }
    //        set
    //        {
    //            this.MeValue = value;
    //        }
    //    }
    //}


    /// <summary>
    /// Esta clase manda la informacion de cada elemento de la matriz para ser visualizada
    /// </summary>
    internal class MatrixComplexDebuggerDisplay
    {
        private ComplexMatrix MeMatrix;
        public MatrixComplexDebuggerDisplay(ComplexMatrix matrix)
        {
            this.MeMatrix = matrix;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public Complex[,] ElementList
        {
            get
            {
                Complex[,] TempElementList = new Complex[this.MeMatrix.RowCount , this.MeMatrix.ColumnCount];

                for (int j = 0; j < this.MeMatrix.ColumnCount; j++)
                {
                    for (int i = 0; i < this.MeMatrix.RowCount; i++)
                    {
                        TempElementList[i, j] = this.MeMatrix[i, j];
                    }
                }
                return TempElementList;
            }
        }

    }


    ///// <summary>
    ///// Esta clase es para mostrar los elementos de la materiz, cada 
    ///// objeto almacena los indices y el valor de un elemento de la matriz
    ///// </summary>
    //[DebuggerDisplay("{MeValue._real}, {MeValue._imaginary}", Name = "[{MeRowIndex}, {MeColumnIndex}]")]
    //internal class MatrixComplexElement
    //{
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private int MeRowIndex;
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private int MeColumnIndex;
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    private Complex MeValue;
    //    /// <summary>
    //    /// Inicializa un  objeto que representa un elemento de la matiz 
    //    /// </summary>
    //    /// <param name="rowIndex">El renglon</param>
    //    /// <param name="columnIndex">la columna</param>
    //    /// <param name="value">El valor del elemento</param>
    //    public MatrixComplexElement(int rowIndex, int columnIndex, Complex value)
    //    {
    //        this.MeValue = value;
    //        this.MeRowIndex = rowIndex;
    //        this.MeColumnIndex = columnIndex;
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public int RowIndex
    //    {
    //        get { return MeRowIndex; }
    //        set
    //        {
    //            MeRowIndex = value;
    //        }
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public int ColumnIndex
    //    {
    //        get { return MeColumnIndex; }
    //        set
    //        {
    //            MeColumnIndex = value;
    //        }
    //    }
    //    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    //    public Complex Value
    //    {
    //        get { return this.MeValue; }
    //        set
    //        {
    //            this.MeValue = value;
    //        }
    //    }
    //}



    /// <summary>
    /// Esta clase manda la informacion de cada elemento del vector para ser visualizada
    /// </summary>
    internal class VectorDebuggerDisplay
    {
        private Vector MeVector;
        public VectorDebuggerDisplay(Vector TheVector)
        {
            this.MeVector = TheVector;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public double[] ElementList
        {
            get
            {
                return this.MeVector.Data;
            }
        }
    }


    /// <summary>
    /// Esta clase manda la informacion de cada elemento del vector para ser visualizada
    /// </summary>
    internal class VectorComplexDebuggerDisplay
    {
        private ComplexVector MeVector;
        public VectorComplexDebuggerDisplay(ComplexVector TheVector)
        {
            this.MeVector = TheVector;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public Complex[] ElementList
        {
            get
            {
                return this.MeVector.Data;
            }
        }

    }



}
