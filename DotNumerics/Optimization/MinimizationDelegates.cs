#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization
{
    /// <summary>
    /// Evaluates the function to be minimized.
    /// </summary>
    /// <param name="X">Vector of length N at which point the function is evaluated, where N id the dimension of the problem.</param>
    /// <returns>The computed function value at the point X.</returns>
    public delegate double OptMultivariateFunction(double[] X);

    ///// <summary>
    ///// Evaluates the function to be minimized and the gradient.
    ///// </summary>
    ///// <param name="X">Vector of length N at which point the function is evaluated, where N id the dimension of the problem.</param>
    ///// <param name="function">The computed function value at the point X.</param>
    ///// <param name="gradient">The computed gradient value at the point X.</param>
    //public delegate void MinimizationFunctionAndGradient(double[] X, ref double function, double[] gradient );


    /// <summary>
    /// Evaluates the gradient of the function to be minimized.
    /// </summary>
    /// <param name="X">Vector of length N at which point the function is evaluated, where N id the dimension of the problem.</param>
    /// <returns>The gradient of the function to be minimized.</returns>
    public delegate double[] OptMultivariateGradient(double[] X);

}
