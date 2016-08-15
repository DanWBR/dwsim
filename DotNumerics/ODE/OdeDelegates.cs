#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.ODE
{


    /// <summary>
    /// Delegate defining the Ordinary Differential Equations (ODEs)  dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(N)).
    /// </summary>
    /// <param name="t">The independent variable.</param>
    /// <param name="y">Array of size N containing the dependent variable values(y(1),y(2),...,y(N)).</param>
    /// <returns>A vector of size N, f(i) = dy(i)/dt that define the ordinary differential equations system, 
    /// where N is the number of differential equations.</returns>
    public delegate double[] OdeFunction(double t, double[] y);

    /// <summary>
    /// Delegate that compute the Jacobian matrix df/dy (size NxN), as a function of the scalar t and the vector y. 
    /// </summary>
    /// <param name="t">The independent variable.</param>
    /// <param name="y">Array of size N containing the dependent variable values(y(1),y(2),...,y(N)).</param>
    /// <returns>The Jacobian matrix df/dy (size NxN).</returns>
    public delegate double[,] OdeJacobian(double t, double[] y);

    /// <summary>
    /// Delegate used for solution otput.
    /// </summary>
    /// <param name="t">The value of t where the solution is calculated.</param>
    /// <param name="y">A array containing the solution of the differential equations at the value t.</param>
    public delegate void OdeSolution(double t, double[] y);


    //public class ODEDelegates
    //{
    //}
}
