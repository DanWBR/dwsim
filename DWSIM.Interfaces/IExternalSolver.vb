'    DWSIM Interface definitions
'    Copyright 2020-2021 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Public Interface IExternalSolverIdentification

    ReadOnly Property ID As String

    ReadOnly Property Description As String

    ReadOnly Property DisplayText As String

    ReadOnly Property Category As Enums.ExternalSolverCategory

    ReadOnly Property SupportsMultiThreading As Boolean

End Interface

Public Interface IExternalLinearSystemSolver

    ''' <summary>
    ''' Solves a system of linear equations.
    ''' </summary>
    ''' <param name="A">Left-side matrix</param>
    ''' <param name="B">Right-side vector</param>
    ''' <returns>The solution vector.</returns>
    Function Solve(A As Double(,), B As Double()) As Double()

End Interface

Public Interface IExternalNonLinearSystemSolver

    ''' <summary>
    ''' Solves a system of non-linear equations.
    ''' </summary>
    ''' <param name="functionbody">Function which gets the current x values and returns the function values.</param>
    ''' <param name="functiongradient">Optional. Function which gets the current x values and returns the gradient values.</param>
    ''' <param name="iterationcallback">Function called n each iteration step. First argument is the x-vector, second argument is f-vector. Return true to stop the iterations or false to continue.</param>
    ''' <param name="vars">Initial value of the variables.</param>
    ''' <param name="maxits">Maximum iterations.</param>
    ''' <param name="tolerance">Tolerance for solution.</param>
    ''' <returns>The solution vector.</returns>
    Function Solve(functionbody As Func(Of Double(), Double()),
                   functiongradient As Func(Of Double(), Double(,)),
                   iterationcallback As Func(Of Double(), Double(), Boolean),
                   vars As Double(),
                   maxits As Integer,
                   tolerance As Double) As Double()

End Interface

Public Interface IExternalNonLinearMinimizationSolver

    ReadOnly Property SupportsBoundedVariables As Boolean

    ReadOnly Property IterationsTaken As Integer

    ReadOnly Property FinalValue As Double

    ''' <summary>
    ''' Minimizes a black-box non-linear equation.
    ''' </summary>
    ''' <param name="functionbody">Function which gets the current x values and returns the function value.</param>
    ''' <param name="functiongradient">Optional. Function which gets the current x values and returns the gradient values.</param>
    ''' <param name="iterationcallback">Function called n each iteration step. First argument is the x-vector, second argument is function value. Return true to stop the iterations or false to continue.</param>
    ''' <param name="vars">Initial value of the variables.</param>
    ''' <param name="lbounds">Optional. Lower bounds for variables.</param>
    ''' <param name="ubounds">Optional. Upper bounds for variables.</param>
    ''' <param name="maxits">Maximum iterations.</param>
    ''' <param name="tolerance">Tolerance for solution.</param>
    ''' <returns>The solution vector corresponding to the function minimum.</returns>
    Function Solve(functionbody As Func(Of Double(), Double),
                   functiongradient As Func(Of Double(), Double()),
                   iterationcallback As Func(Of Double(), Double, Boolean),
                   vars As Double(),
                   lbounds As Double(),
                   ubounds As Double(),
                   maxits As Integer,
                   tolerance As Double) As Double()

End Interface

Public Interface IExternalODESolver

    ''' <summary>
    ''' Method that initializes the ODE to solve.
    ''' </summary>
    ''' <param name="odefunc">A function that evaluates the right side of the differential equations.</param>
    ''' <param name="n">The number of differential equations.</param>
    ''' <param name="x0">The initial value for the independent variable.</param>
    ''' <param name="y0">A vector of size N containing the initial conditions. N is the number of differential equations.</param>
    Sub InitializeODEs(odefunc As Func(Of Double, Double(), Double()), n As Integer, x0 As Double, y0 As Double())

    ''' <summary>
    ''' Computes the solution of the ordinary differential equations.
    ''' </summary>
    ''' <param name="y0">A vector of size N containing the initial conditions. N is the number of differential equations.</param>
    ''' <param name="x0">The initial independent variable value.</param>
    ''' <param name="deltax">The step for the interval of integration (x0, x0+deltax, x0+2*deltax,...,xf).</param>
    ''' <param name="xf">The final independent variable value.</param>
    ''' <param name="odesolution">A delegate where to return the solution.</param>
    ''' <param name="tolerance">Tolerance for solution.</param>
    Sub Solve(y0 As Double(), x0 As Double, deltax As Double, xf As Double, odesolution As Action(Of Double, Double()), tolerance As Double)

End Interface