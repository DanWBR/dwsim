'    Copyright 2020 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Namespace MathEx.OptimizationL

    Public Class HillClimbing

        Public Property Tolerance As Double = 0.0001

        Public Property MaxIterations As Integer = 1000

        Private _Iterations As Integer = 0

        Private fxb As Func(Of Double(), Double)

        Private fxg As Func(Of Double(), Double())

        Private _error As Double

        Private objval, objval0 As Double

        Private Solutions As List(Of Double())

        Private FunctionValues As List(Of Double)

        Public ReadOnly Property Iterations
            Get
                Return _Iterations
            End Get
        End Property

        Sub New()
            LibOptimization.Util.clsRandomXorshiftSingleton.GetInstance.SetDefaultSeed()
        End Sub

        ''' <summary>
        ''' Minimizes a function value using HC algorithm.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of doubles, returns the value of the function.</param>
        ''' <param name="functiongradient">Optional. g(x) where x is a vector of doubles, returns the value of the gradient of the function with respect to each variable.</param>
        ''' <param name="vars">initial values for x</param>
        ''' <param name="lbounds">lower bounds for x</param>
        ''' <param name="ubounds">upper bounds for x</param>
        ''' <returns>vector of variables corresponding to the function's minimum value.</returns>
        Public Function Solve(functionbody As Func(Of Double(), Double), functiongradient As Func(Of Double(), Double()), vars As Double(), Optional lbounds As Double() = Nothing, Optional ubounds As Double() = Nothing) As Double()

            Dim obj As Double = 0.0#

            Solutions = New List(Of Double())
            FunctionValues = New List(Of Double)

            fxb = functionbody
            fxg = functiongradient

            If lbounds Is Nothing Then
                lbounds = vars.Clone()
                For i As Integer = 0 To lbounds.Length - 1
                    lbounds(i) = -1.0E+19
                Next
            End If

            If ubounds Is Nothing Then
                ubounds = vars.Clone()
                For i As Integer = 0 To ubounds.Length - 1
                    ubounds(i) = 1.0E+19
                Next
            End If

            Dim optimization As New LibOptimization.Optimization.clsOptHillClimbing(
                New ObjectiveFunction(functionbody, functiongradient, 0.001, vars.Length))

            'set initialposition
            optimization.InitialPosition = vars

            'set bpundary
            optimization.UpperBounds = ubounds
            optimization.LowerBounds = lbounds

            'init
            optimization.Init()
            If optimization.IsRecentError() = True Then
                Throw New Exception("Optimization error")
            End If

            Dim fval As Double

            'do optimization
            Dim it As Integer = 0
            While (optimization.DoIteration(1) = False)
                it += 1
                If it > MaxIterations Then
                    Throw New Exception("Optimization error - max iterations reached")
                End If
                fval = optimization.Result.Eval
                If fval < Tolerance Then
                    Exit While
                End If
            End While

            'get result
            Return optimization.Result.ToArray()

        End Function

    End Class

End Namespace


