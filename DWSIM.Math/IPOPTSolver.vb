'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Imports Cureos.Numerics

Namespace MathEx.Optimization

    Public Class IPOPTSolver

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

        End Sub

        ''' <summary>
        ''' Minimizes a function value using IPOPT solver.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of doubles, returns the value of the function.</param>
        ''' <param name="functiongradient">Optional. g(x) where x is a vector of doubles, returns the value of the gradient of the function with respect to each variable.</param>
        ''' <param name="vars">initial values for x</param>
        ''' <param name="lbounds">lower bounds for x</param>
        ''' <param name="ubounds">upper bounds for x</param>
        ''' <returns>vector of variables corresponding to the function's minimum value.</returns>
        Public Function Solve(functionbody As Func(Of Double(), Double), functiongradient As Func(Of Double(), Double()), vars As Double(), lbounds As Double(), ubounds As Double()) As Double()

            Dim obj As Double = 0.0#
            Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found

            Solutions = New List(Of Double())
            FunctionValues = New List(Of Double)

            fxb = functionbody
            fxg = functiongradient

            Using problem As New Ipopt(vars.Length, lbounds, ubounds, 0, Nothing, Nothing,
                       0, 0, AddressOf eval_f, AddressOf eval_g,
                       AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                problem.AddOption("tol", Tolerance)
                problem.AddOption("max_iter", MaxIterations)
                problem.AddOption("mu_strategy", "adaptive")
                problem.AddOption("hessian_approximation", "limited-memory")
                problem.AddOption("expect_infeasible_problem", "yes")
                problem.SetIntermediateCallback(AddressOf intermediate)
                status = problem.SolveProblem(vars, obj, Nothing, Nothing, Nothing, Nothing)
                Select Case status
                    Case IpoptReturnCode.Solve_Succeeded,
                            IpoptReturnCode.Solved_To_Acceptable_Level,
                            IpoptReturnCode.Restoration_Failed,
                            IpoptReturnCode.Feasible_Point_Found,
                            IpoptReturnCode.Search_Direction_Becomes_Too_Small,
                            IpoptReturnCode.Infeasible_Problem_Detected,
                            IpoptReturnCode.Maximum_Iterations_Exceeded,
                            IpoptReturnCode.User_Requested_Stop
                        'get solution with lowest function value
                        Return Solutions(FunctionValues.IndexOf(FunctionValues.Min))
                    Case Else
                        Throw New Exception("IPOPT failed to converge.")
                End Select
            End Using

        End Function

        Private Function FunctionGradient(ByVal x() As Double) As Double()

            Dim epsilon As Double = 0.001

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim j, k As Integer

            For j = 0 To x.Length - 1
                For k = 0 To x.Length - 1
                    x1(k) = x(k)
                    x2(k) = x(k)
                Next
                If x(j) <> 0.0# Then
                    x1(j) = x(j) * (1.0# + epsilon)
                    x2(j) = x(j) * (1.0# - epsilon)
                Else
                    x1(j) = x(j) + epsilon
                    x2(j) = x(j) - epsilon
                End If
                f1 = fxb.Invoke(x1)
                f2 = fxb.Invoke(x2)
                g(j) = (f2 - f1) / (x2(j) - x1(j))
            Next

            Return g

        End Function

        'IPOPT

        Private Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean

            Dim fval As Double = fxb.Invoke(x)

            Solutions.Add(x)
            FunctionValues.Add(fval)

            obj_value = fval

            Return True

        End Function

        Private Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean

            Dim g As Double()

            If fxg IsNot Nothing Then
                g = fxg.Invoke(x)
            Else
                g = FunctionGradient(x)
            End If

            grad_f = g

            Return True

        End Function

        Private Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean

            Return True

        End Function

        Private Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
         ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            Return False

        End Function

        Private Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
         ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean

            Return False

        End Function

        Private Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                                     ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                                     ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                                     ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean

            _Iterations += 1

            objval0 = objval
            objval = obj_value

            If Math.Abs(objval - objval0) <= Tolerance And _Iterations > 20 Then
                Return False
            Else
                Return True
            End If

        End Function

    End Class


End Namespace

