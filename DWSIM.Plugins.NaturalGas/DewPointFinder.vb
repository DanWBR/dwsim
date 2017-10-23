Imports DWSIM.Thermodynamics.PropertyPackages
Imports Cureos.Numerics
Imports System.Linq
Imports DWSIM.ExtensionMethods

Public Class DewPointFinder

    Private proppack As PropertyPackage
    Private P As Double = 0.0
    Private Tw As Double = 0.0
    Private wid As Integer = 0

    Dim vznw, vzw, vz0 As Double()

    Public Function CalcDewPoints(ByVal Vz As Double(), ByVal Pressure As Double, ByVal PP As PropertyPackage) As Dictionary(Of String, Double)

        vz0 = Vz.Clone

        Dim n As Integer = Vz.Length - 1

        P = Pressure
        proppack = PP

        If PP.RET_VNAMES.Contains("Water") Then
            wid = PP.RET_VNAMES.ToList.IndexOf("Water")
        Else
            wid = -1
        End If

        'get the current composition, check if there is water and create a new, "dry" composition

        'vz = current composition
        'vznw = dry composition

        vznw = PP.RET_NullVector
        vzw = PP.RET_NullVector

        Dim i As Integer = 0

        If wid <> -1 Then
            If Vz(wid) <> 0.0# Then
                'water is present
                For i = 0 To n
                    If i <> wid Then
                        vznw(i) = Vz(i) / (1 - Vz(wid))
                        vzw(i) = 0.0#
                    Else
                        vznw(i) = 0.0#
                        vzw(i) = 1.0#
                    End If
                Next
            End If
        Else
            'if there is no water, clone the current composition.
            vznw = Vz.Clone
        End If

        'calculate hydrocarbon dew point with no water

        Dim hcdp As Double = 0
        Dim idwdp As Double = 0
        Dim wdp As Double = 0

        Dim results As Object = Nothing

        Try
            results = PP.FlashBase.Flash_PV(vznw, P, 1.0, 300.0, PP)
            hcdp = results(4)
        Catch ex As Exception
        End Try

        idwdp = PP.AUX_TSATi(Vz(wid) * P, wid)

        Try
            results = PP.FlashBase.Flash_PV(Vz, P, 1.0, 300.0, PP)
            Dim vx = results(2)
            If vx(wid) > 0.99 Then
                wdp = results(4)
            Else
                hcdp = results(4)
                Dim obj As Double = 0.0#
                Dim status As IpoptReturnCode = IpoptReturnCode.Feasible_Point_Found
                Using problem As New Ipopt(1, New Double() {100.0}, New Double() {500.0}, 0, Nothing, Nothing,
                            0, 0, AddressOf eval_f, AddressOf eval_g,
                            AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                    problem.AddOption("tol", 0.00000000000001)
                    problem.AddOption("max_iter", 1000)
                    problem.AddOption("mu_strategy", "adaptive")
                    problem.AddOption("hessian_approximation", "limited-memory")
                    problem.SetIntermediateCallback(AddressOf intermediate)
                    status = problem.SolveProblem(New Double() {500}, obj, Nothing, Nothing, Nothing, Nothing)
                End Using
                wdp = Tw
            End If
        Catch ex As Exception
        End Try

        Try
            If wdp > 0.0# Then
                Dim phi_v = proppack.DW_CalcFugCoeff(vz0, Tw, P, State.Vapor)
                Dim phi_l = proppack.DW_CalcFugCoeff(vzw, Tw, P, State.Liquid)
                Dim newK = phi_l.DivideY(phi_v)
                results = PP.FlashBase.Flash_PV(Vz, P, 1.0, wdp, PP, True, newK)
                Dim vx = results(2)
                If vx(wid) > 0.99 Then wdp = results(4)
            End If
        Catch ex As Exception
        End Try

        Dim resdic As New Dictionary(Of String, Double)

        If hcdp > 0.0 Then resdic.Add("H", hcdp)
        If wdp > 0.0 Then resdic.Add("W", wdp)
        If idwdp > 0.0 Then resdic.Add("IW", idwdp)

        Return resdic

    End Function

    Public Function FunctionValue(ByVal x() As Double) As Double

        Tw = x(0)

        Dim phi_v(), phi_l() As Double

        phi_v = proppack.DW_CalcFugCoeff(vz0, Tw, P, State.Vapor)
        phi_l = proppack.DW_CalcFugCoeff(vzw, Tw, P, State.Liquid)

        Dim fval As Double = ((vzw(wid) * phi_l(wid) - vz0(wid) * phi_v(wid))) ^ 2

        Return fval

    End Function


    Public Function FunctionGradient(ByVal x() As Double) As Double()

        Dim epsilon As Double = 0.01

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
            f1 = FunctionValue(x1)
            f2 = FunctionValue(x2)
            g(j) = (f2 - f1) / (x2(j) - x1(j))
        Next

        Return g

    End Function

    Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
        Dim fval As Double = FunctionValue(x)
        obj_value = fval
        Return True
    End Function

    Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
        Dim g As Double() = FunctionGradient(x)
        grad_f = g
        Return True
    End Function

    Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
        Return True
    End Function

    Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
     ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        Return False
    End Function

    Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
     ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        Return False
    End Function

    Public Function intermediate(ByVal alg_mod As IpoptAlgorithmMode, ByVal iter_count As Integer, ByVal obj_value As Double,
                         ByVal inf_pr As Double, ByVal inf_du As Double, ByVal mu As Double,
                         ByVal d_norm As Double, ByVal regularization_size As Double, ByVal alpha_du As Double,
                         ByVal alpha_pr As Double, ByVal ls_trials As Integer) As Boolean
        Return True
    End Function


End Class
