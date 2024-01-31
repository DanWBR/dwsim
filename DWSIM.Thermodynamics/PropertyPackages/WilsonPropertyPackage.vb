Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages

Public Class WilsonPropertyPackage

    Inherits DWSIM.Thermodynamics.PropertyPackages.ActivityCoefficientPropertyPackage

    Public Property WilsonM As WilsonModel

    Public Overrides ReadOnly Property DisplayName As String = "Wilson"

    Public Overrides ReadOnly Property DisplayDescription As String =
            "Uses the Wilson Model to calculate liquid phase activity coefficients."

    Sub New()

        MyBase.New(False)

        WilsonM = New WilsonModel()

        m_act = WilsonM

        ComponentName = "Wilson"
        ComponentDescription = "Wilson Activity Coefficient Property Package"

        IsConfigurable = True
        _packagetype = PropertyPackages.PackageType.ActivityCoefficient

    End Sub

    Public Overrides Function ReturnInstance(typename As String) As Object

        Return New WilsonPropertyPackage()

    End Function

    Public Overrides Sub DisplayEditingForm()

        Dim f As New WilsonPPEditor With {.WilsonPP = Me, ._comps = Flowsheet.SelectedCompounds}
        f.Show()

    End Sub

    Public Overrides Function GetEditingForm() As Form

        Return New WilsonPPEditor With {.WilsonPP = Me, ._comps = Flowsheet.SelectedCompounds}

    End Function

    Public Overrides Function GetModel() As Object

        Return WilsonM

    End Function

    Public Overrides Function CheckMissingInteractionParameters(Vx As Double()) As Boolean

        Dim ipdata(1, 8) As Object

        Dim i1, i2 As Integer
        i1 = 0
        For Each c In CurrentMaterialStream.Phases(0).Compounds.Values
            i2 = 0
            For Each c2 In CurrentMaterialStream.Phases(0).Compounds.Values
                If c.Name <> c2.Name AndAlso Vx(i1) * Vx(i2) > 0.0 Then
                    ipdata = ExcelAddIn.ExcelIntegrationNoAttr.GetInteractionParameterSet(Me, "Wilson", c.ConstantProperties.CAS_Number, c2.ConstantProperties.CAS_Number)
                    Dim i As Integer, sum As Double
                    sum = 0
                    For i = 2 To 3
                        If ipdata(1, i) IsNot Nothing Then sum += ipdata(1, i)
                    Next
                    If sum = 0.0 Then Throw New Exception(String.Format("Wilson error: missing interaction parameters for binary pair {0}/{1}.", c.Name, c2.Name))
                End If
                i2 += 1
            Next
            i1 += 1
        Next

        Return False

    End Function


    Public Overrides Function GetArguments() As Object

        Dim CASIDs = RET_VCAS()

        Dim MolarVolumes(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double

        Dim i As Integer = 0

        For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
            MolarVolumes(i) = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(i, 298.15)
            i += 1
        Next

        Return New Object() {CASIDs, MolarVolumes} 'm3/kmol

    End Function

    Dim actu(5), actn(5), T1 As Double
    Dim ppu As WilsonPropertyPackage
    Dim ms As Streams.MaterialStream
    Dim ppuf As UNIFACPropertyPackage, unifac As Auxiliary.Unifac

    Private Shared skip As Boolean = False

    Public Overrides Sub RunPostMaterialStreamSetRoutine()
        If AutoEstimateMissingNRTLUNIQUACParameters Then
            EstimateMissingInteractionParameters(True)
        End If
    End Sub

    Public Sub EstimateMissingInteractionParameters(verbose As Boolean)

        If Flowsheet Is Nothing Then
            Flowsheet = CurrentMaterialStream.Flowsheet
            If Flowsheet Is Nothing Then Exit Sub
        End If

        Dim CAS_IDS = Flowsheet.SelectedCompounds.Values.Select(Function(c) c.CAS_Number).ToList()
        Dim NAMES = Flowsheet.SelectedCompounds.Keys.ToList()

        Dim i, j As Integer
        i = 0
        For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
            If Not WilsonM.BIPs.ContainsKey(CAS_IDS(i)) Then
                WilsonM.BIPs.Add(CAS_IDS(i), New Dictionary(Of String, Double()))
            End If
            i += 1
        Next

        i = 0
        For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
            If WilsonM.BIPs.ContainsKey(CAS_IDS(i)) Then
                j = 0
                For Each cp2 As ConstantProperties In Flowsheet.SelectedCompounds.Values
                    If cp.Name <> cp2.Name Then
                        If Not WilsonM.BIPs(CAS_IDS(i)).ContainsKey(CAS_IDS(j)) Then
                            If WilsonM.BIPs.ContainsKey(CAS_IDS(j)) Then
                                If Not WilsonM.BIPs(CAS_IDS(j)).ContainsKey(CAS_IDS(i)) Then
                                    WilsonM.BIPs(CAS_IDS(i)).Add(CAS_IDS(j), New Double() {0.0, 0.0})
                                End If
                            End If
                        End If
                    End If
                    j += 1
                Next
            End If
            i += 1
        Next

        Dim x(1) As Double

        For Each item1 In WilsonM.BIPs
            For Each ipset In item1.Value

                Dim idx1 = CAS_IDS.IndexOf(item1.Key)
                Dim idx2 = CAS_IDS.IndexOf(ipset.Key)

                If idx1 >= 0 And idx2 >= 0 Then

                    If ipset.Value(0) = 0 And ipset.Value(1) = 0 And
                        Flowsheet.SelectedCompounds.ContainsKey(NAMES(idx1)) And
                        Flowsheet.SelectedCompounds.ContainsKey(NAMES(idx2)) Then

                        Dim comp1, comp2 As ConstantProperties
                        comp1 = Flowsheet.SelectedCompounds(NAMES(idx1))
                        comp2 = Flowsheet.SelectedCompounds(NAMES(idx2))

                        If comp1.Normal_Boiling_Point < 200 Or comp2.Normal_Boiling_Point < 200 Then

                            ipset.Value(0) = 0.01
                            ipset.Value(1) = 0.01

                        Else

                            Try

                                ppu = New WilsonPropertyPackage
                                ppuf = New UNIFACPropertyPackage

                                ms = New Streams.MaterialStream("", "")
                                unifac = New Auxiliary.Unifac

                                With ms
                                    For Each phase In ms.Phases.Values
                                        With phase
                                            .Compounds.Add(comp1.Name, New Compound(comp1.Name, ""))
                                            .Compounds(comp1.Name).ConstantProperties = comp1
                                            .Compounds.Add(comp2.Name, New Compound(comp2.Name, ""))
                                            .Compounds(comp2.Name).ConstantProperties = comp2
                                        End With
                                    Next
                                End With

                                skip = True

                                ppu.CurrentMaterialStream = ms
                                ppuf.CurrentMaterialStream = ms

                                skip = False

                                T1 = 298.15

                                Dim a1(1), a2(1), a3(1) As Double

                                Dim task1 As Task = TaskHelper.Run(Sub()
                                                                       a1 = unifac.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                                   End Sub)
                                Dim task2 As Task = TaskHelper.Run(Sub()
                                                                       a2 = unifac.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                                   End Sub)
                                Dim task3 As Task = TaskHelper.Run(Sub()
                                                                       a3 = unifac.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                                   End Sub)
                                Task.WaitAll(task1, task2, task3)

                                actu(0) = a1(0)
                                actu(1) = a2(0)
                                actu(2) = a3(0)
                                actu(3) = a1(1)
                                actu(4) = a2(1)
                                actu(5) = a3(1)

                                Dim x1s = New Double() {-1000, -500, 0, 500, 1000}
                                Dim x2s = New Double() {-1000, -500, 0, 500, 1000}

                                Dim fvals As New List(Of Double)
                                Dim xvals As New List(Of Double())

                                Dim OK As Boolean = False

                                For Each x1 In x1s
                                    For Each x2 In x2s

                                        Dim initval2() As Double = New Double() {x1, x2}
                                        Dim lconstr2() As Double = New Double() {-10000, -10000}
                                        Dim uconstr2() As Double = New Double() {+10000, +10000}
                                        Dim finalval2() As Double = Nothing

                                        Dim solver As New MathEx.Optimization.IPOPTSolver
                                        solver.MaxIterations = 100
                                        solver.Tolerance = 0.0001
                                        finalval2 = solver.Solve(AddressOf FunctionValue, Nothing, initval2, lconstr2, uconstr2)

                                        Dim avgerr As Double = 0.0#
                                        For k As Integer = 0 To 5
                                            avgerr += (actn(k) - actu(k)) ^ 2
                                        Next

                                        xvals.Add(finalval2)
                                        fvals.Add(avgerr)

                                        If avgerr < 1.0 Then OK = True

                                        If OK Then Exit For

                                    Next

                                    If OK Then Exit For

                                Next

                                If fvals.Min > 1.0 Then
                                    Throw New Exception("UNIFAC was unable to estimate a valid set of Wilson parameters")
                                End If

                                ipset.Value(0) = xvals(fvals.IndexOf(fvals.Min))(0)
                                ipset.Value(1) = xvals(fvals.IndexOf(fvals.Min))(1)

                                If verbose Then

                                    Console.WriteLine(String.Format("Estimated Wilson IP set for {0}/{1}: {2:N2}/{3:N2}",
                                                                 comp1.Name, comp2.Name, ipset.Value(0), ipset.Value(1)))

                                    If Flowsheet IsNot Nothing Then
                                        Flowsheet.ShowMessage(String.Format("Estimated Wilson IP set for {0}/{1}: {2:N2}/{3:N2}",
                                                                 comp1.Name, comp2.Name, ipset.Value(0), ipset.Value(1)),
                                                                 Interfaces.IFlowsheet.MessageType.Information)
                                    End If

                                End If

                            Catch ex As Exception

                                ipset.Value(0) = 0.0000000001
                                ipset.Value(0) = 0.0000000001

                                'If verbose Then
                                '    Console.WriteLine(String.Format("Error estimating UNIQUAC IP set for {0}/{1}: {2}",
                                '                                 comp1.Name, comp2.Name, ex.ToString()))

                                '    If Flowsheet IsNot Nothing Then
                                '        Flowsheet.ShowMessage(String.Format("Error estimating UNIQUAC IP set for {0}/{1}: {2}",
                                '                                 comp1.Name, comp2.Name, ex.ToString()),
                                '                                 Interfaces.IFlowsheet.MessageType.Information)
                                '    End If

                                'End If

                            End Try

                        End If

                    End If

                End If

            Next
        Next

    End Sub

    Private Function FunctionValue(ByVal x() As Double) As Double

        Dim a1(1), a2(1), a3(1) As Double

        ppu.WilsonM.BIPs.Clear()
        ppu.WilsonM.BIPs.Add(ppu.RET_VCAS()(0), New Dictionary(Of String, Double()))
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0)).Add(ppu.RET_VCAS()(1), New Double() {0.0, 0.0})
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0))(ppu.RET_VCAS()(1))(0) = x(0)
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0))(ppu.RET_VCAS()(1))(1) = x(1)
        ppu.WilsonM.BIPs.Add(ppu.RET_VCAS()(1), New Dictionary(Of String, Double()))
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1)).Add(ppu.RET_VCAS()(0), New Double() {0.0, 0.0})
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1))(ppu.RET_VCAS()(0))(0) = x(1)
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1))(ppu.RET_VCAS()(0))(1) = x(0)

        If GlobalSettings.Settings.EnableParallelProcessing Then
            Try
                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       a1 = ppu.WilsonM.CalcActivityCoefficients(T1, New Double() {0.25, 0.75}, ppu.GetArguments())
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       a2 = ppu.WilsonM.CalcActivityCoefficients(T1, New Double() {0.5, 0.5}, ppu.GetArguments())
                                                   End Sub)
                Dim task3 As Task = TaskHelper.Run(Sub()
                                                       a3 = ppu.WilsonM.CalcActivityCoefficients(T1, New Double() {0.75, 0.25}, ppu.GetArguments())
                                                   End Sub)
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try
        Else
            a1 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.25, 0.75}, ppu.GetArguments())
            a2 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.5, 0.5}, ppu.GetArguments())
            a3 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.75, 0.25}, ppu.GetArguments())
        End If

        actn(0) = a1(0)
        actn(1) = a2(0)
        actn(2) = a3(0)
        actn(3) = a1(1)
        actn(4) = a2(1)
        actn(5) = a3(1)

        Dim fval As Double = 0.0#
        For i As Integer = 0 To 5
            fval += (actn(i) - actu(i)) ^ 2
        Next

        Return fval

    End Function

End Class
