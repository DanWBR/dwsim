'    UNIQUAC Property Package 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Math


Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(UNIQUACPropertyPackage.ClassId)>
    <System.Serializable()> Public Class UNIQUACPropertyPackage

        Inherits PropertyPackages.ActivityCoefficientPropertyPackage

        Public Shadows Const ClassId As String = "5265F953-8825-4a80-9112-A3B68C329E4C"

        Public Property m_uni As Auxiliary.UNIQUAC
            Get
                Return m_act
            End Get
            Set(value As Auxiliary.UNIQUAC)
                m_act = m_uni
            End Set
        End Property

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            Me.m_act = New Auxiliary.UNIQUAC
        End Sub

        Public Sub New()

            MyBase.New(False)

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.ActivityCoefficient

            Me.m_act = New Auxiliary.UNIQUAC

        End Sub

        Public Overrides Sub DisplayEditingForm()

            Dim f As New FormConfigUNIQUAC() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
            f.ShowDialog()

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigUNIQUAC() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

        Public Overrides Function GetModel() As Object
            Return m_uni
        End Function

#Region "    Auxiliary Functions"

        Function RET_VQ() As Object

            Dim subst As Interfaces.ICompound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VQ(i) = subst.ConstantProperties.UNIQUAC_Q
                i += 1
            Next

            Return VQ

        End Function

        Function RET_VR() As Object

            Dim subst As Interfaces.ICompound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VR(i) = subst.ConstantProperties.UNIQUAC_R
                i += 1
            Next

            Return VR

        End Function

        Public Overrides Function CheckMissingInteractionParameters(Vx As Double()) As Boolean

            Dim ipdata(1, 8) As Object

            Dim i1, i2 As Integer
            i1 = 0
            For Each c In CurrentMaterialStream.Phases(0).Compounds.Values
                i2 = 0
                For Each c2 In CurrentMaterialStream.Phases(0).Compounds.Values
                    If c.Name <> c2.Name AndAlso Vx(i1) * Vx(i2) > 0.0 Then
                        ipdata = ExcelAddIn.ExcelIntegration.GetInteractionParameterSet(Me, "UNIQUAC", c.Name, c2.Name)
                        Dim i As Integer, sum As Double
                        sum = 0
                        For i = 2 To 8
                            If ipdata(1, i) IsNot Nothing Then sum += ipdata(1, i)
                        Next
                        If sum = 0.0 Then Throw New Exception(String.Format("UNIQUAC error: missing interaction parameters for binary pair {0}/{1}.", c.Name, c2.Name))
                    End If
                    i2 += 1
                Next
                i1 += 1
            Next

        End Function

#End Region

        Dim actu(5), actn(5) As Double
        Dim ppu As PropertyPackages.UNIQUACPropertyPackage
        Dim uniquac As PropertyPackages.Auxiliary.UNIQUAC
        Dim ms As Streams.MaterialStream
        Dim ppuf As MODFACPropertyPackage, unifac As Auxiliary.NISTMFAC

        Public Sub EstimateMissingInteractionParameters(verbose As Boolean)

            For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
                If Not m_uni.InteractionParameters.ContainsKey(cp.Name) Then
                    m_uni.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
                End If
            Next

            For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
                If m_uni.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In Flowsheet.SelectedCompounds.Values
                        If cp.Name <> cp2.Name Then
                            If Not m_uni.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                If m_uni.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not m_uni.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        m_uni.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.UNIQUAC_IPData)
                                    End If
                                End If
                            End If
                        End If
                    Next
                End If
            Next

            Dim x(1) As Double

            For Each item1 In m_uni.InteractionParameters
                For Each ipset In item1.Value
                    If ipset.Value.A12 = 0 And ipset.Value.A21 = 0 Then

                        Dim comp1, comp2 As ConstantProperties
                        comp1 = Flowsheet.SelectedCompounds(item1.Key)
                        comp2 = Flowsheet.SelectedCompounds(ipset.Key)

                        Try

                            ppu = New PropertyPackages.UNIQUACPropertyPackage
                            ppuf = New PropertyPackages.MODFACPropertyPackage

                            ms = New Streams.MaterialStream("", "")
                            uniquac = New PropertyPackages.Auxiliary.UNIQUAC
                            unifac = New PropertyPackages.Auxiliary.NISTMFAC

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

                            ppu.CurrentMaterialStream = ms
                            ppuf.CurrentMaterialStream = ms

                            Dim T1 = 298.15

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

                            x(0) = 0.0
                            x(1) = 0.0

                            If x(0) = 0 Then x(0) = 0
                            If x(1) = 0 Then x(1) = 0

                            Dim initval2() As Double = New Double() {x(0), x(1)}
                            Dim lconstr2() As Double = New Double() {-10000.0#, -10000.0#}
                            Dim uconstr2() As Double = New Double() {+10000.0#, +10000.0#}
                            Dim finalval2() As Double = Nothing

                            Dim solver As New MathEx.Optimization.IPOPTSolver
                            solver.MaxIterations = 100
                            solver.Tolerance = 0.000001
                            finalval2 = solver.Solve(AddressOf FunctionValue, Nothing, initval2, lconstr2, uconstr2)

                            Dim avgerr As Double = 0.0#
                            For i As Integer = 0 To 5
                                avgerr += (actn(i) - actu(i)) / actu(i) * 100 / 6
                            Next

                            ipset.Value.A12 = finalval2(0)
                            ipset.Value.A21 = finalval2(1)

                            If verbose Then

                                Console.WriteLine(String.Format("Estimated UNIQUAC IP set for {0}/{1}: {2:N2}/{3:N2}/{4}",
                                                         comp1.Name, comp2.Name, finalval2(0), finalval2(1)))

                            End If

                        Catch ex As Exception

                            If verbose Then
                                Console.WriteLine(String.Format("Error estimating UNIQUAC IP set for {0}/{1}: {2}",
                                                         comp1.Name, comp2.Name, ex.ToString()))

                            End If

                        End Try

                    End If
                Next
            Next

        End Sub

        Private Function FunctionValue(ByVal x() As Double) As Double

            Dim a1(1), a2(1), a3(1) As Double

            uniquac.InteractionParameters.Clear()
            uniquac.InteractionParameters.Add(ppu.RET_VIDS()(0), New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
            uniquac.InteractionParameters(ppu.RET_VIDS()(0)).Add(ppu.RET_VIDS()(1), New PropertyPackages.Auxiliary.UNIQUAC_IPData())
            uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A12 = x(0)
            uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A21 = x(1)
            uniquac.InteractionParameters.Add(ppu.RET_VIDS()(1), New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
            uniquac.InteractionParameters(ppu.RET_VIDS()(1)).Add(ppu.RET_VIDS()(0), New PropertyPackages.Auxiliary.UNIQUAC_IPData())
            uniquac.InteractionParameters(ppu.RET_VIDS()(1))(ppu.RET_VIDS()(0)).A12 = x(1)
            uniquac.InteractionParameters(ppu.RET_VIDS()(1))(ppu.RET_VIDS()(0)).A21 = x(0)

            If GlobalSettings.Settings.EnableParallelProcessing Then
                Try
                    Dim task1 As Task = TaskHelper.Run(Sub()
                                                           a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                       End Sub)
                    Dim task2 As Task = TaskHelper.Run(Sub()
                                                           a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                       End Sub)
                    Dim task3 As Task = TaskHelper.Run(Sub()
                                                           a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                       End Sub)
                    Task.WaitAll(task1, task2, task3)
                Catch ae As AggregateException
                    Throw ae.Flatten().InnerException
                End Try
            Else
                a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
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

End Namespace


