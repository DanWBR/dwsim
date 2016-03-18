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

Imports DWSIM.DrawingTools.GraphicObjects

Public Class FrmStabAn

    Inherits System.Windows.Forms.Form

    Dim mat As DWSIM.SimulationObjects.Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim cp As DWSIM.Utilities.TCP.Methods

    Public su As New DWSIM.SystemsOfUnits.Units
    Public cv As New DWSIM.SystemsOfUnits.Converter
    Public nf As String

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Dim gobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetDesignSurface)
            Me.mat = Frm.Collections.FlowsheetObjectCollection(gobj.Name)

            Dim pr As New DWSIM.SimulationObjects.PropertyPackages.PengRobinsonPropertyPackage

            pr.CurrentMaterialStream = mat

            Dim n As Integer = mat.Phases(0).Compounds.Count - 1

            Dim Vz(n) As Double
            Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim i As Integer = 0
            For Each comp In mat.Phases(0).Compounds.Values
                Vz(i) += comp.FracaoMolar.GetValueOrDefault
                i += 1
            Next

            Dim j, k, l As Integer
            i = 0
            Do
                If Vz(i) = 0 Then j += 1
                i = i + 1
            Loop Until i = n + 1

            Dim VTc(n), Vpc(n), Vw(n), VVc(n), VKij(n, n) As Double
            Dim Vm2(UBound(Vz) - j), VPc2(UBound(Vz) - j), VTc2(UBound(Vz) - j), VVc2(UBound(Vz) - j), Vw2(UBound(Vz) - j), VKij2(UBound(Vz) - j, UBound(Vz) - j)

            VTc = pr.RET_VTC()
            Vpc = pr.RET_VPC()
            VVc = pr.RET_VVC()
            Vw = pr.RET_VW()
            VKij = pr.RET_VKij

            i = 0
            k = 0
            Do
                If Vz(i) <> 0 Then
                    Vm2(k) = Vz(i)
                    VTc2(k) = VTc(i)
                    VPc2(k) = Vpc(i)
                    VVc2(k) = VVc(i)
                    Vw2(k) = Vw(i)
                    j = 0
                    l = 0
                    Do
                        If Vz(l) <> 0 Then
                            VKij2(k, j) = VKij(i, l)
                            j = j + 1
                        End If
                        l = l + 1
                    Loop Until l = n + 1
                    k = k + 1
                End If
                i = i + 1
            Loop Until i = n + 1

            Dim px, py As ArrayList
            px = New ArrayList
            py = New ArrayList
            Dim pc As Object = Nothing
            Try

                pc = Me.cp.CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                Dim res As ArrayList = Me.cp.STABILITY_CURVE(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2, , Convert.ToInt32(Me.TextBox2.Text), Convert.ToDouble(Me.TextBox1.Text))

                i = 0
                Do
                    px.Add(Converter.ConvertFromSI(su.temperature, res(i)(0)))
                    py.Add(Converter.ConvertFromSI(su.pressure, res(i)(1)))
                    i += 1
                Loop Until i = res.Count

                With Me.GraphPvap.GraphPane.Legend
                    .Position = ZedGraph.LegendPos.BottomCenter
                    .Border.IsVisible = False
                    .FontSpec.Size = 10
                    .FontSpec.IsDropShadow = False
                    .Gap = 0
                End With
                With Me.GraphPvap.GraphPane
                    .CurveList.Clear()
                    .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Converter.ConvertFromSI(su.temperature, pc(0))}, New Double() {Converter.ConvertFromSI(su.pressure, pc(1))}, Color.Black, ZedGraph.SymbolType.Circle)
                    .AddCurve(DWSIM.App.GetLocalString("LimitedeEstabilidade"), px.ToArray(GetType(Double)), py.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.None).Line.IsSmooth = True
                    .AxisChange(Me.CreateGraphics)
                End With
                Me.GraphPvap.Invalidate()

            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erronoclculododiagra") & vbCrLf & "Motivo: " & ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Finally
                pr.CurrentMaterialStream = Nothing
                pr = Nothing
            End Try

        Else

            Me.mat = Nothing
            Me.LblSelected.Text = ""

        End If

    End Sub

    Private Sub FrmStabAn_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.Frm = My.Application.ActiveSimulation

        Me.cp = New DWSIM.Utilities.TCP.Methods

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox3.Items.Clear()
        For Each mat2 In Me.Frm.Collections.FlowsheetObjectCollection.Values
            Me.ComboBox3.Items.Add(mat2.GraphicObject.Tag.ToString)
        Next

        If Me.ComboBox3.Items.Count > 0 Then Me.ComboBox3.SelectedIndex = 0

        Me.Text = DWSIM.App.GetLocalString("DWSIMUtilitriosLimit")

        With Me.GraphPvap.GraphPane
            .CurveList.Clear()
            '.AddCurve(Me.ComboBox2.SelectedItem, Me.m_vx, Me.m_vy, Color.Blue, ZedGraph.SymbolType.Circle)
            .Title.Text = ""
            .XAxis.Title.Text = "T / " & su.temperature
            .YAxis.Title.Text = "P / " & su.pressure
            .AxisChange(Me.CreateGraphics)
        End With

    End Sub

End Class