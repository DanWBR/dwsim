Imports DWSIM.Interfaces
Imports System.Linq
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.Thermodynamics.Streams

Public Class FormMEBSummary

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As IFlowsheet

    Private Loaded As Boolean = False

    Private Sub FormMEBSummary_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub FormSimulSettings_DockStateChanged(sender As Object, e As EventArgs) Handles Me.DockStateChanged

        If Not Me.DockHandler Is Nothing OrElse Not Me.DockHandler.FloatPane Is Nothing Then

            ' set the bounds of this form's FloatWindow to our desired position and size

            If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
                Dim floatWin = Me.DockHandler.FloatPane.FloatWindow
                If Not floatWin Is Nothing Then
                    floatWin.SetBounds(floatWin.Location.X, floatWin.Location.Y,
                                       925 * GlobalSettings.Settings.DpiScale, 485 * GlobalSettings.Settings.DpiScale)
                End If
            End If

        End If

    End Sub

    Private Sub FormMEBSummary_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        Dim su = Flowsheet.FlowsheetOptions.SelectedUnitSystem
        Dim nf = Flowsheet.FlowsheetOptions.NumberFormat

        lblName.Text = Flowsheet.FlowsheetOptions.SimulationName

        dgv1.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        dgv2.SelectionMode = DataGridViewSelectionMode.FullRowSelect

        dgv1.Columns(3).HeaderText += " (" + su.heatflow + ")"
        dgv1.Columns(5).HeaderText += " (" + su.massflow + ")"
        dgv1.Columns(6).HeaderText += " (" + su.heatflow + ")"
        dgv2.Columns(2).HeaderText += " (" + su.massflow + ")"
        dgv2.Columns(3).HeaderText += " (" + su.temperature + ")"
        dgv2.Columns(4).HeaderText += " (" + su.pressure + ")"
        dgv2.Columns(5).HeaderText += " (" + su.heatflow + ")"

        dgv1.Columns(3).DefaultCellStyle.Format = nf
        dgv1.Columns(4).DefaultCellStyle.Format = nf
        dgv1.Columns(5).DefaultCellStyle.Format = nf
        dgv1.Columns(6).DefaultCellStyle.Format = nf
        dgv2.Columns(2).DefaultCellStyle.Format = nf
        dgv2.Columns(3).DefaultCellStyle.Format = nf
        dgv2.Columns(4).DefaultCellStyle.Format = nf
        dgv2.Columns(5).DefaultCellStyle.Format = nf

        lblMassFlowUnits.Text = su.massflow
        lblPowerUnits.Text = su.heatflow

        Dim equipments = Flowsheet.SimulationObjects.Values.Where(Function(o) TypeOf o Is UnitOpBaseClass And TypeOf o IsNot IIndicator)

        Dim rtftb As New RichTextBox

        Dim totalE As Double = 0.0

        For Each eq In equipments
            'check efficiency
            Dim eff As Nullable(Of Double)
            Dim props = eq.GetType().GetProperties()
            If props.Where(Function(p) p.Name = "Eficiencia").Count > 0 Then
                eff = Convert.ToDouble(eq.GetType().GetProperty("Eficiencia").GetValue(eq))
            ElseIf props.Where(Function(p) p.Name = "ThermalEfficiency").Count > 0 Then
                eff = Convert.ToDouble(eq.GetType().GetProperty("ThermalEfficiency").GetValue(eq))
            ElseIf props.Where(Function(p) p.Name = "Efficiency").Count > 0 Then
                eff = Convert.ToDouble(eq.GetType().GetProperty("Efficiency").GetValue(eq))
            ElseIf props.Where(Function(p) p.Name = "AdiabaticEfficiency").Count > 0 Then
                eff = Convert.ToDouble(eq.GetType().GetProperty("AdiabaticEfficiency").GetValue(eq))
            Else
                eff = Nothing
            End If
            Dim eb = eq.GetPowerGeneratedOrConsumed().ConvertFromSI(su.heatflow)
            totalE += eb
            Dim mbr = eq.GetMassBalanceResidual().ConvertFromSI(su.massflow)
            Dim ebr = eq.GetEnergyBalanceResidual().ConvertFromSI(su.heatflow)
            Dim text As String = ""
            Try
                rtftb.Rtf = eq.Annotation
                text = rtftb.Text
            Catch ex As Exception
            End Try
            dgv1.Rows.Add(eq.GraphicObject.Tag, eq.GetDisplayName(), text,
                          eb, eff, mbr, ebr)
        Next
        dgv1.Sort(dgv1.Columns(2), System.ComponentModel.ListSortDirection.Descending)
        dgv1.ClearSelection()

        tbresidualE.Text = totalE.ToString(nf)

        Dim streams = Flowsheet.SimulationObjects.Values.Where(Function(o) TypeOf o Is IMaterialStream).Select(Function(o) DirectCast(o, MaterialStream))

        Dim totalM = 0.0

        For Each s In streams
            Dim t = s.GetTemperature().ConvertFromSI(su.temperature)
            Dim p = s.GetPressure().ConvertFromSI(su.pressure)
            Dim ef = s.TotalEnergyFlow.ConvertFromSI(su.heatflow)
            Dim mf = s.GetMassFlow().ConvertFromSI(su.massflow)
            If Not s.GraphicObject.InputConnectors(0).IsAttached Then
                totalM += mf
            End If
            If Not s.GraphicObject.OutputConnectors(0).IsAttached Then
                totalM -= mf
            End If
            Dim text As String = ""
            Try
                rtftb.Rtf = s.Annotation
                text = rtftb.Text
            Catch ex As Exception
            End Try
            dgv2.Rows.Add(s.GraphicObject.Tag, text, mf, t, p, ef)
        Next

        dgv2.Sort(dgv2.Columns(1), System.ComponentModel.ListSortDirection.Descending)
        dgv2.ClearSelection()

        tbresidualM.Text = totalM.ToString(nf)

        Loaded = True

        rtftb.Dispose()
        rtftb = Nothing

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        Close()
    End Sub

    Private Sub dgv1_SelectionChanged(sender As Object, e As EventArgs) Handles dgv1.SelectionChanged
        If Not Loaded Then Exit Sub
        If dgv1.SelectedRows.Count > 0 Then
            Dim objname = dgv1.SelectedRows(0).Cells(0).Value
            SelectAndCenter(objname)
        End If
    End Sub

    Private Sub dgv2_SelectionChanged(sender As Object, e As EventArgs) Handles dgv2.SelectionChanged
        If Not Loaded Then Exit Sub
        If dgv2.SelectedRows.Count > 0 Then
            Dim objname = dgv2.SelectedRows(0).Cells(0).Value
            SelectAndCenter(objname)
        End If
    End Sub

    Sub SelectAndCenter(objname As String)

        Dim obj = Flowsheet.GetFlowsheetGraphicObject(objname)
        If Not obj Is Nothing Then
            Try
                Flowsheet.GetSurface().Zoom = 2.0
                Flowsheet.UpdateInterface()
                Flowsheet.UpdateInterface()
                Dim center As Point = New Point(Flowsheet.GetFlowsheetSurfaceWidth() / 2, Flowsheet.GetFlowsheetSurfaceHeight() / 2)
                Flowsheet.GetSurface().OffsetAll(center.X / Flowsheet.GetSurface().Zoom - obj.X, center.Y / Flowsheet.GetSurface().Zoom - obj.Y)
                Flowsheet.GetSurface().SelectedObject = obj
                Flowsheet.UpdateInterface()
                Flowsheet.UpdateInterface()
            Catch ex As Exception
            End Try
        End If

    End Sub

End Class