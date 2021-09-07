Imports System.Linq
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts
Imports DWSIM.ExtensionMethods

Public Class FormConfigureChartObject

    Public Chart As OxyPlotGraphic

    Private Loaded As Boolean = False

    Private Sub FormConfigureChartObject_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        cbFlowsheetObject.Items.Clear()
        cbFlowsheetObject.Items.Add("Dynamic Mode Integrators")
        cbFlowsheetObject.Items.Add("Chart Objects")
        For Each obj In Chart.Flowsheet.SimulationObjects.Values
            cbFlowsheetObject.Items.Add(obj.GraphicObject.Tag)
        Next

        If Chart.OwnerID <> "" Then

            If Chart.Flowsheet.SimulationObjects.ContainsKey(Chart.OwnerID) Then

                Dim obj = Chart.Flowsheet.SimulationObjects(Chart.OwnerID)
                cbFlowsheetObject.SelectedItem = obj.GraphicObject.Tag

                cbChartSelector.Items.Clear()

                Dim models = obj.GetChartModelNames()

                For Each m In models
                    cbChartSelector.Items.Add(m)
                Next

                If models.Contains(Chart.ModelName) Then
                    cbChartSelector.SelectedItem = Chart.ModelName
                End If

            ElseIf Chart.OwnerID = "Dynamic Mode Integrators" Then

                cbFlowsheetObject.SelectedIndex = 0

                cbChartSelector.Items.Clear()

                For Each item In Chart.Flowsheet.DynamicsManager.IntegratorList
                    cbChartSelector.Items.Add(item.Value.Description)
                Next

                cbChartSelector.SelectedItem = Chart.ModelName

            ElseIf Chart.OwnerID = "Chart Objects" Then

                cbFlowsheetObject.SelectedIndex = 1

                cbChartSelector.Items.Clear()

                For Each item In Chart.Flowsheet.Charts.Values
                    cbChartSelector.Items.Add(item.DisplayName)
                Next

                cbChartSelector.SelectedItem = Chart.ModelName

            End If

        End If

        txtHeight.Text = Chart.Height
        txtWidth.Text = Chart.Width

        Loaded = True

    End Sub

    Private Sub cbFlowsheetObject_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlowsheetObject.SelectedIndexChanged

        If Loaded Then

            If cbFlowsheetObject.SelectedIndex > 1 Then
                Dim obj = Chart.Flowsheet.GetFlowsheetSimulationObject(cbFlowsheetObject.SelectedItem.ToString())
                Chart.OwnerID = obj.Name
                cbChartSelector.Items.Clear()
                Dim models = obj.GetChartModelNames()
                For Each m In models
                    cbChartSelector.Items.Add(m)
                Next
            ElseIf cbFlowsheetObject.SelectedIndex = 0 Then
                Chart.OwnerID = cbFlowsheetObject.SelectedItem.ToString
                cbChartSelector.Items.Clear()
                For Each item In Chart.Flowsheet.DynamicsManager.IntegratorList
                    cbChartSelector.Items.Add(item.Value.Description)
                Next
            Else
                Chart.OwnerID = cbFlowsheetObject.SelectedItem.ToString
                cbChartSelector.Items.Clear()
                For Each item In Chart.Flowsheet.Charts.Values
                    cbChartSelector.Items.Add(item.DisplayName)
                Next
            End If

        End If

    End Sub

    Private Sub cbChartSelector_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbChartSelector.SelectedIndexChanged
        If Loaded Then
            Chart.ModelName = cbChartSelector.SelectedItem
        End If
    End Sub

    Private Sub txtWidth_KeyDown(sender As Object, e As KeyEventArgs) Handles txtWidth.KeyDown
        If e.KeyCode = Keys.Enter Then
            Chart.Width = txtWidth.Text.ToDoubleFromCurrent()
        End If
    End Sub

    Private Sub txtHeight_KeyDown(sender As Object, e As KeyEventArgs) Handles txtHeight.KeyDown
        If e.KeyCode = Keys.Enter Then
            Chart.Height = txtHeight.Text.ToDoubleFromCurrent()
        End If
    End Sub

End Class