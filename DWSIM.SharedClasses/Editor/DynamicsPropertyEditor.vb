Public Class DynamicsPropertyEditor

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As ISimulationObject

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub DynamicsPropertyEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetFlowsheet().GetTranslatedString("DynamicProperties") & ")"

        rbPressure.Checked = If(SimObject.DynamicsSpec = Enums.Dynamics.DynamicsSpecType.Pressure, True, False)

        Dim col1 = DirectCast(SimObject.ExtraProperties, IDictionary(Of String, Object))
        Dim col2 = DirectCast(SimObject.ExtraPropertiesDescriptions, IDictionary(Of String, Object))
        Dim col3 = DirectCast(SimObject.ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
        Dim col4 = DirectCast(SimObject.ExtraPropertiesUnits, IDictionary(Of String, Object))

        PropertiesLayout.Controls.Clear()

        For Each p In col1

            If col2.ContainsKey(p.Key) And col3.ContainsKey(p.Key) And col4.ContainsKey(p.Key) Then

                Dim l As New Label With {.Text = p.Key, .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim tb As New TextBox With {.Text = p.Value.ToString, .Dock = DockStyle.Fill, .TextAlign = HorizontalAlignment.Right}
                Dim l2 As New Label With {.Text = col4(p.Key).ToString, .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim l3 As New Label With {.Text = col2(p.Key).ToString, .Dock = DockStyle.Fill, .AutoSize = False}

                AddHandler tb.TextChanged, Sub(s, e)
                                               If Double.TryParse(tb.Text, New Double) Then
                                                   tb.ForeColor = System.Drawing.Color.Blue
                                               Else
                                                   tb.ForeColor = System.Drawing.Color.Red
                                               End If
                                           End Sub

                AddHandler tb.KeyUp, Sub(s, e)
                                         If e.KeyData = Keys.Enter Then
                                             If tb.ForeColor = System.Drawing.Color.Blue Then
                                                 col1(p.Key) = Double.Parse(tb.Text)
                                                 tb.SelectAll()
                                             End If
                                         End If
                                     End Sub

                Dim tl As New TableLayoutPanel
                tl.RowStyles.Clear()
                tl.RowStyles.Add(New RowStyle(SizeType.AutoSize))
                tl.ColumnStyles.Clear()
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.5))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.4))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.1))
                tl.Controls.Add(l, 0, 0)
                tl.Controls.Add(tb, 1, 0)
                tl.Controls.Add(l2, 2, 0)

                Dim tl2 As New TableLayoutPanel
                tl2.RowStyles.Clear()
                tl2.RowStyles.Add(New RowStyle(SizeType.AutoSize))
                tl2.ColumnStyles.Clear()
                tl2.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))

                PropertiesLayout.Controls.Add(tl)
                PropertiesLayout.Controls.Add(tl2)

            End If

        Next

    End Sub

    Private Sub rbPressure_CheckedChanged(sender As Object, e As EventArgs) Handles rbPressure.CheckedChanged, rbFlow.CheckedChanged

        If rbPressure.Checked Then
            SimObject.DynamicsSpec = Enums.Dynamics.DynamicsSpecType.Pressure
        Else
            SimObject.DynamicsSpec = Enums.Dynamics.DynamicsSpecType.Flow
        End If

    End Sub

End Class