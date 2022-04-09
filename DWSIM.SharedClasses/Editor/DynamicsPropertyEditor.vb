Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class DynamicsPropertyEditor

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As ISimulationObject

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Public UpdateCallBack As Action(Of TableLayoutPanel)

    Private Sub DynamicsPropertyEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Shown

        Me.AutoScaleMode = AutoScaleMode.Dpi
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)

        For Each control As Control In Me.Controls
            control.Font = Drawing.SystemFonts.MessageBoxFont
        Next

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetFlowsheet().GetTranslatedString("DynamicProperties") & ")"

        Dim col1 = DirectCast(SimObject.ExtraProperties, IDictionary(Of String, Object))
        Dim col2 = DirectCast(SimObject.ExtraPropertiesDescriptions, IDictionary(Of String, Object))
        Dim col3 = DirectCast(SimObject.ExtraPropertiesUnitTypes, IDictionary(Of String, Object))
        Dim col4 = DirectCast(SimObject.ExtraPropertiesTypes, IDictionary(Of String, Object))

        PropertiesLayout.Controls.Clear()

        PropertiesLayout.SuspendLayout()

        For Each p In col1

            If col2.ContainsKey(p.Key) And col3.ContainsKey(p.Key) Then

                Dim utype As Interfaces.Enums.UnitOfMeasure = col3(p.Key)
                Dim unitsstring = units.GetCurrentUnits(utype)

                Dim value As String
                If Double.TryParse(p.Value, New Double) Then
                    value = cv.ConvertFromSI(units.GetCurrentUnits(utype), Convert.ToDouble(p.Value)).ToString(nf)
                Else
                    value = p.Value.ToString
                End If

                Dim tl As New TableLayoutPanel With {.Height = 24 * GlobalSettings.Settings.DpiScale}
                tl.RowStyles.Clear()
                tl.RowStyles.Add(New RowStyle(SizeType.Percent, 1.0))
                tl.ColumnStyles.Clear()
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.6))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.3))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.1))

                Dim l As New Label With {.Text = p.Key, .Font = New Drawing.Font(Drawing.SystemFonts.MessageBoxFont, Drawing.FontStyle.Bold), .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim l2 As New Label With {.Text = unitsstring, .Font = Drawing.SystemFonts.MessageBoxFont, .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim l3 As New Label With {.Text = col2(p.Key).ToString, .Font = Drawing.SystemFonts.MessageBoxFont, .Dock = DockStyle.Fill, .AutoSize = False, .Height = 46 * GlobalSettings.Settings.DpiScale, .TextAlign = Drawing.ContentAlignment.TopLeft}

                tl.Controls.Add(l, 0, 0)
                tl.Controls.Add(l2, 2, 0)

                Select Case DirectCast(col4(p.Key), System.Type)
                    Case 1.0.GetType()
                        Dim tb As New TextBox With {.Text = value, .Dock = DockStyle.Fill, .TextAlign = HorizontalAlignment.Right}
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
                                                         col1(p.Key) = cv.ConvertToSI(units.GetCurrentUnits(utype), Double.Parse(tb.Text))
                                                         tb.SelectAll()
                                                     End If
                                                 End If
                                             End Sub
                        tl.Controls.Add(tb, 1, 0)
                    Case 1.GetType()
                        Dim control As New NumericUpDown With {.Value = value, .Minimum = Integer.MinValue, .Maximum = Integer.MaxValue,
                            .DecimalPlaces = 0, .Increment = 1,
                            .Dock = DockStyle.Fill, .TextAlign = HorizontalAlignment.Right}
                        AddHandler control.ValueChanged, Sub(s, e)
                                                             col1(p.Key) = control.Value
                                                         End Sub
                        tl.Controls.Add(control, 1, 0)
                    Case True.GetType()
                        Dim control As New CheckBox With {.Checked = value, .CheckAlign = Drawing.ContentAlignment.MiddleRight,
                            .Dock = DockStyle.Fill, .TextAlign = HorizontalAlignment.Right}
                        AddHandler control.CheckedChanged, Sub(s, e)
                                                               col1(p.Key) = control.Checked
                                                           End Sub
                        tl.Controls.Add(control, 1, 0)
                End Select


                Dim tl2 As New TableLayoutPanel With {.Height = 30 * GlobalSettings.Settings.DpiScale}
                tl2.RowStyles.Clear()
                tl2.RowStyles.Add(New RowStyle(SizeType.Percent, 1.0))
                tl2.ColumnStyles.Clear()
                tl2.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
                tl2.Controls.Add(l3, 0, 0)

                tl.Dock = DockStyle.Fill
                tl2.Dock = DockStyle.Fill

                PropertiesLayout.Controls.Add(tl)
                PropertiesLayout.Controls.Add(tl2)

            End If

        Next

        UpdateCallBack?.Invoke(PropertiesLayout)

        PropertiesLayout.ResumeLayout()

        PropertiesLayout.PerformLayout()

        Loaded = True

    End Sub

    Private Sub DynamicsPropertyEditor_Load_1(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

End Class