Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class DynamicsPropertyEditor

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As ISimulationObject

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub DynamicsPropertyEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Shown

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

                Dim l As New Label With {.Text = p.Key, .Font = New Drawing.Font(.Font, Drawing.FontStyle.Bold), .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim tb As New TextBox With {.Text = value, .Dock = DockStyle.Fill, .TextAlign = HorizontalAlignment.Right}
                Dim l2 As New Label With {.Text = unitsstring, .Dock = DockStyle.Fill, .AutoSize = False, .TextAlign = Drawing.ContentAlignment.MiddleLeft}
                Dim l3 As New Label With {.Text = col2(p.Key).ToString, .Dock = DockStyle.Fill, .AutoSize = False, .Height = 46 * GlobalSettings.Settings.DpiScale, .TextAlign = Drawing.ContentAlignment.TopLeft}

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

                Dim tl As New TableLayoutPanel With {.Height = 24 * GlobalSettings.Settings.DpiScale}
                tl.RowStyles.Clear()
                tl.RowStyles.Add(New RowStyle(SizeType.Percent, 1.0))
                tl.ColumnStyles.Clear()
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.4))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.3))
                tl.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 0.3))
                tl.Controls.Add(l, 0, 0)
                tl.Controls.Add(tb, 1, 0)
                tl.Controls.Add(l2, 2, 0)

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

        PropertiesLayout.ResumeLayout()

        PropertiesLayout.PerformLayout()

        Loaded = True

    End Sub

End Class