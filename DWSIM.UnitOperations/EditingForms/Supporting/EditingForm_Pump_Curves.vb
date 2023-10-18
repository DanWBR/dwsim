Imports System.Windows.Forms
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Drawing
Imports System.IO
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class EditingForm_Pump_Curves

    Public selectedpump As UnitOperations.Pump
    Private headunit, powerunit, effunit, flowunit As String
    Public curveeditorshowmode As Integer = 0
    Public loaded As Boolean = False
    Public DatabaseFileName As String

    Private Sub PumpCurvesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Shown

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()

        End Using

        Dim curve As UnitOperations.Auxiliary.PumpOps.Curve
        Dim i As Integer = 0

        curve = Me.selectedpump.Curves("HEAD")
        Me.dgv1.Rows.Clear()
        Me.cb1.SelectedItem = curve.xunit
        Me.cb6.SelectedItem = curve.yunit
        Me.tb1.Text = curve.Name
        Me.ch1.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv1.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString(), curve.y(i).ToString()})
                Next
            End With
        End If

        curve = Me.selectedpump.Curves("POWER")
        Me.dgv2.Rows.Clear()
        Me.cb2.SelectedItem = curve.xunit
        Me.cb7.SelectedItem = curve.yunit
        Me.tb2.Text = curve.Name
        Me.ch2.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv2.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString(), curve.y(i).ToString()})
                Next
            End With
        End If

        curve = Me.selectedpump.Curves("EFF")
        Me.dgv3.Rows.Clear()
        Me.cb3.SelectedItem = curve.xunit
        Me.cb8.SelectedItem = curve.yunit
        Me.tb3.Text = curve.Name
        Me.ch3.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv3.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString(), curve.y(i).ToString()})
                Next
            End With
        End If


        curve = Me.selectedpump.Curves("NPSH")
        Me.dgv4.Rows.Clear()
        Me.cb4.SelectedItem = curve.xunit
        Me.cb9.SelectedItem = curve.yunit
        Me.tb4.Text = curve.Name
        Me.ch4.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv4.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString(), curve.y(i).ToString()})
                Next
            End With
        End If


        curve = Me.selectedpump.Curves("SYSTEM")
        Me.dgv5.Rows.Clear()
        Me.cb5.SelectedItem = curve.xunit
        Me.cb10.SelectedItem = curve.yunit
        Me.tb5.Text = curve.Name
        Me.ch5.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv5.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString(), curve.y(i).ToString()})
                Next
            End With
        End If

        If selectedpump.ImpellerDiameter > 0 Then
            Me.TBImpellerDiam.Text = selectedpump.ImpellerDiameter
        Else
            Me.TBImpellerDiam.Text = 200
        End If
        If selectedpump.ImpellerSpeed > 0 Then
            Me.TBImpellerSpeed.Text = selectedpump.ImpellerSpeed
        Else
            Me.TBImpellerSpeed.Text = 1450
        End If

        If selectedpump.DiameterUnit <> "" Then
            CBDiameterUnit.SelectedItem = selectedpump.DiameterUnit
        Else
            Me.CBDiameterUnit.SelectedItem = "mm"
        End If


        Try
            cbflowunit.SelectedIndex = 0
            cbheadunit.SelectedIndex = 0
            cbeffunit.SelectedIndex = 0
            cbpowerunit.SelectedIndex = 0
        Catch ex As Exception

        End Try

        Try
            DrawChart()
        Catch ex As Exception

        End Try

        loaded = True

    End Sub

    Public Sub PasteData(ByRef dgv As DataGridView)
        Dim tArr() As String
        Dim arT() As String
        Dim i, ii As Integer
        Dim c, cc, r As Integer

        tArr = Clipboard.GetText().Split(Environment.NewLine)

        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(vbTab)
                cc = c
                For ii = 0 To arT.Length - 1
                    cc = GetNextVisibleCol(dgv, cc)
                    If cc > dgv.ColumnCount - 1 Then Exit For
                    If r > dgv.Rows.Count - 1 Then dgv.Rows.Add()
                    dgv.Item(cc, r).Value = arT(ii).TrimStart
                    cc = cc + 1
                Next
                r = r + 1
            End If
        Next

    End Sub

    Private Function GetNextVisibleCol(ByRef dgv As DataGridView, ByVal stidx As Integer) As Integer

        Dim i As Integer

        For i = stidx To dgv.ColumnCount - 1
            If dgv.Columns(i).Visible Then Return i
        Next

        Return Nothing

    End Function

    Private Sub dgv_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles dgv1.KeyDown, dgv5.KeyDown, dgv4.KeyDown, dgv3.KeyDown, dgv2.KeyDown

        If e.KeyCode = Keys.V And e.Control Then
            PasteData(sender)
        End If

    End Sub

    Private Sub ch1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch1.CheckedChanged
        If Me.loaded Then Me.selectedpump.Curves("HEAD").Enabled = Me.ch1.Checked
    End Sub

    Private Sub ch2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch2.CheckedChanged
        If Me.loaded Then Me.selectedpump.Curves("POWER").Enabled = Me.ch2.Checked
    End Sub

    Private Sub ch3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch3.CheckedChanged
        If Me.loaded Then Me.selectedpump.Curves("EFF").Enabled = Me.ch3.Checked
    End Sub

    Private Sub ch4_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch4.CheckedChanged
        If Me.loaded Then Me.selectedpump.Curves("NPSH").Enabled = Me.ch4.Checked
    End Sub

    Private Sub ch5_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch5.CheckedChanged
        If Me.loaded Then Me.selectedpump.Curves("SYSTEM").Enabled = Me.ch5.Checked
    End Sub

    Private Sub tb1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb1.TextChanged
        If Me.loaded Then Me.selectedpump.Curves("HEAD").Name = Me.tb1.Text
    End Sub

    Private Sub tb2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb2.TextChanged
        If Me.loaded Then Me.selectedpump.Curves("POWER").Name = Me.tb2.Text
    End Sub

    Private Sub tb3_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb3.TextChanged
        If Me.loaded Then Me.selectedpump.Curves("EFF").Name = Me.tb3.Text
    End Sub

    Private Sub tb4_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb4.TextChanged
        If Me.loaded Then Me.selectedpump.Curves("NPSH").Name = Me.tb4.Text
    End Sub

    Private Sub tb5_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb5.TextChanged
        If Me.loaded Then Me.selectedpump.Curves("SYSTEM").Name = Me.tb5.Text
    End Sub

    Private Sub cb1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb1.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("HEAD").xunit = Me.cb1.SelectedItem.ToString
    End Sub

    Private Sub cb2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb2.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("POWER").xunit = Me.cb2.SelectedItem.ToString
    End Sub

    Private Sub cb3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb3.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("EFF").xunit = Me.cb3.SelectedItem.ToString
    End Sub

    Private Sub cb4_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb4.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("NPSH").xunit = Me.cb4.SelectedItem.ToString
    End Sub

    Private Sub cb5_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb5.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("SYSTEM").xunit = Me.cb5.SelectedItem.ToString
    End Sub

    Private Sub cb6_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb6.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("HEAD").yunit = Me.cb6.SelectedItem.ToString
    End Sub

    Private Sub cb7_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb7.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("POWER").yunit = Me.cb7.SelectedItem.ToString
    End Sub

    Private Sub cb8_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb8.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("EFF").yunit = Me.cb8.SelectedItem.ToString
    End Sub

    Private Sub cb9_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb9.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("NPSH").yunit = Me.cb9.SelectedItem.ToString
    End Sub

    Private Sub cb10_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb10.SelectedIndexChanged
        If Me.loaded Then Me.selectedpump.Curves("SYSTEM").yunit = Me.cb10.SelectedItem.ToString
    End Sub

    Private Sub dgv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv1.CellValueChanged, dgv5.CellValueChanged, dgv4.CellValueChanged, dgv3.CellValueChanged, dgv2.CellValueChanged


        If loaded Then

            Dim dg As DataGridView = CType(sender, DataGridView)

            If Not dg.Rows(e.RowIndex).Cells(0).ToString = "" _
                And Not dg.Rows(e.RowIndex).Cells(1).ToString = "" Then

                Dim x, y As New ArrayList

                For Each r As DataGridViewRow In dg.Rows
                    If Not r.Cells(0).Value Is Nothing _
                    And Not r.Cells(1).Value Is Nothing Then
                        x.Add(Double.Parse(r.Cells(0).Value.ToString()))
                        y.Add(Double.Parse(r.Cells(1).Value.ToString()))
                    End If
                Next

                Select Case dg.Name
                    Case "dgv1"
                        Me.selectedpump.Curves("HEAD").x = x
                        Me.selectedpump.Curves("HEAD").y = y
                    Case "dgv2"
                        Me.selectedpump.Curves("POWER").x = x
                        Me.selectedpump.Curves("POWER").y = y
                    Case "dgv3"
                        Me.selectedpump.Curves("EFF").x = x
                        Me.selectedpump.Curves("EFF").y = y
                    Case "dgv4"
                        Me.selectedpump.Curves("NPSH").x = x
                        Me.selectedpump.Curves("NPSH").y = y
                    Case "dgv5"
                        Me.selectedpump.Curves("SYSTEM").x = x
                        Me.selectedpump.Curves("SYSTEM").y = y
                End Select

            End If

        End If

    End Sub

    Private Sub cbflowunit_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbflowunit.SelectedIndexChanged
        If Not Me.cbflowunit.SelectedItem Is Nothing Then
            Me.flowunit = Me.cbflowunit.SelectedItem.ToString
            Try
                DrawChart()
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbheadunit_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbheadunit.SelectedIndexChanged
        If Not Me.cbheadunit.SelectedItem Is Nothing Then
            Me.headunit = Me.cbheadunit.SelectedItem.ToString
            Try
                DrawChart()
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbeffunit_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbeffunit.SelectedIndexChanged
        If Not Me.cbeffunit.SelectedItem Is Nothing Then
            Me.effunit = Me.cbeffunit.SelectedItem.ToString
            Try
                DrawChart()
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbpowerunit_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbpowerunit.SelectedIndexChanged
        If Not Me.cbpowerunit.SelectedItem Is Nothing Then
            Me.powerunit = Me.cbpowerunit.SelectedItem.ToString
            Try
                DrawChart()
            Catch ex As Exception
            End Try
        End If
    End Sub

    Sub DrawChart()

        Dim pxh, pyh, pxp, pyp, pxn, pyn, pxs, pys, pxe, pye, pxop, pyop, pxnd, pynd As New ArrayList

        Dim i As Integer
        Dim xunit, yunit As String
        Dim x, y As ArrayList
        'analyze data, interpolate, bla, bla bla...

        Me.selectedpump.CreateCurves()

        If Me.selectedpump.Curves("HEAD").Enabled Then
            x = Me.selectedpump.Curves("HEAD").x
            y = Me.selectedpump.Curves("HEAD").y
            xunit = Me.selectedpump.Curves("HEAD").xunit
            yunit = Me.selectedpump.Curves("HEAD").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxh.Add(cv.ConvertFromSI(flowunit, cv.ConvertToSI(xunit, x(i))))
                    pyh.Add(cv.ConvertFromSI(headunit, cv.ConvertToSI(yunit, y(i))))
                End If
            Next
        End If

        If Me.selectedpump.Curves("POWER").Enabled Then
            x = Me.selectedpump.Curves("POWER").x
            y = Me.selectedpump.Curves("POWER").y
            xunit = Me.selectedpump.Curves("POWER").xunit
            yunit = Me.selectedpump.Curves("POWER").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxp.Add(cv.ConvertFromSI(flowunit, cv.ConvertToSI(xunit, x(i))))
                    pyp.Add(cv.ConvertFromSI(powerunit, cv.ConvertToSI(yunit, y(i))))
                End If
            Next
        End If

        If Me.selectedpump.Curves("EFF").Enabled Then
            x = Me.selectedpump.Curves("EFF").x
            y = Me.selectedpump.Curves("EFF").y
            xunit = Me.selectedpump.Curves("EFF").xunit
            yunit = Me.selectedpump.Curves("EFF").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxe.Add(cv.ConvertFromSI(flowunit, cv.ConvertToSI(xunit, x(i))))
                    If yunit = "%" Then
                        If effunit = "%" Then
                            pye.Add(CDbl(y(i)))
                        Else
                            pye.Add(y(i) / 100)
                        End If
                    Else
                        If effunit = "%" Then
                            pye.Add(y(i) * 100)
                        Else
                            pye.Add(CDbl(y(i)))
                        End If
                    End If
                End If
            Next
        End If

        If Me.selectedpump.Curves("NPSH").Enabled Then
            x = Me.selectedpump.Curves("NPSH").x
            y = Me.selectedpump.Curves("NPSH").y
            xunit = Me.selectedpump.Curves("NPSH").xunit
            yunit = Me.selectedpump.Curves("NPSH").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxn.Add(cv.ConvertFromSI(flowunit, cv.ConvertToSI(xunit, x(i))))
                    pyn.Add(cv.ConvertFromSI(headunit, cv.ConvertToSI(yunit, y(i))))
                End If
            Next
        End If

        If Me.selectedpump.Curves("SYSTEM").Enabled Then
            x = Me.selectedpump.Curves("SYSTEM").x
            y = Me.selectedpump.Curves("SYSTEM").y
            xunit = Me.selectedpump.Curves("SYSTEM").xunit
            yunit = Me.selectedpump.Curves("SYSTEM").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxs.Add(cv.ConvertFromSI(flowunit, cv.ConvertToSI(xunit, x(i))))
                    pys.Add(cv.ConvertFromSI(headunit, cv.ConvertToSI(yunit, y(i))))
                End If
            Next
        End If

        pxop.Add(cv.ConvertFromSI(flowunit, Me.selectedpump.CurveFlow))
        pyop.Add(cv.ConvertFromSI(headunit, Me.selectedpump.CurveSysHead))

        pxnd.Add(cv.ConvertFromSI(flowunit, Me.selectedpump.CurveFlow))
        pynd.Add(cv.ConvertFromSI(headunit, Me.selectedpump.NPSH))

        Dim line As New ZedGraph.LineObj(Color.Black, 0, 0, 0, 0)

        With Me.chart1.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .YAxisList.Clear()
            .Y2AxisList.Clear()
            If pyh.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Head (" & headunit & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Pump Head", pxh.ToArray(GetType(Double)), pyh.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Red
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Red
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 0 'Me.chart1.GraphPane.YAxisList.IndexOf("Head (" & headunit & ")")
                End With
            End If
            If pyp.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Power (" & powerunit & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Pump Power", pxp.ToArray(GetType(Double)), pyp.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.PaleGreen
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.PaleGreen
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 1 'Me.chart1.GraphPane.YAxisList.IndexOf("Power (" & powerunit & ")")
                End With
            End If
            If pye.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Pump Eff. (" & effunit & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Pump Eff.", pxe.ToArray(GetType(Double)), pye.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Blue
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Blue
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 2 'Me.chart1.GraphPane.YAxisList.IndexOf("Pump Eff. (" & effunit & ")")
                End With
            End If
            If pyn.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("NPSH Req. (" & headunit & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("NPSH Req.", pxn.ToArray(GetType(Double)), pyn.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Plum
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Plum
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 3 'Me.chart1.GraphPane.YAxisList.IndexOf("NPSH Req. (" & headunit & ")")
                End With
            End If
            If pys.Count > 0 Then
                With .AddCurve("System Head", pxs.ToArray(GetType(Double)), pys.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Salmon
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Salmon
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 0 'Me.chart1.GraphPane.YAxisList.IndexOf("System Head (" & headunit & ")")
                End With
            End If
            If pyop.Count > 0 Then
                With .AddCurve("Operating Point", pxop.ToArray(GetType(Double)), pyop.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Black
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Black
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 0 'Me.chart1.GraphPane.YAxisList.IndexOf("System Head (" & headunit & ")")
                End With
            End If
            If pynd.Count > 0 Then
                With .AddCurve("NPSH Available", pxnd.ToArray(GetType(Double)), pynd.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.YellowGreen
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.YellowGreen
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 3 'Me.chart1.GraphPane.YAxisList.IndexOf("NPSH Req. (" & headunit & ")")
                End With
            End If
            line = New ZedGraph.LineObj(Color.LightGray, pxop(0), 0, pxop(0), 1)
            line.ZOrder = ZedGraph.ZOrder.E_BehindCurves
            line.IsClippedToChartRect = True
            line.Location.CoordinateFrame = ZedGraph.CoordType.XScaleYChartFraction
            line.Line.Style = Drawing2D.DashStyle.Solid
            .GraphObjList.Add(line)

            With .Legend
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
                .IsHStack = True
                .FontSpec.Size = 10
            End With
            With .XAxis
                .Title.Text = "Flow (" & flowunit & ")"
                .Title.FontSpec.Size = 10
                .Scale.MinAuto = True
                .Scale.MaxAuto = True
                .Scale.FontSpec.Size = 10
            End With
            .Margin.All = 20
            .Title.IsVisible = False
            Me.chart1.IsAntiAlias = True
            .AxisChange(Me.CreateGraphics)
            .AxisChange(Me.CreateGraphics)
            Me.chart1.Invalidate()
        End With

    End Sub

    Private Sub TBImpellerDiam_TextChanged(sender As Object, e As EventArgs) Handles TBImpellerDiam.TextChanged
        If loaded Then
            selectedpump.ImpellerDiameter = TBImpellerDiam.Text
        End If
    End Sub

    Private Sub TB_RPM_TextChanged(sender As Object, e As EventArgs) Handles TBImpellerSpeed.TextChanged
        If loaded Then
            selectedpump.ImpellerSpeed = TBImpellerSpeed.Text
        End If
    End Sub

    Private Sub EditingForm_Pump_Curves_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

    End Sub

    Private Sub tsbImport_Click(sender As Object, e As EventArgs) Handles tsbImport.Click
        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Dim text = handler.ReadAllText()
            Try
                Dim data = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, UnitOperations.Auxiliary.PumpOps.Curve))(text)
                selectedpump.Curves = data
                PumpCurvesEditorForm_Load(sender, e)
            Catch ex As Exception
                MessageBox.Show(selectedpump.GetFlowsheet.GetTranslatedString("ErrorAddingComponent") & " " & ex.Message,
                                selectedpump.GetFlowsheet.GetTranslatedString("Erro"),
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub tsbExport_Click(sender As Object, e As EventArgs) Handles tsbExport.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Try
                Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(selectedpump.Curves, Newtonsoft.Json.Formatting.Indented)
                Using stream As New IO.MemoryStream()
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        writer.Write(jsondata)
                        handler.Write(stream)
                    End Using
                End Using
            Catch ex As Exception
                MessageBox.Show(selectedpump.GetFlowsheet.GetTranslatedString("Erroaosalvararquivo") & " " & ex.Message,
                                selectedpump.GetFlowsheet.GetTranslatedString("Erro"),
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub CBDiameterUnit_SelectedIndexChanged(sender As Object, e As EventArgs) Handles CBDiameterUnit.SelectedIndexChanged
        If loaded Then
            selectedpump.DiameterUnit = CBDiameterUnit.Text
        End If
    End Sub

End Class