Imports System.Drawing

Public Class ComprExprCurveSet

    Public curvedata As Dictionary(Of String, UnitOperations.Auxiliary.PumpOps.Curve)
    Public speed As Integer

    Public loaded As Boolean = False

    Private Sub PumpCurvesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Me.Load

    End Sub

    Public Sub Populate()

        tbRotation.Text = speed

        Dim curve As UnitOperations.Auxiliary.PumpOps.Curve
        Dim i As Integer = 0

        curve = curvedata("HEAD")
        Me.dgv1.Rows.Clear()
        Me.cb1.SelectedItem = curve.xunit
        Me.cb6.SelectedItem = curve.yunit
        Me.ch1.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv1.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString, curve.y(i).ToString})
                Next
            End With
        End If

        curve = curvedata("EFF")
        Me.dgv3.Rows.Clear()
        Me.cb3.SelectedItem = curve.xunit
        Me.cb8.SelectedItem = curve.yunit
        Me.ch3.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv3.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString, curve.y(i).ToString})
                Next
            End With
        End If

        curve = curvedata("POWER")
        Me.dgv4.Rows.Clear()
        Me.ComboBox2.SelectedItem = curve.xunit
        Me.ComboBox1.SelectedItem = curve.yunit
        Me.CheckBox1.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv4.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString, curve.y(i).ToString})
                Next
            End With
        End If

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
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
                For ii = 0 To arT.Length - 1
                    If r > dgv.Rows.Count - 1 Then
                        dgv.Rows.Add()
                        dgv.Rows(0).Cells(0).Selected = True
                    End If
                Next
                r = r + 1
            End If
        Next
        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
                cc = c
                For ii = 0 To arT.Length - 1
                    cc = GetNextVisibleCol(dgv, cc)
                    If cc > dgv.ColumnCount - 1 Then Exit For
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

    Private Sub dgv_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles dgv1.KeyDown, dgv3.KeyDown, dgv4.KeyDown

        If e.KeyCode = Keys.Delete And e.Modifiers = Keys.Shift Then
            Dim toremove As New ArrayList
            For Each c As DataGridViewCell In sender.SelectedCells
                If Not toremove.Contains(c.RowIndex) Then toremove.Add(c.RowIndex)
            Next
            For Each i As Integer In toremove
                Try
                    sender.Rows.RemoveAt(i)
                Catch ex As Exception

                End Try
            Next
        ElseIf e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(sender)
        ElseIf e.KeyCode = Keys.Delete Then
            For Each c As DataGridViewCell In sender.SelectedCells
                c.Value = ""
            Next
        ElseIf e.KeyCode = Keys.C And e.Modifiers = Keys.Control Then
            Clipboard.SetDataObject(sender.GetClipboardContent())
        End If

    End Sub

    Private Sub ch1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch1.CheckedChanged
        If Me.loaded Then curvedata("HEAD").Enabled = Me.ch1.Checked
    End Sub

    Private Sub ch3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch3.CheckedChanged
        If Me.loaded Then curvedata("EFF").Enabled = Me.ch3.Checked
    End Sub

    Private Sub cb1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb1.SelectedIndexChanged
        If Me.loaded Then curvedata("HEAD").xunit = Me.cb1.SelectedItem.ToString
    End Sub

    Private Sub cb3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb3.SelectedIndexChanged
        If Me.loaded Then curvedata("EFF").xunit = Me.cb3.SelectedItem.ToString
    End Sub

    Private Sub cb6_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb6.SelectedIndexChanged
        If Me.loaded Then curvedata("HEAD").yunit = Me.cb6.SelectedItem.ToString
    End Sub

    Private Sub cb8_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb8.SelectedIndexChanged
        If Me.loaded Then curvedata("EFF").yunit = Me.cb8.SelectedItem.ToString
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox2.SelectedIndexChanged
        If Me.loaded Then curvedata("POWER").xunit = Me.ComboBox2.SelectedItem.ToString
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If Me.loaded Then curvedata("POWER").yunit = Me.ComboBox1.SelectedItem.ToString
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If Me.loaded Then curvedata("POWER").Enabled = Me.CheckBox1.Checked
    End Sub

    Private Sub dgv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv1.CellValueChanged, dgv3.CellValueChanged, dgv4.CellValueChanged

        If loaded Then

            Dim dg As DataGridView = CType(sender, DataGridView)

            If Not dg.Rows(e.RowIndex).Cells(0).ToString = "" _
                And Not dg.Rows(e.RowIndex).Cells(1).ToString = "" Then

                Dim x, y As New ArrayList

                For Each r As DataGridViewRow In dg.Rows
                    If Not r.Cells(0).Value Is Nothing _
                    And Not r.Cells(1).Value Is Nothing Then
                        Try
                            x.Add(Double.Parse(r.Cells(0).Value))
                        Catch ex As Exception
                            x.Add(0.0)
                        End Try
                        Try
                            y.Add(Double.Parse(r.Cells(1).Value))
                        Catch ex As Exception
                            y.Add(0.0)
                        End Try
                    End If
                Next

                Select Case dg.Name
                    Case "dgv1"
                        curvedata("HEAD").x = x
                        curvedata("HEAD").y = y
                    Case "dgv3"
                        curvedata("EFF").x = x
                        curvedata("EFF").y = y
                    Case "dgv4"
                        curvedata("POWER").x = x
                        curvedata("POWER").y = y
                End Select

            End If

        End If

    End Sub

    Sub DrawChart()

        Dim pxh, pyh, pxp, pyp, pxe, pye As New ArrayList

        Dim i As Integer
        Dim xunit, yunit As String
        Dim x, y As ArrayList
        'analyze data, interpolate, bla, bla bla...

        If curvedata("HEAD").Enabled Then
            x = curvedata("HEAD").x
            y = curvedata("HEAD").y
            xunit = curvedata("HEAD").xunit
            yunit = curvedata("HEAD").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxh.Add(x(i))
                    pyh.Add(y(i))
                End If
            Next
        End If

        If curvedata("POWER").Enabled Then
            x = curvedata("POWER").x
            y = curvedata("POWER").y
            xunit = curvedata("POWER").xunit
            yunit = curvedata("POWER").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxp.Add(x(i))
                    pyp.Add(y(i))
                End If
            Next
        End If

        If curvedata("EFF").Enabled Then
            x = curvedata("EFF").x
            y = curvedata("EFF").y
            xunit = curvedata("EFF").xunit
            yunit = curvedata("EFF").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxe.Add(x(i))
                    If yunit = "%" Then
                        pye.Add(CDbl(y(i)))
                    Else
                        pye.Add(y(i) * 100)
                    End If
                End If
            Next
        End If

        Dim line As New ZedGraph.LineObj(Color.Black, 0, 0, 0, 0)

        With Me.chart1.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .YAxisList.Clear()
            .Y2AxisList.Clear()
            If pyh.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Head (" & curvedata("HEAD").yunit.ToString & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Head", pxh.ToArray(GetType(Double)), pyh.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Red
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Red
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 0
                End With
            End If
            If pye.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Efficiency (" & curvedata("EFF").yunit.ToString & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Eff.", pxe.ToArray(GetType(Double)), pye.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Blue
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Blue
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 1
                End With
            End If
            If pyp.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis("Power (" & curvedata("POWER").yunit.ToString & ")")
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 10
                .YAxisList.Add(ya0)
                With .AddCurve("Power", pxp.ToArray(GetType(Double)), pyp.ToArray(GetType(Double)), Color.Black)
                    .Line.IsVisible = True
                    .Line.IsSmooth = True
                    .Color = Color.Blue
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Fill.Color = Color.Blue
                    .Symbol.Fill.IsVisible = True
                    .Symbol.Size = 5
                    .YAxisIndex = 2
                End With
            End If

            With .Legend
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
                .IsHStack = True
                .FontSpec.Size = 10
            End With
            With .XAxis
                .Title.Text = "Flow (" & curvedata("HEAD").xunit & ")"
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

    Private Sub tbRotation_TextChanged(sender As Object, e As EventArgs) Handles tbRotation.TextChanged
        Try
            speed = Integer.Parse(tbRotation.Text)
            tbRotation.ForeColor = Color.Blue
        Catch ex As Exception
            tbRotation.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TabControl1_Selected(sender As Object, e As TabControlEventArgs) Handles TabControl1.Selected
        Try
            DrawChart()
        Catch ex As Exception
        End Try
    End Sub
End Class
