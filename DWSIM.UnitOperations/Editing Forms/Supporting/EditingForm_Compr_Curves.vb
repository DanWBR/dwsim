Imports System.Drawing
Imports System.IO

Public Class EditingForm_CompressorExpander_Curves

    Public simobj As ISimulationObject

    Public curveeditorshowmode As Integer = 0
    Public loaded As Boolean = False
    Public DatabaseFileName As String

    Private Sub PumpCurvesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim curve As UnitOperations.Auxiliary.PumpOps.Curve
        Dim i As Integer = 0

        curve = Me.simobj.Curves("HEAD")
        Me.dgv1.Rows.Clear()
        Me.cb1.SelectedItem = curve.xunit
        Me.cb6.SelectedItem = curve.yunit
        Me.tb1.Text = curve.Name
        Me.ch1.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv1.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString, curve.y(i).ToString})
                Next
            End With
        End If

        curve = Me.simobj.Curves("EFF")
        Me.dgv3.Rows.Clear()
        Me.cb3.SelectedItem = curve.xunit
        Me.cb8.SelectedItem = curve.yunit
        Me.tb3.Text = curve.Name
        Me.ch3.Checked = curve.Enabled
        If curve.x.Count > 0 Then
            With Me.dgv3.Rows
                For i = 0 To curve.x.Count - 1
                    .Add(New Object() {curve.x(i).ToString, curve.y(i).ToString})
                Next
            End With
        End If

        If simobj.CurvesDB <> "" Then
            Dim FileInfo As New FileInfo(simobj.CurvesDB)

            If FileInfo.Exists Then
                TsTBDatabase.Text = FileInfo.Name
                DatabaseFileName = simobj.CurvesDB
                TsTBDatabase.ToolTipText = DatabaseFileName
                TsTBDatabase.BackColor = System.Drawing.SystemColors.Control
                TSBtnSaveToDB.Enabled = True
                TSBtnDeletePump.Enabled = True
                TSBtnDisconnectDB.Enabled = True

                Dim PT As String() = Databases.PumpDB.ReadPumpTypes(DatabaseFileName)
                TSCBTypes.Items.Clear()
                TSCBTypes.Items.AddRange(PT)
                TSCBTypes.Text = simobj.EquipType
            Else

                TsTBDatabase.Text = ResMan.GetLocalString("PUMP_Err3")
                DatabaseFileName = ""
                TsTBDatabase.ToolTipText = simobj.CurvesDB
                TsTBDatabase.BackColor = Color.Orange
                TSBtnSaveToDB.Enabled = False
                TSBtnDeletePump.Enabled = False
                TSBtnDisconnectDB.Enabled = False

                TSCBTypes.Items.Clear()
                TSCBTypes.Text = simobj.EquipType
            End If

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
                        dgv.Rows(0).Cells(0).Value = True
                        dgv.Rows(0).Cells(1).Selected = True
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

    Private Sub dgv_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles dgv3.KeyDown, dgv1.KeyDown

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
        If Me.loaded Then Me.simobj.Curves("HEAD").Enabled = Me.ch1.Checked
        SetStatusModified()
    End Sub

    Private Sub ch3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ch3.CheckedChanged
        If Me.loaded Then Me.simobj.Curves("EFF").Enabled = Me.ch3.Checked
        SetStatusModified()
    End Sub

    Private Sub tb1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb1.TextChanged
        If Me.loaded Then Me.simobj.Curves("HEAD").Name = Me.tb1.Text
        SetStatusModified()
    End Sub

    Private Sub tb3_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb3.TextChanged
        If Me.loaded Then Me.simobj.Curves("EFF").Name = Me.tb3.Text
        SetStatusModified()
    End Sub

    Private Sub cb1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb1.SelectedIndexChanged
        If Me.loaded Then Me.simobj.Curves("HEAD").xunit = Me.cb1.SelectedItem.ToString
        SetStatusModified()
    End Sub

    Private Sub cb3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb3.SelectedIndexChanged
        If Me.loaded Then Me.simobj.Curves("EFF").xunit = Me.cb3.SelectedItem.ToString
        SetStatusModified()
    End Sub

    Private Sub cb6_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb6.SelectedIndexChanged
        If Me.loaded Then Me.simobj.Curves("HEAD").yunit = Me.cb6.SelectedItem.ToString
        SetStatusModified()
    End Sub

    Private Sub cb8_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cb8.SelectedIndexChanged
        If Me.loaded Then Me.simobj.Curves("EFF").yunit = Me.cb8.SelectedItem.ToString
        SetStatusModified()
    End Sub

    Private Sub dgv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv3.CellValueChanged, dgv1.CellValueChanged

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
                        Me.simobj.Curves("HEAD").x = x
                        Me.simobj.Curves("HEAD").y = y
                    Case "dgv3"
                        Me.simobj.Curves("EFF").x = x
                        Me.simobj.Curves("EFF").y = y
                End Select

                TSTBStatus.Text = ResMan.GetLocalString("PUMP_Modified")
                TSTBStatus.BackColor = Color.Crimson
                TSTBStatus.ForeColor = Color.White

            End If

        End If

    End Sub

    Sub DrawChart()

        Dim pxh, pyh, pxp, pyp, pxn, pyn, pxs, pys, pxe, pye, pxop, pyop, pxnd, pynd As New ArrayList

        Dim i As Integer
        Dim xunit, yunit As String
        Dim x, y As ArrayList
        'analyze data, interpolate, bla, bla bla...

        Me.simobj.CreateCurves()

        If Me.simobj.Curves("HEAD").Enabled Then
            x = Me.simobj.Curves("HEAD").x
            y = Me.simobj.Curves("HEAD").y
            xunit = Me.simobj.Curves("HEAD").xunit
            yunit = Me.simobj.Curves("HEAD").yunit
            For i = 0 To x.Count - 1
                If Double.TryParse(x(i), New Double) And Double.TryParse(y(i), New Double) Then
                    pxh.Add(x(i))
                    pyh.Add(y(i))
                End If
            Next
        End If

        If Me.simobj.Curves("EFF").Enabled Then
            x = Me.simobj.Curves("EFF").x
            y = Me.simobj.Curves("EFF").y
            xunit = Me.simobj.Curves("EFF").xunit
            yunit = Me.simobj.Curves("EFF").yunit
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
                Dim ya0 As New ZedGraph.YAxis("Head (" & Me.simobj.Curves("HEAD").yunit.ToString & ")")
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
                Dim ya0 As New ZedGraph.YAxis("Eff. (" & Me.simobj.Curves("EFF").yunit.ToString & ")")
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

            With .Legend
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
                .IsHStack = True
                .FontSpec.Size = 10
            End With
            With .XAxis
                .Title.Text = "Flow (" & Me.simobj.Curves("HEAD").xunit & ")"
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

    Private Sub TSBtnNewDB_Click(sender As Object, e As EventArgs) Handles TSBtnNewDB.Click

        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            If Not My.Computer.FileSystem.FileExists(OpenFileDialog1.FileName) Then
                File.WriteAllText(OpenFileDialog1.FileName, "")
            End If
            Databases.ComprExpDB.CreateNew(OpenFileDialog1.FileName, "EquipmentTypes")
        End If

    End Sub

    Private Sub TSBtnConnectDB_Click(sender As Object, e As EventArgs) Handles TSBtnConnectDB.Click
        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            Dim FileInfo As New FileInfo(OpenFileDialog1.FileName)
            TsTBDatabase.Text = FileInfo.Name
            DatabaseFileName = OpenFileDialog1.FileName
            TsTBDatabase.ToolTipText = DatabaseFileName
            TsTBDatabase.BackColor = System.Drawing.SystemColors.Control
            TSBtnDisconnectDB.Enabled = True

            simobj.CurvesDB = DatabaseFileName

            Dim PT As String() = Databases.ComprExpDB.ReadEquipTypes(DatabaseFileName)
            TSCBTypes.Items.Clear()
            TSCBTypes.Items.AddRange(PT)
        End If
    End Sub
    Private Sub TSBtnDisconnectDB_Click(sender As Object, e As EventArgs) Handles TSBtnDisconnectDB.Click
        TsTBDatabase.Text = ""
        DatabaseFileName = ""
        TSCBTypes.Items.Clear()
        TSCBTypes.Text = ""
        simobj.CurvesDB = ""
        simobj.EquipType = ""

        TsTBDatabase.BackColor = System.Drawing.SystemColors.Control
        TSBtnSaveToDB.Enabled = False
        TSBtnDeletePump.Enabled = False
        TSBtnDisconnectDB.Enabled = False
    End Sub
    Private Sub TSBtnSaveToDB_Click(sender As Object, e As EventArgs) Handles TSBtnSaveToDB.Click

        If simobj.EquipType = "" Then
            MsgBox(ResMan.GetLocalString("PUMP_Err1"), MsgBoxStyle.Information Or MsgBoxStyle.OkOnly)
            Exit Sub
        End If

        Databases.ComprExpDB.AddCurve(simobj, DatabaseFileName, True)

        Dim PT As String() = Databases.ComprExpDB.ReadEquipTypes(DatabaseFileName)
        TSCBTypes.Items.Clear()
        TSCBTypes.Items.AddRange(PT)

        TSBtnDeletePump.Enabled = True

        SetStatusSaved()

    End Sub

    Private Sub ToolStripComboBox1_TextChanged(sender As Object, e As EventArgs) Handles TSCBTypes.TextChanged

        simobj.EquipType = TSCBTypes.Text

        If simobj.CurvesDB <> "" Then TSBtnSaveToDB.Enabled = True

    End Sub

    Private Sub TSCBTypes_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TSCBTypes.KeyPress
        If e.KeyChar = " " Then e.KeyChar = "_"

        TSTBStatus.Text = ResMan.GetLocalString("PUMP_Modified")
        TSTBStatus.BackColor = Color.Crimson
        TSTBStatus.ForeColor = Color.White
    End Sub

    Private Sub TSCBTypes_SelectedIndexChanged(sender As Object, e As EventArgs) Handles TSCBTypes.SelectedIndexChanged
        If loaded Then
            simobj.EquipType = TSCBTypes.Text
            Databases.ComprExpDB.ReadEquipData(DatabaseFileName, simobj.EquipType, simobj)
            loaded = False
            PumpCurvesEditorForm_Load(sender, e)
            SetStatusSaved()
        End If
    End Sub

    Private Sub TSBtnDeletePump_Click(sender As Object, e As EventArgs) Handles TSBtnDeletePump.Click
        Dim FileInfo As New FileInfo(simobj.CurvesDB)

        Databases.ComprExpDB.RemoveEquipment(simobj.CurvesDB, simobj.EquipType)
        TSCBTypes.Text = ""

        Dim PT As String() = Databases.ComprExpDB.ReadEquipTypes(DatabaseFileName)
        TSCBTypes.Items.Clear()
        TSCBTypes.Items.AddRange(PT)

        TSBtnSaveToDB.Enabled = False
        TSBtnDeletePump.Enabled = False
    End Sub

    Private Sub SetStatusModified()
        If loaded Then
            TSTBStatus.Text = ResMan.GetLocalString("PUMP_Modified")
            TSTBStatus.BackColor = Color.Crimson
            TSTBStatus.ForeColor = Color.White
        End If
    End Sub
    Private Sub SetStatusSaved()
        If loaded Then
            TSTBStatus.Text = ResMan.GetLocalString("PUMP_Saved")
            TSTBStatus.BackColor = Color.Lime
            TSTBStatus.ForeColor = Color.Black
        End If
    End Sub

    Private Sub FaTabStrip1_TabStripItemSelectionChanged(e As FarsiLibrary.Win.TabStripItemChangedEventArgs) Handles FaTabStrip1.TabStripItemSelectionChanged
        Try
            DrawChart()
        Catch ex As Exception
        End Try
    End Sub
End Class