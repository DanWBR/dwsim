Imports System.IO
Imports System.Windows.Forms
Imports DWSIM.Interfaces
Imports DWSIM.SharedClasses
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Thermodynamics.BaseClasses

Public Class WilsonPPEditor

    Public _comps As Dictionary(Of String, Interfaces.ICompoundConstantProperties)

    Public Property WilsonPP As WilsonPropertyPackage

    Private Loaded As Boolean

    Private Sub WilsonPPEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        If Settings.DpiScale > 1.0 Then
            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Drawing.Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Drawing.Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Drawing.Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.AutoSize = True
            Me.ToolStrip1.Invalidate()
        End If

        Loaded = False

        Dim id1, id2 As String

        For Each cp As ConstantProperties In WilsonPP.Flowsheet.SelectedCompounds.Values
            id1 = cp.CAS_Number
gt1:        If WilsonPP.WilsonM.BIPs.ContainsKey(id1) Then
                For Each cp2 As ConstantProperties In WilsonPP.Flowsheet.SelectedCompounds.Values
                    id2 = cp2.CAS_Number
                    If id1 <> id2 Then
                        If Not WilsonPP.WilsonM.BIPs(id1).ContainsKey(id2) Then
                            'check if collection has id2 as primary id
                            If WilsonPP.WilsonM.BIPs.ContainsKey(id2) Then
                                If Not WilsonPP.WilsonM.BIPs(id2).ContainsKey(id1) Then
                                    WilsonPP.WilsonM.BIPs(id1).Add(id2, New Double() {0, 0})
                                    Dim A12 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(0)
                                    Dim A21 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(1)
                                    dgvu1.Rows.Add(New Object() {cp.Name, cp2.Name, A12, A21})
                                    With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                        .Cells(0).Tag = id1
                                        .Cells(1).Tag = id2
                                    End With
                                End If
                            End If
                        Else
                            Dim A12 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(0)
                            Dim A21 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(1)
                            dgvu1.Rows.Add(New Object() {cp.Name, cp2.Name, A12, A21})
                            With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                .Cells(0).Tag = id1
                                .Cells(1).Tag = id2
                            End With
                        End If
                    End If
                Next
            Else
                WilsonPP.WilsonM.BIPs.Add(id1, New Dictionary(Of String, Double()))
                GoTo gt1
            End If
        Next

        Loaded = True

    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged

        If Loaded Then
            Dim value1 As Object = dgvu1.Rows(e.RowIndex).Cells(2).Value
            Dim value2 As Object = dgvu1.Rows(e.RowIndex).Cells(3).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2, 3
                    WilsonPP.WilsonM.BIPs(id1)(id2) = New Double() {value1, value2}
            End Select
        End If

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()
        Dim BIPs As List(Of PropertyPackages.Auxiliary.UNIQUAC_IPData)

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON file", "*.json")})

        If openedFile IsNot Nothing Then

            Try

                BIPs = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of PropertyPackages.Auxiliary.UNIQUAC_IPData))(openedFile.ReadAllText())

                If MessageBox.Show("Interaction Parameters loaded successfully. Proceed with overwriting current values?",
                                   "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then

                    For Each row As DataGridViewRow In dgvu1.Rows

                        Dim c1 = row.Cells(0).Value
                        Dim c2 = row.Cells(1).Value

                        Dim bip1 = BIPs.Where(Function(b) b.Name1 = c1 And b.Name2 = c2).FirstOrDefault()
                        Dim bip2 = BIPs.Where(Function(b) b.Name1 = c2 And b.Name2 = c1).FirstOrDefault()

                        If bip1 IsNot Nothing Then
                            row.Cells(2).Value = bip1.A12
                            row.Cells(3).Value = bip1.A21
                        End If

                        If bip2 IsNot Nothing Then
                            row.Cells(2).Value = bip2.A21
                            row.Cells(3).Value = bip2.A12
                        End If

                    Next

                End If

            Catch ex As Exception

                MessageBox.Show("Error: " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        End If

    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click

        Dim BIPs As New List(Of PropertyPackages.Auxiliary.UNIQUAC_IPData)

        For Each row As DataGridViewRow In dgvu1.Rows
            BIPs.Add(New PropertyPackages.Auxiliary.UNIQUAC_IPData With {.Name1 = row.Cells(0).Value, .Name2 = row.Cells(1).Value,
                     .A12 = row.Cells(2).Value.ToString().ToDoubleFromCurrent(), .A21 = row.Cells(3).Value.ToString().ToDoubleFromCurrent()})
        Next

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                    Try
                        Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(BIPs, Newtonsoft.Json.Formatting.Indented)
                        writer.Write(jsondata)
                        handler.Write(stream)
                        MessageBox.Show("File saved successfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Catch ex As Exception
                        MessageBox.Show("Error saving file: " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End Using
            End Using
        End If

    End Sub

    Dim actu(5), actn(5) As Double
    Dim ppu As WilsonPropertyPackage
    Dim ms As Streams.MaterialStream
    Dim ppuf, unifac As Object

    Private Sub ToolStripButton3_Click(sender As Object, e As EventArgs) Handles ToolStripButton3.Click
        Estimate("UNIFAC")
    End Sub

    Private Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        Estimate("UNIFAC-LL")
    End Sub

    Private Sub ToolStripButton5_Click(sender As Object, e As EventArgs) Handles ToolStripButton5.Click
        Estimate("MODFAC-Do")
    End Sub

    Private Sub ToolStripButton6_Click(sender As Object, e As EventArgs) Handles ToolStripButton6.Click
        Estimate("MODFAC-NIST")
    End Sub

    Private Function FunctionValue(ByVal x() As Double) As Double

        Dim a1(1), a2(1), a3(1) As Double

        Dim CAS_IDS = ppu.RET_VCAS().ToList()
        Dim NAMES = ppu.RET_VNAMES().ToList()

        ppu.WilsonM.BIPs.Clear()
        ppu.WilsonM.BIPs.Add(ppu.RET_VCAS()(0), New Dictionary(Of String, Double()))
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0)).Add(ppu.RET_VCAS()(1), New Double() {0.0, 0.0})
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0))(ppu.RET_VCAS()(1))(0) = x(0)
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(0))(ppu.RET_VCAS()(1))(1) = x(1)
        ppu.WilsonM.BIPs.Add(ppu.RET_VCAS()(1), New Dictionary(Of String, Double()))
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1)).Add(ppu.RET_VCAS()(0), New Double() {0.0, 0.0})
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1))(ppu.RET_VCAS()(0))(0) = x(1)
        ppu.WilsonM.BIPs(ppu.RET_VCAS()(1))(ppu.RET_VCAS()(0))(1) = x(0)

        If GlobalSettings.Settings.EnableParallelProcessing Then
            Try
                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       a1 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.25, 0.75}, ppu.GetArguments())
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       a2 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.5, 0.5}, ppu.GetArguments())
                                                   End Sub)
                Dim task3 As Task = TaskHelper.Run(Sub()
                                                       a3 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.75, 0.25}, ppu.GetArguments())
                                                   End Sub)
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try
        Else
            a1 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.25, 0.75}, ppu.GetArguments())
            a2 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.5, 0.5}, ppu.GetArguments())
            a3 = ppu.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.75, 0.25}, ppu.GetArguments())
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

    Private Sub Estimate(method As String)

        Dim row As Integer = dgvu1.SelectedCells(0).RowIndex
        Dim x(1) As Double

        Cursor = Cursors.WaitCursor

        ms = New Streams.MaterialStream("", "")

        ppu = New WilsonPropertyPackage

        If method = "UNIFAC" Then
            ppuf = New PropertyPackages.UNIFACPropertyPackage
            unifac = New PropertyPackages.Auxiliary.Unifac
        ElseIf method = "UNIFAC-LL" Then
            ppuf = New PropertyPackages.UNIFACLLPropertyPackage
            unifac = New PropertyPackages.Auxiliary.UnifacLL
        ElseIf method = "MODFAC-NIST" Then
            ppuf = New PropertyPackages.NISTMFACPropertyPackage
            unifac = New PropertyPackages.Auxiliary.NISTMFAC
        Else
            ppuf = New PropertyPackages.MODFACPropertyPackage
            unifac = New PropertyPackages.Auxiliary.Modfac
        End If

        Dim id1 As String = dgvu1.Rows(row).Cells(0).Value.ToString
        Dim id2 As String = dgvu1.Rows(row).Cells(1).Value.ToString

        Dim comp1, comp2 As ConstantProperties
        comp1 = _comps(id1)
        comp2 = _comps(id2)

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

        If GlobalSettings.Settings.EnableGPUProcessing Then Calculator.InitComputeDevice()

        Dim T1 = 298.15

        Dim a1(1), a2(1), a3(1) As Double

        Try

            If GlobalSettings.Settings.EnableParallelProcessing Then
                If GlobalSettings.Settings.EnableGPUProcessing Then GlobalSettings.Settings.gpu.EnableMultithreading()
                Try
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
                Catch ae As AggregateException
                    Throw ae.Flatten().InnerException
                Finally
                    If GlobalSettings.Settings.EnableGPUProcessing Then
                        GlobalSettings.Settings.gpu.DisableMultithreading()
                        GlobalSettings.Settings.gpu.FreeAll()
                    End If
                End Try
            Else
                a1 = unifac.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                a2 = unifac.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                a3 = unifac.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
            End If

            actu(0) = a1(0)
            actu(1) = a2(0)
            actu(2) = a3(0)
            actu(3) = a1(1)
            actu(4) = a2(1)
            actu(5) = a3(1)

            x(0) = dgvu1.Rows(row).Cells(2).Value
            x(1) = dgvu1.Rows(row).Cells(3).Value

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

            dgvu1.Rows(row).Cells(2).Value = finalval2(0)
            dgvu1.Rows(row).Cells(3).Value = finalval2(1)

        Catch ex As Exception


        Finally

            Cursor = Cursors.Default

        End Try

    End Sub


End Class