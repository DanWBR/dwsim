'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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


Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Text
Imports DotNumerics
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormConfigUNIQUAC

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormUNIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

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

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Loaded = False

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Me.KryptonDataGridView2.DataSource = Nothing

        If _pp.ComponentName.ToString.Contains("Raoult") Or
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Vapor")) Then
            Me.FaTabStripItem2.Visible = False
            Exit Sub
        Else
            Me.FaTabStripItem2.Visible = True
        End If

        Me.KryptonDataGridView2.Rows.Clear()

        Dim ppu As PropertyPackages.UNIQUACPropertyPackage = _pp

        Dim nf As String = "N2"

        For Each cp As ConstantProperties In _comps.Values
gt0:        If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.Name <> cp2.Name Then
                        If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                            'check if collection has id2 as primary id
                            If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                    ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                    Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                    KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf)})
                                    With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                        .Cells(0).Tag = cp.Name
                                        .Cells(1).Tag = cp2.Name
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                            KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf)})
                            With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                .Cells(0).Tag = cp.Name
                                .Cells(1).Tag = cp2.Name
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                GoTo gt0
            End If
        Next

        dgvu1.Rows.Clear()

        For Each cp As ConstantProperties In _comps.Values
gt1:        If ppu.m_uni.InteractionParameters.ContainsKey(cp.Name) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.Name <> cp2.Name Then
                        If Not ppu.m_uni.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                            'check if collection has id2 as primary id
                            If ppu.m_uni.InteractionParameters.ContainsKey(cp2.Name) Then
                                If Not ppu.m_uni.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                    ppu.m_uni.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.UNIQUAC_IPData)
                                    Dim a12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).A12
                                    Dim a21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).A21
                                    Dim b12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).B12
                                    Dim b21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).B21
                                    Dim c12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).C12
                                    Dim c21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).C21
                                    dgvu1.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(b21, nf), Format(c12, nf), Format(c21, nf)})
                                    With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                        .Cells(0).Tag = cp.Name
                                        .Cells(1).Tag = cp2.Name
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).A12
                            Dim a21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).A21
                            Dim b12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).B12
                            Dim b21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).B21
                            Dim c12 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).C12
                            Dim c21 As Double = ppu.m_uni.InteractionParameters(cp.Name)(cp2.Name).C21
                            dgvu1.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(b21, nf), Format(c12, nf), Format(c21, nf)})
                            With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                .Cells(0).Tag = cp.Name
                                .Cells(1).Tag = cp2.Name
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_uni.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
                GoTo gt1
            End If
        Next

        Loaded = True

    End Sub

    Private Sub KryptonDataGridView1_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs)

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Not Me.KryptonDataGridView2.SelectedCells(0) Is Nothing Then
            If Me.KryptonDataGridView2.SelectedCells(0).RowIndex <> Me.KryptonDataGridView2.SelectedCells(0).ColumnIndex Then
                Dim Vc1 As Double = _comps(Me.KryptonDataGridView2.Rows(Me.KryptonDataGridView2.SelectedCells(0).RowIndex).Tag).Critical_Volume
                Dim Vc2 As Double = _comps(Me.KryptonDataGridView2.Columns(Me.KryptonDataGridView2.SelectedCells(0).ColumnIndex).Tag).Critical_Volume

                Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

                Me.KryptonDataGridView2.SelectedCells(0).Value = tmp

            End If
        End If
    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            Dim ppu As PropertyPackages.UNIQUACPropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Dim oldvalue As Double = 0.0#, param As String = ""
            Select Case e.ColumnIndex
                Case 2
                    param = "UNIQUAC_A12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).A12
                    ppu.m_uni.InteractionParameters(id1)(id2).A12 = value
                Case 3
                    param = "UNIQUAC_A21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).A21
                    ppu.m_uni.InteractionParameters(id1)(id2).A21 = value
                Case 4
                    param = "UNIQUAC_B12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).B12
                    ppu.m_uni.InteractionParameters(id1)(id2).B12 = value
                Case 5
                    param = "UNIQUAC_B21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).B21
                    ppu.m_uni.InteractionParameters(id1)(id2).B21 = value
                Case 6
                    param = "UNIQUAC_C12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).C12
                    ppu.m_uni.InteractionParameters(id1)(id2).C12 = value
                Case 7
                    param = "UNIQUAC_C21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).C21
                    ppu.m_uni.InteractionParameters(id1)(id2).C21 = value
            End Select
        End If
    End Sub

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim oldvalue As Double = 0.0#
            Dim newvalue As Double = 0.0#
            Dim ppu As PropertyPackages.UNIQUACPropertyPackage = _pp
            Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                    newvalue = Convert.ToDouble(value)
                    ppu.m_pr.InteractionParameters(id1)(id2).kij = CDbl(value)
            End Select
            If Not _form Is Nothing Then
                _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged, .Name = _pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"),
                                                                       .OldValue = oldvalue, .NewValue = newvalue, .ObjID = id1, .ObjID2 = id2,
                                                                       .Tag = _pp, .PropertyName = "PR_IP"})
            End If
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex

        Dim id1 As String = Me.KryptonDataGridView2.Rows(row).Cells(0).Tag.ToString
        Dim id2 As String = Me.KryptonDataGridView2.Rows(row).Cells(1).Tag.ToString

        Dim comp1, comp2 As ConstantProperties
        comp1 = _comps(id1)
        comp2 = _comps(id2)

        Dim Vc1 As Double = comp1.Critical_Volume
        Dim Vc2 As Double = comp2.Critical_Volume

        Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

        Me.KryptonDataGridView2.Rows(row).Cells(2).Value = tmp

    End Sub

    Dim actu(5), actn(5) As Double
    Dim ppu As PropertyPackages.UNIQUACPropertyPackage
    Dim uniquac As PropertyPackages.Auxiliary.UNIQUAC
    Dim ms As Streams.MaterialStream
    Dim ppuf, unifac As Object

    Private Function FunctionValue(ByVal x() As Double) As Double

        Dim a1(1), a2(1), a3(1) As Double

        uniquac.InteractionParameters.Clear()
        uniquac.InteractionParameters.Add(ppu.RET_VIDS()(0), New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
        uniquac.InteractionParameters(ppu.RET_VIDS()(0)).Add(ppu.RET_VIDS()(1), New PropertyPackages.Auxiliary.UNIQUAC_IPData())
        uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A12 = x(0)
        uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A21 = x(1)
        uniquac.InteractionParameters.Add(ppu.RET_VIDS()(1), New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
        uniquac.InteractionParameters(ppu.RET_VIDS()(1)).Add(ppu.RET_VIDS()(0), New PropertyPackages.Auxiliary.UNIQUAC_IPData())
        uniquac.InteractionParameters(ppu.RET_VIDS()(1))(ppu.RET_VIDS()(0)).A12 = x(1)
        uniquac.InteractionParameters(ppu.RET_VIDS()(1))(ppu.RET_VIDS()(0)).A21 = x(0)

        If GlobalSettings.Settings.EnableParallelProcessing Then
            Try
                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                   End Sub)
                Dim task3 As Task = TaskHelper.Run(Sub()
                                                       a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                                   End Sub)
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try
        Else
            a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
            a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
            a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
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

        ppu = New PropertyPackages.UNIQUACPropertyPackage
        uniquac = New PropertyPackages.Auxiliary.UNIQUAC


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

        Dim id1 As String = dgvu1.Rows(row).Cells(0).Tag.ToString
        Dim id2 As String = dgvu1.Rows(row).Cells(1).Tag.ToString

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

            x(0) = dgvu1.Rows(row).Cells(3).Value
            x(1) = dgvu1.Rows(row).Cells(4).Value

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
            dgvu1.Rows(row).Cells(4).Value = 0.0#
            dgvu1.Rows(row).Cells(5).Value = 0.0#
            dgvu1.Rows(row).Cells(6).Value = 0.0#
            dgvu1.Rows(row).Cells(7).Value = 0.0#

        Catch ex As Exception


        Finally

            Cursor = Cursors.Default

        End Try

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "uniquacip.dat")
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
                            row.Cells(4).Value = bip1.B12
                            row.Cells(5).Value = bip1.B21
                            row.Cells(6).Value = bip1.C12
                            row.Cells(7).Value = bip1.C21
                        End If

                        If bip2 IsNot Nothing Then
                            row.Cells(2).Value = bip2.A21
                            row.Cells(3).Value = bip2.A12
                            row.Cells(4).Value = bip2.B21
                            row.Cells(5).Value = bip2.B12
                            row.Cells(6).Value = bip2.C21
                            row.Cells(7).Value = bip2.C12
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
                     .A12 = row.Cells(2).Value.ToString().ToDoubleFromCurrent(), .A21 = row.Cells(3).Value.ToString().ToDoubleFromCurrent(),
                     .B12 = row.Cells(4).Value.ToString().ToDoubleFromCurrent(), .B21 = row.Cells(5).Value.ToString().ToDoubleFromCurrent(),
                     .C12 = row.Cells(6).Value.ToString().ToDoubleFromCurrent(), .C21 = row.Cells(7).Value.ToString().ToDoubleFromCurrent()})
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

    Private Sub ToolStripButton3_Click(sender As Object, e As EventArgs) Handles ToolStripButton3.Click
        Estimate("UNIFAC")
    End Sub

    Private Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        Estimate("UNIFAC-LL")
    End Sub

    Private Sub ToolStripButton5_Click(sender As Object, e As EventArgs) Handles ToolStripButton5.Click
        Estimate("MODFAC")
    End Sub

    Private Sub ToolStripButton6_Click(sender As Object, e As EventArgs) Handles ToolStripButton6.Click
        Estimate("MODFAC-NIST")
    End Sub

    Private Sub dgv1_EditingControlShowing(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewEditingControlShowingEventArgs) Handles dgvu1.EditingControlShowing

        Dim cb As ComboBox = TryCast(e.Control, ComboBox)

        If cb IsNot Nothing Then
            AddHandler cb.SelectionChangeCommitted, AddressOf SelectionChanged
        End If

    End Sub

    Private Sub SelectionChanged(ByVal sender As Object, ByVal e As EventArgs)

        Dim cb As ComboBox = sender
        Dim cell As DataGridViewCell = Me.dgvu1.SelectedCells(0)

        cell.Value = cb.SelectedItem.ToString

    End Sub

End Class
