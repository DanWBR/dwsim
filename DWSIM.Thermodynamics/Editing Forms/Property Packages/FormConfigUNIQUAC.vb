'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.


Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Text
Imports DotNumerics

Public Class FormConfigUNIQUAC

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormUNIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Loaded = False

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Me.KryptonDataGridView2.DataSource = Nothing

        If _pp.ComponentName.ToString.Contains("Raoult") Or _
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Vapor")) Then
            Me.FaTabStripItem2.Visible = False
            Exit Sub
        Else
            Me.FaTabStripItem2.Visible = True
        End If

        Me.KryptonDataGridView2.Rows.Clear()

        Dim ppu As PropertyPackages.UNIQUACPropertyPackage = _pp

        Dim nf As String = "0.####"

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
                                    dgvu1.Rows.Add(New Object() {(cp.Name), (cp2.Name), "", Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(b21, nf), Format(c12, nf), Format(c21, nf)})
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
                            dgvu1.Rows.Add(New Object() {(cp.Name), (cp2.Name), "", Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(b21, nf), Format(c12, nf), Format(c21, nf)})
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

        For Each r As DataGridViewRow In dgvu1.Rows
            Dim cb As DataGridViewComboBoxCell = r.Cells(2)
            cb.Items.Clear()
            Dim ipsets As List(Of BaseClasses.InteractionParameter) = Databases.UserIPDB.GetStoredIPsets(r.Cells(0).Value, r.Cells(1).Value, "UNIQUAC")
            cb.Items.Add(ipsets.Count)
            For Each ip As InteractionParameter In ipsets
                Dim strb As New StringBuilder
                For Each kvp As KeyValuePair(Of String, Object) In ip.Parameters
                    strb.Append(kvp.Key & ": " & Double.Parse(kvp.Value).ToString("N2") & ", ")
                Next
                strb.Append("{" & ip.DataType & " / " & ip.Description & "}")
                cb.Items.Add(strb.ToString)
                cb.Tag = ipsets
            Next
            r.Cells(2).Value = cb.Items(0)
        Next


        Loaded = True

    End Sub

    Private Sub KryptonDataGridView1_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs)

        'If Me.Loaded Then
        '    If e.ColumnIndex = 1 Then
        '        If Double.TryParse(e.FormattedValue, New Integer) = False Then
        '            Me.KryptonDataGridView1.Rows(e.RowIndex).ErrorText = _
        '                Calculator.GetLocalString("Ovalorinseridoinvlid")
        '            e.Cancel = True
        '        ElseIf CDbl(e.FormattedValue) < 0 Then
        '            Me.KryptonDataGridView1.Rows(e.RowIndex).ErrorText = _
        '                Calculator.GetLocalString("Ovalorinseridoinvlid")
        '            e.Cancel = True
        '        End If
        '    End If
        'End If

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
                    Dim cb As DataGridViewComboBoxCell = dgvu1.Rows(e.RowIndex).Cells(2)
                    If value <> "" Then
                        Dim i As Integer = -1
                        For Each s As String In cb.Items
                            If value = s And i <> -1 Then
                                Dim ipset As InteractionParameter = cb.Tag(i)
                                With dgvu1.Rows(e.RowIndex)
                                    If ipset.Parameters.ContainsKey("A12") Then .Cells(3).Value = ipset.Parameters("A12")
                                    If ipset.Parameters.ContainsKey("A21") Then .Cells(4).Value = ipset.Parameters("A21")
                                    If ipset.Parameters.ContainsKey("B12") Then .Cells(5).Value = ipset.Parameters("B12")
                                    If ipset.Parameters.ContainsKey("B21") Then .Cells(6).Value = ipset.Parameters("B21")
                                    If ipset.Parameters.ContainsKey("C12") Then .Cells(7).Value = ipset.Parameters("C12")
                                    If ipset.Parameters.ContainsKey("C21") Then .Cells(8).Value = ipset.Parameters("C21")
                                End With
                            End If
                            i += 1
                        Next
                    End If
                Case 3
                    param = "UNIQUAC_A12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).A12
                    ppu.m_uni.InteractionParameters(id1)(id2).A12 = value
                Case 4
                    param = "UNIQUAC_A21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).A21
                    ppu.m_uni.InteractionParameters(id1)(id2).A21 = value
                Case 5
                    param = "UNIQUAC_B12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).B12
                    ppu.m_uni.InteractionParameters(id1)(id2).B12 = value
                Case 6
                    param = "UNIQUAC_B21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).B21
                    ppu.m_uni.InteractionParameters(id1)(id2).B21 = value
                Case 7
                    param = "UNIQUAC_C12"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).C12
                    ppu.m_uni.InteractionParameters(id1)(id2).C12 = value
                Case 8
                    param = "UNIQUAC_C21"
                    oldvalue = ppu.m_uni.InteractionParameters(id1)(id2).C21
                    ppu.m_uni.InteractionParameters(id1)(id2).C21 = value
            End Select
            If Not _form Is Nothing Then
                _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                   .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, param, oldvalue, value),
                                                                   .OldValue = oldvalue, .NewValue = value, .ObjID = id1, .ObjID2 = id2,
                                                                   .Tag = _pp, .PropertyName = param})
            End If
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

        If GlobalSettings.Settings.EnableParallelProcessing Then
            Try
                Dim task1 As Task = New Task(Sub()
                                                 a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
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

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click, Button2.Click, Button5.Click, Button6.Click

        Dim row As Integer = dgvu1.SelectedCells(0).RowIndex
        Dim x(1) As Double

        Cursor = Cursors.WaitCursor

        ms = New Streams.MaterialStream("", "")

        ppu = New PropertyPackages.UNIQUACPropertyPackage
        uniquac = New PropertyPackages.Auxiliary.UNIQUAC


        If sender.Name = "Button1" Then
            ppuf = New PropertyPackages.UNIFACPropertyPackage
            unifac = New PropertyPackages.Auxiliary.Unifac
        ElseIf sender.Name = "Button5" Then
            ppuf = New PropertyPackages.UNIFACLLPropertyPackage
            unifac = New PropertyPackages.Auxiliary.UnifacLL
        ElseIf sender.Name = "Button6" Then
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

        If Not chkTdep.Checked Then

            Dim T1 = 298.15

            Dim a1(1), a2(1), a3(1) As Double

            Try

                If GlobalSettings.Settings.EnableParallelProcessing Then
                    If GlobalSettings.Settings.EnableGPUProcessing Then GlobalSettings.Settings.gpu.EnableMultithreading()
                    Try
                        Dim task1 As Task = New Task(Sub()
                                                         a1 = unifac.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                     End Sub)
                        Dim task2 As Task = New Task(Sub()
                                                         a2 = unifac.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                     End Sub)
                        Dim task3 As Task = New Task(Sub()
                                                         a3 = unifac.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                     End Sub)
                        task1.Start()
                        task2.Start()
                        task3.Start()
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

                Dim variables(1) As Optimization.OptBoundVariable
                For i As Integer = 0 To 1
                    variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
                Next
                Dim solver As New Optimization.Simplex
                solver.Tolerance = 0.000001
                solver.MaxFunEvaluations = 5000
                finalval2 = solver.ComputeMin(AddressOf FunctionValue, variables)

                Dim avgerr As Double = 0.0#
                For i As Integer = 0 To 5
                    avgerr += (actn(i) - actu(i)) / actu(i) * 100 / 6
                Next

                'If sender.Name = "Button1" Then
                '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation @ T = " & Format(T1, "N2") & " K using UNIFAC finished, average activity coefficient error = " & Format(avgerr, "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
                'ElseIf sender.Name = "Button5" Then
                '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation @ T = " & Format(T1, "N2") & " K using UNIFAC-LL finished, average activity coefficient error = " & Format(avgerr, "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
                'ElseIf sender.Name = "Button6" Then
                '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation @ T = " & Format(T1, "N2") & " K using NIST-MODFAC finished, average activity coefficient error = " & Format(avgerr, "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
                'Else
                '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation @ T = " & Format(T1, "N2") & " K using MODFAC finished, average activity coefficient error = " & Format(avgerr, "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
                'End If

                dgvu1.Rows(row).Cells(3).Value = finalval2(0)
                dgvu1.Rows(row).Cells(4).Value = finalval2(1)
                dgvu1.Rows(row).Cells(5).Value = 0.0#
                dgvu1.Rows(row).Cells(6).Value = 0.0#
                dgvu1.Rows(row).Cells(7).Value = 0.0#
                dgvu1.Rows(row).Cells(8).Value = 0.0#

            Catch ex As Exception

                'ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation finished with an error: " & ex.ToString, Color.Red, DWSIM.FormClasses.TipoAviso.Informacao)

            Finally

                Cursor = Cursors.Default

            End Try

        Else

            Me.GroupBox3.Enabled = False

            Me.BackgroundWorker1.RunWorkerAsync(New String() {sender.Name})

        End If

    End Sub

    Private Sub BackgroundWorker1_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        Dim Tmin, Tmax, T1, dT As Double, i, j As Integer

        Tmin = MathEx.Common.Max(ppu.RET_VTF())
        Tmax = MathEx.Common.Min(ppu.RET_VTB())

        dT = (Tmax - Tmin) / 5

        Dim a12, a21 As New ArrayList

        Dim avgerr As Double = 0.0#

        For i = Tmin To Tmax Step dT

            T1 = i

            Dim a1(1), a2(1), a3(1) As Double

            If GlobalSettings.Settings.EnableParallelProcessing Then
                If GlobalSettings.Settings.EnableGPUProcessing Then GlobalSettings.Settings.gpu.EnableMultithreading()
                Try
                    Dim task1 As Task = New Task(Sub()
                                                     a1 = unifac.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ(), ppu.RET_VR, ppuf.RET_VEKI)
                                                 End Sub)
                    Dim task2 As Task = New Task(Sub()
                                                     a2 = unifac.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                 End Sub)
                    Dim task3 As Task = New Task(Sub()
                                                     a3 = unifac.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ(), ppuf.RET_VR, ppuf.RET_VEKI)
                                                 End Sub)
                    task1.Start()
                    task2.Start()
                    task3.Start()
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

            Dim initval2() As Double = New Double() {0.0#, 0.0#}

            If a12.Count > 0 Then
                initval2 = New Double() {a12(a12.Count - 1), a21(a21.Count - 1)}
            End If

            Dim lconstr2() As Double = New Double() {-10000.0#, -10000.0#}
            Dim uconstr2() As Double = New Double() {+10000.0#, +10000.0#}
            Dim finalval2() As Double = Nothing

            Dim variables(1) As Optimization.OptBoundVariable
            For j = 0 To 1
                variables(j) = New Optimization.OptBoundVariable("x" & CStr(j + 1), initval2(j), False, lconstr2(j), uconstr2(j))
            Next
            Dim solver As New Optimization.Simplex
            solver.Tolerance = 0.000001
            solver.MaxFunEvaluations = 5000
            finalval2 = solver.ComputeMin(AddressOf FunctionValue, variables)

            a12.Add(finalval2(0))
            a21.Add(finalval2(1))

            For j = 0 To 5
                avgerr += (actn(j) - actu(j)) / actu(j) * 100 / 6 / 5
            Next

        Next

        'regress calculated parameters to obtain temperature dependency

        Dim px(4), py_a12(4), py_a21(4) As Double

        For i = 0 To 4
            px(i) = Tmin + i * dT
            py_a12(i) = a12(i)
            py_a21(i) = a21(i)
        Next

        Dim obj As Object = Nothing
        Dim lmfit As New MathEx.LM.LMFit

        Dim c_a12(2), c_a21(2) As Double
        Dim r_a12, r_a21, n_a12, n_a21 As Double

        c_a12(0) = py_a12(0)
        c_a12(1) = 0.1
        c_a12(2) = 0.01

        obj = lmfit.GetCoeffs(px, py_a12, c_a12.Clone, MathEx.LM.LMFit.FitType.SecondDegreePoly, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
        c_a12 = obj(0)
        r_a12 = obj(2)
        n_a12 = obj(3)

        c_a21(0) = py_a21(0)
        c_a21(1) = 0.1
        c_a21(2) = 0.01

        obj = lmfit.GetCoeffs(px, py_a21, c_a21.Clone, MathEx.LM.LMFit.FitType.SecondDegreePoly, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
        c_a21 = obj(0)
        r_a21 = obj(2)
        n_a21 = obj(3)

        e.Result = New Object() {c_a12(0), c_a21(0), c_a12(1), c_a21(1), c_a12(2), c_a21(2), avgerr, Tmin, Tmax, e.Argument(0)}

    End Sub

    Private Sub BackgroundWorker1_RunWorkerCompleted(sender As Object, e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorker1.RunWorkerCompleted

        Me.GroupBox3.Enabled = True
        Cursor = Cursors.Default

        If e.Error Is Nothing Then

            'If e.Result(9) = "Button1" Then
            '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation from " & Format(e.Result(7), "N2") & " to " & Format(e.Result(8), "N2") & " K using UNIFAC finished, average activity coefficient error = " & Format(e.Result(6), "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
            'ElseIf e.Result(9) = "Button5" Then
            '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation from " & Format(e.Result(7), "N2") & " to " & Format(e.Result(8), "N2") & " K using UNIFAC-LL finished, average activity coefficient error = " & Format(e.Result(6), "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
            'Else
            '    ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation from " & Format(e.Result(7), "N2") & " to " & Format(e.Result(8), "N2") & " K using MODFAC finished, average activity coefficient error = " & Format(e.Result(6), "N2") & "%.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)
            'End If

            Dim row As Integer = dgvu1.SelectedCells(0).RowIndex

            dgvu1.Rows(row).Cells(3).Value = e.Result(0)
            dgvu1.Rows(row).Cells(4).Value = e.Result(1)
            dgvu1.Rows(row).Cells(5).Value = e.Result(2)
            dgvu1.Rows(row).Cells(6).Value = e.Result(3)
            dgvu1.Rows(row).Cells(7).Value = e.Result(4)
            dgvu1.Rows(row).Cells(8).Value = e.Result(5)

        Else

            ' ppu.CurrentMaterialStream.FlowSheet.WriteToLog("UNIQUAC interaction parameter estimation from " & Format(e.Result(7), "N2") & " to " & Format(e.Result(8), "N2") & " K using MODFAC finished with an error: " & e.Error.ToString, Color.Red, DWSIM.FormClasses.TipoAviso.Informacao)

        End If

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "uniquacip.dat")
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
