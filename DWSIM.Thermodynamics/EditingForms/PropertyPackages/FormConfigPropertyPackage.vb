'    Copyright 2008 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.IO
Imports System.Drawing

Public Class FormConfigPropertyPackage

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False

    Private Sub FormConfigPR_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        Me.KryptonDataGridView2.DataSource = Nothing
    End Sub

    Private Sub FillUNIFACParamTable(ByVal type As String)

        'fill UNIFAC interaction parameter list
        Dim k, l As Integer
        Dim PrimaryGroups As New SortedList()
        Dim uni As Object = Nothing
        Dim g1, g2 As Integer
        Dim ip, pg As String

        Select Case type
            Case "UNIFAC"
                uni = New PropertyPackages.Auxiliary.Unifac
            Case "UNIFACLL"
                uni = New PropertyPackages.Auxiliary.UnifacLL
            Case "MODFAC"
                uni = New PropertyPackages.Auxiliary.Modfac
            Case "NIST-MODFAC"
                uni = New PropertyPackages.Auxiliary.NISTMFAC
        End Select

        'create list of all subgroups and primary groups
        PrimaryGroups.Clear()
        For Each cp As ConstantProperties In _comps.Values
            If type = "UNIFAC" Or type = "UNIFACLL" Then
                For Each ufg In cp.UNIFACGroups.Keys
                    pg = uni.UnifGroups.Groups(Integer.Parse(ufg)).PrimGroupName
                    If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.UnifGroups.Groups(ufg).PrimaryGroup)
                Next
            ElseIf type = "NIST-MODFAC" Then
                For Each ufg In cp.NISTMODFACGroups.Keys
                    pg = uni.ModfGroups.Groups(Integer.Parse(ufg)).MainGroupName
                    If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.ModfGroups.Groups(ufg).PrimaryGroup)
                Next
            Else
                For Each ufg In cp.MODFACGroups.Keys
                    pg = uni.ModfGroups.Groups(Integer.Parse(ufg)).MainGroupName
                    If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.ModfGroups.Groups(ufg).PrimaryGroup)
                Next
            End If
        Next

        IPGrid.ColumnCount = PrimaryGroups.Count + 1
        IPGrid.RowCount = PrimaryGroups.Count + _comps.Count + 1
        IPGrid.Columns(0).HeaderText = "Component"
        k = 1
        For Each gn As String In PrimaryGroups.Keys
            IPGrid.Columns(k).HeaderText = gn
            IPGrid.Item(k, _comps.Count).Value = gn
            IPGrid.Item(k, _comps.Count).ToolTipText = "Main group"
            IPGrid.Item(0, _comps.Count + k).Value = gn
            IPGrid.Item(0, _comps.Count + k).ToolTipText = "Main group"
            IPGrid.Item(0, _comps.Count + k).Style.Alignment = DataGridViewContentAlignment.MiddleRight
            IPGrid.Item(0, _comps.Count + k).Style.BackColor = Color.Crimson
            IPGrid.Item(k, _comps.Count).Style.BackColor = Color.Crimson
            IPGrid.Item(0, _comps.Count + k).Style.ForeColor = Color.Yellow
            IPGrid.Item(k, _comps.Count).Style.ForeColor = Color.Yellow
            k += 1
        Next
        IPGrid.Item(0, _comps.Count).Style.ForeColor = Color.Yellow
        IPGrid.Item(0, _comps.Count).Style.BackColor = Color.Crimson
        IPGrid.Item(0, _comps.Count).Selected = True
        IPGrid.Item(0, _comps.Count).Value = "Interaction Parameters"
        IPGrid.Item(0, _comps.Count).Style.Alignment = DataGridViewContentAlignment.MiddleRight

        For k = 1 To PrimaryGroups.Count
            For l = 0 To _comps.Count - 1
                IPGrid.Item(k, l).Style.BackColor = Color.LightGray
            Next
        Next

        'List of components with their groups
        For Each s1 As String In PrimaryGroups.Keys
            For Each s2 As String In PrimaryGroups.Keys
                g1 = PrimaryGroups.Item(s1)
                g2 = PrimaryGroups.Item(s2)

                If g1 = g2 Then
                    IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Style.BackColor = Color.Black
                Else
                    If type = "UNIFAC" Or type = "UNIFACLL" Then
                        If uni.UnifGroups.InteracParam.ContainsKey(g1) Then
                            If uni.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                                ip = uni.UnifGroups.InteracParam(g1).Item(g2)
                            Else
                                ip = "X"
                            End If
                        Else
                            ip = "X"
                        End If
                    Else
                        If uni.ModfGroups.InteracParam_aij.ContainsKey(g1) And uni.ModfGroups.InteracParam_aij(g1).ContainsKey(g2) Then
                            ip = "A: " & uni.ModfGroups.InteracParam_aij(g1).Item(g2) & vbCrLf & "B: " & uni.ModfGroups.InteracParam_bij(g1).Item(g2) & vbCrLf & "C: " & uni.ModfGroups.InteracParam_cij(g1).Item(g2)

                            If uni.ModfGroups.InteracParam_aij(g1).Item(g2) = 0 Then IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Style.BackColor = Color.Yellow
                        Else
                            If uni.ModfGroups.InteracParam_aij.ContainsKey(g2) And uni.ModfGroups.InteracParam_aij(g2).ContainsKey(g1) Then
                                ip = "A: " & uni.ModfGroups.InteracParam_aji(g2).Item(g1) & vbCrLf & "B: " & uni.ModfGroups.InteracParam_bji(g2).Item(g1) & vbCrLf & "C: " & uni.ModfGroups.InteracParam_cji(g2).Item(g1)

                                If uni.ModfGroups.InteracParam_aji(g2).Item(g1) = 0 Then IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Style.BackColor = Color.Yellow
                            Else
                                ip = "X"
                            End If
                        End If
                    End If

                    IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Style.WrapMode = DataGridViewTriState.True
                    IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Value = ip
                    If ip = "X" Then
                        IPGrid.Item(PrimaryGroups.IndexOfKey(s1) + 1, PrimaryGroups.IndexOfKey(s2) + 1 + _comps.Count).Style.BackColor = Color.Yellow
                    End If
                End If

            Next
        Next

        'Fill table with subgoup list of components
        k = 0
        For Each cp As ConstantProperties In _comps.Values
            IPGrid.Item(0, k).Value = cp.Name
            IPGrid.Item(0, k).Style.BackColor = Color.CadetBlue
            IPGrid.Item(0, k).Style.ForeColor = Color.White
            IPGrid.Item(0, k).Style.Alignment = DataGridViewContentAlignment.MiddleRight
            If type = "UNIFAC" Or type = "UNIFACLL" Then
                If cp.UNIFACGroups.Count > 0 Then
                    For Each ufg As String In cp.UNIFACGroups.Keys
                        l = Integer.Parse(ufg)
                        pg = uni.UnifGroups.Groups(l).PrimGroupName
                        l = PrimaryGroups.IndexOfKey(pg)
                        IPGrid.Item(l + 1, k).Value = IPGrid.Item(l + 1, k).Value + cp.UNIFACGroups.Item(ufg)
                    Next
                Else
                    IPGrid.Item(0, k).Style.BackColor = Color.Yellow
                    IPGrid.Item(0, k).Style.ForeColor = Color.Black
                End If
            ElseIf type = "MODFAC" Then
                If cp.MODFACGroups.Count > 0 Then
                    For Each ufg As String In cp.MODFACGroups.Keys
                        l = Integer.Parse(ufg)
                        pg = uni.ModfGroups.Groups(l).MainGroupName
                        l = PrimaryGroups.IndexOfKey(pg)
                        g1 = IPGrid.Item(l + 1, k).Value
                        g2 = cp.MODFACGroups.Item(ufg)
                        IPGrid.Item(l + 1, k).Value = g1 + g2
                    Next
                Else
                    IPGrid.Item(0, k).Style.BackColor = Color.Yellow
                    IPGrid.Item(0, k).Style.ForeColor = Color.Black
                End If
            Else 'NIST-MODFAC
                If cp.NISTMODFACGroups.Count > 0 Then
                    For Each ufg As String In cp.NISTMODFACGroups.Keys
                        l = Integer.Parse(ufg)
                        pg = uni.ModfGroups.Groups(l).MainGroupName
                        l = PrimaryGroups.IndexOfKey(pg)
                        g1 = IPGrid.Item(l + 1, k).Value
                        g2 = cp.NISTMODFACGroups.Item(ufg)
                        IPGrid.Item(l + 1, k).Value = g1 + g2
                    Next
                Else
                    IPGrid.Item(0, k).Style.BackColor = Color.Yellow
                    IPGrid.Item(0, k).Style.ForeColor = Color.Black
                End If
            End If
            k += 1
        Next
    End Sub

    Private Sub FormConfigPR_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Me.KryptonDataGridView2.DataSource = Nothing

        If _pp.ComponentName.ToString.Contains("Raoult") Or
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Vapor")) Or
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Chao-Seader")) Or
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Grayson-Streed")) Or
           _pp.ComponentName.ToString.Contains("CoolProp") Then
            TabControl1.TabPages.Remove(TabPage1)
            TabControl1.TabPages.Remove(TabPageU)
            Exit Sub
        End If
        If TypeOf _pp Is UNIFACPropertyPackage Then
            TabControl1.SelectedTab = Me.TabPageU
            TabPageU.Text = "UNIFAC"
        ElseIf TypeOf _pp Is UNIFACLLPropertyPackage Then
            TabControl1.SelectedTab = Me.TabPageU
            TabPageU.Text = "UNIFAC-LL"
        ElseIf TypeOf _pp Is MODFACPropertyPackage Then
            TabControl1.SelectedTab = Me.TabPageU
            TabPageU.Text = "MODFAC (Dortmund)"
        ElseIf TypeOf _pp Is NISTMFACPropertyPackage Then
            TabControl1.SelectedTab = Me.TabPageU
            TabPageU.Text = "MODFAC (NIST)"
        Else
            TabControl1.TabPages.Remove(TabPageU)
        End If
        Me.KryptonDataGridView2.Rows.Clear()

        Dim nf As String = "N6"

        If TypeOf _pp Is SRKPropertyPackage Then

            Dim ppu As SRKPropertyPackage = _pp

            For Each cp As ConstantProperties In _comps.Values
gt1:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt1
                End If
            Next

        ElseIf TypeOf _pp Is UNIFACPropertyPackage Then

            Dim ppu As UNIFACPropertyPackage = _pp

            FillUNIFACParamTable("UNIFAC")

            For Each cp As ConstantProperties In _comps.Values
gtu:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtu
                End If
            Next

        ElseIf TypeOf _pp Is UNIFACLLPropertyPackage Then

            Dim ppu As UNIFACLLPropertyPackage = _pp

            FillUNIFACParamTable("UNIFACLL")

            For Each cp As ConstantProperties In _comps.Values
gtul:           If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtul
                End If
            Next

        ElseIf TypeOf _pp Is MODFACPropertyPackage Then

            Dim ppu As MODFACPropertyPackage = _pp

            FillUNIFACParamTable("MODFAC")

            For Each cp As ConstantProperties In _comps.Values
gtmu:           If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtmu
                End If
            Next
        ElseIf TypeOf _pp Is NISTMFACPropertyPackage Then

            Dim ppu As NISTMFACPropertyPackage = _pp

            FillUNIFACParamTable("NIST-MODFAC")

            For Each cp As ConstantProperties In _comps.Values
gtmun:          If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtmun
                End If
            Next

        ElseIf TypeOf _pp Is PengRobinson1978PropertyPackage Then

            Dim ppu As PengRobinson1978PropertyPackage = _pp

            For Each cp As ConstantProperties In _comps.Values
gt2:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt2
                End If
            Next

        ElseIf TypeOf _pp Is PengRobinsonPropertyPackage Then

            Dim ppu As PengRobinsonPropertyPackage = _pp

            For Each cp As ConstantProperties In _comps.Values
gt3:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, Format(a12, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt3
                End If
            Next

        End If

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

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim oldvalue As Double = 0.0#
            Dim newvalue As Double = 0.0#
            Dim id1 As String = ""
            Dim id2 As String = ""
            If TypeOf _pp Is SRKPropertyPackage Then
                Dim ppu As PropertyPackages.SRKPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            ElseIf TypeOf _pp Is PengRobinson1978PropertyPackage Then
                Dim ppu As PropertyPackages.PengRobinson1978PropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            ElseIf TypeOf _pp Is PengRobinsonPropertyPackage Then
                Dim ppu As PropertyPackages.PengRobinsonPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            ElseIf TypeOf _pp Is UNIFACPropertyPackage Then
                Dim ppu As PropertyPackages.UNIFACPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            ElseIf TypeOf _pp Is UNIFACLLPropertyPackage Then
                Dim ppu As PropertyPackages.UNIFACLLPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            ElseIf TypeOf _pp Is MODFACPropertyPackage Then
                Dim ppu As PropertyPackages.MODFACPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = Convert.ToDouble(value)
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                End Select
            End If
            If Not _form Is Nothing Then
                _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                   .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, "PR_IP", oldvalue, newvalue),
                                                                   .OldValue = oldvalue, .NewValue = newvalue, .ObjID = id1, .ObjID2 = id2,
                                                                   .Tag = _pp, .PropertyName = "PR_IP"})
            End If
        End If

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        For Each r2 As DataGridViewCell In Me.KryptonDataGridView2.SelectedCells

            Dim r = Me.KryptonDataGridView2.Rows(r2.RowIndex)

            Dim id1 As String = r.Cells(0).Tag.ToString
            Dim id2 As String = r.Cells(1).Tag.ToString

            Dim comp1, comp2 As ConstantProperties
            comp1 = _comps(id1)
            comp2 = _comps(id2)

            Dim Vc1 As Double = comp1.Critical_Volume
            Dim Vc2 As Double = comp2.Critical_Volume

            Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

            r.Cells(2).Value = tmp

        Next

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim ppu As Object = _pp
        If ppu.ComponentName.Contains("SRK") Then
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "srk_ip.dat")
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "pr_ip.dat")
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        For Each r As DataGridViewRow In Me.KryptonDataGridView2.Rows

            If r.Cells(2).Value = "" Then

                Dim id1 As String = r.Cells(0).Tag.ToString
                Dim id2 As String = r.Cells(1).Tag.ToString

                Dim comp1, comp2 As ConstantProperties
                comp1 = _comps(id1)
                comp2 = _comps(id2)

                Dim Vc1 As Double = comp1.Critical_Volume
                Dim Vc2 As Double = comp2.Critical_Volume

                Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

                r.Cells(2).Value = tmp

            End If

        Next

    End Sub

End Class
