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
Imports unvell.ReoGrid.DataFormat
Imports unvell.ReoGrid
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormConfigPropertyPackage

    Inherits FormConfigPropertyPackageBase

    Public Loaded As Boolean = False

    Private Sub SetupGrid()

        Dim comps = _pp.Flowsheet.SelectedCompounds.Keys

        With gridKij.Worksheets(0)
            .SelectionForwardDirection = SelectionForwardDirection.Down
            .SetScale(Settings.DpiScale)
            .SetRows(comps.Count)
            .SetCols(comps.Count)
            .SetColumnsWidth(0, comps.Count, 100)
            .RowHeaderWidth = 100
            For i = 0 To comps.Count - 1
                .ColumnHeaders(i).Text = comps(i)
                .RowHeaders(i).Text = comps(i)
            Next
            .SetRangeDataFormat(0, 0, comps.Count, comps.Count, CellDataFormatFlag.Number,
            New NumberDataFormatter.NumberFormatArgs With {
                .DecimalPlaces = 4,
                .UseSeparator = True,
                .NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus
            })
            .SetRangeStyles(0, 0, comps.Count, comps.Count, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Right
            })
            .SetRangeStyles(0, 0, comps.Count, comps.Count, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.VerticalAlign,
                .VAlign = ReoGridVerAlign.Middle
            })
            .SetRangeStyles(0, 0, comps.Count, comps.Count, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.FontAll,
                .FontName = SystemFonts.MessageBoxFont.Name,
                .FontSize = SystemFonts.MessageBoxFont.SizeInPoints
            })
            Dim Changing As Boolean = False
            AddHandler .CellDataChanged,
                Sub(sender, e)
                    If Loaded And Not Changing Then
                        Try
                            SetKijVal(.RowHeaders(e.Cell.Row).Text, .ColumnHeaders(e.Cell.Column).Text, e.Cell.Data)
                            Changing = True
                            .Cells(e.Cell.Column, e.Cell.Row).Data = e.Cell.Data
                        Catch ex As Exception
                        Finally
                            Changing = False
                        End Try
                    End If
                End Sub
            For i = 0 To comps.Count - 1
                For j = 0 To comps.Count - 1
                    If i = j Then
                        .Cells(i, j).IsReadOnly = True
                        .Cells(i, j).Style.BackColor = unvell.ReoGrid.Graphics.SolidColor.LightSteelBlue
                    End If
                Next
            Next
        End With

    End Sub

    Private Sub FormConfigPR_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        SetupGrid()

        Dim sheet = gridKij.Worksheets(0)

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

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

        Dim i, j As Integer

        If TypeOf _pp Is SRKPropertyPackage Then

            lblModel.Text = "Model Parameters: SRK EOS kij"

            Dim ppu As SRKPropertyPackage = _pp
            i = 0
            For Each cp As ConstantProperties In _comps.Values
gt1:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt1
                End If
                i += 1
            Next

        ElseIf TypeOf _pp Is UNIFACPropertyPackage Then

            lblModel.Text = "Model Parameters: PR EOS kij"

            Dim ppu As UNIFACPropertyPackage = _pp

            FillUNIFACParamTable("UNIFAC")

            i = 0
            For Each cp As ConstantProperties In _comps.Values
gtu:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtu
                End If
                i += 1
            Next

        ElseIf TypeOf _pp Is UNIFACLLPropertyPackage Then

            lblModel.Text = "Model Parameters: PR EOS kij"

            Dim ppu As UNIFACLLPropertyPackage = _pp

            FillUNIFACParamTable("UNIFACLL")
            i = 0
            For Each cp As ConstantProperties In _comps.Values
gtul:           If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtul
                End If
                i += 1
            Next

        ElseIf TypeOf _pp Is MODFACPropertyPackage Then

            lblModel.Text = "Model Parameters: PR EOS kij"

            Dim ppu As MODFACPropertyPackage = _pp

            FillUNIFACParamTable("MODFAC")

            i = 0
            For Each cp As ConstantProperties In _comps.Values
gtmu:           If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtmu
                End If
                i += 1
            Next
        ElseIf TypeOf _pp Is NISTMFACPropertyPackage Then

            lblModel.Text = "Model Parameters: PR EOS kij"

            Dim ppu As NISTMFACPropertyPackage = _pp

            FillUNIFACParamTable("NIST-MODFAC")

            i = 0
            For Each cp As ConstantProperties In _comps.Values
gtmun:          If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gtmun
                End If
                i += 1
            Next

        ElseIf TypeOf _pp Is PengRobinson1978PropertyPackage Then

            lblModel.Text = "Model Parameters: PR78 EOS kij"

            Dim ppu As PengRobinson1978PropertyPackage = _pp

            i = 0
            For Each cp As ConstantProperties In _comps.Values
gt2:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt2
                End If
                i += 1
            Next

        ElseIf TypeOf _pp Is PengRobinsonPropertyPackage Then

            lblModel.Text = "Model Parameters: PR EOS kij"

            Dim ppu As PengRobinsonPropertyPackage = _pp

            i = 0
            For Each cp As ConstantProperties In _comps.Values
gt3:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                    j = 0
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        sheet.SetCellData(i, j, a12)
                                        sheet.SetCellData(j, i, a12)
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                sheet.SetCellData(i, j, a12)
                                sheet.SetCellData(j, i, a12)
                            End If
                        End If
                        j += 1
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.PR_IPData))
                    GoTo gt3
                End If
                i += 1
            Next

        End If

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub SetKijVal(id1 As String, id2 As String, newvalue As Double)
        If Loaded Then
            If TypeOf _pp Is SRKPropertyPackage Then
                Dim ppu As PropertyPackages.SRKPropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            ElseIf TypeOf _pp Is PengRobinson1978PropertyPackage Then
                Dim ppu As PropertyPackages.PengRobinson1978PropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            ElseIf TypeOf _pp Is PengRobinsonPropertyPackage Then
                Dim ppu As PropertyPackages.PengRobinsonPropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            ElseIf TypeOf _pp Is UNIFACPropertyPackage Then
                Dim ppu As PropertyPackages.UNIFACPropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            ElseIf TypeOf _pp Is UNIFACLLPropertyPackage Then
                Dim ppu As PropertyPackages.UNIFACLLPropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            ElseIf TypeOf _pp Is MODFACPropertyPackage Then
                Dim ppu As PropertyPackages.MODFACPropertyPackage = _pp
                If ppu.m_pr.InteractionParameters.ContainsKey(id1) Then
                    If ppu.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = newvalue
                    Else
                        ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                    End If
                Else
                    ppu.m_pr.InteractionParameters(id2)(id1).kij = newvalue
                End If
            End If
        End If

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        Dim selected = gridKij.Worksheets(0).SelectionRange

        For i = selected.Row To selected.EndRow
            For j = selected.Col To selected.EndCol

                If i <> j Then

                    Dim id1 As String = gridKij.Worksheets(0).RowHeaders(i).Text
                    Dim id2 As String = gridKij.Worksheets(0).ColumnHeaders(j).Text

                    Dim comp1, comp2 As ConstantProperties
                    comp1 = _comps(id1)
                    comp2 = _comps(id2)

                    Dim Vc1 As Double = comp1.Critical_Volume
                    Dim Vc2 As Double = comp2.Critical_Volume

                    Dim tmp As Double = 1.0 - 8.0 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ 0.3333 + Vc2 ^ 0.3333) ^ 3.0)

                    gridKij.Worksheets(0).Cells(i, j).Data = tmp
                    gridKij.Worksheets(0).Cells(j, i).Data = tmp

                End If

            Next
        Next

        _pp.AreModelParametersDirty = True

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim ppu As Object = _pp
        If ppu.ComponentName.Contains("SRK") Then
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "srk_ip.dat")
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "pr_ip.dat")
        End If
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

    Private Sub Button2_Click_1(sender As Object, e As EventArgs) Handles Button2.Click

        Dim comps = _pp.Flowsheet.SelectedCompounds.Keys

        Dim BIPs As New List(Of Auxiliary.PR_IPData)

        With gridKij.Worksheets(0)
            For i = 0 To comps.Count - 1
                For j = 0 To comps.Count - 1
                    If i <> j Then
                        Dim ki, kj As Integer
                        ki = i
                        kj = j
                        Dim bip1 = BIPs.Where(Function(b) b.Name1 = comps(ki) And b.Name2 = comps(kj)).FirstOrDefault()
                        Dim bip2 = BIPs.Where(Function(b) b.Name1 = comps(kj) And b.Name2 = comps(ki)).FirstOrDefault()
                        If bip1 Is Nothing And bip2 Is Nothing Then
                            Dim value As Double = .Cells(i, j).Data
                            BIPs.Add(New Auxiliary.PR_IPData With {.Name1 = comps(i), .Name2 = comps(j), .kij = value})
                        End If
                    End If
                Next
            Next
        End With

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Using stream As New System.IO.MemoryStream()
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

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()
        Dim BIPs As List(Of PropertyPackages.Auxiliary.PR_IPData)

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON file", "*.json")})

        If openedFile IsNot Nothing Then

            Try

                Dim comps = _pp.Flowsheet.SelectedCompounds.Keys

                BIPs = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of PropertyPackages.Auxiliary.PR_IPData))(openedFile.ReadAllText())

                If MessageBox.Show("Interaction Parameters loaded successfully. Proceed with overwriting current values?",
                                   "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then

                    With gridKij.Worksheets(0)

                        For i = 0 To comps.Count - 1

                            For j = 0 To comps.Count - 1

                                If i <> j Then

                                    Dim c1 = comps(i)
                                    Dim c2 = comps(j)

                                    Dim bip1 = BIPs.Where(Function(b) b.Name1 = c1 And b.Name2 = c2).FirstOrDefault()
                                    Dim bip2 = BIPs.Where(Function(b) b.Name1 = c2 And b.Name2 = c1).FirstOrDefault()

                                    If bip1 IsNot Nothing Then
                                        .Cells(i, j).Data = bip1.kij
                                    End If

                                    If bip2 IsNot Nothing Then
                                        .Cells(i, j).Data = bip2.kij
                                    End If

                                End If

                            Next

                        Next

                    End With

                End If

            Catch ex As Exception

                MessageBox.Show("Error: " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        End If

    End Sub

End Class
