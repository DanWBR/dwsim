'    Regression Utility for Interaction coefficients for
'    UNIFAC / MODFAC / NIST-MODFAC model
'
'    Copyright 2015 Gregor Reichert
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
Imports DWSIM.MathOps.MathEx
Imports System.Math
Imports ZedGraph
Imports DotNumerics
Imports Cureos.Numerics
Imports DWSIM.DWSIM.Optimization.DatRegression
Imports System.Threading.Tasks
Imports System.Linq
Imports System.IO
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormUNIFACRegression

    Inherits Form

    Public mycase As New UNIFACIPRegressionCase

    Public cv As SystemsOfUnits.Converter
    Public ci As Globalization.CultureInfo = Globalization.CultureInfo.CurrentUICulture
    Public _pp As Object
    Public _comps As New Dictionary(Of String, BaseClasses.ConstantProperties)
    Public uni As Object = Nothing
    Public IP As New BaseClasses.InteractionParameter

    Dim mat As Streams.MaterialStream
    Dim GI1, GI2 As Integer
    Dim GN1, GN2, Comp1, Comp2 As String

    Dim FileHandler As IVirtualFile

    Private Sub FormUNIFACRegression_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)
        cv = New SystemsOfUnits.Converter
        mat = New MaterialStream("", "")
        Comp1 = ""
        Comp2 = ""
    End Sub

    Private Sub tbTitle_TextChanged(sender As Object, e As EventArgs) Handles tbTitle.TextChanged
        Me.Text = tbTitle.Text
    End Sub

    Private Sub FillIPGrid(sender As Object, e As EventArgs) Handles BtnShowIPGrid.Click
        'fill UNIFAC interaction parameter list
        Dim k, l As Integer
        Dim PrimaryGroups As New SortedList()

        Dim g1, g2 As Integer
        Dim ip, pg, myType As String

        myType = ""

        'create list of all subgroups and primary groups
        PrimaryGroups.Clear()
        Select Case cbModel.SelectedItem
            Case "UNIFAC"
                myType = "UNIFAC"
                For Each cp As ConstantProperties In _comps.Values
                    For Each ufg In cp.UNIFACGroups.Keys
                        pg = uni.UnifGroups.Groups(Integer.Parse(ufg)).PrimGroupName
                        If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.UnifGroups.Groups(ufg).PrimaryGroup)
                    Next
                Next
            Case "Modified UNIFAC (Dortmund)"
                myType = "MODFAC"
                For Each cp As ConstantProperties In _comps.Values
                    For Each ufg In cp.MODFACGroups.Keys
                        pg = uni.ModfGroups.Groups(Integer.Parse(ufg)).MainGroupName
                        If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.ModfGroups.Groups(ufg).PrimaryGroup)
                    Next
                Next
            Case "Modified UNIFAC (NIST)"
                myType = "MODFAC-NIST"
                For Each cp As ConstantProperties In _comps.Values
                    For Each ufg In cp.NISTMODFACGroups.Keys
                        pg = uni.ModfGroups.Groups(Integer.Parse(ufg)).MainGroupName
                        If Not PrimaryGroups.ContainsKey(pg) Then PrimaryGroups.Add(pg, uni.ModfGroups.Groups(ufg).PrimaryGroup)
                    Next
                Next
        End Select

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
                    If myType = "UNIFAC" Then
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
            If myType = "UNIFAC" Then
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
            ElseIf myType = "MODFAC" Then
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

    Private Sub CheckModel(sender As Object, e As EventArgs) Handles cbModel.SelectedIndexChanged
        If cbModel.SelectedItem <> "" Then
            cbCompound1.Visible = True
            cbCompound2.Visible = True
            LblC1.Visible = True
            LblC2.Visible = True
            IPGrid.ColumnCount = 1
            IPGrid.RowCount = 0
            TBaij.Text = ""
            TBaji.Text = ""
            TBbij.Text = ""
            TBbji.Text = ""
            TBcij.Text = ""
            TBcji.Text = ""
            GN1 = ""
            GN2 = ""
            LblMGi.Text = ""
            LblMGj.Text = ""

            _pp = Nothing

            'get list of compounds
            Dim compounds As New ArrayList
            Select Case cbModel.SelectedItem
                Case "UNIFAC"
                    For Each c As ConstantProperties In FormMain.AvailableComponents.Values
                        If c.UNIFACGroups.Count > 0 Then
                            compounds.Add(c.Name & " (" & c.CurrentDB & ")")
                        End If
                    Next
                    uni = New Thermodynamics.PropertyPackages.Auxiliary.Unifac
                    TBbij.Visible = False
                    Lblbij.Visible = False
                    TBbji.Visible = False
                    Lblbji.Visible = False
                    TBcij.Visible = False
                    Lblcij.Visible = False
                    TBcji.Visible = False
                    Lblcji.Visible = False
                    BtnNewTemp.Visible = True
                    TbUnifacTemp.Visible = True
                    LblTUnit.Visible = True
                Case "Modified UNIFAC (Dortmund)"
                    For Each c As ConstantProperties In FormMain.AvailableComponents.Values
                        If c.MODFACGroups.Count > 0 Then
                            compounds.Add(c.Name & " (" & c.CurrentDB & ")")
                        End If
                    Next
                    uni = New Thermodynamics.PropertyPackages.Auxiliary.Modfac
                    TBbij.Visible = True
                    Lblbij.Visible = True
                    TBbji.Visible = True
                    Lblbji.Visible = True
                    TBcij.Visible = True
                    Lblcij.Visible = True
                    TBcji.Visible = True
                    Lblcji.Visible = True
                    BtnNewTemp.Visible = False
                    TbUnifacTemp.Visible = False
                    LblTUnit.Visible = False
                Case "Modified UNIFAC (NIST)"
                    For Each c As ConstantProperties In FormMain.AvailableComponents.Values
                        If c.NISTMODFACGroups.Count > 0 Then
                            compounds.Add(c.Name & " (" & c.CurrentDB & ")")
                        End If
                    Next
                    uni = New Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC
                    TBbij.Visible = True
                    Lblbij.Visible = True
                    TBbji.Visible = True
                    Lblbji.Visible = True
                    TBcij.Visible = True
                    Lblcij.Visible = True
                    TBcji.Visible = True
                    Lblcji.Visible = True
                    BtnNewTemp.Visible = False
                    TbUnifacTemp.Visible = False
                    LblTUnit.Visible = False
            End Select

            compounds.Sort()
            cbCompound1.Items.Clear()
            cbCompound2.Items.Clear()
            cbCompound1.Items.AddRange(compounds.ToArray())
            cbCompound2.Items.AddRange(compounds.ToArray())
        Else
            cbCompound1.Visible = False
            cbCompound2.Visible = False
            LblC1.Visible = False
            LblC2.Visible = False
        End If
    End Sub
    Private Sub cbCompound_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCompound1.SelectedIndexChanged, cbCompound2.SelectedIndexChanged
        Dim C1 = cbCompound1.SelectedItem
        Dim C2 = cbCompound2.SelectedItem
        _comps.Clear()
        If C1 <> "" Then
            Comp1 = RTrim(C1.Split("(")(0))
            mycase.Component1 = RTrim(C1.Split("(")(0))
            _comps.Add(C1, FormMain.AvailableComponents(mycase.Component1))

        End If
        If C2 <> "" Then
            Comp2 = RTrim(C2.Split("(")(0))
            mycase.Component2 = RTrim(C2.Split("(")(0))
            _comps.Add(C2, FormMain.AvailableComponents(mycase.Component2))

        End If
        If _comps.Count = 2 Then
            BtnShowIPGrid.Visible = True
        Else
            BtnShowIPGrid.Visible = False
        End If
    End Sub

    Private Sub IPGrid_CellDoubleClick(sender As Object, e As DataGridViewCellEventArgs) Handles IPGrid.CellDoubleClick
        Dim col As Integer = e.ColumnIndex
        Dim row As Integer = e.RowIndex
        Dim i, j As Integer

        If col > 0 And row > 2 And uni IsNot Nothing Then
            Dim T As Type = uni.GetType

            GN1 = IPGrid.Item(col, 2).Value
            GN2 = IPGrid.Item(0, row).Value

            Select Case T.Name
                Case "Unifac"
                    Dim gr As Thermodynamics.PropertyPackages.Auxiliary.Unifac = uni
                    For Each group In gr.UnifGroups.Groups
                        If group.Value.PrimGroupName = GN1 Then GI1 = group.Value.PrimaryGroup
                        If group.Value.PrimGroupName = GN2 Then GI2 = group.Value.PrimaryGroup
                    Next
                    TBaij.Text = EvalIPNF("a", GI1, GI2)
                    TBaji.Text = EvalIPNF("a", GI2, GI1)
                    TBbij.Text = ""
                    TBbji.Text = ""
                    TBcij.Text = ""
                    TBcji.Text = ""
                Case "Modfac"
                    Dim gr As Thermodynamics.PropertyPackages.Auxiliary.Modfac = uni
                    For Each group In gr.ModfGroups.Groups
                        If group.Value.MainGroupName = GN1 Then GI1 = group.Value.PrimaryGroup
                        If group.Value.MainGroupName = GN2 Then GI2 = group.Value.PrimaryGroup
                    Next
                    TBaij.Text = EvalIPNF("a", GI1, GI2)
                    TBaji.Text = EvalIPNF("a", GI2, GI1)
                    TBbij.Text = EvalIPNF("b", GI1, GI2)
                    TBbji.Text = EvalIPNF("b", GI2, GI1)
                    TBcij.Text = EvalIPNF("c", GI1, GI2)
                    TBcji.Text = EvalIPNF("c", GI2, GI1)
                Case "NISTMFAC"
                    Dim gr As Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC = uni
                    For Each group In gr.ModfGroups.Groups
                        If group.Value.MainGroupName = GN1 Then GI1 = group.Value.PrimaryGroup
                        If group.Value.MainGroupName = GN2 Then GI2 = group.Value.PrimaryGroup
                    Next
                    TBaij.Text = EvalIPNF("a", GI1, GI2)
                    TBaji.Text = EvalIPNF("a", GI2, GI1)
                    TBbij.Text = EvalIPNF("b", GI1, GI2)
                    TBbji.Text = EvalIPNF("b", GI2, GI1)
                    TBcij.Text = EvalIPNF("c", GI1, GI2)
                    TBcji.Text = EvalIPNF("c", GI2, GI1)
            End Select

            LblMGi.Text = GI1 & " - " & GN1
            LblMGj.Text = GI2 & " - " & GN2

            For i = 3 To 7
                For j = 0 To GridExpData.RowCount - 1
                    GridExpData.Item(i, j).Value = Nothing
                Next
            Next
        End If

    End Sub
    Function EvalIPNF(ByVal F As Char, ByVal g1 As Integer, ByVal g2 As Integer) As Double
        EvalIPNF = 0
        Dim T As Type = uni.GetType
        Select Case T.Name
            Case "NISTMFAC"
                Dim gr As Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC = uni
                Select Case F
                    Case "a"
                        If gr.ModfGroups.InteracParam_aij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_aij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_aij(g2)(g1)
                            End If
                        End If
                    Case "b"
                        If gr.ModfGroups.InteracParam_bij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_bij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_bij(g2)(g1)
                            End If
                        End If
                    Case "c"
                        If gr.ModfGroups.InteracParam_cij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_cij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_cij(g2)(g1)
                            End If
                        End If
                End Select
            Case "MODFAC"
                Dim gr As Thermodynamics.PropertyPackages.Auxiliary.Modfac = uni
                Select Case F
                    Case "a"
                        If gr.ModfGroups.InteracParam_aij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_aij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_aij(g2)(g1)
                            End If
                        End If
                    Case "b"
                        If gr.ModfGroups.InteracParam_bij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_bij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_bij(g2)(g1)
                            End If
                        End If
                    Case "c"
                        If gr.ModfGroups.InteracParam_cij.ContainsKey(g1) Then
                            If gr.ModfGroups.InteracParam_cij(g1).ContainsKey(g2) Then
                                EvalIPNF = gr.ModfGroups.InteracParam_cij(g2)(g1)
                            End If
                        End If
                End Select
            Case "Unifac"
                Dim gr As Thermodynamics.PropertyPackages.Auxiliary.Unifac = uni
                If gr.UnifGroups.InteracParam.ContainsKey(g1) Then
                    If gr.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                        EvalIPNF = gr.UnifGroups.InteracParam(g2)(g1)
                    End If
                End If
        End Select
    End Function
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
        dgv.RowCount = tArr.Length
        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(vbTab)
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
    Private Sub GridExpData_KeyDown(sender As Object, e As KeyEventArgs) Handles GridExpData.KeyDown
        If e.KeyCode = Keys.Delete And e.Modifiers = Keys.Shift Then
            Dim toremove As New ArrayList
            For Each c As DataGridViewCell In Me.GridExpData.SelectedCells
                If Not toremove.Contains(c.RowIndex) Then toremove.Add(c.RowIndex)
            Next
            For Each i As Integer In toremove
                Try
                    Me.GridExpData.Rows.RemoveAt(i)
                Catch ex As Exception

                End Try
            Next
        ElseIf e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(GridExpData)
        End If
    End Sub
    Private Function CalcError(ByVal IP(,) As Double, ByVal T As Double, ByVal x1 As Double, ByRef x2 As Double) As Object

        If mat Is Nothing Then mat = New MaterialStream("", "")

        If _pp Is Nothing Then
            Dim ppm As New CAPEOPENManager()

            Select Case cbModel.SelectedItem
                Case "Modified UNIFAC (NIST)"
                    _pp = ppm.GetPropertyPackage("Modified UNIFAC (NIST)")
                Case "Modified UNIFAC (Dortmund)"
                    _pp = ppm.GetPropertyPackage("Modified UNIFAC (Dortmund)")
                Case "UNIFAC"
                    _pp = ppm.GetPropertyPackage("UNIFAC")
            End Select

            ppm.Dispose()
            ppm = Nothing

        End If

        Dim P, L1, L2, V, Err As Double

        P = 101314
        Dim VZ(1), VX1(1), VX2(1) As Double
        VZ(0) = (x1 + x2) / 2
        VZ(1) = 1 - VZ(0)

        If cbModel.SelectedItem = "UNIFAC" Then
            _pp.m_uni.UnifGroups.InteracParam(GI1)(GI2) = IP(0, 0)
            _pp.m_uni.UnifGroups.InteracParam(GI2)(GI1) = IP(1, 0)
        Else
            _pp.m_uni.ModfGroups.InteracParam_aij(GI1)(GI2) = IP(0, 0)
            _pp.m_uni.ModfGroups.InteracParam_aij(GI2)(GI1) = IP(1, 0)
            _pp.m_uni.ModfGroups.InteracParam_bij(GI1)(GI2) = IP(0, 1)
            _pp.m_uni.ModfGroups.InteracParam_bij(GI2)(GI1) = IP(1, 1)
            _pp.m_uni.ModfGroups.InteracParam_cij(GI1)(GI2) = IP(0, 2)
            _pp.m_uni.ModfGroups.InteracParam_cij(GI2)(GI1) = IP(1, 2)
        End If

        mat.Phases(0).Properties.pressure = P
        mat.Phases(0).Properties.temperature = T
        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Clear() 'delete old assignment
        Next
        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Add(Comp1, New BaseClasses.Compound(Comp1, ""))
            phase.Compounds(Comp1).ConstantProperties = _comps.Values(0)

            phase.Compounds.Add(Comp2, New BaseClasses.Compound(Comp2, ""))
            phase.Compounds(Comp2).ConstantProperties = _comps.Values(1)
        Next
        _pp.CurrentMaterialStream = mat


        mat.Phases(0).Properties.temperature = T
        Dim slle As New FlashAlgorithms.SimpleLLE()
        Dim resultL As Object = slle.Flash_PT(VZ, P, T, _pp)
        L1 = resultL(0)
        L2 = resultL(5)
        V = resultL(1)
        VX1 = resultL(2)
        VX2 = resultL(6)

        Err = (x1 - VX1(0)) ^ 2 + (x2 - VX2(0)) ^ 2
        Return {Err * 100000.0, VX1(0), VX2(0)}

    End Function
    Private Sub BtnRegressIP_Click(sender As Object, e As EventArgs) Handles BtnRegressIP.Click

        Dim T, Dist As Double
        Dim aij, aji, mij, mji, MinError, MaxT, MinT As Double
        Dim Simplex(2) As Object
        Dim MinS, MidS, MaxS As Integer
        Dim CalcRes As Object
        Dim IP(1, 2) As Double
        Dim Cnt, k, i As Integer

        Cursor = Cursors.WaitCursor

        'initialise interaction parameters
        For k = 0 To 1
            For i = 0 To 2
                IP(k, i) = 0
            Next
        Next
        aij = 0
        aji = 0
        MinT = GridExpData.Item(2, 0).Value + 273.15
        MaxT = MinT

        For i = 0 To GridExpData.Rows.Count - 1
            Cnt = 3

            T = GridExpData.Item(2, i).Value + 273.15
            If T < MinT Then MinT = T
            If T > MaxT Then MaxT = T

            'initialize search

            'simplex 0
            CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
            Simplex(0) = {IP(0, 0), IP(1, 0), CalcRes(0)}

            'simplex 1
            IP(0, 0) = aij + 10
            IP(1, 0) = aji
            CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
            Simplex(1) = {IP(0, 0), IP(1, 0), CalcRes(0)}

            'simplex 2
            IP(0, 0) = aij
            IP(1, 0) = aji + 10
            CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
            Simplex(2) = {IP(0, 0), IP(1, 0), CalcRes(0)}

            'search Min, Mid und Max simplex
            MinS = 0
            MaxS = 0
            For k = 1 To 2
                If Simplex(k)(2) < Simplex(MinS)(2) Then MinS = k
                If Simplex(k)(2) > Simplex(MaxS)(2) Then MaxS = k
            Next
            MidS = 3 - MinS - MaxS
            MinError = Simplex(MinS)(2)

            Do
                'calc new simplex
                mij = (Simplex(MinS)(0) + Simplex(MidS)(0)) / 2
                mji = (Simplex(MinS)(1) + Simplex(MidS)(1)) / 2
                aij = 2 * mij - Simplex(MaxS)(0)
                aji = 2 * mji - Simplex(MaxS)(1)

                'replace Max value with new simplex
                IP(0, 0) = aij
                IP(1, 0) = aji
                CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
                Cnt += 1

                If CalcRes(0) < Simplex(MaxS)(2) Then
                    Simplex(MaxS) = {IP(0, 0), IP(1, 0), CalcRes(0)}
                Else
                    aij = mij + 0.5 * (mij - aij)
                    aji = mji + 0.5 * (mji - aji)
                    IP(0, 0) = aij
                    IP(1, 0) = aji
                    CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
                    Cnt += 1
                    Simplex(MaxS) = {IP(0, 0), IP(1, 0), CalcRes(0)}
                End If

                'search Min, Mid und Max simplex
                MinS = 0
                MaxS = 0
                For k = 1 To 2
                    If Simplex(k)(2) < Simplex(MinS)(2) Then MinS = k
                    If Simplex(k)(2) > Simplex(MaxS)(2) Then MaxS = k
                Next
                MidS = 3 - MinS - MaxS
                MinError = Simplex(MinS)(2)

                Dist = Sqrt((Simplex(0)(0) - Simplex(1)(0)) ^ 2 + (Simplex(0)(1) - Simplex(1)(1)) ^ 2)
                Dist += Sqrt((Simplex(0)(0) - Simplex(2)(0)) ^ 2 + (Simplex(0)(1) - Simplex(2)(1)) ^ 2)
                Dist += Sqrt((Simplex(1)(0) - Simplex(2)(0)) ^ 2 + (Simplex(1)(1) - Simplex(2)(1)) ^ 2)

                'Display iteration status

                TBStatus.Text = "Status: Regression running" & vbNewLine & "Dataset: " & i + 1 & "/" & GridExpData.RowCount & vbNewLine & "Iteration: " & Cnt

                Application.DoEvents()
            Loop Until (MinError < 0.001) Or Dist < 1
            GridExpData.Item(6, i).Value = aij
            GridExpData.Item(7, i).Value = aji
        Next


        TbUnifacTemp.Text = (MaxT + MinT) / 2 - 273.15
        BtnNewTemp_Click(sender, e)

        Cursor = Cursors.Default
        TBStatus.Text = "Status: Idle"

    End Sub
    Private Sub BtnNewTemp_Click(sender As Object, e As EventArgs) Handles BtnNewTemp.Click
        'do linear regression for aij, aji, bij, bji from results
        Dim mwT, mwaij, mwaji, naij, naji, d As Double

        For i = 0 To GridExpData.Rows.Count - 1
            If GridExpData.Item(6, i).Value = Nothing Or GridExpData.Item(7, i).Value = Nothing Then Exit Sub

            mwT += (GridExpData.Item(2, i).Value + 273.15) / GridExpData.Rows.Count
            mwaij += GridExpData.Item(6, i).Value / GridExpData.Rows.Count
            mwaji += GridExpData.Item(7, i).Value / GridExpData.Rows.Count
        Next
        For i = 0 To GridExpData.Rows.Count - 1
            naij += (GridExpData.Item(2, i).Value + 273.15 - mwT) * (GridExpData.Item(6, i).Value - mwaij)
            naji += (GridExpData.Item(2, i).Value + 273.15 - mwT) * (GridExpData.Item(7, i).Value - mwaji)
            d += (GridExpData.Item(2, i).Value + 273.15 - mwT) ^ 2
        Next


        TBbij.Text = naij / d
        TBbji.Text = naji / d
        TBaij.Text = mwaij - naij / d * mwT
        TBaji.Text = mwaji - naji / d * mwT

        If cbModel.SelectedItem = "UNIFAC" Then
            If TbUnifacTemp.Text = "" Then
                TBaij.Text = "0"
                TBaji.Text = "0"
                TBbij.Text = "0"
                TBbji.Text = "0"
                Exit Sub
            End If

            TBaij.Text = TBaij.Text + TBbij.Text * (TbUnifacTemp.Text + 273.15)
            TBaji.Text = TBaji.Text + TBbji.Text * (TbUnifacTemp.Text + 273.15)
            TBbij.Text = "0"
            TBbji.Text = "0"
        End If
    End Sub
    Private Sub BtnDrawChart_Click(sender As Object, e As EventArgs) Handles BtnDrawChart.Click

        'Exit if no experimental data are available
        If GridExpData.RowCount = 0 Then Exit Sub

        Dim i As Integer = GridExpData.Rows.Count - 1
        Dim Ti(i), vx1exp(i), vx2exp(i), vtexp(i), vx1calc(i), vx2calc(i), TMin, TMax, T As Double

        Dim IP(1, 2) As Double
        Dim CalcRes As Object

        IP(0, 0) = TBaij.Text
        IP(1, 0) = TBaji.Text
        If cbModel.SelectedItem <> "UNIFAC" Then
            IP(0, 1) = TBbij.Text
            IP(1, 1) = TBbji.Text
            IP(0, 2) = TBcij.Text
            IP(1, 2) = TBcji.Text
        End If

        For i = 0 To GridExpData.Rows.Count - 1
            If GridExpData.Item(0, i).Value <> Nothing Then vx1exp(i) = Double.Parse(GridExpData.Item(0, i).Value, ci)
            If GridExpData.Item(1, i).Value <> Nothing Then vx2exp(i) = Double.Parse(GridExpData.Item(1, i).Value, ci)
            If GridExpData.Item(2, i).Value <> Nothing Then vtexp(i) = Double.Parse(GridExpData.Item(2, i).Value, ci)

            T = vtexp(i) + 273.15
            CalcRes = CalcError(IP, T, GridExpData.Item(0, i).Value, GridExpData.Item(1, i).Value)
            GridExpData.Item(3, i).Value = CalcRes(1).ToString
            GridExpData.Item(4, i).Value = CalcRes(2).ToString
            GridExpData.Item(5, i).Value = CalcRes(0).ToString

            If GridExpData.Item(3, i).Value <> Nothing Then vx1calc(i) = Double.Parse(GridExpData.Item(3, i).Value, ci)
            If GridExpData.Item(4, i).Value <> Nothing Then vx2calc(i) = Double.Parse(GridExpData.Item(4, i).Value, ci)
        Next
        TMin = vtexp.Min
        TMax = vtexp.Max
        With graph.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .Title.Text = Comp1 & " / " & Comp2
            .YAxis.Title.Text = "Temperature °C"
            .XAxis.Title.Text = Comp1 & " mole fraction"
            .XAxis.Scale.Max = 1
            If vx1exp.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("ExpData"), vx1exp, vtexp, Color.Red, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = False
                    .Symbol.IsVisible = True
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                End With
            End If
            If vx2exp.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("ExpData"), vx2exp, vtexp, Color.Red, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = False
                    .Symbol.IsVisible = True
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Border.Color = Color.Black
                End With
            End If
            If vx1calc.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("CalcData"), vx1calc, vtexp, Color.Green, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = True
                    .Symbol.IsVisible = False
                End With
            End If
            If vx2calc.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("CalcData"), vx2calc, vtexp, Color.Green, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = True
                    .Symbol.IsVisible = False
                End With
            End If
        End With
        graph.GraphPane.AxisChange()
        FaTabStrip1.SelectedItem = TSChart
    End Sub


    Private Sub BtnDrawIPs_Click(sender As Object, e As EventArgs) Handles BtnDrawIPs.Click
        'Exit if no experimental data are available
        If GridExpData.RowCount = 0 Then Exit Sub

        Dim i As Integer = GridExpData.Rows.Count - 1
        Dim Ti(i), aij(i), aji(i), aijc(i), ajic(i) As Double
        Dim RegrAvail As Boolean = False

        For i = 0 To GridExpData.Rows.Count - 1
            If GridExpData.Item(6, i).Value <> Nothing Then
                aij(i) = Double.Parse(GridExpData.Item(6, i).Value, ci)
                RegrAvail = True
            End If

            If GridExpData.Item(7, i).Value <> Nothing Then
                aji(i) = Double.Parse(GridExpData.Item(7, i).Value, ci)
                RegrAvail = True
            End If

            If GridExpData.Item(2, i).Value <> Nothing Then Ti(i) = Double.Parse(GridExpData.Item(2, i).Value, ci)
            aijc(i) = TBaij.Text + TBbij.Text * (Ti(i) + 273.15)
            ajic(i) = TBaji.Text + TBbji.Text * (Ti(i) + 273.15)
        Next

        With graph.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .Title.Text = Comp1 & " / " & Comp2
            .YAxis.Title.Text = "aij / aji"
            .XAxis.Title.Text = Comp1 & "Temperature °C"
            .XAxis.Scale.MaxAuto = True
            .YAxis.Scale.MaxAuto = True
            If aij.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("Data aij"), Ti, aij, Color.Green, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = False
                    .Symbol.IsVisible = True
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Border.Color = Color.Black
                End With
            End If
            If aji.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("Data aji"), Ti, aji, Color.Red, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = False
                    .Symbol.IsVisible = True
                    .Symbol.Type = ZedGraph.SymbolType.Circle
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Border.Color = Color.Black
                End With
            End If
            If aijc.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("Regression aij"), Ti, aijc, Color.Blue, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = True
                    .Symbol.IsVisible = False
                End With
            End If
            If ajic.Count > 0 Then
                With .AddCurve(DWSIM.App.GetLocalString("Regression aji"), Ti, ajic, Color.Violet, ZedGraph.SymbolType.Circle)
                    .Line.IsVisible = True
                    .Symbol.IsVisible = False
                End With
            End If
        End With

        graph.GraphPane.AxisChange()
        FaTabStrip1.SelectedItem = TSChart
    End Sub
    Sub LoadCase(ByVal mycase As UNIFACIPRegressionCase, ByVal first As Boolean)
        Dim i As Integer

        With mycase
            tbTitle.Text = .Title
            tbDescription.Text = .Description
            tbIPDBName.Text = .Databasepath

            cbModel.SelectedItem = .ModelType
            cbCompound1.SelectedItem = .Component1
            cbCompound2.SelectedItem = .Component2

            GridExpData.RowCount = .DataLLE.Count
            For i = 0 To .DataLLE.Count - 1
                GridExpData.Item(0, i).Value = .DataLLE(i)(0)
                GridExpData.Item(1, i).Value = .DataLLE(i)(1)
                GridExpData.Item(2, i).Value = .DataLLE(i)(2)
                GridExpData.Item(6, i).Value = .DataLLE(i)(3)
                GridExpData.Item(7, i).Value = .DataLLE(i)(4)
            Next
            TBaij.Text = .aij
            TBaji.Text = .aji
            TBbij.Text = .bij
            TBbji.Text = .bji
            TBcij.Text = .cij
            TBcji.Text = .cji

            GI1 = .MainGr_i
            GI2 = .MainGr_j

            GN1 = .MainGrNamei
            GN2 = .MainGrNamej

        End With
        FillIPGrid(Nothing, Nothing)

        LblMGi.Text = GI1 & " - " & GN1
        LblMGj.Text = GI2 & " - " & GN2

    End Sub
    Sub StoreData()
        Dim i As Integer

        With mycase
            .Title = tbTitle.Text
            .Description = tbDescription.Text
            .Databasepath = tbIPDBName.Text

            .Component1 = cbCompound1.SelectedItem
            .Component2 = cbCompound2.SelectedItem
            .ModelType = cbModel.SelectedItem

            .MainGr_i = GI1
            .MainGr_j = GI2
            .MainGrNamei = GN1
            .MainGrNamej = GN2

            .aij = TBaij.Text
            .aji = TBaji.Text
            .bij = TBbij.Text
            .bji = TBbji.Text
            .cij = TBcij.Text
            .cji = TBcji.Text

            .DataLLE.Clear()
            For i = 0 To GridExpData.Rows.Count - 1
                .DataLLE.Add({CType(GridExpData.Item(0, i).Value, Double), CType(GridExpData.Item(1, i).Value, Double), CType(GridExpData.Item(2, i).Value, Double), CType(GridExpData.Item(6, i).Value, Double), CType(GridExpData.Item(7, i).Value, Double)})
            Next
        End With
    End Sub

    Private Sub BtnSelectIPDB_Click(sender As Object, e As EventArgs) Handles BtnSelectIPDB.Click

        'Select Interaction user database

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml")})

        If handler IsNot Nothing Then
            FileHandler = handler
            tbIPDBName.Text = handler.FullPath
        End If

    End Sub

    Private Sub BtnNewIPDB_Click(sender As Object, e As EventArgs) Handles BtnNewIPDB.Click
        'Create new Interaction Parameter User Database

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml")})

        If handler IsNot Nothing Then
            FileHandler = handler
            tbIPDBName.Text = handler.FullPath
            Using stream As New IO.MemoryStream()
                handler.Write(stream)
            End Using
        End If

    End Sub

    Private Sub BtnSaveIPDB_Click(sender As Object, e As EventArgs) Handles BtnSaveIPDB.Click

        StoreData()

        IP.Comp1 = mycase.MainGr_i
        IP.Comp2 = mycase.MainGr_j
        IP.Model = mycase.ModelType
        IP.Description = mycase.Description
        IP.RegressionFile = mycase.Filename

        IP.Parameters.Clear()

        IP.Parameters("aij") = mycase.aij
        IP.Parameters("aji") = mycase.aji

        If IP.Model <> "UNIFAC" Then
            IP.Parameters("bij") = mycase.bij
            IP.Parameters("bji") = mycase.bji
            IP.Parameters("cij") = mycase.cij
            IP.Parameters("cji") = mycase.cji
        End If

        If Me.tbIPDBName.Text <> "" Then
            Try
                Using stream As New MemoryStream
                    If FileHandler.Exists() Then
                        Using str = FileHandler.OpenRead()
                            str.CopyTo(stream)
                            stream.Position = 0
                        End Using
                    End If
                    Databases.UserIPDB.AddInteractionParameters(New BaseClasses.InteractionParameter() {IP}, stream, True)
                    FileHandler.Write(stream)
                End Using
                MessageBox.Show(DWSIM.App.GetLocalString("ParametrosAdicionadosComSucesso"))
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") & ex.Message.ToString, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub


    Private Sub FormUNIFACRegression_HelpRequested(sender As Object, hlpevent As HelpEventArgs) Handles MyBase.HelpRequested
        Select Case FaTabStrip1.SelectedItem.Name
            Case "TSInformation"
                DWSIM.App.HelpRequested("UR_Case_Definition_1.htm")
            Case "TSModel"
                DWSIM.App.HelpRequested("UR_Model_Definition_2.htm")
            Case "TSData"
                DWSIM.App.HelpRequested("UR_Data_Regression_3.htm")
            Case "TSChart"
                DWSIM.App.HelpRequested("UR_Diagrams_4.htm")
        End Select

    End Sub
End Class
<System.Serializable()> Public Class UNIFACIPRegressionCase
    Public Filename As String = ""
    Public Databasepath As String = ""
    Public su As SystemsOfUnits.Units
    Public Title, Description As String
    Public Component1, Component2 As String
    Public ModelType As String
    Public MainGr_i, MainGr_j As Integer
    Public MainGrNamei, MainGrNamej As String
    Public aij, aji, bij, bji, cij, cji As Double

    Public DataLLE As New ArrayList
End Class