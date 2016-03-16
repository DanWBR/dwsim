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


Imports Ciloci.Flee
Imports DWSIM.DWSIM.Utilities.Spreadsheet
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Linq

Public Class SpreadsheetForm
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public OldValue As String = ""
    Public OldTag As String = ""

    Public formc As FormFlowsheet
    Public loaded As Boolean = False

    Protected m_e As IGenericExpression(Of Object)
    Protected m_eopt As ExpressionContext

    Protected ccparams As DWSIM.Utilities.Spreadsheet.SpreadsheetCellParameters

    Public dt1(99, 25) As Object
    Public dt2(99, 25) As Object

    Public StopWriting As Boolean = False
    Public WriteMode As Boolean = False

    Public InternalCounter As Integer = 0

    Public Property Expr() As IGenericExpression(Of Object)
        Get
            Return m_e
        End Get
        Set(ByVal value As IGenericExpression(Of Object))
            m_e = value
        End Set
    End Property

    Public Property ExpContext() As ExpressionContext
        Get
            Return m_eopt
        End Get
        Set(ByVal value As ExpressionContext)
            m_eopt = value
        End Set
    End Property

    Private Sub UISpreadsheetEditorForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        e.Cancel = True
        Me.Hide()
    End Sub

    Public Sub UISpreadsheetEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        formc = My.Application.ActiveSimulation

        If Me.loaded = False Then
            Me.DataGridView1.Rows.Add(100)
            CopyFromDT()
        End If

        Dim i As Integer = 1
        For Each row As DataGridViewRow In Me.DataGridView1.Rows
            row.HeaderCell.Value = i.ToString
            i = i + 1
        Next

        Me.ExpContext = New Ciloci.Flee.ExpressionContext
        With Me.ExpContext
            .Imports.AddType(GetType(System.Math))
            .Imports.AddType(GetType(System.String))
            .Imports.AddType(GetType(Microsoft.VisualBasic.Strings))
            .Imports.AddType(GetType(DWSIM.MathEx.Common))
        End With

        DefineVariables()
        loaded = True

        With Me.DataGridView1.Columns
            .Item(0).HeaderText = "A"
            .Item(1).HeaderText = "B"
            .Item(2).HeaderText = "C"
            .Item(3).HeaderText = "D"
            .Item(4).HeaderText = "E"
            .Item(5).HeaderText = "F"
            .Item(6).HeaderText = "G"
            .Item(7).HeaderText = "H"
            .Item(8).HeaderText = "I"
            .Item(9).HeaderText = "J"
            .Item(10).HeaderText = "K"
            .Item(11).HeaderText = "L"
            .Item(12).HeaderText = "M"
            .Item(13).HeaderText = "N"
            .Item(14).HeaderText = "O"
            .Item(15).HeaderText = "P"
            .Item(16).HeaderText = "Q"
            .Item(17).HeaderText = "R"
            .Item(18).HeaderText = "S"
            .Item(19).HeaderText = "T"
            .Item(20).HeaderText = "U"
            .Item(21).HeaderText = "V"
            .Item(22).HeaderText = "W"
            .Item(23).HeaderText = "X"
            .Item(24).HeaderText = "Y"
            .Item(25).HeaderText = "Z"
        End With

    End Sub

    ''' <summary>
    ''' Gets the Cell instance.
    ''' </summary>
    ''' <param name="cell">Cell reference in "Letter-Number" style (ex. "A1").</param>
    ''' <returns></returns>
    ''' <remarks>To get the actual cell's value, read the 'Value' property of the returning object.</remarks>
    Public Function GetCellValue(ByVal cell As String) As DataGridViewCell

        Dim column, row As Integer
        Dim colLetra As String

        colLetra = cell.Substring(0, 1)
        row = Convert.ToInt32(cell.Substring(1)) - 1

        Select Case colLetra
            Case "A"
                column = 0
            Case "B"
                column = 1
            Case "C"
                column = 2
            Case "D"
                column = 3
            Case "E"
                column = 4
            Case "F"
                column = 5
            Case "G"
                column = 6
            Case "H"
                column = 7
            Case "I"
                column = 8
            Case "J"
                column = 9
            Case "K"
                column = 10
            Case "L"
                column = 11
            Case "M"
                column = 12
            Case "N"
                column = 13
            Case "O"
                column = 14
            Case "P"
                column = 15
            Case "Q"
                column = 16
            Case "R"
                column = 17
            Case "S"
                column = 18
            Case "T"
                column = 19
            Case "U"
                column = 20
            Case "V"
                column = 21
            Case "W"
                column = 22
            Case "X"
                column = 23
            Case "Y"
                column = 24
            Case "Z"
                column = 25
            Case Else
                Return Nothing
        End Select

        Return Me.DataGridView1.Rows(row).Cells(column)

    End Function

    ''' <summary>
    ''' Sets a cell's value.
    ''' </summary>
    ''' <param name="cell">Cell reference in "Letter-Number" style (ex. "A1").</param>
    ''' <param name="value">The value to set.</param>
    ''' <remarks></remarks>
    Public Sub SetCellValue(ByVal cell As String, value As Object)

        UpdateValue(GetCellValue(cell), value)

        'EvaluateAll()

    End Sub


    Private Sub DataGridView1_CellBeginEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellCancelEventArgs) Handles DataGridView1.CellBeginEdit

        Dim c, r As Integer

        c = e.ColumnIndex
        r = e.RowIndex

        Dim Cell As DataGridViewCell = Me.DataGridView1.Rows(r).Cells(c)

        Me.OldValue = Cell.Value
        ccparams = Cell.Tag
        Me.OldTag = ccparams.Expression

        Cell.Value = ccparams.Expression
        Me.tbValue.Text = ccparams.Expression

    End Sub

    Private Sub DataGridView1_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellEndEdit

        Dim c, r As Integer
        Dim expression As String

        c = e.ColumnIndex
        r = e.RowIndex

        Dim Cell As DataGridViewCell = Me.DataGridView1.Rows(r).Cells(c)
        expression = Cell.Value

        UpdateValue(Cell, expression)

        If chkUpdate.Checked Then EvaluateAll()

        Cell = Nothing

    End Sub

    Private Sub DataGridView1_SelectionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles DataGridView1.SelectionChanged
        If Me.DataGridView1.SelectedCells.Count > 0 Then
            Me.tbCell.Text = Me.GetCellString(Me.DataGridView1.SelectedCells(0))
            ccparams = Me.DataGridView1.SelectedCells(0).Tag
            If Not ccparams Is Nothing Then Me.tbValue.Text = ccparams.Expression
        End If
    End Sub

    ''' <summary>
    ''' Gets the cell address in letter-number style.
    ''' </summary>
    ''' <param name="cell">DataGridViewCell instance.</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function GetCellString(ByVal cell As DataGridViewCell) As String

        Dim str As String = Me.DataGridView1.Columns(cell.ColumnIndex).Name & Convert.ToString(cell.RowIndex + 1)

        Return str

    End Function

    Public Function GetCellString() As String()

        Dim names As New ArrayList

        For Each col As DataGridViewColumn In Me.DataGridView1.Columns
            For Each r As DataGridViewRow In Me.DataGridView1.Rows
                names.Add(col.Name & r.HeaderCell.Value.ToString)
            Next
        Next

        Return names.ToArray(Type.GetType("System.String"))

    End Function

    Private Sub UISpreadsheetEditorForm_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.loaded = True
    End Sub


    Sub DefineVariables()

        If TypeOf My.Application.ActiveSimulation Is FormFlowsheet Then

            If Me.formc Is Nothing Then Me.formc = My.Application.ActiveSimulation

        End If

        For Each r As DataGridViewRow In Me.DataGridView1.Rows
            For Each ce As DataGridViewCell In r.Cells
                Me.ExpContext.Variables.DefineVariable(Me.GetCellString(ce), GetType(Double))
            Next
        Next

    End Sub

    Sub GetValues()
        For Each r As DataGridViewRow In Me.DataGridView1.Rows
            For Each ce As DataGridViewCell In r.Cells
                Dim val As Double = 0
                Double.TryParse(ce.Value, val)
                Me.ExpContext.Variables(Me.GetCellString(ce)) = val
            Next
        Next
    End Sub

    Public Sub CopyToDT()

        Dim i, j As Integer

        i = 0
        For Each r As DataGridViewRow In Me.DataGridView1.Rows
            j = 0
            For Each ce As DataGridViewCell In r.Cells
                dt1(i, j) = ce.Value
                dt2(i, j) = ce.Tag
                j = j + 1
            Next
            i = i + 1
        Next

    End Sub

    Public Function CopyDT1ToString() As String

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        Dim text As String = ""
        For i As Integer = 0 To dt1.GetUpperBound(0)
            For j As Integer = 0 To dt1.GetUpperBound(1)
                If Double.TryParse(dt1(i, j), Globalization.NumberStyles.Any, ci, New Double) Then
                    text += Double.Parse(dt1(i, j), ci).ToString + ";"
                Else
                    text += dt1(i, j) + ";"
                End If
            Next
            text = text.TrimEnd(";") + "|"
        Next
        text = text.TrimEnd("|")

        Return text

    End Function

    Public Function CopyDT2ToString() As String

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        Dim text As String = ""
        For i As Integer = 0 To dt2.GetUpperBound(0)
            For j As Integer = 0 To dt1.GetUpperBound(1)
                If Not dt2(i, j) Is Nothing Then
                    Dim xel As New XElement("dummy", DirectCast(dt2(i, j), SpreadsheetCellParameters).SaveData.ToArray)
                    text += xel.ToString + ";"
                Else
                    text += " ;"
                End If
            Next
            text = text.TrimEnd(";") + "|"
        Next
        text = text.TrimEnd("|")

        Return text

    End Function

    Public Sub CopyDT1FromString(text As String)

        Dim rows() As String = text.Split("|")
        Dim n As Integer = rows.Length - 1
        Dim m As Integer = 0
        If n > 0 And m > 0 Then
            Dim elm(n, m) As Object
            For i As Integer = 0 To n
                If n > 0 Then
                    m = rows(i).Split(";").Length - 1
                End If
                For j As Integer = 0 To m
                    elm(i, j) = Double.Parse(rows(i).Split(";")(j))
                Next
            Next
            dt1 = elm
        End If

    End Sub

    Public Sub CopyDT2FromString(text As String)

        Dim rows() As String = text.Split("|")
        Dim n As Integer = rows.Length - 1
        Dim m As Integer = 0
        If n > 0 Then
            m = rows(0).Split(";").Length - 1
        End If
        If n > 0 And m > 0 Then
            Dim elm(n, m) As Object
            For i As Integer = 0 To n
                For j As Integer = 0 To m
                    Dim scp As New SpreadsheetCellParameters()
                    Dim element As New XElement("dummy")
                    Dim xmltext As String = rows(i).Split(";")(j)
                    If xmltext <> " " Then
                        element = XElement.Parse(xmltext)
                        scp.LoadData(element.Elements.ToList)
                        elm(i, j) = scp
                    Else
                        elm(i, j) = scp
                    End If
                Next
            Next
            dt2 = elm
        End If

    End Sub

    Public Sub CopyFromDT()

        Dim i, j As Integer

        i = 0
        For Each r As DataGridViewRow In Me.DataGridView1.Rows
            j = 0
            For Each ce As DataGridViewCell In r.Cells
                ce.Value = dt1(i, j)
                If dt2(i, j) Is Nothing Then
                    ce.Tag = New DWSIM.Utilities.Spreadsheet.SpreadsheetCellParameters
                ElseIf TypeOf dt2(i, j) Is SpreadsheetCellParameters Then
                    ce.Tag = dt2(i, j)
                    ce.ToolTipText = dt2(i, j).ToolTipText
                ElseIf TypeOf dt2(i, j) Is Object Then
                    Dim cellparam As New SpreadsheetCellParameters
                    With cellparam
                        .Expression = dt2(i, j)
                        If CStr(dt2(i, j)).StartsWith(":") Then
                            .CellType = VarType.Read
                            Dim str As String()
                            str = CStr(dt2(i, j)).Split(New Char() {","})
                            .ObjectID = str(0).Substring(1)
                            .PropID = str(1)
                        Else
                            .CellType = VarType.Expression
                        End If
                    End With
                    ce.Tag = cellparam
                    ce.ToolTipText = cellparam.ToolTipText
                End If
                j = j + 1
            Next
            i = i + 1
        Next

    End Sub

    Private Sub tbValue_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles tbValue.KeyDown

        If e.KeyCode = Keys.Enter Then

            If Not Me.DataGridView1.SelectedCells(0) Is Nothing Then

                Dim expression As String
                expression = tbValue.Text

                UpdateValue(Me.DataGridView1.SelectedCells(0), expression)

            End If

        End If

    End Sub

    ''' <summary>
    ''' Updates the cell value, parsing the expression if any.
    ''' </summary>
    ''' <param name="cell"></param>
    ''' <param name="expression"></param>
    ''' <remarks></remarks>
    Sub UpdateValue(ByRef cell As DataGridViewCell, ByVal expression As String)


        If TypeOf My.Application.OpenForms(0).ActiveMdiChild Is FormFlowsheet Then

            If formc Is Nothing Then formc = My.Application.ActiveSimulation

            If Me.ExpContext Is Nothing Then
                Me.ExpContext = New Ciloci.Flee.ExpressionContext
                With Me.ExpContext
                    .Imports.AddType(GetType(System.Math))
                    .Imports.AddType(GetType(System.String))
                    .Imports.AddType(GetType(Microsoft.VisualBasic.Strings))
                    .Imports.AddType(GetType(DWSIM.MathEx.Common))
                End With
            End If

            If Me.loaded = False Then DefineVariables()
            GetValues()

            Me.tbValue.Text = expression

            Try
                ccparams = cell.Tag
                ccparams.Expression = expression
                If ccparams.CellType = VarType.Write Then
                    If formc.Collections.FlowsheetObjectCollection.ContainsKey(ccparams.ObjectID) Then
                        ccparams.ToolTipText = DWSIM.App.GetLocalString("CellWillWrite") & vbCrLf & _
                        DWSIM.App.GetLocalString("Objeto") & ": " & formc.Collections.FlowsheetObjectCollection(ccparams.ObjectID).GraphicObject.Tag & vbCrLf & _
                        DWSIM.App.GetLocalString("Propriedade") & ": " & DWSIM.App.GetPropertyName(ccparams.PropID)
                        cell.Style.BackColor = Color.LightBlue
                    Else
                        ccparams.CellType = VarType.Expression
                        ccparams.ToolTipText = expression
                    End If
                    cell.ToolTipText = ccparams.ToolTipText
                End If
                If expression <> "" Then
                    If expression.Substring(0, 1) = "=" Then
                        Me.ExpContext.Options.ParseCulture = System.Globalization.CultureInfo.InvariantCulture
                        Me.ExpContext.ParserOptions.DecimalSeparator = "."
                        Me.ExpContext.ParserOptions.FunctionArgumentSeparator = ";"
                        Me.Expr = Me.ExpContext.CompileGeneric(Of Object)(expression.Substring(1))
                        cell.Value = Expr.Evaluate
                        If Not ccparams.CellType = VarType.Write Then cell.Style.BackColor = Color.LightYellow
                    ElseIf expression.Substring(0, 1) = ":" Then
                        Dim str As String()
                        Dim obj, prop As String
                        str = expression.Split(New Char() {","})
                        obj = str(0).Substring(1)
                        ccparams.ObjectID = obj
                        If str.Length < 3 Then
                            prop = str(1)
                        Else
                            prop = str(1) & "," & str(2)
                        End If
                        ccparams.PropID = prop
                        cell.Value = formc.Collections.FlowsheetObjectCollection(obj).GetPropertyValue(prop, formc.Options.SelectedUnitSystem)
                        ccparams.ToolTipText = DWSIM.App.GetLocalString("CellIsReading") & vbCrLf & _
                        DWSIM.App.GetLocalString("Objeto") & ": " & formc.Collections.FlowsheetObjectCollection(ccparams.ObjectID).GraphicObject.Tag & vbCrLf & _
                        DWSIM.App.GetLocalString("Propriedade") & ": " & DWSIM.App.GetPropertyName(prop) & vbCrLf & _
                        DWSIM.App.GetLocalString("CurrentValue") & ": " & cell.Value & _
                        " " & formc.Collections.FlowsheetObjectCollection(obj).GetPropertyUnit(prop, formc.Options.SelectedUnitSystem)
                        cell.ToolTipText = ccparams.ToolTipText
                        cell.Style.BackColor = Color.LightGreen
                    Else
                        cell.Value = expression
                        If ccparams.CellType <> VarType.Write Then
                            ccparams.ToolTipText = expression
                            cell.ToolTipText = ccparams.ToolTipText
                            cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                        End If
                    End If
                Else
                    cell.Value = ""
                    ccparams.ToolTipText = ""
                    cell.ToolTipText = ccparams.ToolTipText
                    cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                End If
            Catch ex As Exception
                cell.Value = Me.OldValue
                ccparams.ToolTipText = ""
                cell.Tag = ccparams.Clone
                cell.ToolTipText = ccparams.ToolTipText
                cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                My.Application.ActiveSimulation.WriteToLog(Me.Text & ": " & DWSIM.App.GetLocalString("Invalidexpressiononcell") & " " & GetCellString(cell) & " - " & ex.Message, Color.Brown, DWSIM.Flowsheet.MessageType.Information)
            End Try

        End If


    End Sub

    ''' <summary>
    ''' Updates the cell colors.
    ''' </summary>
    ''' <remarks></remarks>
    Sub UpdateColors()

        For Each row As DataGridViewRow In Me.DataGridView1.Rows

            For Each cell As DataGridViewCell In row.Cells

                Try
                    ccparams = cell.Tag
                    Dim expression = ccparams.Expression
                    If ccparams.CellType = VarType.Write Then
                        cell.Style.BackColor = Color.LightBlue
                    End If
                    If expression <> "" Then
                        If expression.Substring(0, 1) = ":" Then
                            cell.Style.BackColor = Color.LightGreen
                        Else
                            cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                        End If
                    Else
                        cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                    End If
                Catch ex As Exception
                    cell.Style.BackColor = cell.OwningColumn.DefaultCellStyle.BackColor
                End Try

            Next

        Next

    End Sub


    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        If Not Me.DataGridView1.SelectedCells(0) Is Nothing Then

            Dim expression As String
            expression = tbValue.Text

            UpdateValue(Me.DataGridView1.SelectedCells(0), expression)

        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If Not Me.DataGridView1.SelectedCells(0) Is Nothing Then

            tbValue.Text = ""

            Dim expression As String
            expression = tbValue.Text

            UpdateValue(Me.DataGridView1.SelectedCells(0), expression)

        End If
    End Sub

    Private Sub ContextMenuStrip1_Opening(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles ContextMenuStrip1.Opening

        If Me.DataGridView1.SelectedCells.Count = 0 Then
            e.Cancel = True
        Else
            Me.CelulaToolStripMenuItem.Text = GetCellString(Me.DataGridView1.SelectedCells(0))
        End If

    End Sub

    Private Sub CopiarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CopiarToolStripMenuItem.Click
        My.Computer.Clipboard.SetData("specialFormat", Me.DataGridView1.SelectedCells(0).Tag)
    End Sub

    Private Sub ColarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ColarToolStripMenuItem.Click
        Me.DataGridView1.SelectedCells(0).Tag = CType(My.Computer.Clipboard.GetData("specialFormat"), SpreadsheetCellParameters)
        UpdateValue(Me.DataGridView1.SelectedCells(0), CType(Me.DataGridView1.SelectedCells(0).Tag, SpreadsheetCellParameters).Expression)
    End Sub

    Private Sub LimparToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LimparToolStripMenuItem.Click
        For Each sc In Me.DataGridView1.SelectedCells
            sc.Value = ""
            sc.Tag = New SpreadsheetCellParameters
            UpdateValue(sc, "")
        Next
    End Sub

    Private Sub AvaliarFórmulaToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AvaliarFórmulaToolStripMenuItem.Click
        For Each sc In Me.DataGridView1.SelectedCells
            If Not sc.Tag Is Nothing Then
                ccparams = sc.Tag
                UpdateValue(sc, ccparams.Expression)
            End If
        Next
    End Sub

    Private Sub ImportarDadosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ImportarDadosToolStripMenuItem.Click
        Dim frmps As New FormPropSelection
        frmps.ssheet = Me
        frmps.ShowDialog(Me)
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        EvaluateAll()
    End Sub

    ''' <summary>
    ''' Evaluates all the cells in the spreadsheet, reading and writing the values to/from the flowsheet accordingly.
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub EvaluateAll()

        Try

            If Not formc Is Nothing Then

                If formc.Options.CalculatorActivated Then

                    StopWriting = True
                    Dim forcestop As Boolean = False
                    Dim delta As Double = 0.0#
                    Dim i As Integer

                    i = 0
                    For Each r As DataGridViewRow In Me.DataGridView1.Rows
                        For Each ce As DataGridViewCell In r.Cells
                            ccparams = ce.Tag
                            If Not ccparams Is Nothing Then
                                If ccparams.Expression <> "" Then
                                    'If ccparams.Expression.Substring(0, 1) = "=" Or ccparams.Expression.Substring(0, 1) = ":" Then
                                    ccparams.PrevVal = ce.Value
                                    UpdateValue(ce, ccparams.Expression)
                                    ccparams.CurrVal = ce.Value
                                    'End If
                                    If ccparams.CellType = VarType.Write Then
                                        i += 1
                                        delta += Math.Abs((Convert.ToDouble(ccparams.CurrVal) - Convert.ToDouble(ccparams.PrevVal)) / Convert.ToDouble(ccparams.PrevVal))
                                        If Double.IsNaN(delta) Or Double.IsInfinity(delta) Then forcestop = True
                                    End If
                                End If
                            End If
                        Next
                    Next

                    If delta > i * Convert.ToDouble(Me.tbTolerance.Text) Then StopWriting = False

                    If Me.WriteMode Then
                        If Not StopWriting Then
                            If Not forcestop Then WriteAll()
                        End If
                        If InternalCounter = 0 Then WriteAll()
                    End If

                    InternalCounter += 1

                End If
            End If
        Catch ex As Exception
            Console.WriteLine(ex.ToString)
        End Try

    End Sub

    ''' <summary>
    ''' Write the values from the cells to the flowsheet, for those who have this behavior.
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub WriteAll()

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Nothing
        Dim su As DWSIM.SystemsOfUnits.Units = My.Application.ActiveSimulation.Options.SelectedUnitSystem
        For Each r As DataGridViewRow In Me.DataGridView1.Rows
            For Each ce As DataGridViewCell In r.Cells
                ccparams = ce.Tag
                If Not ccparams Is Nothing Then
                    If ccparams.CellType = VarType.Write And Not ce.Value Is Nothing Then
                        obj = formc.Collections.FlowsheetObjectCollection(ccparams.ObjectID)
                        obj.SetPropertyValue(ccparams.PropID, ce.Value, su)
                        'Call function to calculate flowsheet
                        Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                        With objargs
                            .Calculado = False
                            .Nome = obj.Nome
                            .Tag = obj.GraphicObject.Tag
                            .Tipo = obj.GraphicObject.TipoObjeto
                        End With
                        My.Application.ActiveSimulation.CalculationQueue.Enqueue(objargs)
                    End If
                End If
            Next
        Next

    End Sub

    Private Sub ExportarDadosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExportarDadosToolStripMenuItem.Click
        Dim frmps As New FormPropSelection
        frmps.ssheet = Me
        frmps.mode = 1
        frmps.ShowDialog(Me)
    End Sub

    Private Sub chkWriteMode_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkWriteMode.CheckedChanged
        Me.WriteMode = chkWriteMode.Checked
    End Sub

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                             DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                             DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                             DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                             DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Clipboard.SetDataObject(Me.DataGridView1.GetClipboardContent)
    End Sub
End Class
