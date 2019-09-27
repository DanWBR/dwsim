Imports unvell.ReoGrid
Imports System.Linq

Public Class FormNewSpreadsheet

    Protected ccparams As Global.DWSIM.SharedClasses.Spreadsheet.SpreadsheetCellParameters

    Public dt1 As New List(Of List(Of Object))

    Public dt2 As New List(Of List(Of Object))

    Public Property SpreadsheetControl As unvell.ReoGrid.Editor.ReoGridEditor

    Public Property Flowsheet As FormFlowsheet

    Public ReadOnly Property Spreadsheet As unvell.ReoGrid.ReoGridControl
        Get
            Return SpreadsheetControl.grid
        End Get
    End Property


    Private Sub FormNewSpreadsheet_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        SpreadsheetControl = New unvell.ReoGrid.Editor.ReoGridEditor()

        SpreadsheetControl.Dock = DockStyle.Fill

        SpreadsheetControl.SetupUILanguage()

        Controls.Add(SpreadsheetControl)

        MoveSpreadsheetMenu()

    End Sub

    Public Sub MoveSpreadsheetMenu()

        Dim menustrip1 As MenuStrip = SpreadsheetControl.SpreadsheetTSMI.GetCurrentParent()

        menustrip1.Items.Remove(SpreadsheetControl.SpreadsheetTSMI)

        Flowsheet.MenuStrip1.Items.Insert(4, SpreadsheetControl.SpreadsheetTSMI)

    End Sub


    Public Function GetCellValue(address As String) As Cell

        Return Spreadsheet.Worksheets(0).Cells(address)

    End Function

    Public Sub SetCellValue(ByVal cell As String, value As Object)

        Spreadsheet.Worksheets(0).Cells(cell).Data = value

    End Sub

    Public Sub EvaluateAll()

        Spreadsheet.Worksheets(0).Recalculate()

    End Sub

    Public Sub WriteAll()

    End Sub

    Public Function GetCellString() As String()

        Return Spreadsheet.Worksheets(0).Cells.Select(Function(x) x.Address).ToArray()

    End Function

    Public Function GetCellString(ByVal cell As Cell) As String

        Return cell.Address

    End Function

    Public Sub CopyToDT()

        Dim i, j As Integer

        dt1 = New List(Of List(Of Object))
        dt2 = New List(Of List(Of Object))

        Dim c As Cell

        For i = 0 To Spreadsheet.Worksheets(0).RowCount - 1
            dt1.Add(New List(Of Object))
            dt2.Add(New List(Of Object))
            For j = 0 To Spreadsheet.Worksheets(0).ColumnCount - 1
                c = Spreadsheet.Worksheets(0).Cells(i, j)
                Try
                    dt1(i).Add(c.Data)
                Catch ex As Exception
                End Try
                Try
                    dt2(i).Add(c.Tag)
                Catch ex As Exception
                End Try
            Next
        Next

    End Sub

    Public Function CopyDT1ToString() As String

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        Dim text As String = ""
        For i As Integer = 0 To dt1.Count - 1
            For j As Integer = 0 To dt1(0).Count - 1
                If dt1.Count - 1 >= i AndAlso dt1(0).Count - 1 >= j Then
                    If Double.TryParse(dt1(i, j), Globalization.NumberStyles.Any, ci, New Double) Then
                        text += Double.Parse(dt1(i, j), ci).ToString & ";"
                    Else
                        If dt1(i, j) IsNot Nothing Then
                            text += dt1(i, j).ToString() & ";"
                        Else
                            text += ";"
                        End If
                    End If
                End If
            Next
            text = text.TrimEnd(";") & "|"
        Next
        text = text.TrimEnd("|")

        Return text

    End Function

    Public Function CopyDT2ToString() As String

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

        Dim text As String = ""
        For i As Integer = 0 To dt2.Count - 1
            For j As Integer = 0 To dt1(0).Count - 1
                If dt2.Count - 1 >= i AndAlso dt2(0).Count - 1 >= j Then
                    If Not dt2(i)(j) Is Nothing Then
                        Try
                            Dim xel As New XElement("dummy", DirectCast(dt2(i, j), Spreadsheet.SpreadsheetCellParameters).SaveData.ToArray)
                            text += xel.ToString + ";"
                        Catch ex As Exception
                            text += " ;"
                        End Try
                    Else
                        text += " ;"
                    End If
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
            Try
                For i As Integer = 0 To n
                    If n > 0 Then
                        m = rows(i).Split(";").Length - 1
                    End If
                    For j As Integer = 0 To m
                        elm(i, j) = Double.Parse(rows(i).Split(";")(j))
                    Next
                Next
            Catch ex As Exception
            End Try
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
                    Dim scp As New Spreadsheet.SpreadsheetCellParameters()
                    Try
                        Dim element As New XElement("dummy")
                        Dim text0 = rows(i).Replace("&gt;", "greater_than").Replace("&lt;", "less_than")
                        Dim xmltext As String = text0.Split(";")(j)
                        If xmltext <> " " Then
                            Dim text1 = xmltext.Replace("greater_than", "&gt;").Replace("less_than", "&lt;")
                            element = XElement.Parse(text1)
                            scp.LoadData(element.Elements.ToList)
                            elm(i, j) = scp
                        Else
                            elm(i, j) = scp
                        End If
                    Catch ex As Exception
                    End Try
                Next
            Next
            dt2 = elm
        End If

    End Sub

    Public Sub CopyFromDT()

        Dim i, j, n1, m1, n2, m2, maxrow, maxcol As Integer

        n1 = dt1.GetUpperBound(0) - 1
        m1 = dt1.GetUpperBound(1) - 1

        n2 = dt2.GetUpperBound(0) - 1
        m2 = dt2.GetUpperBound(1) - 1

        maxrow = Me.DataGridView1.Rows.Count - 1
        maxcol = Me.DataGridView1.Columns.Count - 1

        For i = 0 To n1
            For j = 0 To m1
                If i <= maxrow And j <= maxcol Then
                    If dt1(i, j) IsNot Nothing Then Me.DataGridView1.Rows.Item(i).Cells(j).Value = dt1(i, j)
                End If
                If i <= n2 And j <= m2 And i <= maxrow And j <= maxcol Then
                    If dt2(i, j) Is Nothing Then
                        Me.DataGridView1.Rows.Item(i).Cells(j).Tag = New Spreadsheet.SpreadsheetCellParameters
                    ElseIf TypeOf dt2(i, j) Is Spreadsheet.SpreadsheetCellParameters Then
                        Me.DataGridView1.Rows.Item(i).Cells(j).Tag = dt2(i, j)
                        Me.DataGridView1.Rows.Item(i).Cells(j).ToolTipText = dt2(i, j).ToolTipText
                    ElseIf TypeOf dt2(i, j) Is Object Then
                        Dim cellparam As New Spreadsheet.SpreadsheetCellParameters
                        Try
                            With cellparam
                                .Expression = dt2(i, j)
                                If CStr(dt2(i, j)).StartsWith(":") Then
                                    .CellType = Spreadsheet.VarType.Read
                                    Dim str As String()
                                    str = CStr(dt2(i, j)).Split(New Char() {","})
                                    .ObjectID = str(0).Substring(1)
                                    .PropID = str(1)
                                Else
                                    .CellType = Spreadsheet.VarType.Expression
                                End If
                            End With
                        Catch ex As Exception
                        End Try
                        Me.DataGridView1.Rows.Item(i).Cells(j).Tag = cellparam
                        Me.DataGridView1.Rows.Item(i).Cells(j).ToolTipText = cellparam.ToolTipText
                    End If
                End If
            Next
        Next

    End Sub

End Class