Imports System.Linq
#If LINUX Then
Imports DWSIM.CrossPlatform.UI.Controls.ReoGrid
Imports DWSIM.CrossPlatform.UI.Controls.ReoGrid.Formula
Imports Eto.Forms
Imports Cell = DWSIM.CrossPlatform.UI.Controls.ReoGrid.Cell
#Else
Imports unvell.ReoGrid
Imports unvell.ReoGrid.Formula
#End If

Public Class FormNewSpreadsheet

    Protected ccparams As Global.DWSIM.SharedClasses.Spreadsheet.SpreadsheetCellParameters

    Public dt1 As New List(Of List(Of Object))

    Public dt2 As New List(Of List(Of Object))

#If LINUX Then
    Public Property SpreadsheetControl As ReoGridFullControl
#Else
    Public Property SpreadsheetControl As unvell.ReoGrid.Editor.ReoGridEditor
#End If

    Public Property Flowsheet As FormFlowsheet

    Public Property Loaded As Boolean = False

    Private Columns As New List(Of String)

#If LINUX Then
    Public ReadOnly Property Spreadsheet As ReoGridControl
        Get
            Return SpreadsheetControl.GridControl
        End Get
    End Property
#Else
    Public ReadOnly Property Spreadsheet As unvell.ReoGrid.ReoGridControl
        Get
            Return SpreadsheetControl?.grid
        End Get
    End Property
#End If

    Public Sub Initialize()

#If LINUX Then

        SpreadsheetControl = New ReoGridFullControl()

        Dim nativesheet = WinFormsHelpers.ToNative(SpreadsheetControl, True)

        nativesheet.Dock = DockStyle.Fill

        Controls.Add(nativesheet)

#Else

        SpreadsheetControl = New unvell.ReoGrid.Editor.ReoGridEditor()

        AddHandler SpreadsheetControl.ImportarDadosToolStripMenuItem.Click, Sub(s2, e2)
                                                                                Dim frmps As New FormPropSelection
                                                                                frmps.ssheet = Spreadsheet
                                                                                frmps.ShowDialog(Me)
                                                                            End Sub

        AddHandler SpreadsheetControl.ExportarDadosToolStripMenuItem.Click, Sub(s2, e2)
                                                                                Dim frmps As New FormPropSelection
                                                                                frmps.ssheet = Spreadsheet
                                                                                frmps.mode = 1
                                                                                frmps.ShowDialog(Me)
                                                                            End Sub

        AddHandler SpreadsheetControl.create2DXYChartFromSelectionToolStripMenuItem.Click, Sub(s2, e2)
                                                                                               CreateChartFromRange(s2, e2)
                                                                                           End Sub

        SpreadsheetControl.Dock = DockStyle.Fill

        SpreadsheetControl.SetupUILanguage()

        Controls.Add(SpreadsheetControl)

#End If

        Spreadsheet.CurrentWorksheet.Name = "MAIN"

#If Not LINUX Then

        SpreadsheetControl.NewDocumentOnLoad = False

#End If

        Columns.AddRange({"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"})

        Loaded = True

    End Sub

    Sub CreateChartFromRange(sender As Object, e As EventArgs)

        Dim tabpage As New System.Windows.Forms.TabPage

        Dim chart As New Charts.Chart

        Dim data = Spreadsheet.CurrentWorksheet.GetRangeData(Spreadsheet.CurrentWorksheet.SelectionRange)

        Dim firstcol, lastcol, firstrow, lastrow As Integer

        firstcol = Spreadsheet.CurrentWorksheet.SelectionRange.Col
        lastcol = Spreadsheet.CurrentWorksheet.SelectionRange.EndCol
        firstrow = Spreadsheet.CurrentWorksheet.SelectionRange.Row + 1
        lastrow = Spreadsheet.CurrentWorksheet.SelectionRange.EndRow + 1

        Dim hasheaders As Boolean = Not Double.TryParse(Spreadsheet.CurrentWorksheet.Cells(firstrow - 1, firstcol).Data, New Double)

        Dim name = Spreadsheet.CurrentWorksheet.Name

        Dim xcol As String = Spreadsheet.CurrentWorksheet.SelectionRange.StartPos.ToAddress().Trim(New Char() {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0"})

        Dim ycols As New List(Of String)

        For i As Integer = firstcol + 1 To lastcol
            ycols.Add(Spreadsheet.CurrentWorksheet.Cells(0, i).Address().Trim(New Char() {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0"}))
        Next

        If hasheaders Then
            firstrow += 1
        End If

        For Each item In ycols
            chart.SpreadsheetDataSourcesX.Add(name & "!" & xcol & firstrow & ":" & xcol & lastrow)
            chart.SpreadsheetDataSourcesY.Add(name & "!" & item & firstrow & ":" & item & lastrow)
        Next

        Dim chartcontrol = New TwoDimChartControl() With {.Dock = DockStyle.Fill,
                             .Flowsheet = Flowsheet,
                             .Chart = chart,
                             .Spreadsheet = Flowsheet.FormSpreadsheet.Spreadsheet}

        tabpage.Controls.Add(chartcontrol)

        If hasheaders Then
            chartcontrol.UpdatePlotModelData()
            Dim j = 0
            For i As Integer = firstcol + 1 To lastcol
                DirectCast(chart.PlotModel, OxyPlot.PlotModel).Series(j).Title =
                    Spreadsheet.CurrentWorksheet.Cells(firstrow - 2, i).Data
                j += 1
            Next
            DirectCast(chart.PlotModel, OxyPlot.PlotModel).Axes(0).Title =
                    Spreadsheet.CurrentWorksheet.Cells(firstrow - 2, firstcol).Data
            DirectCast(chart.PlotModel, OxyPlot.PlotModel).Axes(1).Title = ""
            If DirectCast(chart.PlotModel, OxyPlot.PlotModel).Series.Count = 1 Then
                DirectCast(chart.PlotModel, OxyPlot.PlotModel).Axes(1).Title =
                    Spreadsheet.CurrentWorksheet.Cells(firstrow - 2, firstcol + 1).Data
            End If
        End If

        tabpage.Text = chart.DisplayName

        Flowsheet.Charts.Add(chart.ID, chart)

        Flowsheet.FormCharts.TabControl1.TabPages.Add(tabpage)

        Flowsheet.FormCharts.Activate()
        Flowsheet.FormCharts.TabControl1.SelectedTab = tabpage

    End Sub

    Private Sub FormNewSpreadsheet_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        If Not Loaded Then
            Initialize()
            CopyFromDT()
        End If

#If Not LINUX Then

        MoveSpreadsheetMenu()

#End If

        SetCustomFunctions()

    End Sub

    Public Sub SetCustomFunctions()

        Formula.FormulaExtension.CustomFunctions("GETPROPVAL") = Function(cell, args) As Object
                                                                     If args.Length = 2 Then
                                                                         Try
                                                                             Return Flowsheet.SimulationObjects(args(0).ToString).GetPropertyValue(args(1).ToString)
                                                                         Catch ex As Exception
                                                                             Return "ERROR: " & ex.Message
                                                                         End Try
                                                                     ElseIf args.Length = 3 Then
                                                                         Try
                                                                             Dim obj = Flowsheet.SimulationObjects(args(0).ToString)
                                                                             Dim val = obj.GetPropertyValue(args(1).ToString)
                                                                             Return ConvertUnits(Double.Parse(val), obj.GetPropertyUnit(args(1).ToString), args(2).ToString)
                                                                         Catch ex As Exception
                                                                             Return "ERROR: " & ex.Message
                                                                         End Try
                                                                     Else
                                                                         Return "INVALID ARGS"
                                                                     End If
                                                                 End Function

        Formula.FormulaExtension.CustomFunctions("SETPROPVAL") = Function(cell, args) As Object
                                                                     If args.Length = 3 Then
                                                                         Try
                                                                             Dim ws = cell.Worksheet
                                                                             Dim wcell = ws.Cells(ws.RowCount - 1, ws.ColumnCount - 1)
                                                                             wcell.Formula = args(2).ToString.Trim(Chr(34))
                                                                             Evaluator.Evaluate(wcell)
                                                                             Dim val = wcell.Data
                                                                             Flowsheet.SimulationObjects(args(0).ToString).SetPropertyValue(args(1).ToString, val)
                                                                             wcell.Formula = ""
                                                                             wcell.Data = ""
                                                                             Return String.Format("EXPORT OK [{0}, {1} = {2}]", Flowsheet.SimulationObjects(args(0).ToString).GraphicObject.Tag, args(1).ToString, val)
                                                                         Catch ex As Exception
                                                                             Return "ERROR: " & ex.Message
                                                                         End Try
                                                                     ElseIf args.Length = 4 Then
                                                                         Try
                                                                             Dim obj = Flowsheet.SimulationObjects(args(0).ToString)
                                                                             Dim prop = args(1).ToString
                                                                             Dim ws = cell.Worksheet
                                                                             Dim wcell = ws.Cells(ws.RowCount - 1, ws.ColumnCount - 1)
                                                                             wcell.Formula = args(2).ToString.Trim(Chr(34))
                                                                             Evaluator.Evaluate(wcell)
                                                                             Dim val = wcell.Data
                                                                             wcell.Formula = ""
                                                                             wcell.Data = ""
                                                                             Dim units = args(3).ToString
                                                                             Dim newval = ConvertUnits(Double.Parse(val), units, obj.GetPropertyUnit(prop))
                                                                             obj.SetPropertyValue(prop, newval)
                                                                             Return String.Format("EXPORT OK [{0}, {1} = {2} {3}]", obj.GraphicObject.Tag, prop, val, units)
                                                                         Catch ex As Exception
                                                                             Return "ERROR: " & ex.Message
                                                                         End Try
                                                                     Else
                                                                         Return "INVALID ARGS"
                                                                     End If
                                                                 End Function

        Formula.FormulaExtension.CustomFunctions("GETPROPUNITS") = Function(cell, args) As Object
                                                                       If args.Length = 2 Then
                                                                           Try
                                                                               Return Flowsheet.SimulationObjects(args(0).ToString).GetPropertyUnit(args(1).ToString)
                                                                           Catch ex As Exception
                                                                               Return "ERROR: " & ex.Message
                                                                           End Try
                                                                       Else
                                                                           Return "INVALID ARGS"
                                                                       End If
                                                                   End Function

        Formula.FormulaExtension.CustomFunctions("GETOBJID") = Function(cell, args) As Object
                                                                   If args.Length = 1 Then
                                                                       Try
                                                                           Return Flowsheet.GetFlowsheetSimulationObject(args(0).ToString).Name
                                                                       Catch ex As Exception
                                                                           Return "ERROR: " & ex.Message
                                                                       End Try
                                                                   Else
                                                                       Return "INVALID ARGS"
                                                                   End If
                                                               End Function

        Formula.FormulaExtension.CustomFunctions("GETOBJNAME") = Function(cell, args) As Object
                                                                     If args.Length = 1 Then
                                                                         Try
                                                                             Return Flowsheet.SimulationObjects(args(0).ToString).GraphicObject.Tag
                                                                         Catch ex As Exception
                                                                             Return "ERROR: " & ex.Message
                                                                         End Try
                                                                     Else
                                                                         Return "INVALID ARGS"
                                                                     End If
                                                                 End Function

    End Sub

    Public Sub MoveSpreadsheetMenu()

#If Not LINUX Then

        Dim menustrip1 As MenuStrip = SpreadsheetControl.SpreadsheetTSMI.GetCurrentParent()

        menustrip1.Items.Remove(SpreadsheetControl.SpreadsheetTSMI)

        Flowsheet.MenuStrip1.Items.Insert(4, SpreadsheetControl.SpreadsheetTSMI)

        SpreadsheetControl.SpreadsheetTSMI.Enabled = False

#End If

    End Sub


    Public Function GetCellValue(address As String) As Cell

        Return Spreadsheet.Worksheets(0).Cells(address)

    End Function

    Public Sub SetCellValue(ByVal cell As String, value As Object)

        Spreadsheet.Worksheets(0).Cells(cell).Data = value

    End Sub

    Public Sub EvaluateAll()

        Spreadsheet?.Worksheets(0).Recalculate()

    End Sub

    Public Sub WriteAll()

        For Each ws In Spreadsheet.Worksheets
            ws.Recalculate()
        Next

    End Sub

    Public Function GetCellString() As String()

        Dim celllist As New List(Of String)

        For Each item In Columns
            For i As Integer = 1 To 100
                celllist.Add(item & i.ToString)
            Next
        Next

        Return celllist.ToArray

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
                    If Double.TryParse(dt1(i)(j), Globalization.NumberStyles.Any, ci, New Double) Then
                        text += Double.Parse(dt1(i)(j), ci).ToString & ";"
                    Else
                        If dt1(i)(j) IsNot Nothing Then
                            text += dt1(i)(j).ToString() & ";"
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
                            Dim xel As New XElement("dummy", DirectCast(dt2(i)(j), Spreadsheet.SpreadsheetCellParameters).SaveData.ToArray)
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
        Dim value As Object
        Dim ci = System.Globalization.CultureInfo.InvariantCulture
        Dim format = Globalization.NumberStyles.Any - Globalization.NumberStyles.AllowThousands
        If n > 0 Then
            m = rows(0).Split(";").Length - 1
        End If
        If n > 0 And m > 0 Then
            Dim elm As New List(Of List(Of Object))
            Try
                For i As Integer = 0 To n
                    elm.Add(New List(Of Object))
                    If n > 0 Then
                        m = rows(i).Split(";").Length - 1
                    End If
                    For j As Integer = 0 To m
                        value = rows(i).Split(";")(j)
                        If Double.TryParse(value, format, ci, New Double) Then
                            elm(i).Add(Double.Parse(value, format, ci))
                        Else
                            elm(i).Add(value)
                        End If
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
            Dim elm As New List(Of List(Of Object))
            For i As Integer = 0 To n
                elm.Add(New List(Of Object))
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
                            elm(i).Add(scp)
                        Else
                            elm(i).Add(scp)
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

        n1 = dt1.Count - 1

        If n1 = -1 Then Exit Sub

        m1 = dt1(0).Count - 1

        n2 = dt2.Count - 1
        m2 = dt2(0).Count - 1

        maxrow = Spreadsheet.Worksheets(0).RowCount - 1
        maxcol = Spreadsheet.Worksheets(0).ColumnCount - 1

        Dim sheet = Spreadsheet.Worksheets(0)

        For i = 0 To n1
            For j = 0 To m1
                If i <= maxrow And j <= dt1(i).Count - 1 Then
                    If dt1(i)(j) IsNot Nothing Then sheet.Cells(i, j).Data = dt1(i)(j)
                End If
                If i <= n2 And j <= dt2(i).Count Then
                    If dt2(i)(j) Is Nothing Then
                        sheet.Cells(i, j).Tag = New Spreadsheet.SpreadsheetCellParameters
                    ElseIf TypeOf dt2(i)(j) Is Spreadsheet.SpreadsheetCellParameters Then
                        sheet.Cells(i, j).Tag = dt2(i)(j)
                    ElseIf TypeOf dt2(i)(j) Is Object Then
                        Dim cellparam As New Spreadsheet.SpreadsheetCellParameters
                        Try
                            With cellparam
                                .Expression = dt2(i)(j)
                                If CStr(dt2(i)(j)).StartsWith(":") Then
                                    .CellType = SharedClasses.Spreadsheet.VarType.Read
                                    Dim str As String()
                                    str = CStr(dt2(i)(j)).Split(New Char() {","})
                                    .ObjectID = str(0).Substring(1)
                                    .PropID = str(1)
                                Else
                                    .CellType = SharedClasses.Spreadsheet.VarType.Expression
                                End If
                            End With
                        Catch ex As Exception
                        End Try
                        sheet.Cells(i, j).Tag = cellparam
                    End If
                End If
            Next
        Next

        ParseOldData()

    End Sub

    Sub ParseOldData()

        'Dim separator = System.Threading.Thread.CurrentThread.CurrentCulture.TextInfo.ListSeparator
        Dim separator = ";"

        Dim esheet = Spreadsheet.NewWorksheet("EXPORTS")

        Dim elist As New List(Of Tuple(Of String, String, String, String))

        Dim i, j As Integer

        Dim cell As Cell

        For i = 0 To Spreadsheet.Worksheets(0).RowCount - 1
            For j = 0 To Spreadsheet.Worksheets(0).ColumnCount - 1
                cell = Spreadsheet.Worksheets(0).Cells(i, j)
                ccparams = cell.Tag
                If Not ccparams Is Nothing Then
                    Dim expression = ccparams.Expression
                    Select Case ccparams.CellType
                        Case SharedClasses.Spreadsheet.VarType.Expression, SharedClasses.Spreadsheet.VarType.Read
                            If expression <> "" Then
                                If expression.Substring(0, 1) = "=" Then
                                    cell.Formula = expression.TrimStart("=").ToUpper()
#If Not LINUX Then
                                    If Not ccparams.CellType = SharedClasses.Spreadsheet.VarType.Write Then cell.Style.BackColor = Color.LightYellow
#End If
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
                                    cell.Formula = String.Format("GETPROPVAL({3}{0}{3}{1}{3}{2}{3})", obj, separator, prop, Chr(34))
                                    'ccparams.ToolTipText = DWSIM.App.GetLocalString("CellIsReading") & vbCrLf &
                                    'DWSIM.App.GetLocalString("Objeto") & ": " & formc.Collections.FlowsheetObjectCollection(ccparams.ObjectID).GraphicObject.Tag & vbCrLf &
                                    'DWSIM.App.GetLocalString("Propriedade") & ": " & DWSIM.App.GetPropertyName(prop) & vbCrLf &
                                    'DWSIM.App.GetLocalString("CurrentValue") & ": " & cell.Value &
                                    '" " & formc.Collections.FlowsheetObjectCollection(obj).GetPropertyUnit(prop, formc.Options.SelectedUnitSystem)
#If Not LINUX Then
                                    cell.Style.BackColor = Color.LightGreen
#End If
                                Else
                                    cell.Data = expression
                                    If ccparams.CellType <> SharedClasses.Spreadsheet.VarType.Write And ccparams.CellType <> SharedClasses.Spreadsheet.VarType.Unit Then
                                        ccparams.ToolTipText = expression
                                    End If
                                End If
                            End If
                        Case SharedClasses.Spreadsheet.VarType.Unit
#If Not LINUX Then
                            cell.Style.BackColor = Color.Beige
#End If
                        Case SharedClasses.Spreadsheet.VarType.Write
                            If expression.Substring(0, 1) = "=" Then
                                cell.Formula = expression.TrimStart("=").ToUpper()
#If Not LINUX Then
                                If Not ccparams.CellType = SharedClasses.Spreadsheet.VarType.Write Then cell.Style.BackColor = Color.LightYellow
#End If
                            Else
                                cell.Data = expression
                                If ccparams.CellType <> SharedClasses.Spreadsheet.VarType.Write And ccparams.CellType <> SharedClasses.Spreadsheet.VarType.Unit Then
                                    ccparams.ToolTipText = expression
                                End If
                            End If
                            elist.Add(New Tuple(Of String, String, String, String)(String.Format("SETPROPVAL({4}{1}{4}{0}{4}{2}{4}{0}{4}{3}{4})", separator, ccparams.ObjectID, ccparams.PropID, "MAIN!" & cell.Address, Chr(34)),
                                                                           ccparams.ObjectID, ccparams.PropID, ccparams.PropUnit))
#If Not LINUX Then
                            cell.Style.BackColor = Color.LightBlue
#End If
                    End Select

                End If
            Next
        Next

        esheet.Cells(0, 0).Data = "EXPRESSION"
        esheet.Cells(0, 1).Data = "OBJECT"
        esheet.Cells(0, 2).Data = "PROPERTY"
        esheet.Cells(0, 3).Data = "UNITS"

        esheet.Cells(0, 0).Style.Bold = True
        esheet.Cells(0, 1).Style.Bold = True
        esheet.Cells(0, 2).Style.Bold = True
        esheet.Cells(0, 3).Style.Bold = True

        i = 1
        For Each item In elist
            esheet.Cells(i, 0).Formula = item.Item1
            esheet.Cells(i, 1).Data = Flowsheet.SimulationObjects(item.Item2).GraphicObject.Tag
            esheet.Cells(i, 2).Data = item.Item3
            esheet.Cells(i, 3).Data = item.Item4
        Next

    End Sub

    Private Sub FormNewSpreadsheet_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        Dim sheet = Spreadsheet.NewWorksheet("DUMMY1234567890")

        Spreadsheet.CurrentWorksheet = Spreadsheet.Worksheets("DUMMY1234567890")

        Spreadsheet.CurrentWorksheet = Spreadsheet.Worksheets(0)

        Spreadsheet.RemoveWorksheet(sheet)

    End Sub

#If Not LINUX Then
    Private Sub FormNewSpreadsheet_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged
        If SpreadsheetControl IsNot Nothing Then SpreadsheetControl.SpreadsheetTSMI.Enabled = Me.Visible
    End Sub
#End If

End Class