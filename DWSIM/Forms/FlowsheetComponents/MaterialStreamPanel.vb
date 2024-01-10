Imports System.IO
Imports System.Linq
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports unvell.ReoGrid
Imports unvell.ReoGrid.DataFormat

Public Class MaterialStreamPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Protected Conversor As SystemsOfUnits.Converter
    Protected filename As String = ""
    Protected Flowsheet As FormFlowsheet
    Protected RowsCreated As Boolean = False

    Private Sub frmMatList_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        If Settings.DpiScale > 1.0 Then
            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.AutoSize = True
            Me.ToolStrip1.Invalidate()
        End If

        Me.Flowsheet = My.Application.ActiveSimulation

        grid1.Worksheets(0).SetScale(Settings.DpiScale)

    End Sub

    Public Function ReturnForm(ByVal str As String) As WeifenLuo.WinFormsUI.Docking.IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Private Sub SetupGrid()

        Dim ms As New Streams.MaterialStream("", "", Flowsheet, Nothing)
        Flowsheet.AddComponentsRows(ms)
        Dim nms = Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream).Count

        Dim props = ms.GetProperties(PropertyType.ALL)

        grid1.Readonly = True

        With grid1.Worksheets(0)
            .SetCols(2 + nms)
            .SetRows(props.Count + 1)
            .SetScale(Settings.DpiScale)
            .SetColumnsWidth(2, .ColumnCount, 100)
            .SetRangeStyles(0, 1, .RowCount, .ColumnCount, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Center
            })
            .SetRangeStyles(0, 2, .RowCount, .ColumnCount, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.VerticalAlign,
                .VAlign = ReoGridVerAlign.Middle
            })
            .SetRangeStyles(0, 0, .RowCount, .ColumnCount, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.FontAll,
                .FontName = System.Drawing.SystemFonts.MessageBoxFont.Name,
                .FontSize = System.Drawing.SystemFonts.MessageBoxFont.SizeInPoints
            })
            For i = 1 To props.Count
                .Cells(i, 0).Data = Flowsheet.GetTranslatedString1(props(i - 1))
                .Cells(i, 1).Data = ms.GetPropertyUnit(props(i - 1), Flowsheet.Options.SelectedUnitSystem1)
            Next
            .SetRangeStyles(0, 0, .RowCount, 2, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Left
            })
            .SetColumnsWidth(0, 1, 225)
            .SetColumnsWidth(1, 1, 75)
            .SetRangeStyles(0, 0, .RowCount, 2, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.FontAll,
                .FontName = System.Drawing.SystemFonts.MessageBoxFont.Name,
                .FontSize = System.Drawing.SystemFonts.MessageBoxFont.SizeInPoints,
                .Bold = True
            })
            .SetRangeStyles(0, 0, 1, .ColumnCount, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.FontAll,
                .FontName = System.Drawing.SystemFonts.MessageBoxFont.Name,
                .FontSize = System.Drawing.SystemFonts.MessageBoxFont.SizeInPoints,
                .Bold = True
            })
            .Cells(0, 0).Data = "Property / Streams"
            .Cells(0, 1).Data = "Units"
            .SetRangeDataFormat(1, 2, .RowCount - 1, .ColumnCount, CellDataFormatFlag.Number,
            New NumberDataFormatter.NumberFormatArgs With {
                .DecimalPlaces = 4,
                .UseSeparator = True,
                .NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus
            })
            .SetRangeStyles(0, 1, 1, .ColumnCount, New WorksheetRangeStyle With {
                .Flag = PlainStyleFlag.HorizontalAlign,
                .HAlign = ReoGridHorAlign.Center
            })
        End With

    End Sub

    Private Sub UpdateTable()

        Me.Flowsheet = My.Application.ActiveSimulation

        SetupGrid()

        If Not Flowsheet Is Nothing Then
            Dim ms As Streams.MaterialStream
            RowsCreated = False
            Dim i, j As Integer
            i = 2
            Dim mslist = Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Streams.MaterialStream).OrderBy(Function(s) s.GraphicObject.Tag)
            For Each ms In mslist
                grid1.Worksheets(0).Cells(0, i).Data = ms.GraphicObject.Tag
                Dim props = ms.GetProperties(PropertyType.ALL)
                j = 1
                For Each p In props
                    Dim val = ms.GetPropertyValue(p, Flowsheet.Options.SelectedUnitSystem1)
                    If Double.TryParse(val, New Double) Then
                        If Double.IsNaN(val) Or Double.IsInfinity(val) Then
                            grid1.Worksheets(0).Cells(j, i).Data = ""
                        Else
                            grid1.Worksheets(0).Cells(j, i).Data = val
                        End If
                    Else
                        grid1.Worksheets(0).Cells(j, i).Data = val
                    End If
                    j += 1
                Next
                i += 1
            Next
        End If

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        UpdateTable()

        If grid1.Worksheets(0).RowCount > 0 Then ToolStripButton2.Enabled = True

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

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        grid1.Worksheets(0).SelectAll()
        grid1.Worksheets(0).Copy()
    End Sub

    Private Sub MaterialStreamPanel_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

    Private Sub ToolStripButton3_Click(sender As Object, e As EventArgs) Handles ToolStripButton3.Click
        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Excel File", "*.xlsx")})

        If handler IsNot Nothing Then
            Using stream As New MemoryStream()
                Try
                    grid1.Save(stream, IO.FileFormat.Excel2007)
                    handler.Write(stream)
                    MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End Using
        End If
    End Sub
End Class