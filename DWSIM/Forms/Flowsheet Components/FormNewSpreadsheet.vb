Public Class FormNewSpreadsheet

    Public Property SpreadsheetControl As unvell.ReoGrid.Editor.ReoGridEditor

    Public Property Flowsheet As FormFlowsheet

    Public ReadOnly Property Spreadsheet
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

End Class