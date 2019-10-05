Public Class FormCharts

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Sub FormCharts_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        If Flowsheet.ChartCollection.Count > 0 Then
            For Each item In Flowsheet.ChartCollection
                Dim tabpage As New TabPage
                Dim chart = item.Value
                tabpage.Controls.Add(New TwoDimChartControl() With {.Dock = DockStyle.Fill,
                                 .Flowsheet = Flowsheet,
                                 .Chart = chart,
                                 .Spreadsheet = Flowsheet.FormSpreadsheet.Spreadsheet})
                tabpage.Text = chart.DisplayName
                TabControl1.TabPages.Add(tabpage)
            Next
        End If

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim tabpage As New TabPage

        Dim chart As New Charts.Chart

        tabpage.Controls.Add(New TwoDimChartControl() With {.Dock = DockStyle.Fill,
                             .Flowsheet = Flowsheet,
                             .Chart = chart,
                             .Spreadsheet = Flowsheet.FormSpreadsheet.Spreadsheet})

        tabpage.Text = chart.DisplayName

        Flowsheet.Charts.Add(chart.ID, chart)

        TabControl1.TabPages.Add(tabpage)

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        If Me.TabControl1.TabPages.Count > 0 Then
            If Me.TabControl1.SelectedTab IsNot Nothing Then
                Dim chart = DirectCast(TabControl1.SelectedTab.Controls(0), TwoDimChartControl).Chart
                If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                   DWSIM.App.GetLocalString("Ateno2"),
                                   MessageBoxButtons.YesNo,
                                   MessageBoxIcon.Question) = DialogResult.Yes Then
                    Flowsheet.Charts.Remove(chart.ID)
                    Me.TabControl1.TabPages.Remove(Me.TabControl1.SelectedTab)
                End If
            End If
        End If

    End Sub

End Class