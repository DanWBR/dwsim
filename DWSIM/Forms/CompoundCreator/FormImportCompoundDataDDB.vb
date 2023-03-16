Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.Databases.DDBStructureLink

Public Class FormImportCompoundDataDDB

    Private CurrentPanel As String = ""
    Private data As New Dictionary(Of String, List(Of String()))
    Private compound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Public BaseCompound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub FormImportCompoundDataKDB_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CurrentPanel = "Panel1"
    End Sub

    Private Sub btnPrev_Click(sender As Object, e As EventArgs) Handles btnPrev.Click
        Select Case CurrentPanel
            Case "Panel1"
            Case "Panel2"
                Panel1.BringToFront()
                CurrentPanel = "Panel1"
                btnPrev.Enabled = False
                btnNext.Text = DWSIM.App.GetLocalString("NextButton")
        End Select
    End Sub

    Private Sub btnNext_Click(sender As Object, e As EventArgs) Handles btnNext.Click

        Select Case CurrentPanel

            Case "Panel1"

                btnPrev.Enabled = True
                Panel3.BringToFront()
                CurrentPanel = "Panel2"
                btnNext.Text = DWSIM.App.GetLocalString("Import")

                Dim fsearch As New FormSearchingDDB()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim searchtext As String = tbSearchString.Text

                FormMain.AnalyticsProvider?.RegisterEvent("Online Compound DDB Request", searchtext, Nothing)

                Me.Enabled = False

                fsearch.Show()

                Dim t As New Task(Of Dictionary(Of String, List(Of String())))(Function()
                                                                                   Return DDBStructureParser.GetData(DDBStructureParser.GetID(searchtext))
                                                                               End Function, tcs.Token)

                t.ContinueWith(Sub()
                                   UIThread(Sub()
                                                fsearch.Close()
                                                Enabled = True
                                                Focus()
                                                If t.Exception Is Nothing Then
                                                    data = t.Result
                                                    AddPropertiesToGrid()
                                                Else
                                                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundOnlineSourceError"), DWSIM.App.GetLocalString("Erro"))
                                                End If

                                            End Sub)
                               End Sub, TaskContinuationOptions.ExecuteSynchronously)

                AddHandler fsearch.btnCancel.Click, Sub()
                                                        fsearch.Close()
                                                        Me.Enabled = True
                                                        Focus()
                                                        tcs.Cancel()
                                                    End Sub

                t.Start()

            Case "Panel2"

                CopySelectedData()

                Me.DialogResult = Windows.Forms.DialogResult.OK

                Me.Close()

        End Select
    End Sub

    Private Sub tbSearchString_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSearchString.KeyDown
        If e.KeyCode = Keys.Enter Then btnNext_Click(sender, e)
    End Sub

    Sub AddPropertiesToGrid()

        Dim okimg = My.Resources.accept
        Dim noimg = My.Resources.cross

        Me.dgResults.Rows.Clear()
        With compound
            Me.dgResults.Rows.Add(New Object() {If(data.ContainsKey("Original"), okimg, noimg), "Original UNIFAC Structure Data", If(data.ContainsKey("Original"), True, False)})
            Me.dgResults.Rows.Add(New Object() {If(data.ContainsKey("Modified"), okimg, noimg), "Modified UNIFAC (Dortmund) Structure Data", If(data.ContainsKey("Modified"), True, False)})
        End With


    End Sub

    Private Sub CopySelectedData()

        With BaseCompound
            If dgResults.Rows(0).Cells(2).Value = True Then
                If .UNIFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                .UNIFACGroups.Clear()
                For Each item In data("Original")
                    .UNIFACGroups.Add(item(1), item(2))
                Next
            End If
            If dgResults.Rows(1).Cells(2).Value = True Then
                If .MODFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                .MODFACGroups.Clear()
                For Each item In data("Modified")
                    .MODFACGroups.Add(item(1), item(2))
                Next
                If CheckBox1.Checked Then
                    If .NISTMODFACGroups Is Nothing Then .NISTMODFACGroups = New SortedList
                    .NISTMODFACGroups.Clear()
                    For Each sg As String In .MODFACGroups.Keys
                        .NISTMODFACGroups.Add(sg, .MODFACGroups(sg))
                    Next
                End If
            End If
        End With


    End Sub

End Class