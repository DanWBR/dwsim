Imports System.Linq

Public Class FormCompoundList

    Public Compounds As List(Of String)

    Public SelectedCompound As String

    Private Sub FormCompoundList_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ListBox1.Items.AddRange(Compounds.OrderBy(Function(n) n).ToArray())

        ChangeDefaultFont()

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox1.SelectedIndexChanged

        If MessageBox.Show("Confirm selection?", "Question", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then

            SelectedCompound = ListBox1.SelectedItem.ToString()

            Close()

        Else

            SelectedCompound = Nothing

        End If

    End Sub


End Class