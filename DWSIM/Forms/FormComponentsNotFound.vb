Public Class FormComponentsNotFound

    Public LoaderExceptions As List(Of Exception)

    Private Sub FormComponentsNotFound_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        For Each exc As ComponentNotFoundException In LoaderExceptions
            If exc.ProductName IsNot Nothing Then
                dgv.Rows.Add(New Object() {exc.ProductName, exc.ProductDescription,
                             exc.ProductVersion, exc.ProductAuthor, exc.ProductPage,
                             exc.ProductContactInfo})
            Else

            End If
        Next

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Close()

    End Sub

End Class