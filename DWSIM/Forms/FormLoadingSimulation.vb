﻿Public Class FormLoadingSimulation

    Public Sub FormLoadingSimulation_Load(sender As Object, e As EventArgs) Handles MyBase.Load


        Label1.Parent = ProgressBar1
        Label1.BackColor = Color.Transparent

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

End Class