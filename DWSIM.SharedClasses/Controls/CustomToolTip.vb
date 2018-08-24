Imports System.Drawing

Public Class CustomTooltip

    Inherits ToolTip

    Sub New()

        MyBase.New()

        Me.OwnerDraw = True

        AddHandler Me.Draw, AddressOf OnDraw

    End Sub

    Public Sub New(ByVal Cont As System.ComponentModel.IContainer)

        MyBase.New(Cont)

        Me.OwnerDraw = True

        AddHandler Me.Draw, AddressOf OnDraw

    End Sub

    Private Sub OnDraw(ByVal sender As Object, ByVal e As DrawToolTipEventArgs)

        Dim myfont As New Font("Consolas", 9, FontStyle.Bold, GraphicsUnit.Pixel)
        Dim sf As StringFormat = New StringFormat
        Try
            sf.Alignment = StringAlignment.Far
            sf.LineAlignment = StringAlignment.Far
            sf.FormatFlags = StringFormatFlags.MeasureTrailingSpaces
            Dim fsize = e.Graphics.MeasureString(e.ToolTipText, myfont, 200, sf)
            Dim rect = New Rectangle(e.Bounds.X, e.Bounds.Y, fsize.Width + 4, fsize.Height + 4)
            Dim rect2 = New Rectangle(e.Bounds.X + 2, e.Bounds.Y + 2, fsize.Width + 2, fsize.Height + 2)
            e.Graphics.FillRectangle(Brushes.SteelBlue, rect)
            e.Graphics.DrawString(e.ToolTipText, myfont, Brushes.White, rect)
        Finally
            sf.Dispose()
            myfont.Dispose()
        End Try

    End Sub

End Class
