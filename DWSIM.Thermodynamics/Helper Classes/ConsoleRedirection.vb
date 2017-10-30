Imports System.Text
Imports System.IO
Imports System.Windows.Forms

Namespace ConsoleRedirection

    Public Class TextBoxStreamWriter

        Inherits TextWriter

        Private _tbox As TextBox

        Public Sub New(ByVal tbox As TextBox)
            _tbox = tbox
        End Sub

        Public Overrides Sub Write(value As String)
            MyBase.Write(value)
            _tbox.UIThread(Sub()
                               If Not _tbox Is Nothing Then
                                   _tbox.AppendText(value)
                               End If
                           End Sub)
        End Sub

        Public Overrides Sub Write(ByVal value As Char)
            MyBase.Write(value)
            _tbox.UIThread(Sub()
                               If Not _tbox Is Nothing Then
                                   _tbox.AppendText(value.ToString())
                               End If
                           End Sub)
        End Sub

        Public Overrides ReadOnly Property Encoding() As Encoding
            Get
                Return System.Text.Encoding.UTF8
            End Get
        End Property

    End Class

End Namespace
