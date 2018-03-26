Public Class Host

    Public Shared Items As New Concurrent.ConcurrentBag(Of InspectorItem)

    Public Shared CurrentSolutionID As String = ""

    Public Shared CurrentItem As InspectorItem

    Public Shared Function GetNewInspectorItem() As InspectorItem

        If GlobalSettings.Settings.InspectorEnabled Then

            Return New Inspector.InspectorItem

        Else

            Return Nothing

        End If

    End Function

    Public Shared Sub CheckAndAdd(ii As InspectorItem, callingmethod As String, method As String, name As String, description As String)

        If ii IsNot Nothing Then
            With ii
                .CallingMethodName = callingmethod
                .MethodName = method
                .Name = name
                .Description = description
            End With
            If Host.CurrentItem IsNot Nothing Then
                Host.CurrentItem.Items.Add(ii)
            Else
                Inspector.Host.Items.Add(ii)
            End If
        End If

    End Sub

End Class

Public Class InspectorItem

    Public Property ID As String = ""

    Public Property Name As String = ""

    Public Property Description As String = ""

    Public Property MethodName As String = ""

    Public Property CallingMethodName As String = ""

    Public Property Paragraphs As New Concurrent.ConcurrentBag(Of String)

    Public Property SolutionID As String = ""

    Public Property ThreadID As Integer = -1

    Public Property StartTime As DateTime = DateTime.Now

    Public Property Items As New List(Of InspectorItem)

    Sub New()
        ID = Guid.NewGuid().ToString()
        StartTime = Date.Now
        ThreadID = System.Threading.Thread.CurrentThread.ManagedThreadId
        SolutionID = Host.CurrentSolutionID
    End Sub

    Public Function GetHTML() As String

        Dim stb As New Text.StringBuilder

        stb.AppendLine("<html>
                        <head>
                        <style>
                            body {
                                font-family: Arial, Helvetica, sans-serif;
                            }
                        </style>")
        stb.AppendLine("<script src='http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js'></script>")
        stb.AppendLine("<script type='text/x-mathjax-config'> MathJax.Hub.Config({ 'HTML-CSS': { scale: 100, linebreaks: { automatic: true } }, SVG: { linebreaks: { automatic:true } }, displayAlign: 'left' }); </script>")
        stb.AppendLine("<script type='text/javascript' async src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_HTML'></script>")
        stb.AppendLine("</head><section class='main'>")
        stb.AppendLine("<div class='post'>")
        stb.AppendLine(String.Format("<h1>{0}</h1><h2>{1}</h2>", Name, Description))
        For Each p In Paragraphs.Reverse()
            stb.AppendLine(String.Format("<p>{0}</p>", p).Replace("<math>", "$$").Replace("</math>", "$$").Replace("<math_inline>", "\(").Replace("</math_inline>", "\)"))
        Next
        stb.AppendLine("</div></section></html>")

        Return stb.ToString()

    End Function

End Class
