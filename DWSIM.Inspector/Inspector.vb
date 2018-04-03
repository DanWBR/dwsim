Imports System.Drawing
Imports System.Runtime.CompilerServices
Imports System.IO

Public Class Host

    Public Shared Items As New List(Of InspectorItem)

    Public Shared CurrentSolutionID As String = ""

    Public Shared CurrentItem As InspectorItem

    Public Shared Sub SetCurrent(ii As InspectorItem)

        CurrentItem = ii

    End Sub

    Public Shared Function GetNewInspectorItem(<CallerMemberName> Optional memberName As String = "", <CallerFilePath> Optional fileName As String = "", <CallerLineNumber> Optional lineNumber As Integer = 0) As InspectorItem

        If GlobalSettings.Settings.InspectorEnabled Then
            Return New Inspector.InspectorItem With {.CodePath = (fileName & "#L" & lineNumber).Replace(fileName.Substring(0, fileName.IndexOf("\dwsim5\") + 7), "https://github.com/DanWBR/dwsim5/blob/master").Replace("\", "/")}
        Else
            Return Nothing
        End If

    End Function

    Public Shared Sub CheckAndAdd(ii As InspectorItem, callingmethod As String, method As String, name As String, description As String, Optional current As Boolean = False)

        If ii IsNot Nothing Then
            With ii
                .CallingMethodName = callingmethod
                .MethodName = method
                .Name = name
                .Description = description
            End With
            If Host.CurrentItem IsNot Nothing Then
                ii.ParentID = Host.CurrentItem.ID
                Host.CurrentItem.Items.Add(ii)
            Else
                ii.ParentID = -1
                Inspector.Host.Items.Add(ii)
            End If
            If current Then Inspector.Host.CurrentItem = ii
        End If
    End Sub

End Class

Public Class InspectorItem

    Public Property ID As String = ""

    Public Property ParentID As String = ""

    Public Property Name As String = ""

    Public Property Description As String = ""

    Public Property MethodName As String = ""

    Public Property CallingMethodName As String = ""

    Public Property Paragraphs As New Concurrent.ConcurrentBag(Of String)

    Public Property SolutionID As String = ""

    Public Property ThreadID As Integer = -1

    Public Property StartTime As DateTime = DateTime.Now

    Public Property Items As New List(Of InspectorItem)

    Public Property CodePath As String = ""

    Sub New()
        ID = Guid.NewGuid().ToString()
        StartTime = Date.Now
        ThreadID = System.Threading.Thread.CurrentThread.ManagedThreadId
        SolutionID = Host.CurrentSolutionID
        Dim st As New StackTrace()
    End Sub

    Public Function GetHTML() As String

        Dim stb As New System.Text.StringBuilder

        stb.AppendLine("<html>
                        <head>
                        <style>
                            body {
                                font-family: Arial, Helvetica, sans-serif;
                                font-size: 80%;
                            }
                        </style>")
        stb.AppendLine("<script src='http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js'></script>")

        If GlobalSettings.Settings.RunningPlatform = GlobalSettings.Settings.Platform.Windows Then
            stb.AppendLine("<script type='text/javascript' async src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=default'></script>")
        Else
            stb.AppendLine("<script type='text/x-mathjax-config'> MathJax.Hub.Config({ 'CommonHTML': { scale: 100, linebreaks: { automatic: true } }, SVG: { linebreaks: { automatic:true } }, displayAlign: 'left' }); </script>")
            stb.AppendLine("<script type='text/javascript' async src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-MML-AM_CHTML'></script>")
        End If

        stb.AppendLine("</head><section class='main'>")
        stb.AppendLine("<div class='post'>")
        stb.AppendLine(String.Format("<h1>{0}</h1><h2>{1}</h2>", Name, Description))
        stb.AppendLine("<hr>")
        stb.AppendLine(String.Format("<div><div style='float:right;'><a target='_blank' style='border:0;' href='{0}'><img style='border:0;' src='{1}' alt='View on GitHub' width='150'></a></div><div style='style='float:left;height:100px;line-height:100px;'><b>Source Code (Visual Basic)</b>: {0}</div></div>", CodePath.Replace("https://github.com/DanWBR/dwsim5/blob/master/", ""), GetImagePath("viewongithub.png")))
        stb.AppendLine("<hr>")
        For Each p In Paragraphs.Reverse()
            stb.AppendLine(String.Format("<p>{0}</p>", p).Replace("<math>", "$$").Replace("</math>", "$$").Replace("<math_inline>", "\(").Replace("</math_inline>", "\)").Replace("<m>", "$$").Replace("</m>", "$$").Replace("<mi>", "\(").Replace("</mi>", "\)"))
        Next
        stb.AppendLine("<hr>")
        stb.AppendLine("<b>Item Details</b><br/>")
        stb.AppendLine(String.Format("ID: {0}<br/>", ID))
        stb.AppendLine(String.Format("Parent Item ID: {0}<br/>", ParentID))
        stb.AppendLine(String.Format("Thread ID: {0}<br/>", ThreadID))
        stb.AppendLine(String.Format("Date Created: {0}", StartTime))
        stb.AppendLine("</div></section></html>")

        Return stb.ToString()

    End Function

    Private Function GetImagePath(filename As String) As String

        Return "http://dwsim.inforside.com.br/inspector/images/" & filename

    End Function

    Public Shared Function GetImageHTML(filename As String) As String

        Dim ipath = "http://dwsim.inforside.com.br/inspector/images/" & filename
        Return String.Format("<img style='border:0;' src='{0}' alt=''>", ipath)

    End Function

End Class

Public Module InspectorExtensions

    <System.Runtime.CompilerServices.Extension()>
    Public Sub SetCurrent(ii As InspectorItem)

        Inspector.Host.CurrentItem = ii

    End Sub


End Module
