Imports System.Drawing
Imports System.Runtime.CompilerServices
Imports System.IO

Public Class Host

    Public Shared Items As New List(Of InspectorItem)

    Private Shared _currentsid As String = ""

    Public Shared Property CurrentSolutionID As String
        Get
            Return _currentsid
        End Get
        Set(value As String)
            _currentsid = value
            If GlobalSettings.Settings.ClearInspectorHistoryOnNewCalculationRequest Then
                Items.Clear()
            End If
        End Set
    End Property

    Public Shared CurrentItem As InspectorItem

    Public Shared Sub SetCurrent(ii As InspectorItem)

        CurrentItem = ii

    End Sub

    Public Shared Function GetNewInspectorItem(<CallerMemberName> Optional memberName As String = "", <CallerFilePath> Optional fileName As String = "", <CallerLineNumber> Optional lineNumber As Integer = 0) As InspectorItem

        If GlobalSettings.Settings.InspectorEnabled And Not GlobalSettings.Settings.ExcelMode And Not GlobalSettings.Settings.CAPEOPENMode Then
            Return New Inspector.InspectorItem With {.CodePath = (fileName & "#L" & lineNumber).Replace(fileName.Substring(0, fileName.LastIndexOf("\dwsim\") + 7), "https://github.com/DanWBR/dwsim/blob/windows/").Replace("\", "/")}
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
                Host.CurrentItem.Paragraphs.Add(String.Format("<div style='color:gray'>[Calling function <i>{0}</i> ({1} - {2})]</div>", method, name, description))
                Host.CurrentItem.Items.Add(ii)
            Else
                ii.ParentID = -1
                Inspector.Host.Items.Add(ii)
            End If
            If current Then Inspector.Host.CurrentItem = ii
        End If
    End Sub

    Public Shared Function GetItemAndChildren(ii As InspectorItem) As List(Of InspectorItem)

        Return GetItems(ii).ToList()

    End Function

    Public Shared Function GetItems(ByVal iitem As InspectorItem) As List(Of InspectorItem)

        Dim myItems As List(Of InspectorItem) = New List(Of InspectorItem)()

        For Each i As InspectorItem In iitem.Items
            GetInspectorItems(i, myItems)
        Next

        Return myItems

    End Function

    Private Shared Sub GetInspectorItems(ByVal item As InspectorItem, ByVal items As List(Of InspectorItem))

        items.Add(item)

        For Each i As InspectorItem In item.Items
            GetInspectorItems(i, items)
        Next

    End Sub

End Class

Public Class InspectorItem

    Public Property ID As String = ""

    Public Property ParentID As String = ""

    Public Property Name As String = ""

    Public Property Description As String = ""

    Public Property MethodName As String = ""

    Public Property CallingMethodName As String = ""

    Public Property Paragraphs As New List(Of String)

    Public Property SolutionID As String = ""

    Public Property ThreadID As Integer = -1

    Public Property TimeTaken As TimeSpan

    Public Property Items As New List(Of InspectorItem)

    Public Property CodePath As String = ""

    Private Property _Counter As New Stopwatch

    Sub New()
        ID = Guid.NewGuid().ToString()
        ThreadID = System.Threading.Thread.CurrentThread.ManagedThreadId
        SolutionID = Host.CurrentSolutionID
        _Counter.Start()
    End Sub

    Public Sub Close()
        _Counter.Stop()
        TimeTaken = _Counter.Elapsed
    End Sub

    Public Function GetHTML() As String

        Dim stb As New System.Text.StringBuilder

        stb.AppendLine("<html>
                        <head>
                        <style>
                            body {
                                font-family: Arial, Helvetica, sans-serif;
                                font-size: 100%;
                            }
                        </style>")

        If GlobalSettings.Settings.RunningPlatform = GlobalSettings.Settings.Platform.Windows Then
            stb.AppendLine("<script type='text/javascript' async src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=default'></script>")
        Else
            stb.AppendLine("<script type='text/x-mathjax-config'> MathJax.Hub.Config({ 'CommonHTML': { scale: 100, linebreaks: { automatic: true } }, SVG: { linebreaks: { automatic:true } }, displayAlign: 'left' }); </script>")
            stb.AppendLine("<script type='text/javascript' async src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'></script>")
        End If

        stb.AppendLine("</head><section class='main'>")
        stb.AppendLine("<div class='post'>")
        stb.AppendLine(String.Format("<h1>{0}</h1><h2>{1}</h2>", Name, Description))
        stb.AppendLine("<hr>")
        stb.AppendLine(String.Format("<div><div style='float:right;height:40px;line-height:40px;vertical-align:middle;'><a target='_blank' style='border:0;' href='{0}'><img style='border:0;' src='{2}' alt='View on GitHub' width='200'></a></div><div style='float:left;height:40px;line-height:40px;vertical-align:middle;'><b>Source Code (Visual Basic)</b>: {1}</div></div>", CodePath, CodePath.Replace("https://github.com/DanWBR/dwsim/blob/windows/", ""), GetImagePath("viewongithub.png")))
        stb.AppendLine("<hr style='clear:both;'>")
        For Each p In Paragraphs
            stb.AppendLine(String.Format("<p>{0}</p>", p).Replace("<math>", "$$").Replace("</math>", "$$").Replace("<math_inline>", "\(").Replace("</math_inline>", "\)").Replace("<m>", "$$").Replace("</m>", "$$").Replace("<mi>", "\(").Replace("</mi>", "\)"))
        Next
        stb.AppendLine("</div></section></html>")

        Return stb.ToString()

    End Function

    Private Function GetImagePath(filename As String) As String

        Return "https://dwsim.org/inspector/images/" & filename

    End Function

    Public Shared Function GetImageHTML(filename As String) As String

        Dim ipath = "https://dwsim.org/inspector/images/" & filename
        Return String.Format("<img style='border:0;' src='{0}' alt=''>", ipath)

    End Function

End Class

Public Module InspectorExtensions

    <System.Runtime.CompilerServices.Extension()>
    Public Sub SetCurrent(ii As InspectorItem)

        Inspector.Host.CurrentItem = ii

    End Sub


End Module
