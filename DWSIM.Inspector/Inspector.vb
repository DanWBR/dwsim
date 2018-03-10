Public Class Collection

    Public Shared Results As New Dictionary(Of String, List(Of InspectorItem))

    Public Shared CurrentItemID As String = ""

End Class

Public Class InspectorItem

    Public Property Name As String = ""
    Public Property Description As String = ""

    Public Property HTMLParagraphs As New List(Of String)

    Public Property CalculationRequestID As String = ""
    Public Property ThreadID As Integer = -1

    Public Property Time As DateTime = DateTime.Now

    Public Property Items As New List(Of InspectorItem)

    Sub New()
        Time = Date.Now
        ThreadID = System.Threading.Thread.CurrentThread.ManagedThreadId
    End Sub

End Class
