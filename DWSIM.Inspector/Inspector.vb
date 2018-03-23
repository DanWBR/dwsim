Public Class Host

    Public Shared Items As New Concurrent.ConcurrentBag(Of InspectorItem)

    Public Shared CurrentSolutionID As String = ""

    Public Shared Function GetNewInspectorItem() As InspectorItem

        If GlobalSettings.Settings.InspectorEnabled Then

            Return New Inspector.InspectorItem

        Else

            Return Nothing

        End If

    End Function

    Public Shared Sub CheckAndAdd(IObj As InspectorItem, callingmethod As String, method As String, name As String, description As String)

        If IObj IsNot Nothing Then
            Inspector.Host.Items.Add(IObj)
            With IObj
                .CallingMethodName = callingmethod
                .MethodName = method
                .Name = name
                .Description = description
            End With
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

    Public Property EndTime As DateTime

    Sub New()
        ID = Guid.NewGuid().ToString()
        StartTime = Date.Now
        ThreadID = System.Threading.Thread.CurrentThread.ManagedThreadId
        SolutionID = Host.CurrentSolutionID
    End Sub

End Class
