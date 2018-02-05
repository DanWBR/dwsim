Namespace CalculationMonitoring

    Public Class CalculationResults

        Public Shared Results As New Dictionary(Of String, List(Of CalculationDetails))

    End Class

    Public Class CalculationDetails

        Public Property Name As String = ""
        Public Property Description As String = ""
        Public Property Context As String = ""
        Public Property Result As Object

        Public Property CalculationRequestID As String = ""

        Public Property RequestingObjectID As String = ""

        Public Property ThreadID As Integer = -1

        Public Property Time As DateTime = DateTime.Now

        Sub New()

        End Sub

    End Class

End Namespace
