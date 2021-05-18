Public Interface IAnalyticsProvider

    Sub RegisterEvent(name As String, description As String, data As Dictionary(Of String, String))

    Sub RegisterError(name As String, description As String, ex As Exception, data As Dictionary(Of String, String))

    Sub Initialize()

End Interface
