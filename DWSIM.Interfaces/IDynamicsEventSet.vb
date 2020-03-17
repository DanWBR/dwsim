Public Interface IDynamicsEventSet

    Property ID As String

    Property Description As String

    Property Events As Dictionary(Of String, IDynamicsEvent)

End Interface
