Public Interface IReactionSet

    Property ID() As String

    Property Name() As String

    Property Description() As String

    ReadOnly Property Reactions() As Dictionary(Of String, IReactionSetBase)

End Interface
