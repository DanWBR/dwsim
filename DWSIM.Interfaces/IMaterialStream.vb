Public Interface IMaterialStream

    Property SpecType As Enums.StreamSpec

    Property IsElectrolyteStream As Boolean

    Property ReferenceSolvent As String

    Property InputComposition As Dictionary(Of String, Double)

    Property CompositionBasis As Enums.CompositionBasis

End Interface
