Public Interface IMaterialStream

    Property SpecType As Enums.StreamSpec

    Property IsElectrolyteStream As Boolean

    Property ReferenceSolvent As String

    Property InputComposition As Dictionary(Of String, Double)

    Property CompositionBasis As Enums.CompositionBasis

    ReadOnly Property Phases() As Dictionary(Of Integer, IPhase)

    Property AtEquilibrium As Boolean

    Sub SetPhaseComposition(Vx As Array, phs As Integer)

    Sub SetOverallComposition(Vx As Array)

    Function Clone() As IMaterialStream

    ReadOnly Property Flowsheet As IFlowsheet

    Sub Validate()

    Sub ClearAllProps()

    Function GetPropertyPackageObject() As Object

    Function GetPropertyPackageObjectCopy() As Object

    Sub SetPropertyPackageObject(pp As Object)

    Sub SetCurrentMaterialStream(ms As Object)

End Interface
