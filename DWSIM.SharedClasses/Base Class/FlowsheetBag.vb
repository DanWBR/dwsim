Namespace Flowsheet

    Public Class FlowsheetBag

        Implements IFlowsheetBag

        Sub New()

        End Sub

        Public Property SimulationObjects As New Dictionary(Of String, ISimulationObject) Implements IFlowsheetBag.SimulationObjects

        Public Property GraphicObjects As New Dictionary(Of String, IGraphicObject) Implements IFlowsheetBag.GraphicObjects

        Public Property Compounds As New Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheetBag.Compounds

        Public Property PropertyPackages As New Dictionary(Of String, IPropertyPackage) Implements IFlowsheetBag.PropertyPackages

        Public Property Reactions As New Dictionary(Of String, IReaction) Implements IFlowsheetBag.Reactions

        Public Property ReactionSets As New Dictionary(Of String, IReactionSet) Implements IFlowsheetBag.ReactionSets

    End Class

End Namespace
