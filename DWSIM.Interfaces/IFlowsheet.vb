Public Interface IFlowsheet

    Enum MessageType
        Information
        Warning
        GeneralError
        Tip
        Other
    End Enum

    ReadOnly Property Reactions As Dictionary(Of String, IReaction)

    ReadOnly Property ReactionSets As Dictionary(Of String, IReactionSet)

    ReadOnly Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    ReadOnly Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    ReadOnly Property PropertyPackages As Dictionary(Of String, IPropertyPackage)

    ReadOnly Property SelectedCompounds As Dictionary(Of String, ICompoundConstantProperties)

    Property FilePath As String

    Sub ShowMessage(ByVal text As String, ByVal mtype As MessageType)

    Sub ShowDebugInfo(ByVal text As String, ByVal level As Integer)

    Sub CheckStatus()

    Sub RequestCalculation(Optional ByVal sender As ISimulationObject = Nothing)

    Function GetTranslatedString(text As String, locale As String) As String

    Function GetTranslatedString(text As String) As String

    ReadOnly Property FlowsheetOptions As IFlowsheetOptions

    Function GetFlowsheetSimulationObject(tag As String) As ISimulationObject

    Function GetSelectedFlowsheetSimulationObject(tag As String) As ISimulationObject

    Sub DisplayForm(form As Object)

    Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer)

    Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject)

    Function GetFlowsheetBag() As IFlowsheetBag

    Sub AddCompoundsToMaterialStream(stream As IMaterialStream)

    Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, tag As String) As ISimulationObject

End Interface

Public Interface IFlowsheetBag

    ReadOnly Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    ReadOnly Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    ReadOnly Property Compounds As Dictionary(Of String, ICompoundConstantProperties)

    ReadOnly Property PropertyPackages As Dictionary(Of String, IPropertyPackage)

    ReadOnly Property Reactions As Dictionary(Of String, IReaction)

    ReadOnly Property ReactionSets As Dictionary(Of String, IReactionSet)

    Sub SaveToXML(file As String)

    Sub UpdateProcessData(xdoc As XDocument)

End Interface

Public Interface IFlowsheetGUI

    Sub ShowMessage(ByVal text As String, ByVal mtype As IFlowsheet.MessageType)

    Sub ShowDebugInfo(ByVal text As String, ByVal level As Integer)

    Sub CheckStatus()

    Function GetTranslatedString(text As String, locale As String) As String

    Function GetTranslatedString(text As String) As String

End Interface

Public Interface IFlowsheetCalculationQueue

    Property CalculationQueue As Queue(Of ICalculationArgs)

End Interface

Public Interface ICalculationArgs

    Property Sender As String
    Property Calculated As Boolean
    Property Tag As String
    Property Name As String
    Property ObjectType As Enums.GraphicObjects.ObjectType

End Interface
