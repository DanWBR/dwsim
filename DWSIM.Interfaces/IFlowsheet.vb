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

    Property RedirectMessages As Boolean

    ReadOnly Property UtilityPlugins As Dictionary(Of String, IUtilityPlugin)

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

    Sub UpdateOpenEditForms()

    Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer)

    Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject)

    Function GetFlowsheetBag() As IFlowsheetBag

    Sub AddCompoundsToMaterialStream(stream As IMaterialStream)

    Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, tag As String) As ISimulationObject

    Sub AddGraphicObject(obj As IGraphicObject)

    Sub AddSimulationObject(obj As ISimulationObject)

    Function GetUtility(uttype As Enums.FlowsheetUtility) As IAttachedUtility

    Function GetSurface() As Object

    Function GetNewInstance() As IFlowsheet

    Sub AddPropertyPackage(obj As IPropertyPackage)

    Property MasterFlowsheet As IFlowsheet

    Sub AddUndoRedoAction(action As IUndoRedoAction)

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

    Function GetSurface() As Object

End Interface

Public Interface IFlowsheetGUI

    Sub ShowMessage(ByVal text As String, ByVal mtype As IFlowsheet.MessageType)

    Sub ShowDebugInfo(ByVal text As String, ByVal level As Integer)

    Sub CheckStatus()

    Function GetTranslatedString(text As String, locale As String) As String

    Function GetTranslatedString(text As String) As String

    Sub ProcessScripts(eventType As Enums.Scripts.EventType, objectType As Enums.Scripts.ObjectType, obj As String)

    Sub UpdateInterface()

    Sub UpdateInformation()

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
