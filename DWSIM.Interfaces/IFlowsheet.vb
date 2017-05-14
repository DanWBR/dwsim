'    DWSIM Interface definitions
'    Copyright 2010-2017 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

''' <summary>
''' The IFlowsheet interface is the main interface which should be implemented by the Flowsheet class. 
''' It provides direct access to the various flowsheet components and helper functions to manipulate objects.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheet

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

    Property MasterUnitOp As ISimulationObject

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

    Sub UpdateSpreadsheet()

    Sub WriteSpreadsheetVariables()

    Property MobileCompatibilityMode As Boolean

    Property Message As String

    Sub SetMessageListener(act As Action(Of String))

    Property Solved As Boolean

    Property ErrorMessage As String

    Sub RunCodeOnUIThread(act As Action)

End Interface

''' <summary>
''' This is an interface which provides direct access to collections of flowsheet objects.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetBag

    ReadOnly Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    ReadOnly Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    ReadOnly Property Compounds As Dictionary(Of String, ICompoundConstantProperties)

    ReadOnly Property PropertyPackages As Dictionary(Of String, IPropertyPackage)

    ReadOnly Property Reactions As Dictionary(Of String, IReaction)

    ReadOnly Property ReactionSets As Dictionary(Of String, IReactionSet)

    Sub SaveToXML(file As String)

    Sub UpdateProcessData(xdoc As XDocument)

    Function GetSurface() As Object

    'For COM compatibility

    ReadOnly Property SimulationObjectsArray As ISimulationObject()

    ReadOnly Property GraphicObjectsArray As IGraphicObject()

    ReadOnly Property CompoundsArray As ICompoundConstantProperties()

    ReadOnly Property PropertyPackagesArray As IPropertyPackage()

    ReadOnly Property ReactionsArray As IReaction()

    ReadOnly Property ReactionSetsArray As IReactionSet()

End Interface

''' <summary>
''' This is an interface which defines helper functions to a Flowsheet GUI implementation.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetGUI

    Sub ShowMessage(ByVal text As String, ByVal mtype As IFlowsheet.MessageType)

    Sub ShowDebugInfo(ByVal text As String, ByVal level As Integer)

    Sub CheckStatus()

    Function GetTranslatedString(text As String, locale As String) As String

    Function GetTranslatedString(text As String) As String

    Sub ProcessScripts(eventType As Enums.Scripts.EventType, objectType As Enums.Scripts.ObjectType, obj As String)

    Sub UpdateInterface()

    Sub UpdateInformation()


End Interface

''' <summary>
''' This interface defines the calculation queue to be used by the flowsheet solver.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetCalculationQueue

    Property CalculationQueue As Queue(Of ICalculationArgs)

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ICalculationArgs

    Property Sender As String
    Property Calculated As Boolean
    Property Tag As String
    Property Name As String
    Property ObjectType As Enums.GraphicObjects.ObjectType

End Interface
