'    DWSIM Interface definitions
'    Copyright 2010-2021 Daniel Wagner O. de Medeiros
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

    Property DynamicMode As Boolean

    Property FileDatabaseProvider As IFileDatabaseProvider

    Property DynamicsManager As IDynamicsManager

    Property ExtraProperties As Dynamic.ExpandoObject

    Function GetApplicationObject() As Object

    Property AvailableCompounds As Dictionary(Of String, ICompoundConstantProperties)

    Property ExternalSolvers As Dictionary(Of String, IExternalSolverIdentification)

    Function SaveToXML() As XDocument

    Property FilePath As String

    Property RedirectMessages As Boolean

    ReadOnly Property UtilityPlugins As Dictionary(Of String, IUtilityPlugin)

    Property MasterUnitOp As ISimulationObject

    Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    Property PropertyPackages As Dictionary(Of String, IPropertyPackage)

    Property AvailablePropertyPackages As Dictionary(Of String, IPropertyPackage)

    Property AvailableSystemsOfUnits As List(Of IUnitsOfMeasure)

    Property Reactions As Dictionary(Of String, IReaction)

    Property ReactionSets As Dictionary(Of String, IReactionSet)

    Property SelectedCompounds As Dictionary(Of String, ICompoundConstantProperties)

    Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    Property StoredSolutions As Dictionary(Of String, List(Of XElement))

    Property Charts As Dictionary(Of String, IChart)

    Property WatchItems As List(Of IWatchItem)

    Sub ShowMessage(ByVal text As String, ByVal mtype As MessageType, Optional ByVal exceptionID As String = "")

    Sub ShowDebugInfo(ByVal text As String, ByVal level As Integer)

    Sub CheckStatus()

    Sub RequestCalculation(Optional ByVal sender As ISimulationObject = Nothing, Optional ByVal ChangeCalculationOrder As Boolean = False)

    Function GetTranslatedString(text As String, locale As String) As String

    Function GetTranslatedString(text As String) As String

    ReadOnly Property FlowsheetOptions As IFlowsheetOptions

    Function GetFlowsheetSimulationObject(tag As String) As ISimulationObject

    Function GetSelectedFlowsheetSimulationObject(tag As String) As ISimulationObject

    Function GetObject(name As String) As ISimulationObject

    Function GetCompound(name As String) As ICompoundConstantProperties

    Function GetPropertyPackage(name As String) As IPropertyPackage

    Function GetReaction(name As String) As IReaction

    Function GetReactionSet(name As String) As IReactionSet

    Sub AutoLayout()

    Sub DisplayForm(form As Object)

    Sub UpdateOpenEditForms()

    Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer)

    Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject)

    Function GetFlowsheetBag() As IFlowsheetBag

    Sub AddCompoundsToMaterialStream(stream As IMaterialStream)

    Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, tag As String) As ISimulationObject

    Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, id As String, tag As String) As ISimulationObject

    Sub AddGraphicObject(obj As IGraphicObject)

    Sub AddSimulationObject(obj As ISimulationObject)

    Function GetUtility(uttype As Enums.FlowsheetUtility) As IAttachedUtility

    Function GetSurface() As Object

    Function GetSurfaceControl() As Object

    Function GetNewInstance() As IFlowsheet

    Sub AddPropertyPackage(obj As IPropertyPackage)

    Property MasterFlowsheet As IFlowsheet

    Sub AddUndoRedoAction(action As IUndoRedoAction)

    Sub UpdateSpreadsheet(act As Action)

    Sub WriteSpreadsheetVariables(act As Action)

    Function GetSpreadsheetData(range As String) As List(Of String())

    Property MobileCompatibilityMode As Boolean

    Property Message As String

    Sub SetMessageListener(act As Action(Of String, MessageType))

    Property Solved As Boolean

    Property ErrorMessage As String

    Sub RunCodeOnUIThread(act As Action)

    Function GetDockPanel() As Object

    Sub LoadFromXML(xdoc As XDocument)

    Sub Initialize()

    Sub Reset()

    Sub DeleteSelectedObject(sender As Object, e As EventArgs, gobj As IGraphicObject, Optional confirmation As Boolean = True, Optional triggercalc As Boolean = False)

    Sub ProcessScripts(eventType As Enums.Scripts.EventType, objectType As Enums.Scripts.ObjectType, obj As String)

    Property Scripts As Dictionary(Of String, IScript)

    Sub UpdateInterface()

    Sub UpdateInformation()

    Sub RefreshInterface()

    Function GetFlowsheetSurfaceWidth() As Integer

    Function GetFlowsheetSurfaceHeight() As Integer

    Function ChangeCalculationOrder(objects As List(Of String)) As List(Of String)

    Function GetProcessData() As List(Of XElement)

    Sub LoadProcessData(data As List(Of XElement))

    Function GetSpreadsheetObject() As Object

    Sub SetTranslateTextExternalFunction(act As Func(Of String, String))

    Function GetScriptText(name As String) As String

    Sub RunScript(name As String)

    Function GetSimulationFilePath() As String

    Function GetSimulationFileDirectory() As String

    Sub ClearLog()

    Property PythonPreprocessor() As Action(Of String)

    Property SupressMessages As Boolean

    Sub RequestSave()

    Sub RequestSaveWithDirectory(directory As String)

    Sub RequestSaveWithPath(filepath As String)

    Sub ToggleFlowsheetAnimation()

    Function RunCodeOnUIThread2(act As Action) As Task

End Interface

''' <summary>
''' This is an interface which provides direct access to collections of flowsheet objects.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetBag

    Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    Property Compounds As Dictionary(Of String, ICompoundConstantProperties)

    Property PropertyPackages As Dictionary(Of String, IPropertyPackage)

    Property Reactions As Dictionary(Of String, IReaction)

    Property ReactionSets As Dictionary(Of String, IReactionSet)

    Sub SaveToXML(file As String)

    Sub UpdateProcessData(xdoc As XDocument)

    Function GetSurface() As Object

    'For COM compatibility

    Property SimulationObjectsArray As ISimulationObject()

    Property GraphicObjectsArray As IGraphicObject()

    Property CompoundsArray As ICompoundConstantProperties()

    Property PropertyPackagesArray As IPropertyPackage()

    Property ReactionsArray As IReaction()

    Property ReactionSetsArray As IReactionSet()

End Interface

''' <summary>
''' This is an interface which defines helper functions to a Flowsheet GUI implementation.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetGUI

    Sub ShowMessage(ByVal text As String, ByVal mtype As IFlowsheet.MessageType, Optional ByVal exceptionID As String = "")

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
