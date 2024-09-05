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

    Sub NaturalLayout()

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

    Function GetSpreadsheetFormat(range As String) As List(Of String())

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

    Property WeatherProvider As IWeatherProvider

    ''' <summary>
    ''' Creates a Conversion reaction object.
    ''' </summary>
    ''' <param name="name">Name/ID of the reaction</param>
    ''' <param name="description">Description of the reaction</param>
    ''' <param name="compounds_and_stoichcoeffs">Compound names and stoichiometric coefficients</param>
    ''' <param name="basecompound">Name of the base reaction compound</param>
    ''' <param name="reactionphase">Reaction phase ('Mixture', 'Vapor', 'Liquid' or 'Solid')</param>
    ''' <param name="conversionExpression">Expression for evaluating conversion of the base compound as a function of temperature in K.</param>
    Function CreateConversionReaction(name As String, description As String,
                                      compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                      basecompound As String, reactionphase As String,
                                      conversionExpression As String) As IReaction

    ''' <summary>
    ''' Creates an Equilibrium reaction object.
    ''' </summary>
    ''' <param name="name">Name/ID of the reaction</param>
    ''' <param name="description">Description of the reaction</param>
    ''' <param name="compounds_and_stoichcoeffs">Compound names and stoichiometric coefficients</param>
    ''' <param name="basecompound">Name of the base reaction compound</param>
    ''' <param name="reactionphase">Reaction phase ('Mixture', 'Vapor', 'Liquid' or 'Solid')</param>
    ''' <param name="basis">Reaction basis ('Activity', 'Fugacity', 'Molar Concentration', 'Molar Fraction', 'Mass Concentration', 'Mass Fraction' or 'Partial Pressure')</param>
    ''' <param name="lnKeq_fT">Expression for evaulating the equilibrium constant as a function of temperature in K. Will use gibbs energy of formation if empty.</param>
    ''' <param name="Tapproach">Temperature value that will be added or removed to the system temperature in order to evaluate the equilibrium constant</param>
    ''' <param name="units">Basis units. See the list of <see cref="SharedClasses.SystemsOfUnits.Units.GetUnitSet()">supported units</see></param>
    Function CreateEquilibriumReaction(name As String, description As String,
                                      compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                      basecompound As String, reactionphase As String,
                                      basis As String, units As String, Tapproach As Double, lnKeq_fT As String) As IReaction

    ''' <summary>
    ''' Creates a Kinetic reaction object.
    ''' </summary>
    ''' <param name="name">Name/ID of the reaction</param>
    ''' <param name="description">Description of the reaction</param>
    ''' <param name="compounds_and_stoichcoeffs">Compound names and stoichiometric coefficients</param>
    ''' <param name="directorders">Compound names and direct order coefficients</param>
    ''' <param name="reverseorders">Compound names and reverse order coefficients</param>
    ''' <param name="basecompound">Name of the base reaction compound</param>
    ''' <param name="reactionphase">Reaction phase ('Mixture', 'Vapor', 'Liquid' or 'Solid')</param>
    ''' <param name="basis">Reaction basis ('Activity', 'Fugacity', 'Molar Concentration', 'Molar Fraction', 'Mass Concentration', 'Mass Fraction' or 'Partial Pressure')</param>
    ''' <param name="amountunits">Amount units. See the list of <see cref="SharedClasses.SystemsOfUnits.Units.GetUnitSet()">supported units</see></param>
    ''' <param name="rateunits">Rate units. See the list of <see cref="SharedClasses.SystemsOfUnits.Units.GetUnitSet()">supported units</see</param>
    ''' <param name="Aforward">Arrhenius forward reaction's A parameter. Ignored if Expr_forward is not empty.</param>
    ''' <param name="Eforward">Arrhenius forward reaction's E parameter. Ignored if Expr_forward is not empty.</param>
    ''' <param name="Areverse">Arrhenius reverse reaction's A parameter. Ignored if Expr_reverse is not empty.</param>
    ''' <param name="Ereverse">Arrhenius reverse reaction's E parameter. Ignored if Expr_reverse is not empty.</param>
    ''' <param name="Expr_forward">User-defined expression for forward reaction kinetics as a function of temperature in K. Will use Arrhenius if empty.</param>
    ''' <param name="Expr_reverse">User-defined expression for reverse reaction kinetics as a function of temperature in K. Will use Arrhenius if empty.</param>
    ''' <returns></returns>
    Function CreateKineticReaction(name As String, description As String,
                                    compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                    directorders As Dictionary(Of String, Double),
                                    reverseorders As Dictionary(Of String, Double),
                                    basecompound As String, reactionphase As String,
                                    basis As String, amountunits As String, rateunits As String,
                                    Aforward As Double, Eforward As Double, Areverse As Double, Ereverse As Double,
                                    Expr_forward As String, Expr_reverse As String) As IReaction

    ''' <summary>
    ''' Creates a Heterogeneous Catalytic reaction object.
    ''' </summary>
    ''' <param name="name">Name/ID of the reaction</param>
    ''' <param name="description">Description of the reaction</param>
    ''' <param name="compounds_and_stoichcoeffs">Compound names and stoichiometric coefficients</param>
    ''' <param name="basecompound">Name of the base reaction compound</param>
    ''' <param name="reactionphase">Reaction phase ('Mixture', 'Vapor', 'Liquid' or 'Solid')</param>
    ''' <param name="basis">Reaction basis ('Activity', 'Fugacity', 'Molar Concentration', 'Molar Fraction', 'Mass Concentration', 'Mass Fraction' or 'Partial Pressure')</param>
    ''' <param name="amountunits">Amount units. See the list of <see cref="SharedClasses.SystemsOfUnits.Units.GetUnitSet()">supported units</see></param>
    ''' <param name="rateunits">Rate units. See the list of <see cref="SharedClasses.SystemsOfUnits.Units.GetUnitSet()">supported units</see></param>
    ''' <param name="numeratorExpression">Numerator expression</param>
    ''' <param name="denominatorExpression">Denominator expression</param>
    ''' <returns>Reaction object</returns>
    Function CreateHetCatReaction(name As String, description As String,
                                    compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                    basecompound As String, reactionphase As String,
                                    basis As String, amountunits As String, rateunits As String,
                                    numeratorExpression As String, denominatorExpression As String) As IReaction

    ''' <summary>
    ''' Creates a Reaction Set object.
    ''' </summary>
    ''' <param name="name">Name/ID of the reaction set</param>
    ''' <param name="description">Description</param>
    ''' <returns></returns>
    Function CreateReactionSet(name As String, description As String) As IReactionSet

    ''' <summary>
    ''' Add a reaction to the flowsheet.
    ''' </summary>
    ''' <param name="reactionSet">reaction object</param>
    Sub AddReaction(reaction As IReaction)

    ''' <summary>
    ''' Add a reaction set to the flowsheet.
    ''' </summary>
    ''' <param name="reaction">reaction set object</param>
    Sub AddReactionSet(reactionSet As IReactionSet)

    ''' <summary>
    ''' Adds a reaction to a reaction set.
    ''' </summary>
    ''' <param name="reactionID">reaction ID</param>
    ''' <param name="reactionSetID">reaction set ID</param>
    ''' <param name="enabled">True if the reaction is to be active in the reaction set</param>
    ''' <param name="rank">Rank  (0, 1, 2, 3...) of the reaction in the reaction set</param>
    Sub AddReactionToSet(reactionID As String, reactionSetID As String, enabled As Boolean, rank As Integer)

    ''' <summary>
    ''' Gets a list of all available Property Packages.
    ''' </summary>
    ''' <returns>A list containing the names of the available property packages.</returns>
    Function GetAvailablePropertyPackages() As List(Of String)

    ''' <summary>
    ''' Creates and returns a new Property Package.
    ''' </summary>
    ''' <param name="name">Name of the Property Package as returned by <see cref="GetAvailablePropertyPackages">GetAvailablePropertyPackages</see></param>
    ''' <returns></returns>
    Function CreatePropertyPackage(name As String) As IPropertyPackage

    ''' <summary>
    ''' Creates and adds a Property Package to the flowsheet.
    ''' </summary>
    ''' <param name="name">Name of the Property Package as returned by <see cref="GetAvailablePropertyPackages">GetAvailablePropertyPackages</see>.</param>
    ''' <returns></returns>
    Function CreateAndAddPropertyPackage(name As String) As IPropertyPackage

    ''' <summary>
    ''' Adds a compound to the flowsheet.
    ''' </summary>
    ''' <param name="compname">Compound name exactly as in the database.</param>
    ''' <returns></returns>
    Function AddCompound(compname As String) As ICompoundConstantProperties

    Sub SetDirtyStatus()

    Function GetAvailableFlowsheetObjectTypeNames() As Array

    Function AddFlowsheetObject(typename As String, objname As String) As ISimulationObject

    Sub DisplayBrowserWindow(url As String)

    Sub DisplayDockableBrowserWindow(url As String)

    Sub RequestCalculation2(Wait As Boolean)

    Sub RequestCalculation3(obj As ISimulationObject, Wait As Boolean)

    Property MessagesLog As List(Of String)

    Property AvailableExternalUnitOperations As Dictionary(Of String, IExternalUnitOperation)

    Function GetSnapshot(type As Enums.SnapshotType, Optional obj As ISimulationObject = Nothing) As XDocument

    Sub RestoreSnapshot(data As XDocument, type As Enums.SnapshotType)

    Sub RegisterSnapshot(stype As Enums.SnapshotType, Optional obj As ISimulationObject = Nothing)

    Sub ResetCalculationStatus()

    Sub CloseOpenEditForms()

    Function Clone() As IFlowsheet

    Property Results As IFlowsheetResults

    Property GHGEmissionCompositions As Dictionary(Of String, IGHGComposition)

    Function GetResultIDs() As List(Of String)

    Function GetResultValue(id As String) As Double

    Function GetResultUnits(id As String) As String

    Property ParticleSizeDistributions As List(Of ISolidParticleSizeDistribution)

    Sub ReleaseResources()

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
