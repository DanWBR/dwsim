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

Imports System.Dynamic
Imports DWSIM.Interfaces.Enums
''' <summary>
''' This interface defines the basic properties of Simulation Objects (Unit Operations, Material Streams and Energy Streams)
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ISimulationObject

    Property DynamicsSpec As Enums.Dynamics.DynamicsSpecType

    Property DynamicsOnly As Boolean

    ReadOnly Property SupportsDynamicMode As Boolean

    ReadOnly Property HasPropertiesForDynamicMode As Boolean

    Property ExtraProperties As Dynamic.ExpandoObject

    Property ExtraPropertiesDescriptions As ExpandoObject

    Property ExtraPropertiesUnitTypes As ExpandoObject

    Property ExtraPropertiesTypes As ExpandoObject

    Property AttachedUtilities As List(Of IAttachedUtility)

    Sub CreateDynamicProperties()

    Sub DisplayEditForm()

    Sub DisplayDynamicsEditForm()

    Sub DisplayExtraPropertiesEditForm()

    Sub UpdateEditForm()

    Sub UpdateDynamicsEditForm()

    Sub UpdateExtraPropertiesEditForm()

    Property PreferredFlashAlgorithmTag As String

    Property ErrorMessage() As String

    Function GetDebugReport() As String

    Sub AppendDebugLine(text As String)

    Property Calculated As Boolean

    Property DebugMode As Boolean

    Property DebugText As String

    <Xml.Serialization.XmlIgnore> Property LastUpdated As Date

    Sub CheckSpec(val As Double, onlypositive As Boolean, paramname As String)

    Function GetVersion() As Version

    ReadOnly Property MobileCompatible As Boolean

    Property ObjectClass As Enums.SimulationObjectClass

    Sub Validate()

    ''' <summary>
    ''' Get a list of all properties of the object.
    ''' </summary>
    ''' <param name="proptype">Type of the property.</param>
    ''' <returns>A list of property identifiers.</returns>
    ''' <remarks>More details at https://dwsim.org/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetProperties(ByVal proptype As Enums.PropertyType) As String()
    Sub AddExtraProperty(pname As String, pvalue As Object)

    ''' <summary>
    ''' Gets the value of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns>Property value.</returns>
    ''' <remarks>More details at https://dwsim.org/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetPropertyValue(ByVal prop As String, Optional ByVal su As IUnitsOfMeasure = Nothing) As Object
    Sub RemoveExtraProperty(pname As String)
    Sub ClearExtraProperties()
    Sub AddDynamicProperty(pname As String, pdesc As String, pvalue As Double, punittype As UnitOfMeasure, ptype As System.Type)

    ''' <summary>
    ''' Gets the units of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns>Property units.</returns>
    ''' <remarks>More details at https://dwsim.org/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As IUnitsOfMeasure = Nothing) As String
    Sub SetExtraPropertyValue(pname As String, pvalue As Object)
    Sub RemoveDynamicProperty(pname As String)
    Function GetExtraPropertyValue(pname As String) As Object

    ''' <summary>
    ''' Sets the value of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="propval">Property value to set at the specified units.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns></returns>
    ''' <remarks>More details at https://dwsim.org/wiki/index.php?title=Object_Property_Codes </remarks>
    Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As IUnitsOfMeasure = Nothing) As Boolean

    Function GetDefaultProperties() As String()

    Property Name() As String

    Property GraphicObject() As IGraphicObject
    ''' <summary>
    ''' Checks if an Adjust operation is attached to this object.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property IsAdjustAttached() As Boolean

    ''' <summary>
    ''' If an Adjust object is attached to this object, returns its ID.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property AttachedAdjustId() As String

    ''' <summary>
    ''' If an Adjust object is attached to this object, returns a variable describing how this object is used by it (manipulated, controlled or reference).
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property AdjustVarType() As Enums.AdjustVarType

    ''' <summary>
    ''' Checks if an Specification operation is attached to this object.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property IsSpecAttached() As Boolean

    ''' <summary>
    ''' If an Specification object is attached to this object, returns its ID.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property AttachedSpecId() As String
    Function AddDynamicProperty(id As String, value As Object) As Boolean

    ''' <summary>
    ''' If an Specification object is attached to this object, returns a variable describing how this object is used by it (target or source).
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property SpecVarType() As Enums.SpecVarType

    Property Annotation() As String

    Sub RunDynamicModel()

    Sub Calculate(Optional ByVal args As Object = Nothing)

    Function GetDynamicProperty(id As String) As Object

    Sub DeCalculate(Optional ByVal args As Object = Nothing)

    Sub Solve()

    Sub PerformPostCalcValidation()

    Property PropertyPackage As IPropertyPackage

    Sub SetPropertyPackageInstance(PP As IPropertyPackage)

    Function ClearPropertyPackageInstance() As Boolean

    ReadOnly Property IsFunctional As Boolean

    Function GetDynamicPropertyUnitType(id As String) As UnitOfMeasure

    Sub SetFlowsheet(fobj As Object)

    Function GetFlowsheet() As IFlowsheet

    Function GetIconBitmap() As Object

    Function GetDisplayName() As String

    Function GetDisplayDescription() As String

    Sub CloseDynamicsEditForm()

    Sub CloseEditForm()

    Function CloneXML() As Object

    Function CloneJSON() As Object

    Function GetPropertyDescription(prop As String) As String

    Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

    Function GetStructuredReport() As List(Of Tuple(Of Enums.ReportItemType, String()))

    Function GetChartModel(name As String) As Object

    Function GetChartModelNames() As List(Of String)

    Function IsDynamicProperty(pname As String) As Boolean

    Function GetEnergyBalanceResidual() As Double

    Function GetMassBalanceResidual() As Double

    Function GetPowerGeneratedOrConsumed() As Double

    Function GetDynamicResidenceTime() As Double

    Function GetDynamicVolume() As Double

    Function GetDynamicContents() As Double

    Function GetAsObject() As Object

    ReadOnly Property IsSource As Boolean

    ReadOnly Property IsSink As Boolean

    Sub ConnectEnergyStream(stream As ISimulationObject)

    Sub ConnectProductEnergyStream(stream As ISimulationObject, portnumber As Integer)

    Sub ConnectFeedEnergyStream(stream As ISimulationObject, portnumber As Integer)

    Sub ConnectProductMaterialStream(stream As ISimulationObject, portnumber As Integer)

    Sub ConnectFeedMaterialStream(stream As ISimulationObject, portnumber As Integer)

    Function GetConnectionPortsList() As List(Of String)

    Function GetConnectionPortsInfo() As List(Of IConnectionPortInfo)

    ReadOnly Property IsDirty As Boolean

    ReadOnly Property CanUsePreviousResults As Boolean

    Sub SetDirtyStatus(value As Boolean)

    Sub SetCanUsePreviousResults(value As Boolean)

    Sub CheckDirtyStatus()

    Function GetEditingForm() As System.Windows.Forms.Form

    Function GetPreferredGraphicObjectWidth() As Double

    Function GetPreferredGraphicObjectHeight() As Double

    Property GHGEmissionData As IGHGEmitter

    Function GetEnergyConsumption() As Double

End Interface
