Public Interface ISimulationObject

    Property AttachedUtilities As List(Of IAttachedUtility)

    Sub DisplayEditForm()

    Sub UpdateEditForm()

    Property PreferredFlashAlgorithm As Enums.FlashMethod

    Property ErrorMessage() As String

    Function GetDebugReport() As String

    Sub AppendDebugLine(text As String)

    Property Calculated As Boolean

    Property DebugMode As Boolean

    Property DebugText As String

    <Xml.Serialization.XmlIgnore> Property CreatedWithThreadID As Integer

    <Xml.Serialization.XmlIgnore> Property LastUpdated As Date

    Sub CheckSpec(val As Double, onlypositive As Boolean, paramname As String)

    Function GetVersion() As Version

    Sub Validate()

    ''' <summary>
    ''' Get a list of all properties of the object.
    ''' </summary>
    ''' <param name="proptype">Type of the property.</param>
    ''' <returns>A list of property identifiers.</returns>
    ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetProperties(ByVal proptype As Enums.PropertyType) As String()

    ''' <summary>
    ''' Gets the value of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns>Property value.</returns>
    ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetPropertyValue(ByVal prop As String, Optional ByVal su As IUnitsOfMeasure = Nothing) As Object

    ''' <summary>
    ''' Gets the units of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns>Property units.</returns>
    ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
    Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As IUnitsOfMeasure = Nothing) As String

    ''' <summary>
    ''' Sets the value of a property.
    ''' </summary>
    ''' <param name="prop">Property identifier.</param>
    ''' <param name="propval">Property value to set at the specified units.</param>
    ''' <param name="su">Units system to use. Null to use the default (SI) system.</param>
    ''' <returns></returns>
    ''' <remarks>More details at http://dwsim.inforside.com.br/wiki/index.php?title=Object_Property_Codes </remarks>
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

    ''' <summary>
    ''' If an Specification object is attached to this object, returns a variable describing how this object is used by it (target or source).
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Property SpecVarType() As Enums.SpecVarType

    Property Annotation() As String

    Sub Calculate(Optional ByVal args As Object = Nothing)

    Sub DeCalculate(Optional ByVal args As Object = Nothing)

    Property PropertyPackage As IPropertyPackage

    Sub SetFlowsheet(fobj As Object)

    Function GetFlowsheet() As IFlowsheet

    Function GetIconBitmap() As Object

    Function GetDisplayName() As String

    Function GetDisplayDescription() As String

    Sub CloseEditForm()

End Interface
