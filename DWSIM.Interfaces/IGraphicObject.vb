Imports DWSIM.Interfaces.Enums.GraphicObjects

Public Interface IGraphicObject

    Sub Draw(surface As Object)

    Property Editor As Object

    Function Clone() As IGraphicObject

    Property ShapeOverride() As ShapeIcon

    Property Status() As Status

    Property Description() As String

    Property AdditionalInfo() As Object

    Property Shape() As Integer

    Property FlippedH() As Boolean

    Property FlippedV() As Boolean

    Property ObjectType() As ObjectType

    Property IsEnergyStream() As Boolean

    Property Active() As Boolean

    Property Tag() As String

    Property AutoSize() As Boolean

    Property IsConnector() As Boolean

    Property X() As Integer

    Property Y() As Integer

    Property Name() As String

    Property Height() As Integer

    Property Width() As Integer

    Property InputConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property OutputConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property SpecialConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property EnergyConnector() As IConnectionPoint

    Property Rotation As Integer

    Property Calculated As Boolean

    Property Position As IPoint

    Property Selected As Boolean

    Property Owner As ISimulationObject

    Function HitTest(zoomedSelection As Object) As Boolean

End Interface

Public Interface IConnectionPoint

    Property X() As Integer

    Property Y() As Integer

    Property Type() As ConType

    Property Direction() As ConDir

    Property AttachedConnector() As IConnectorGraphicObject

    Property IsAttached() As Boolean

    Property ConnectorName() As String

    Property Position As IPoint

    Property IsEnergyConnector As Boolean

    Property Active As Boolean

End Interface

Public Interface IConnectorGraphicObject

    Property AttachedFromConnectorIndex() As Integer

    Property AttachedToConnectorIndex() As Integer

    Property AttachedToEnergy() As Boolean

    Property AttachedFromEnergy() As Boolean

    Property AttachedFrom() As IGraphicObject

    Property AttachedTo() As IGraphicObject

    Property AttachedToOutput() As Boolean

    Property AttachedFromInput() As Boolean

End Interface
