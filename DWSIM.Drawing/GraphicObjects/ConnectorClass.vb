Namespace GraphicObjects

    <Serializable()> Public Class ConnectionPoint

        Implements Interfaces.IConnectionPoint

        Public Property AttachedConnector As Interfaces.IConnectorGraphicObject = Nothing Implements Interfaces.IConnectionPoint.AttachedConnector

        Public Property ConnectorName As String = "" Implements Interfaces.IConnectionPoint.ConnectorName

        Public Property Direction As Interfaces.Enums.GraphicObjects.ConDir = Interfaces.Enums.GraphicObjects.ConDir.Right Implements Interfaces.IConnectionPoint.Direction

        Public Property IsAttached As Boolean = False Implements Interfaces.IConnectionPoint.IsAttached

        Public Property Type As Interfaces.Enums.GraphicObjects.ConType Implements Interfaces.IConnectionPoint.Type

        Public Property X As Integer = 0 Implements Interfaces.IConnectionPoint.X

        Public Property Y As Integer = 0 Implements Interfaces.IConnectionPoint.Y

        Public Property Position As Interfaces.IPoint = New DrawingTools.Point() Implements Interfaces.IConnectionPoint.Position

    End Class

End Namespace