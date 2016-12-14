Namespace GraphicObjects

    <Serializable()> Public Class ConnectionPoint

        Implements Interfaces.IConnectionPoint

        Public Property AttachedConnector As Interfaces.IConnectorGraphicObject = Nothing Implements Interfaces.IConnectionPoint.AttachedConnector

        Public Property ConnectorName As String = "" Implements Interfaces.IConnectionPoint.ConnectorName

        Public Property Direction As Interfaces.Enums.GraphicObjects.ConDir = Interfaces.Enums.GraphicObjects.ConDir.Right Implements Interfaces.IConnectionPoint.Direction

        Private _IsAttached As Boolean = False

        Public Property IsAttached As Boolean Implements Interfaces.IConnectionPoint.IsAttached
            Get
                If AttachedConnector Is Nothing Then _IsAttached = False
                If Not AttachedConnector Is Nothing Then
                    If AttachedConnector.AttachedTo Is Nothing Then _IsAttached = False
                    If AttachedConnector.AttachedFrom Is Nothing Then _IsAttached = False
                End If
                Return _IsAttached
            End Get
            Set(value As Boolean)
                _IsAttached = value
            End Set
        End Property

        Public Property Type As Interfaces.Enums.GraphicObjects.ConType Implements Interfaces.IConnectionPoint.Type

        Public Property X As Integer = 0 Implements Interfaces.IConnectionPoint.X

        Public Property Y As Integer = 0 Implements Interfaces.IConnectionPoint.Y

        Public Property Position As Interfaces.IPoint = New DrawingTools.Point() Implements Interfaces.IConnectionPoint.Position

        Public Property Active As Boolean Implements Interfaces.IConnectionPoint.Active

        Public Property IsEnergyConnector As Boolean Implements Interfaces.IConnectionPoint.IsEnergyConnector
    End Class

End Namespace