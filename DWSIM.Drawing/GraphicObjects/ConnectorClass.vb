Imports System.Drawing

Namespace GraphicObjects

    Public Enum ConType

        ConIn = -1
        ConOut = 1
        ConEn = 0
        ConSp = 2

    End Enum
    Public Enum ConDir
        Up
        Down
        Right
        Left
    End Enum
    <Serializable()> Public Class ConnectionPoint

        Protected mPosition As Point
        Protected mType As ConType
        Protected mDir As ConDir = ConDir.Right
        Protected mIsAttached As Boolean = False
        Protected mAttachedConnector As ConnectorGraphic = Nothing
        Protected mConnectorName As String = ""

        Public Overridable Property Position() As Point

            Get
                Return mPosition
            End Get
            Set(ByVal pt As Point)
                mPosition = pt
            End Set

        End Property

        Public Overridable Property Type() As ConType

            Get
                Return mType
            End Get
            Set(ByVal ConnectionType As ConType)
                mType = ConnectionType
            End Set

        End Property
        Public Overridable Property Direction() As ConDir

            Get
                Return mDir
            End Get
            Set(ByVal ConnectionDirection As ConDir)
                mDir = ConnectionDirection
            End Set

        End Property
        Public Overridable Property AttachedConnector() As ConnectorGraphic

            Get
                Return mAttachedConnector
            End Get
            Set(ByVal connector As ConnectorGraphic)
                mAttachedConnector = connector
            End Set

        End Property

        Public Overridable Property IsAttached() As Boolean

            Get
                Return mIsAttached
            End Get
            Set(ByVal Value As Boolean)
                mIsAttached = Value
            End Set

        End Property

        Public Overridable Property ConnectorName() As String

            Get
                Return mConnectorName
            End Get
            Set(ByVal ConnName As String)
                mConnectorName = ConnName
            End Set

        End Property

    End Class

End Namespace