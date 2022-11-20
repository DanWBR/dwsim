Imports System.Runtime.Serialization
Imports DWSIM.Interfaces

Public Class ObjectConnectionChangedArgs

    Inherits EventArgs

    Public Property FromObject As IGraphicObject

    Public Property ToObject As IGraphicObject

    Public Sub New(objfrom As IGraphicObject, objto As IGraphicObject)
        FromObject = objfrom
        ToObject = objto
    End Sub

End Class

Public Class SelectionChangedEventArgs

    Inherits EventArgs

    Private m_SelectedObject As IGraphicObject

    Public Sub New(ByVal selectedObject As IGraphicObject)
        m_SelectedObject = selectedObject
    End Sub

    Public ReadOnly Property SelectedObject() As IGraphicObject
        Get
            Return m_SelectedObject
        End Get
    End Property
End Class

Public Enum StatusUpdateType
    ObjectRotated = 0
    ObjectMoved = 1
    ObjectDeleted = 2
    SurfaceZoomChanged = 3
    FileLoaded = 4
    FileSaved = 5
    SelectionChanged = 6
    ObjectAdded = 7
    ObjectRenamed = 8
End Enum

Public Class StatusUpdateEventArgs
    Inherits EventArgs
    Private m_UpdateType As StatusUpdateType
    Private m_SelectedObject As IGraphicObject
    Private m_Message As String
    Private m_Coord As SKPoint
    Private m_Amount As Single

    Public Sub New(ByVal UpdateType As StatusUpdateType, _
            ByVal Selection As IGraphicObject, _
            ByVal StatusMessage As String, _
            ByVal Coord As SKPoint, ByVal Amt As Single)
        m_UpdateType = UpdateType
        m_SelectedObject = Selection
        m_Message = StatusMessage
        m_Coord = Coord
        m_Amount = Amt
    End Sub

    Public ReadOnly Property Type() As StatusUpdateType
        Get
            Return m_UpdateType
        End Get
    End Property
    Public ReadOnly Property SelectedObject() As IGraphicObject
        Get
            Return m_SelectedObject
        End Get
    End Property

    Public ReadOnly Property Message() As String
        Get
            Return m_Message
        End Get
    End Property

    Public ReadOnly Property Coordinates() As SKPoint
        Get
            Return m_Coord
        End Get
    End Property

    Public ReadOnly Property Amount() As Single
        Get
            Return m_Amount
        End Get
    End Property

End Class

