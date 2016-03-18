'Copyright (C) 2002 Microsoft Corporation
'All rights reserved.
'
'THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
'EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF
'MERCHANTIBILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
'
'Date: June 2002
'Author: Duncan Mackenzie
'
'Requires the release version of .NET Framework

Imports DWSIM.DrawingTools.GraphicObjects
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Runtime.Serialization

Public Class SelectionChangedEventArgs
    Inherits EventArgs
    Private m_SelectedObject As GraphicObject

    Public Sub New(ByVal selectedObject As GraphicObject)
        m_SelectedObject = selectedObject
    End Sub

    Public ReadOnly Property SelectedObject() As GraphicObject
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
End Enum

Public Class StatusUpdateEventArgs
    Inherits EventArgs
    Private m_UpdateType As StatusUpdateType
    Private m_SelectedObject As GraphicObject
    Private m_Message As String
    Private m_Coord As Point
    Private m_Amount As Single

    Public Sub New(ByVal UpdateType As StatusUpdateType, _
            ByVal Selection As GraphicObject, _
            ByVal StatusMessage As String, _
            ByVal Coord As Point, ByVal Amt As Single)
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
    Public ReadOnly Property SelectedObject() As GraphicObject
        Get
            Return m_SelectedObject
        End Get
    End Property

    Public ReadOnly Property Message() As String
        Get
            Return m_Message
        End Get
    End Property

    Public ReadOnly Property Coordinates() As Point
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

