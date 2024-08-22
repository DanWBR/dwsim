Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports s = DWSIM.GlobalSettings.Settings
Imports System.Windows.Forms

Namespace GraphicObjects

    Public Class ConnectionPoint

        Implements IConnectionPoint

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

        Public Property Position As IPoint Implements IConnectionPoint.Position

        Public Property Active As Boolean = True Implements IConnectionPoint.Active

        Public Property IsEnergyConnector As Boolean = False Implements IConnectionPoint.IsEnergyConnector

    End Class

    Public Class ConnectorGraphic

        Inherits ShapeGraphic

        Implements Interfaces.IConnectorGraphicObject

        <Xml.Serialization.XmlIgnore> Public Property PointList As New List(Of Point)

        Public Property Straight As Boolean = False Implements IConnectorGraphicObject.Straight

        Public Property ConnectorPath As SKPath

        Public Property ConnectorPathExpanded As SKPath

        Public Shared Property ExpandFactor As Integer = 20

#Region "Constructors"

        Public Sub New()

        End Sub

        Public Sub New(ByVal startPosition As Point)
            Me.New()
            Me.SetStartPosition(startPosition)
            Me.IsConnector = True
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New Point(posX, posY))
            Me.IsConnector = True
        End Sub

        Public Sub New(ByVal startPosition As Point, ByVal endPosition As Point)
            Me.New(startPosition)
            Me.SetEndPosition(endPosition)
            Me.AutoSize = False
            Me.IsConnector = True
        End Sub

        Public Sub New(ByVal startX As Integer, ByVal startY As Integer, ByVal endPosition As Point)
            Me.New(New Point(startX, startY), endPosition)
            Me.IsConnector = True
        End Sub

        Public Sub New(ByVal startX As Integer, ByVal startY As Integer, ByVal endX As Integer, ByVal endY As Integer)
            Me.New(New Point(startX, startY), New Point(endX, endY))
            Me.IsConnector = True
        End Sub

#End Region

        Public Sub UpdateStatus2(ByRef ConnPen As SKPaint, ByVal Conn As ConnectorGraphic)

            ConnPen.Color = GraphicsSurface.ForegroundColor

            If Conn.AttachedFrom.Status = Enums.GraphicObjects.Status.Calculated And Conn.AttachedTo.Status = Enums.GraphicObjects.Status.Calculated Then
                If s.DarkMode Then
                    ConnPen.Color = SKColors.WhiteSmoke
                Else
                    ConnPen.Color = SKColors.SteelBlue
                End If
            Else
                ConnPen.Color = SKColors.Salmon
            End If

        End Sub

        Public Function GetStartPosition() As Point
            Return Me.Position
        End Function

        Public Sub SetStartPosition(ByVal Value As Point)
            'Me.SetPosition(Value.ToSKPoint)
        End Sub

        Public Function GetEndPosition() As Point
            Dim endPosition As New Point(Me.X, Me.Y)
            endPosition.X += Me.Width
            endPosition.Y += Me.Height
            Return endPosition
        End Function

        Public Sub SetEndPosition(ByVal Value As Point)
            Width = Value.X - Me.X
            Height = Value.Y - Me.Y
        End Sub

        Public Sub SetupPositioning()

            'posicionar pontos nos primeiros slots livres

            PointList = New List(Of Point)

            Dim StartPos, EndPos As New Point

            Dim StartDir, EndDir As ConDir

            If Me.AttachedFrom.ObjectType = ObjectType.EnergyStream Then
                If Me.AttachedTo.ObjectType = ObjectType.ShortcutColumn Or
                Me.AttachedTo.ObjectType = ObjectType.OT_EnergyRecycle Or
                Me.AttachedTo.ObjectType = ObjectType.Vessel Then
                    StartPos = Me.AttachedFrom.OutputConnectors(0).Position
                    EndPos = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Position
                    StartDir = Me.AttachedFrom.OutputConnectors(0).Direction
                    EndDir = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Direction
                Else
                    StartPos = Me.AttachedFrom.OutputConnectors(0).Position
                    StartDir = Me.AttachedFrom.OutputConnectors(0).Direction
                    If AttachedToOutput Then
                        EndPos = Me.AttachedTo.OutputConnectors(Me.AttachedToConnectorIndex).Position
                        EndDir = Me.AttachedTo.OutputConnectors(Me.AttachedToConnectorIndex).Direction
                    Else
                        If Me.AttachedTo.EnergyConnector.Active Then
                            EndPos = Me.AttachedTo.EnergyConnector.Position
                            EndDir = Me.AttachedTo.EnergyConnector.Direction
                        Else
                            EndPos = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Position
                            EndDir = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Direction
                        End If
                    End If
                End If
            ElseIf Me.AttachedFromConnectorIndex = -1 Then
                StartPos = Me.AttachedFrom.EnergyConnector.Position
                EndPos = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Position
                StartDir = Me.AttachedFrom.EnergyConnector.Direction
                EndDir = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Direction
            Else
                If AttachedFromInput Then
                    StartPos = Me.AttachedFrom.InputConnectors(Me.AttachedFromConnectorIndex).Position
                    StartDir = Me.AttachedFrom.InputConnectors(Me.AttachedFromConnectorIndex).Direction
                Else
                    StartPos = Me.AttachedFrom.OutputConnectors(Me.AttachedFromConnectorIndex).Position
                    StartDir = Me.AttachedFrom.OutputConnectors(Me.AttachedFromConnectorIndex).Direction
                End If
                If AttachedToOutput Then
                    EndPos = Me.AttachedTo.OutputConnectors(Me.AttachedToConnectorIndex).Position
                    EndDir = Me.AttachedTo.OutputConnectors(Me.AttachedToConnectorIndex).Direction
                Else
                    EndPos = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Position
                    EndDir = Me.AttachedTo.InputConnectors(Me.AttachedToConnectorIndex).Direction
                End If
            End If

            Dim DeltaX, DeltaY As Integer
            DeltaX = 10
            DeltaY = 10

            Dim XM, YM As Double

            Dim LeftTop1, RightBottom1, LeftTop2, RightBottom2 As New Point
            LeftTop1.X = Me.AttachedFrom.X
            LeftTop1.Y = Me.AttachedFrom.Y
            RightBottom1.X = Me.AttachedFrom.X + Me.AttachedFrom.Width
            RightBottom1.Y = Me.AttachedFrom.Y + Me.AttachedFrom.Height
            LeftTop2.X = Me.AttachedTo.X
            LeftTop2.Y = Me.AttachedTo.Y
            RightBottom2.X = Me.AttachedTo.X + Me.AttachedTo.Width
            RightBottom2.Y = Me.AttachedTo.Y + Me.AttachedTo.Height


            'Check Rotation

            If Me.AttachedFrom.Rotation >= 90 And Me.AttachedFrom.Rotation < 180 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Left
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Down
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Right
                End If
            ElseIf Me.AttachedFrom.Rotation >= 180 And Me.AttachedFrom.Rotation < 270 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Right
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Left
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Down
                End If
            ElseIf Me.AttachedFrom.Rotation >= 270 And Me.AttachedFrom.Rotation < 360 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Down
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Right
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Left
                End If
            End If

            If Me.AttachedTo.Rotation >= 90 And Me.AttachedTo.Rotation < 180 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Left
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Down
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Right
                End If
            ElseIf Me.AttachedTo.Rotation >= 180 And Me.AttachedTo.Rotation < 270 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Right
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Left
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Down
                End If
            ElseIf Me.AttachedTo.Rotation >= 270 And Me.AttachedTo.Rotation < 360 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Down
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Right
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Left
                End If
            End If

            'Apply Rotation

            If Me.AttachedFrom.Rotation <> 0 Then
                Dim angle_rad As Double = Me.AttachedFrom.Rotation * Math.PI / 180
                Dim center As New Point(Me.AttachedFrom.X + Me.AttachedFrom.Width / 2, Me.AttachedFrom.Y + Me.AttachedFrom.Height / 2)
                Dim x As Double = StartPos.X - center.X
                Dim y As Double = StartPos.Y - center.Y
                StartPos.X = center.X + (x * Math.Cos(angle_rad) + y * Math.Sin(angle_rad))
                StartPos.Y = center.Y - (x * -Math.Sin(angle_rad) + y * Math.Cos(angle_rad))
            End If
            If Me.AttachedTo.Rotation <> 0 Then
                Dim angle_rad As Double = Me.AttachedTo.Rotation * Math.PI / 180
                Dim center As New Point(Me.AttachedTo.X + Me.AttachedTo.Width / 2, Me.AttachedTo.Y + Me.AttachedTo.Height / 2)
                Dim x As Double = EndPos.X - center.X
                Dim y As Double = EndPos.Y - center.Y
                EndPos.X = center.X + (x * Math.Cos(angle_rad) + y * Math.Sin(angle_rad))
                EndPos.Y = center.Y - (x * -Math.Sin(angle_rad) + y * Math.Cos(angle_rad))
            End If

            'Check Flipping

            If Me.AttachedFrom.FlippedV Then
                Dim center As New Point(Me.AttachedFrom.X + Me.AttachedFrom.Width / 2, Me.AttachedFrom.Y + Me.AttachedFrom.Height / 2)
                Dim y As Double = StartPos.Y - center.Y
                StartPos.Y = center.Y - y
                If StartDir = ConDir.Down Then StartDir = ConDir.Up
                If StartDir = ConDir.Up Then StartDir = ConDir.Down
            End If
            If Me.AttachedFrom.FlippedH Then
                Dim center As New Point(Me.AttachedFrom.X + Me.AttachedFrom.Width / 2, Me.AttachedFrom.Y + Me.AttachedFrom.Height / 2)
                Dim x As Double = StartPos.X - center.X
                StartPos.X = center.X - x
                If StartDir = ConDir.Left Then StartDir = ConDir.Right
                If StartDir = ConDir.Right Then StartDir = ConDir.Left
            End If
            If Me.AttachedTo.FlippedV Then
                Dim center As New Point(Me.AttachedTo.X + Me.AttachedTo.Width / 2, Me.AttachedTo.Y + Me.AttachedTo.Height / 2)
                Dim y As Double = EndPos.Y - center.Y
                EndPos.Y = center.Y - y
                If EndDir = ConDir.Down Then EndDir = ConDir.Up
                If EndDir = ConDir.Up Then EndDir = ConDir.Down
            End If
            If Me.AttachedTo.FlippedH Then
                Dim center As New Point(Me.AttachedTo.X + Me.AttachedTo.Width / 2, Me.AttachedTo.Y + Me.AttachedTo.Height / 2)
                Dim x As Double = EndPos.X - center.X
                EndPos.X = center.X - x
                If EndDir = ConDir.Left Then EndDir = ConDir.Right
                If EndDir = ConDir.Right Then EndDir = ConDir.Left
            End If

            'Construct path of stream
            PointList.Add(New Point(StartPos.X, StartPos.Y))

            '================== EndDir Right =======================

            If StartDir = ConDir.Down And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    If EndPos.Y >= StartPos.Y + DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))

                        XM = (RightBottom1.X + LeftTop2.X) / 2
                        If XM < RightBottom1.X + DeltaX Then XM = LeftTop1.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X - DeltaX
                    If XM > LeftTop1.X - DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = LeftTop1.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < StartPos.Y + DeltaY Then YM = StartPos.Y + DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            ElseIf StartDir = ConDir.Up And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    If EndPos.Y <= StartPos.Y - DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))

                        XM = (RightBottom1.X + LeftTop2.X) / 2
                        If XM < RightBottom1.X + DeltaX Then XM = LeftTop1.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X - DeltaX
                    If XM > LeftTop1.X - DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = LeftTop1.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                    If YM > StartPos.Y - DeltaY Then YM = StartPos.Y - DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            ElseIf StartDir = ConDir.Right And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) >= (StartPos.X + DeltaX) Then
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, StartPos.Y))
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, EndPos.Y))
                Else
                    PointList.Add(New Point((StartPos.X + DeltaX), StartPos.Y))

                    XM = EndPos.X - DeltaX

                    YM = (LeftTop2.Y + RightBottom1.Y) / 2
                    If RightBottom2.Y + DeltaY > LeftTop1.Y - DeltaY Then YM = RightBottom1.Y + DeltaY
                    If YM < RightBottom2.Y + DeltaY And YM > LeftTop2.Y - DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < (RightBottom1.Y + LeftTop2.Y) / 2 Then YM = (RightBottom1.Y + LeftTop2.Y) / 2

                    PointList.Add(New Point((StartPos.X + DeltaX), YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))

                End If
            ElseIf StartDir = ConDir.Left And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                    If EndPos.Y > LeftTop1.Y - DeltaY And EndPos.Y < RightBottom1.Y + DeltaY Then
                        If StartPos.Y < EndPos.Y Then
                            YM = LeftTop1.Y - DeltaY
                        Else
                            YM = RightBottom1.Y + DeltaY
                        End If

                        PointList.Add(New Point(StartPos.X - DeltaX, YM))
                        PointList.Add(New Point((RightBottom1.X + LeftTop2.X) / 2, YM))
                        PointList.Add(New Point((RightBottom1.X + LeftTop2.X) / 2, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X - DeltaX, EndPos.Y))
                    End If
                Else
                    XM = StartPos.X - DeltaX
                    If XM > EndPos.X - DeltaX Then XM = EndPos.X - DeltaX

                    If StartPos.Y > LeftTop2.Y - DeltaY And StartPos.Y < RightBottom2.Y + DeltaY Then
                        PointList.Add(New Point((StartPos.X + RightBottom2.X) / 2, StartPos.Y))
                        If StartPos.Y < EndPos.Y Then
                            YM = LeftTop2.Y - DeltaY
                        Else
                            YM = RightBottom2.Y + DeltaY
                        End If
                        PointList.Add(New Point((StartPos.X + RightBottom2.X) / 2, YM))
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(XM, EndPos.Y))
                    Else
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                End If
            ElseIf StartDir = ConDir.Right And EndDir = ConDir.Down Then
                If (EndPos.Y - DeltaY) > StartPos.Y Then
                    If EndPos.X >= StartPos.X + DeltaX Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        YM = (StartPos.Y + EndPos.Y) / 2
                        If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                        If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                        PointList.Add(New Point(StartPos.X + DeltaX, StartPos.Y))
                        PointList.Add(New Point(StartPos.X + DeltaX, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    XM = StartPos.X + DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    YM = EndPos.Y - DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            ElseIf StartDir = ConDir.Left And EndDir = ConDir.Down Then
                If (EndPos.Y - DeltaY) > StartPos.Y Then
                    If EndPos.X <= StartPos.X - DeltaX Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        YM = (StartPos.Y + EndPos.Y) / 2
                        If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                        If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                        PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                        PointList.Add(New Point(StartPos.X - DeltaX, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    XM = StartPos.X - DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = LeftTop2.X - DeltaX
                    YM = EndPos.Y - DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            ElseIf StartDir = ConDir.Up And EndDir = ConDir.Down Then
                YM = StartPos.Y - DeltaY
                If YM < EndPos.Y - DeltaY Then
                    XM = EndPos.X
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = (RightBottom1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    YM = EndPos.Y - DeltaY
                    If StartPos.X > LeftTop2.X - DeltaX And StartPos.X < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        YM = (RightBottom2.Y + StartPos.Y) / 2
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = EndPos.Y - DeltaY
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                End If
            End If

            If StartDir = ConDir.Down And EndDir = ConDir.Down Then
                YM = (StartPos.Y + EndPos.Y) / 2
                If YM < StartPos.Y + DeltaY Then
                    XM = (RightBottom1.X + LeftTop2.X) / 2
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    End If
                    PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))
                    PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                    YM = EndPos.Y - DeltaY
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                Else
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            ElseIf StartDir = ConDir.Right And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) > (StartPos.X + DeltaX) Then
                    If EndPos.Y < RightBottom1.Y + DeltaY And EndPos.Y > LeftTop1.Y - DeltaY Then
                        If EndPos.Y < (LeftTop1.Y + RightBottom1.Y) / 2 Then
                            YM = RightBottom2.Y + DeltaY
                        Else
                            YM = LeftTop2.Y - DeltaY
                        End If
                        PointList.Add(New Point((StartPos.X + LeftTop2.X) / 2, StartPos.Y))
                        PointList.Add(New Point((StartPos.X + LeftTop2.X) / 2, YM))
                        PointList.Add(New Point(EndPos.X + DeltaX, YM))
                        PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                    Else
                        PointList.Add(New Point(EndPos.X + DeltaX, StartPos.Y))
                        PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                    End If
                Else
                    PointList.Add(New Point(StartPos.X + DeltaX, StartPos.Y))
                    If EndPos.Y < RightBottom1.Y + DeltaY And EndPos.Y > LeftTop1.Y - DeltaY Then
                        If EndPos.Y < (LeftTop1.Y + RightBottom1.Y) / 2 Then
                            YM = LeftTop1.Y - DeltaY
                        Else
                            YM = RightBottom1.Y + DeltaY
                        End If
                        PointList.Add(New Point(StartPos.X + DeltaX, YM))
                        PointList.Add(New Point((RightBottom2.X + LeftTop1.X) / 2, YM))
                        PointList.Add(New Point((RightBottom2.X + LeftTop1.X) / 2, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X + DeltaX, EndPos.Y))
                    End If

                End If
            ElseIf StartDir = ConDir.Left And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) > (StartPos.X - DeltaX) Then
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < RightBottom1.Y + DeltaY And YM > LeftTop1.Y - DeltaY Then YM = LeftTop1.Y - DeltaY
                    If YM < RightBottom2.Y + DeltaY And YM > LeftTop2.Y - DeltaY Then YM = LeftTop2.Y - DeltaY
                    PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                    PointList.Add(New Point(StartPos.X - DeltaX, YM))
                    PointList.Add(New Point(EndPos.X + DeltaX, YM))
                    PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                Else
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, StartPos.Y))
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, EndPos.Y))
                End If
            ElseIf StartDir = ConDir.Down And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) < StartPos.X Then
                    If EndPos.Y >= StartPos.Y + DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))

                        XM = (LeftTop1.X + RightBottom2.X) / 2
                        If XM > LeftTop1.X - DeltaX Then XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X + DeltaX
                    If XM < RightBottom1.X + DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = RightBottom1.X + DeltaX
                    YM = (StartPos.Y + LeftTop2.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < StartPos.Y + DeltaY Then YM = StartPos.Y + DeltaY
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            ElseIf StartDir = ConDir.Up And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) < StartPos.X Then
                    If EndPos.Y <= StartPos.Y - DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))

                        XM = (LeftTop1.X + RightBottom2.X) / 2
                        If XM > LeftTop1.X - DeltaX Then XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X + DeltaX
                    If XM < RightBottom1.X + DeltaX And EndPos.Y > StartPos.Y - DeltaY Then XM = RightBottom1.X + DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                    If YM > StartPos.Y - DeltaY Then YM = StartPos.Y - DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            ElseIf StartDir = ConDir.Left And EndDir = ConDir.Up Then
                If EndPos.X < StartPos.X - DeltaX Then
                    If StartPos.Y > EndPos.Y + DeltaY Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        XM = (StartPos.X + EndPos.X) / 2
                        If XM < RightBottom2.X + DeltaX Then XM = LeftTop2.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                        PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                    End If

                Else
                    XM = StartPos.X - DeltaX
                    If XM > LeftTop2.X - DeltaX Then XM = LeftTop2.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < RightBottom2.Y + DeltaY Then YM = EndPos.Y + DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = RightBottom1.Y + DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            ElseIf StartDir = ConDir.Right And EndDir = ConDir.Up Then
                If EndPos.X > StartPos.X + DeltaX Then
                    If StartPos.Y > EndPos.Y + DeltaY Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        XM = (StartPos.X + EndPos.X) / 2
                        If XM > LeftTop2.X - DeltaX Then XM = RightBottom2.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                        PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                    End If

                Else
                    XM = StartPos.X + DeltaX
                    If XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < EndPos.Y + DeltaY Then YM = EndPos.Y + DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = RightBottom1.Y + DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            ElseIf StartDir = ConDir.Up And EndDir = ConDir.Up Then
                If EndPos.Y + DeltaY < StartPos.Y - DeltaY Then
                    YM = (StartPos.Y + EndPos.Y) / 2
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                Else
                    XM = (StartPos.X + EndPos.X) / 2
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then XM = RightBottom1.X + DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then XM = RightBottom1.X + DeltaX
                    End If
                    PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))
                    PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                    PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                    PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                End If
            ElseIf StartDir = ConDir.Down And EndDir = ConDir.Up Then
                YM = StartPos.Y + DeltaY
                XM = EndPos.X
                If YM > EndPos.Y + DeltaY Then
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = (LeftTop1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    YM = EndPos.Y + DeltaY
                    If StartPos.X > LeftTop2.X - DeltaX And StartPos.X < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        YM = (LeftTop1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = EndPos.Y + DeltaY
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                End If
            End If

            'finish path

            PointList.Add(New Point(EndPos.X, EndPos.Y))

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            SetupPositioning()

            Dim path As New SKPath()

            path.MoveTo(PointList(0).X, PointList(0).Y)
            For i As Integer = 1 To PointList.Count - 1
                path.LineTo(PointList(i).X, PointList(i).Y)
            Next

            ConnectorPath = path

            Select Case DrawMode

                Case 0, 2

                    'default

                    Dim myPen As New SKPaint

                    With myPen
                        .IsStroke = True
                        .StrokeWidth = LineWidth
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateCorner(2.0F)
                        If AttachedFrom.Status = Enums.GraphicObjects.Status.Calculated And AttachedTo.Status = Enums.GraphicObjects.Status.Calculated Then
                            If s.DarkMode Then
                                .Color = SKColors.WhiteSmoke
                            Else
                                .Color = SKColors.SteelBlue
                            End If
                        Else
                            .Color = SKColors.Salmon
                        End If
                        If AttachedFrom.ObjectType = ObjectType.OT_Recycle Or AttachedFrom.ObjectType = ObjectType.OT_EnergyRecycle Then
                            If AttachedFrom.Active Then
                                .Color = SKColors.SandyBrown
                            Else
                                .Color = SKColors.Gray
                            End If
                            .PathEffect = SKPathEffect.CreateCompose(SKPathEffect.CreateDash(New Single() {2, 2}, 4), .PathEffect)
                        End If
                        If AttachedTo.ObjectType = ObjectType.OT_Recycle Or AttachedTo.ObjectType = ObjectType.OT_EnergyRecycle Then
                            If AttachedTo.Active Then
                                .Color = SKColors.SandyBrown
                            Else
                                .Color = SKColors.Gray
                            End If
                            .PathEffect = SKPathEffect.CreateCompose(SKPathEffect.CreateDash(New Single() {2, 2}, 4), .PathEffect)
                        End If
                    End With

                    Dim myPen2 As New SKPaint

                    With myPen2
                        .IsStroke = True
                        .StrokeWidth = LineWidth + 4
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateCorner(6.0F)
                        .Color = SKColors.White.WithAlpha(200)
                    End With

                    If Not Straight Then

                        If Not GlobalSettings.Settings.DarkMode Then canvas.DrawPath(path, myPen2)
                        canvas.DrawPath(path, myPen)

                    Else

                        canvas.DrawLine(New SKPoint(PointList.First.X, PointList.First.Y),
                                        New SKPoint(PointList.Last.X, PointList.Last.Y), myPen)

                    End If

                Case 1

                    'b/w

                    Dim myPen As New SKPaint

                    With myPen
                        .Color = SKColors.Gray
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateCorner(2.0F)
                        If AttachedFrom.ObjectType = ObjectType.OT_Recycle Or AttachedTo.ObjectType = ObjectType.OT_EnergyRecycle Then
                            If AttachedFrom.Active Then
                                .Color = SKColors.SandyBrown
                            Else
                                .Color = SKColors.Gray
                            End If
                            .PathEffect = SKPathEffect.CreateCompose(SKPathEffect.CreateDash(New Single() {2, 2}, 4), .PathEffect)
                        End If
                        If AttachedTo.ObjectType = ObjectType.OT_Recycle Or AttachedTo.ObjectType = ObjectType.OT_EnergyRecycle Then
                            If AttachedTo.Active Then
                                .Color = SKColors.SandyBrown
                            Else
                                .Color = SKColors.Gray
                            End If
                            .PathEffect = SKPathEffect.CreateCompose(SKPathEffect.CreateDash(New Single() {2, 2}, 4), .PathEffect)
                        End If
                    End With

                    If Not Straight Then

                        canvas.DrawPath(path, myPen)

                    Else

                        canvas.DrawLine(New SKPoint(PointList.First.X, PointList.First.Y),
                                        New SKPoint(PointList.Last.X, PointList.Last.Y), myPen)

                    End If

                Case 2
                    'Gas/Liquid Flows
                Case 3
                    'Temperature Gradients
                Case 4
                    'Pressure Gradients
                Case 5
                    'Temperature/Pressure Gradients
            End Select

            Dim bounds As New SKRect

            ConnectorPath.GetBounds(bounds)

            X = bounds.Left
            Y = bounds.Top
            Width = bounds.Width
            Height = bounds.Height

        End Sub

        Public Property AttachedFrom As Interfaces.IGraphicObject = Nothing Implements Interfaces.IConnectorGraphicObject.AttachedFrom

        Public Property AttachedFromConnectorIndex As Integer = -1 Implements Interfaces.IConnectorGraphicObject.AttachedFromConnectorIndex

        Public Property AttachedFromEnergy As Boolean = False Implements Interfaces.IConnectorGraphicObject.AttachedFromEnergy

        Public Property AttachedTo As Interfaces.IGraphicObject = Nothing Implements Interfaces.IConnectorGraphicObject.AttachedTo

        Public Property AttachedToConnectorIndex As Integer = -1 Implements Interfaces.IConnectorGraphicObject.AttachedToConnectorIndex

        Public Property AttachedToEnergy As Boolean = False Implements Interfaces.IConnectorGraphicObject.AttachedToEnergy

        Public Property AttachedToOutput As Boolean = False Implements Interfaces.IConnectorGraphicObject.AttachedToOutput

        Public Property AttachedFromInput As Boolean = False Implements Interfaces.IConnectorGraphicObject.AttachedFromInput

        Public Overrides Function HitTest(pt As SKPoint) As Boolean

            If ConnectorPathExpanded IsNot Nothing Then
                Return ConnectorPathExpanded.Contains(pt.X, pt.Y)
            Else
                Return False
            End If

        End Function

        Public Overrides Function GetPointValue(type As PointValueType, X As Integer, Y As Integer, args As List(Of Object)) As Double

            If X >= 0 And X <= Width + 5 And Y >= 0 And Y <= Height + 5 Then
                If HitTest(New SKPoint(Me.X + X, Me.Y + Y)) Then
                    Dim im = TryCast(AttachedFrom?.Owner, IMaterialStream)
                    Dim om = TryCast(AttachedTo?.Owner, IMaterialStream)
                    If im Is Nothing And om Is Nothing Then Return Double.NaN
                    Dim ms As IMaterialStream = IIf(im Is Nothing, om, im)
                    Dim v1 As Double
                    Select Case type
                        Case PointValueType.Temperature
                            v1 = ms.GetTemperature()
                        Case PointValueType.Pressure
                            v1 = ms.GetPressure()
                        Case PointValueType.Flow
                            v1 = ms.GetMassFlow()
                        Case PointValueType.EnergyFlow
                            v1 = ms.GetEnergyFlow()
                        Case PointValueType.Concentration
                            v1 = ms.GetCompoundMassConcentration(args(0))
                        Case PointValueType.CompoundMassFlow
                            v1 = ms.Phases(0).Compounds(args(0)).MassFlow.GetValueOrDefault()
                        Case PointValueType.CompoundMolarFlow
                            v1 = ms.Phases(0).Compounds(args(0)).MolarFlow.GetValueOrDefault()
                        Case PointValueType.CompoundMassFraction
                            v1 = ms.Phases(0).Compounds(args(0)).MassFraction.GetValueOrDefault()
                        Case PointValueType.CompoundMolarFraction
                            v1 = ms.Phases(0).Compounds(args(0)).MoleFraction.GetValueOrDefault()
                    End Select
                    Return v1
                Else
                    Return Double.NaN
                End If
            Else
                Return Double.NaN
            End If

        End Function

    End Class

End Namespace