Imports System.Collections.Generic
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.Interfaces
Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects

    Public MustInherit Class GraphicObjectExtension

        Implements IGraphicObjectExtension, ICustomXMLSerialization

        Public Function GetPaint(color As SKColor)

            Dim p As New SKPaint

            With p
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = color
                .IsStroke = False
            End With

            Return p

        End Function

        Public Function GetStrokePaint(color As SKColor, StrokeWidth As Single)

            Dim p As New SKPaint

            With p
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = color
                .IsStroke = True
                .StrokeWidth = StrokeWidth
            End With

            Return p

        End Function

        Public Function MeasureString(text As String, paint As SKPaint) As SKSize

            Dim trect As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(text, 0, 0).GetBounds(trect)

            Return New SKSize(trect.Width, trect.Height)

        End Function

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements ICustomXMLSerialization.SaveData

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        Public Function GetBoundsRectangle() As SKRect
            Return New SKRect(Owner.X + RelativePosition.X, Owner.Y + RelativePosition.Y, Width, Height)
        End Function

        Public Shared Function ReturnInstance(typename As String) As IGraphicObjectExtension
            Dim t As Type = Type.GetType(typename, False)
            If Not t Is Nothing Then Return Activator.CreateInstance(t) Else Return Nothing
        End Function

#Region "Constructors"

        Public Sub New()

        End Sub

        Public Sub New(ByVal position As SKPoint, owner As IGraphicObject)

            Me.New()
            _relposition = New Point(position.X, position.Y)
            Me.Owner = owner

        End Sub

#End Region

        Public Sub DrawRoundRect(ByVal g As Object, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal radius As Integer, ByVal myBrush As SKPaint)

            If width / 2 < radius Then
                radius = width / 2 - 2
            ElseIf height / 2 < radius Then
                radius = height / 2 - 2
            End If

            Dim gp As SKPath = New SKPath()

            gp.AddRoundedRect(New SKRect(x, y, x + width, y + height), radius, radius, SKPathDirection.Clockwise)

            gp.Close()

            DirectCast(g, SKCanvas).DrawPath(gp, myBrush)

        End Sub

        Public Property Active As Boolean Implements IGraphicObjectExtension.Active

        Public Function Clone() As IGraphicObjectExtension Implements IGraphicObjectExtension.Clone
            Return Nothing
        End Function

        Public Property Description As String Implements IGraphicObjectExtension.Description

        Public MustOverride Sub Draw(surface As Object) Implements IGraphicObjectExtension.Draw

        Public Property Height As Integer Implements IGraphicObjectExtension.Height

        Public Function HitTest(zoomedSelection As Object) As Boolean Implements IGraphicObjectExtension.HitTest

            Dim rect As SKRect = DirectCast(zoomedSelection, SKRect)

            Return rect.Left <= Owner.X + RelativePosition.X And rect.Top <= Owner.Y + RelativePosition.Y And rect.Right >= Owner.X + RelativePosition.X + Width And rect.Bottom >= Owner.Y + RelativePosition.Y + Height

        End Function

        Public Property Name As String Implements IGraphicObjectExtension.Name

        Public Property Owner As IGraphicObject Implements IGraphicObjectExtension.Owner

        Private _relposition As New Point

        Public Property RelativePosition As IPoint Implements IGraphicObjectExtension.RelativePosition
            Get
                Return _relposition
            End Get
            Set(value As IPoint)
                _relposition = value
            End Set
        End Property

        Public Property Selected As Boolean Implements IGraphicObjectExtension.Selected

        Public Property Tag As String Implements IGraphicObjectExtension.Tag

        Public Property Width As Integer Implements IGraphicObjectExtension.Width

    End Class

End Namespace

