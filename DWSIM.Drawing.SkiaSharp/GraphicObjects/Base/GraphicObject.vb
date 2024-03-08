Imports System.Collections.Generic
Imports System.IO
Imports System.Linq
Imports System.Xml.Linq
Imports DWSIM.Interfaces
Imports Interfaces = DWSIM.Interfaces

Namespace GraphicObjects

    Public MustInherit Class GraphicObject

        Implements IGraphicObject, ICustomXMLSerialization

        Private Shared _rtp, _btp, _itp, _bitp, _ttp As SKTypeface

        Public Shared Sub SetGlobalTypeface(gtext As String)

            _ttp = SKFontManager.Default.MatchCharacter(gtext(0))

        End Sub

        Public Property RegularTypeFace As SKTypeface
            Get
                If Not GlobalSettings.Settings.TranslatorActivated Then
                    Return _rtp
                Else
                    Return _ttp
                End If
            End Get
            Set(value As SKTypeface)
                _rtp = value
            End Set
        End Property

        Public Property BoldTypeFace As SKTypeface
            Get
                If Not GlobalSettings.Settings.TranslatorActivated Then
                    Return _btp
                Else
                    Return _ttp
                End If

            End Get
            Set(value As SKTypeface)
                _btp = value
            End Set
        End Property

        Public Property ItalicTypeFace As SKTypeface
            Get
                If Not GlobalSettings.Settings.TranslatorActivated Then
                    Return _itp
                Else
                    Return _ttp
                End If

            End Get
            Set(value As SKTypeface)
                _itp = value
            End Set
        End Property

        Public Property BoldItalicTypeFace As SKTypeface
            Get
                If Not GlobalSettings.Settings.TranslatorActivated Then
                    Return _bitp
                Else
                    Return _ttp
                End If

            End Get
            Set(value As SKTypeface)
                _bitp = value
            End Set
        End Property

        Public Property FontStyle As Enums.GraphicObjects.FontStyle = Enums.GraphicObjects.FontStyle.Regular Implements IGraphicObject.FontStyle

        Friend EmbeddedResourceIconName As String = ""

        Friend Image As SKImage

        Public Function GetFont() As SKTypeface

            If Not GlobalSettings.Settings.TranslatorActivated Then
                Select Case FontStyle
                    Case Enums.GraphicObjects.FontStyle.Regular
                        Return RegularTypeFace
                    Case Enums.GraphicObjects.FontStyle.Bold
                        Return BoldTypeFace
                    Case Enums.GraphicObjects.FontStyle.BoldItalic
                        Return BoldItalicTypeFace
                    Case Enums.GraphicObjects.FontStyle.Italic
                        Return ItalicTypeFace
                    Case Else
                        Return RegularTypeFace
                End Select
            Else
                Return _ttp
            End If

        End Function

        Public Overrides Function ToString() As String
            If Tag <> "" Then
                Return Tag
            Else
                Return MyBase.ToString()
            End If
        End Function

        Public Function GetPaint(color As SKColor) As SKPaint

            Dim p As New SKPaint

            With p
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = color
                .IsStroke = False
                p.Typeface = RegularTypeFace
            End With

            Return p

        End Function

        Public Function GetStrokePaint(color As SKColor, StrokeWidth As Single) As SKPaint

            Dim p As New SKPaint

            With p
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = color
                .IsStroke = True
                .StrokeWidth = StrokeWidth
                p.Typeface = RegularTypeFace
            End With

            Return p

        End Function

        Public Function GetRect(x As Single, y As Single, width As Single, height As Single)

            Return New SKRect(x, y, x + width, y + height)

        End Function

        Public Function GetDrawingRect()

            Return New SKRect(X, Y, X + Width, Y + Height)

        End Function

        Public Function MeasureString(text As String, paint As SKPaint) As SKSize

            Dim trect As New SKRect(0, 0, 2, 2)

            Dim aa = paint.IsAntialias

            paint.IsAntialias = False

            paint.GetTextPath(text, 0, 0).GetBounds(trect)

            paint.IsAntialias = aa

            Return New SKSize(trect.Width, trect.Height)

        End Function

        Public Sub StraightCanvas(canvas As SKCanvas)

            If FlippedV And Not FlippedH Then
                canvas.Scale(1, -1, (X + Width / 2), (Y + Height / 2))
            ElseIf FlippedH And Not FlippedV Then
                canvas.Scale(-1, 1, (X + Width / 2), (Y + Height / 2))
            ElseIf FlippedH And FlippedV Then
                canvas.Scale(-1, -1, (X + Width / 2), (Y + Height / 2))
            End If
            canvas.RotateDegrees(Rotation, X + Width / 2, Y + Height / 2)

        End Sub

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements ICustomXMLSerialization.SaveData

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("InputConnectors"))

                For Each cp As ConnectionPoint In InputConnectors
                    If cp.IsAttached And Not cp.AttachedConnector Is Nothing Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached),
                                                                                        New XAttribute("ConnType", cp.Type.ToString),
                                                                                        New XAttribute("AttachedFromObjID", cp.AttachedConnector.AttachedFrom.Name.ToString),
                                                                                        New XAttribute("AttachedFromConnIndex", cp.AttachedConnector.AttachedFromConnectorIndex),
                                                                                        New XAttribute("AttachedFromEnergyConn", cp.AttachedConnector.AttachedFromEnergy.ToString)))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached)))
                    End If
                Next

                .Add(New XElement("OutputConnectors"))

                For Each cp As ConnectionPoint In OutputConnectors
                    If cp.IsAttached And Not cp.AttachedConnector Is Nothing Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached),
                                                                                        New XAttribute("ConnType", cp.Type.ToString),
                                                                                        New XAttribute("AttachedToObjID", cp.AttachedConnector.AttachedTo.Name.ToString),
                                                                                        New XAttribute("AttachedToConnIndex", cp.AttachedConnector.AttachedToConnectorIndex),
                                                                                        New XAttribute("AttachedToEnergyConn", cp.AttachedConnector.AttachedToEnergy.ToString)))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached)))
                    End If
                Next

                .Add(New XElement("EnergyConnector"))

                If EnergyConnector.IsAttached Then
                    elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", EnergyConnector.IsAttached),
                                                                                    New XAttribute("AttachedToObjID", EnergyConnector.AttachedConnector.AttachedTo.Name.ToString),
                                                                                        New XAttribute("AttachedToConnIndex", EnergyConnector.AttachedConnector.AttachedToConnectorIndex),
                                                                                        New XAttribute("AttachedToEnergyConn", EnergyConnector.AttachedConnector.AttachedToEnergy.ToString)),
                                                                                        New XAttribute("AttachedFromObjID", EnergyConnector.AttachedConnector.AttachedFrom.Name.ToString),
                                                                                        New XAttribute("AttachedFromConnIndex", EnergyConnector.AttachedConnector.AttachedFromConnectorIndex),
                                                                                        New XAttribute("AttachedFromEnergyConn", EnergyConnector.AttachedConnector.AttachedFromEnergy.ToString))
                Else
                    elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", EnergyConnector.IsAttached)))
                End If

                .Add(New XElement("SpecialConnectors"))

                For Each cp As ConnectionPoint In SpecialConnectors
                    If cp.IsAttached Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached),
                                                                                        New XAttribute("AttachedToObjID", cp.AttachedConnector.AttachedTo.Name.ToString),
                                                                                        New XAttribute("AttachedToConnIndex", cp.AttachedConnector.AttachedToConnectorIndex),
                                                                                        New XAttribute("AttachedToEnergyConn", cp.AttachedConnector.AttachedToEnergy.ToString)),
                                                                                        New XAttribute("AttachedFromObjID", cp.AttachedConnector.AttachedFrom.Name.ToString),
                                                                                        New XAttribute("AttachedFromConnIndex", cp.AttachedConnector.AttachedFromConnectorIndex),
                                                                                        New XAttribute("AttachedFromEnergyConn", cp.AttachedConnector.AttachedFromEnergy.ToString))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached)))
                    End If
                Next

            End With

            Return elements

        End Function

        Public Shared Function ReturnInstance(typename As String) As IGraphicObject
            Dim t As Type = Type.GetType(typename, False)
            If Not t Is Nothing Then Return Activator.CreateInstance(t) Else Return Nothing
        End Function

        Public Overridable Property Calculated() As Boolean Implements IGraphicObject.Calculated
            Get
                Select Case Status
                    Case Enums.GraphicObjects.Status.Calculated
                        Return True
                    Case Enums.GraphicObjects.Status.Calculating
                        Return False
                    Case Enums.GraphicObjects.Status.ErrorCalculating
                        Return False
                    Case Enums.GraphicObjects.Status.Idle
                        Return True
                    Case Enums.GraphicObjects.Status.Inactive
                        Return False
                    Case Enums.GraphicObjects.Status.NotCalculated
                        Return False
                    Case Else
                        Return False
                End Select
            End Get
            Set(ByVal Value As Boolean)
                If Value = False Then
                    Status = Enums.GraphicObjects.Status.ErrorCalculating
                Else
                    Status = Enums.GraphicObjects.Status.Calculated
                End If
            End Set
        End Property

        Public Overridable Overloads Function HitTest(ByVal pt As SKPoint) As Boolean
            Return pt.X >= X And pt.Y >= Y And pt.X <= X + Width And pt.Y <= Y + Height
        End Function

        Public Overridable Function IsInRect(ByVal rect As SKRect) As Boolean
            Return rect.Contains(New SKRect(X, Y, X + Width, Y + Height))
        End Function

#Region "Constructors"
        Public Sub New()
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize)
            Me.New(graphicPosition)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize)
            Me.New(New SKPoint(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer,
                ByVal width As Integer, ByVal height As Integer)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height))
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal Rotation As Single)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal Rotation As Single)
            Me.New(New SKPoint(posX, posY), Rotation)
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize, ByVal Rotation As Single)
            Me.New(graphicPosition, Rotation)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer,
            ByVal graphicSize As SKSize, ByVal Rotation As Single)
            Me.New(New SKPoint(posX, posY), graphicSize, Rotation)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer,
                               ByVal height As Integer, ByVal Rotation As Single)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height), Rotation)
        End Sub

#End Region

        Public Overridable Function GetPosition() As SKPoint
            Return New SKPoint(X, Y)
        End Function

        Public Overridable Function GetCenterPosition() As SKPoint
            Return New SKPoint(X + Width / 2, Y + Height / 2)
        End Function

        Public Overridable Sub SetPosition(ByVal Value As SKPoint)
            X = Value.X
            Y = Value.Y
        End Sub

        Public Overridable Sub SetPosition(ByVal X0 As Integer, ByVal Y0 As Integer)
            X = X0
            Y = Y0
        End Sub

        Public Overridable Sub SetSize(ByVal Value As SKSize)
            Width = Value.Width
            Height = Value.Height
        End Sub

        Public Overridable Function GetSize() As SKSize
            Dim mySize As New SKSize(Width, Height)
            Return mySize
        End Function

        Public Overridable Sub Draw(ByVal g As Object)

            PositionConnectors()

        End Sub

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

        Public Overridable Sub PositionConnectors() Implements IGraphicObject.PositionConnectors

            'To be implemented in derived classes.

        End Sub

        Public Overridable Sub CreateConnectors(ByVal InCount As Integer, ByVal OutCount As Integer)

            'Creates all the connection points.

            For I As Integer = 1 To InCount

                Dim Con As New ConnectionPoint
                Con.Type = Enums.GraphicObjects.ConType.ConIn
                InputConnectors.Add(Con)

            Next

            For I As Integer = 1 To OutCount

                Dim Con As New ConnectionPoint
                Con.Type = Enums.GraphicObjects.ConType.ConOut
                OutputConnectors.Add(Con)

            Next

        End Sub

        Public Property Active As Boolean = True Implements IGraphicObject.Active

        Public Property AdditionalInfo As Object = Nothing Implements IGraphicObject.AdditionalInfo

        Public Property Description As String = "" Implements IGraphicObject.Description

        Public Property FlippedH As Boolean = False Implements IGraphicObject.FlippedH

        Public Property FlippedV As Boolean = False Implements IGraphicObject.FlippedV

        Public Property IsEnergyStream As Boolean = False Implements IGraphicObject.IsEnergyStream

        Public Property ObjectType As Enums.GraphicObjects.ObjectType = Enums.GraphicObjects.ObjectType.Nenhum Implements IGraphicObject.ObjectType

        Public Property Shape As Integer = 0 Implements IGraphicObject.Shape

        Public Property ShapeOverride As Enums.GraphicObjects.ShapeIcon = Enums.GraphicObjects.ShapeIcon.DefaultShape Implements IGraphicObject.ShapeOverride

        Public Property Status As Enums.GraphicObjects.Status = Enums.GraphicObjects.Status.NotCalculated Implements IGraphicObject.Status

        Public Property AutoSize As Boolean = False Implements IGraphicObject.AutoSize

        Public Property Height As Integer = 20 Implements IGraphicObject.Height

        Public Property IsConnector As Boolean = False Implements IGraphicObject.IsConnector

        Public Property Name As String = "" Implements IGraphicObject.Name

        Public Property Tag As String = "" Implements IGraphicObject.Tag

        Public Property Width As Integer = 20 Implements IGraphicObject.Width

        Public Property X As Single = 0.0 Implements IGraphicObject.X

        Public Property Y As Single = 0.0 Implements IGraphicObject.Y

        Public Property EnergyConnector As IConnectionPoint = New ConnectionPoint() With {.IsEnergyConnector = True} Implements IGraphicObject.EnergyConnector

        Public Property InputConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.InputConnectors

        Public Property OutputConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.OutputConnectors

        Public Property SpecialConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.SpecialConnectors

        Public Sub Draw1(surface As Object) Implements IGraphicObject.Draw
            Draw(surface)
        End Sub

        Public Property Selected As Boolean = False Implements IGraphicObject.Selected

        <Xml.Serialization.XmlIgnore> Property Owner As ISimulationObject Implements IGraphicObject.Owner

        Public Property Position As IPoint Implements IGraphicObject.Position

        Public Function HitTest1(zoomedSelection As Object) As Boolean Implements IGraphicObject.HitTest

            Dim rect As SKRect = DirectCast(zoomedSelection, SKRect)

            Return rect.Left <= X And rect.Top <= Y And rect.Right >= X + Width And rect.Bottom >= Y + Height

        End Function

        Public Function Clone() As IGraphicObject Implements IGraphicObject.Clone
            Dim newobj = Activator.CreateInstance(Me.GetType)
            DirectCast(newobj, ICustomXMLSerialization).LoadData(Me.SaveData)
            Return newobj
        End Function

        Public Overridable Sub DisplayControlPanelModeEditor() Implements IGraphicObject.DisplayControlPanelModeEditor

            ControlPanelModeEditorDisplayDelegate?.Invoke()

        End Sub

        Public Property Editor As Object Implements IGraphicObject.Editor

        Public Property Rotation As Integer Implements IGraphicObject.Rotation

        Public Property DrawOverride As Action(Of Object) Implements IGraphicObject.DrawOverride

        Public Property Extensions As Dictionary(Of String, IGraphicObjectExtension) Implements IGraphicObject.Extensions

        Public Property ControlPanelModeEditorDisplayDelegate As Action Implements IGraphicObject.ControlPanelModeEditorDisplayDelegate

        Public Property DoubleClickAction As Action(Of Object) Implements IGraphicObject.DoubleClickAction

        Public Property DrawMode As Integer = 0 Implements IGraphicObject.DrawMode

        <Xml.Serialization.XmlIgnore>
        Public Property Flowsheet As IFlowsheet Implements IGraphicObject.Flowsheet

        Public Property DrawLabel As Boolean = True Implements IGraphicObject.DrawLabel

        Public Function GetForeColor() As SKColor

            Return Drawing.SkiaSharp.GraphicsSurface.ForegroundColor

        End Function

        Public Function GetBackColor() As SKColor

            Return Drawing.SkiaSharp.GraphicsSurface.BackgroundColor

        End Function

        Public Overridable Function GetPointValue(type As Enums.GraphicObjects.PointValueType, X As Integer, Y As Integer, args As List(Of Object)) As Double Implements IGraphicObject.GetPointValue

            Return Double.NaN

        End Function

        Public Overridable Function GetIconAsBitmap() As System.Drawing.Bitmap Implements IGraphicObject.GetIconAsBitmap

            If EmbeddedResourceIconName <> "" Then

                Dim bmp As New System.Drawing.Bitmap(Me.GetType().Assembly.GetManifestResourceStream(String.Format("DWSIM.Drawing.SkiaSharp.{0}", EmbeddedResourceIconName)))
                Return bmp

            Else

                Return Nothing

            End If

        End Function

        Public Overridable Function GetIconAsStream() As MemoryStream Implements IGraphicObject.GetIconAsStream

            If EmbeddedResourceIconName <> "" Then

                Using us = Me.GetType().Assembly.GetManifestResourceStream(String.Format("DWSIM.Drawing.SkiaSharp.{0}", EmbeddedResourceIconName))

                    Dim ms As New MemoryStream()
                    us.CopyTo(ms)
                    ms.Position = 0
                    Return ms

                End Using

            Else

                Return Nothing

            End If

        End Function

        Public Sub ReleaseReferences() Implements IGraphicObject.ReleaseReferences

            Owner = Nothing

        End Sub

    End Class

End Namespace

