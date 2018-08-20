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

Imports System.Drawing
Imports System.Collections.Generic
Imports System.Linq
Imports DWSIM.Interfaces
Imports System.Xml.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    <System.Serializable()> Public MustInherit Class GraphicObject

        Implements IGraphicObject

        Friend m_Rotation As Single

        Public Function IsRunningOnMono() As Boolean
            Return Not Type.GetType("Mono.Runtime") Is Nothing
        End Function

        Public Function GetBoundsRectangle() As Rectangle
            Return New Rectangle(X, Y, Width, Height)
        End Function

        Public Shared Function ReturnInstance(typename As String) As IGraphicObject
            Dim t As Type = Type.GetType(typename, False)
            If Not t Is Nothing Then Return Activator.CreateInstance(t) Else Return Nothing
        End Function

        Public Overridable Property Calculated() As Boolean Implements IGraphicObject.Calculated
            Get
                Select Case Status
                    Case Status.Calculated
                        Return True
                    Case Status.Calculating
                        Return False
                    Case Status.ErrorCalculating
                        Return False
                    Case Status.Idle
                        Return True
                    Case Status.Inactive
                        Return False
                    Case Status.NotCalculated
                        Return False
                    Case Else
                        Return False
                End Select
            End Get
            Set(ByVal Value As Boolean)
                If Value = False Then
                    Status = Status.ErrorCalculating
                Else
                    Status = Status.Calculated
                End If
            End Set
        End Property

        Public Overridable Overloads Function HitTest(ByVal pt As System.Drawing.Point) As Boolean
            Dim gp As New Drawing2D.GraphicsPath()
            Dim myMatrix As New Drawing2D.Matrix()
            gp.AddRectangle(New Rectangle(X, Y, Width, Height))
            If Me.Rotation <> 0 Then
                myMatrix.RotateAt(Me.Rotation, New PointF(Me.X + Me.Width / 2, Me.Y + Me.Height / 2), _
                    Drawing.Drawing2D.MatrixOrder.Append)
            End If
            gp.Transform(myMatrix)
            Return gp.IsVisible(pt)
        End Function

        Public Overridable Overloads Function HitTest(ByVal rect As Rectangle) As Boolean
            'is this object contained within the supplied rectangle
            Dim gp As New Drawing2D.GraphicsPath()
            Dim myMatrix As New Drawing2D.Matrix()
            gp.AddRectangle(New Rectangle(X, Y, Width, Height))
            If Me.Rotation <> 0 Then
                myMatrix.RotateAt(Me.Rotation, New PointF(Me.X + Me.Width / 2, Me.Y + Me.Height / 2), _
                    Drawing.Drawing2D.MatrixOrder.Append)
            End If
            gp.Transform(myMatrix)
            Dim gpRect As Rectangle = Rectangle.Round(gp.GetBounds)
            Return rect.Contains(gpRect)
        End Function

#Region "Constructors"
        Protected Sub New()
        End Sub

        Protected Sub New(ByVal graphicPosition As Point)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New Point(posX, posY))
        End Sub

        Protected Sub New(ByVal graphicPosition As Point, ByVal graphicSize As Size)
            Me.New(graphicPosition)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As Size)
            Me.New(New Point(posX, posY), graphicSize)
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer, _
                ByVal width As Integer, ByVal height As Integer)
            Me.New(New Point(posX, posY), New Size(width, height))
        End Sub

        Protected Sub New(ByVal graphicPosition As Point, ByVal Rotation As Single)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Rotation = Rotation
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal Rotation As Single)
            Me.New(New Point(posX, posY), Rotation)
        End Sub

        Protected Sub New(ByVal graphicPosition As Point, ByVal graphicSize As Size, ByVal Rotation As Single)
            Me.New(graphicPosition, Rotation)
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer, _
            ByVal graphicSize As Size, ByVal Rotation As Single)
            Me.New(New Point(posX, posY), graphicSize, Rotation)
        End Sub

        Protected Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, _
                               ByVal height As Integer, ByVal Rotation As Single)
            Me.New(New Point(posX, posY), New Size(width, height), Rotation)
        End Sub

#End Region

        Public Overridable Function GetPosition() As Point
            Dim myPosition As New Point(X, Y)
            Return myPosition
        End Function

        Public Overridable Sub SetPosition(ByVal Value As Point)
            X = Value.X
            Y = Value.Y
        End Sub

        Public Overridable Sub SetSize(ByVal Value As Size)
            Width = Value.Width
            Height = Value.Height
        End Sub

        Public Overridable Function GetSize() As Size
            Dim mySize As New Size(Width, Height)
            Return mySize
        End Function

        Public Overridable Property Rotation() As Integer Implements IGraphicObject.Rotation
            Get
                Return m_Rotation
                If IsRunningOnMono() Then Return 0
            End Get
            Set(ByVal Value As Integer)
                If System.Math.Abs(Value) <= 360 Then
                    m_Rotation = Value
                    If IsRunningOnMono() Then m_Rotation = 0
                Else
                    Throw New ArgumentOutOfRangeException("Rotation", "Rotation must be between -360.0 and 360.0")
                End If
            End Set
        End Property

        Public Overridable Sub Draw(ByVal g As Graphics)

            PositionConnectors()

            RotateConnectors()

        End Sub

        Public Sub DrawRoundRect(ByVal g As Graphics, ByVal p As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal radius As Single, ByVal myBrush As Brush)

            If width / 2 < radius Then
                radius = width / 2 - 2
            ElseIf height / 2 < radius Then
                radius = height / 2 - 2
            End If

            Dim gp As Drawing2D.GraphicsPath = New Drawing2D.GraphicsPath

            gp.AddLine(x + radius, y, x + width - radius, y)
            gp.AddArc(x + width - radius, y, radius, radius, 270, 90)
            gp.AddLine(x + width, y + radius, x + width, y + height - radius)
            gp.AddArc(x + width - radius, y + height - radius, radius, radius, 0, 90)
            gp.AddLine(x + width - radius, y + height, x + radius, y + height)
            gp.AddArc(x, y + height - radius, radius, radius, 90, 90)
            gp.AddLine(x, y + height - radius, x, y + radius)
            gp.AddArc(x, y, radius, radius, 180, 90)

            gp.CloseFigure()

            g.DrawPath(p, gp)
            g.FillPath(myBrush, gp)

            gp.Dispose()

        End Sub

        Public Overridable Sub PositionConnectors()

            'To be implemented in derived classes.

        End Sub

        Public Sub RotateConnectors()

            Dim center As Point = New Point(X + Me.Width / 2, Y + Me.Height / 2)
            Dim pt As Point
            Dim raio, angulo As Double
            Dim con As ConnectionPoint
            For Each con In Me.InputConnectors
                pt = con.Position
                raio = ((pt.X - center.X) ^ 2 + (pt.Y - center.Y) ^ 2) ^ 0.5
                angulo = Math.Atan2(pt.Y - center.Y, pt.X - center.X)
                pt.X = center.X + raio * Math.Cos(angulo + Me.Rotation / 360 * 2 * Math.PI)
                pt.Y = center.Y + raio * Math.Sin(angulo + Me.Rotation / 360 * 2 * Math.PI)
                con.Position = pt
            Next
            For Each con In Me.OutputConnectors
                pt = con.Position
                raio = ((pt.X - center.X) ^ 2 + (pt.Y - center.Y) ^ 2) ^ 0.5
                angulo = Math.Atan2(pt.Y - center.Y, pt.X - center.X)
                pt.X = center.X + raio * Math.Cos(angulo + Me.Rotation / 360 * 2 * Math.PI)
                pt.Y = center.Y + raio * Math.Sin(angulo + Me.Rotation / 360 * 2 * Math.PI)
                con.Position = pt
            Next
            With Me.EnergyConnector
                pt = .Position
                raio = ((pt.X - center.X) ^ 2 + (pt.Y - center.Y) ^ 2) ^ 0.5
                angulo = Math.Atan2(pt.Y - center.Y, pt.X - center.X)
                pt.X = center.X + raio * Math.Cos(angulo + Me.Rotation / 360 * 2 * Math.PI)
                pt.Y = center.Y + raio * Math.Sin(angulo + Me.Rotation / 360 * 2 * Math.PI)
                .Position = pt
            End With

        End Sub

        Public Overridable Sub CreateConnectors(ByVal InCount As Integer, ByVal OutCount As Integer)

            'Creates all the connection points.

            For I As Integer = 1 To InCount

                Dim Con As New ConnectionPoint
                Con.Type = ConType.ConIn
                InputConnectors.Add(Con)

            Next

            For I As Integer = 1 To OutCount

                Dim Con As New ConnectionPoint
                Con.Type = ConType.ConOut
                OutputConnectors.Add(Con)

            Next

        End Sub

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            'DWSIM Mobile compatibility

            If ObjectType = Enums.GraphicObjects.ObjectType.CompressorExpander Then ObjectType = Enums.GraphicObjects.ObjectType.Compressor
            If ObjectType = Enums.GraphicObjects.ObjectType.HeaterCooler Then ObjectType = Enums.GraphicObjects.ObjectType.Heater

            'Other Properties

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture
            Dim xel As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "AdditionalInfo").SingleOrDefault

            If Not xel Is Nothing Then
                Dim arr As New ArrayList
                For Each xel2 As XElement In xel.Elements
                    arr.Add(XMLSerializer.XMLSerializer.StringToArray2(xel2.Value, ci, Type.GetType("System.Double")))
                Next
                Me.AdditionalInfo = arr.ToArray(Type.GetType("System.Object"))
            End If
            Return True

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                If Not Me.AdditionalInfo Is Nothing Then
                    .Add(New XElement("AdditionalInfo", New XElement("Info", XMLSerializer.XMLSerializer.ArrayToString2(Me.AdditionalInfo(0), ci)),
                                                        New XElement("Info", XMLSerializer.XMLSerializer.ArrayToString2(Me.AdditionalInfo(1), ci)),
                                                        New XElement("Info", XMLSerializer.XMLSerializer.ArrayToString2(Me.AdditionalInfo(2), ci)),
                                                        New XElement("Info", XMLSerializer.XMLSerializer.ArrayToString2(Me.AdditionalInfo(3), ci))))
                End If

                .Add(New XElement("InputConnectors"))

                For Each cp As ConnectionPoint In InputConnectors
                    If cp.IsAttached And Not cp.AttachedConnector Is Nothing Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci)),
                                                                                        New XAttribute("ConnType", cp.Type.ToString),
                                                                                        New XAttribute("AttachedFromObjID", cp.AttachedConnector.AttachedFrom.Name.ToString),
                                                                                        New XAttribute("AttachedFromConnIndex", cp.AttachedConnector.AttachedFromConnectorIndex),
                                                                                        New XAttribute("AttachedFromEnergyConn", cp.AttachedConnector.AttachedFromEnergy.ToString)))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci))))
                    End If
                Next

                .Add(New XElement("OutputConnectors"))

                For Each cp As ConnectionPoint In OutputConnectors
                    If cp.IsAttached And Not cp.AttachedConnector Is Nothing Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci)), _
                                                                                        New XAttribute("ConnType", cp.Type.ToString),
                                                                                        New XAttribute("AttachedToObjID", cp.AttachedConnector.AttachedTo.Name.ToString), _
                                                                                        New XAttribute("AttachedToConnIndex", cp.AttachedConnector.AttachedToConnectorIndex), _
                                                                                        New XAttribute("AttachedToEnergyConn", cp.AttachedConnector.AttachedToEnergy.ToString)))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci))))
                    End If
                Next

                .Add(New XElement("EnergyConnector"))

                If EnergyConnector.IsAttached Then
                    elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", EnergyConnector.IsAttached.ToString(ci)),
                                                                                    New XAttribute("AttachedToObjID", EnergyConnector.AttachedConnector.AttachedTo.Name.ToString), _
                                                                                        New XAttribute("AttachedToConnIndex", EnergyConnector.AttachedConnector.AttachedToConnectorIndex), _
                                                                                        New XAttribute("AttachedToEnergyConn", EnergyConnector.AttachedConnector.AttachedToEnergy.ToString)), _
                                                                                        New XAttribute("AttachedFromObjID", EnergyConnector.AttachedConnector.AttachedFrom.Name.ToString), _
                                                                                        New XAttribute("AttachedFromConnIndex", EnergyConnector.AttachedConnector.AttachedFromConnectorIndex), _
                                                                                        New XAttribute("AttachedFromEnergyConn", EnergyConnector.AttachedConnector.AttachedFromEnergy.ToString))
                Else
                    elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", EnergyConnector.IsAttached.ToString(ci))))
                End If

                .Add(New XElement("SpecialConnectors"))

                For Each cp As ConnectionPoint In SpecialConnectors
                    If cp.IsAttached Then
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci)),
                                                                                        New XAttribute("AttachedToObjID", cp.AttachedConnector.AttachedTo.Name.ToString), _
                                                                                        New XAttribute("AttachedToConnIndex", cp.AttachedConnector.AttachedToConnectorIndex), _
                                                                                        New XAttribute("AttachedToEnergyConn", cp.AttachedConnector.AttachedToEnergy.ToString)), _
                                                                                        New XAttribute("AttachedFromObjID", cp.AttachedConnector.AttachedFrom.Name.ToString), _
                                                                                        New XAttribute("AttachedFromConnIndex", cp.AttachedConnector.AttachedFromConnectorIndex), _
                                                                                        New XAttribute("AttachedFromEnergyConn", cp.AttachedConnector.AttachedFromEnergy.ToString))
                    Else
                        elements(elements.Count - 1).Add(New XElement("Connector", New XAttribute("IsAttached", cp.IsAttached.ToString(ci))))
                    End If
                Next

            End With

            Return elements

        End Function

        Public Property Active As Boolean = True Implements IGraphicObject.Active

        Public Property AdditionalInfo As Object = Nothing Implements IGraphicObject.AdditionalInfo

        Public Property Description As String = "" Implements IGraphicObject.Description

        Public Property FlippedH As Boolean = False Implements IGraphicObject.FlippedH

        Public Property FlippedV As Boolean = False Implements IGraphicObject.FlippedV

        Public Property IsEnergyStream As Boolean = False Implements IGraphicObject.IsEnergyStream

        Public Property ObjectType As ObjectType = Enums.GraphicObjects.ObjectType.Nenhum Implements IGraphicObject.ObjectType

        Public Property Shape As Integer = 0 Implements IGraphicObject.Shape

        Public Property ShapeOverride As ShapeIcon = ShapeIcon.DefaultShape Implements IGraphicObject.ShapeOverride

        Public Property Status As Status = Enums.GraphicObjects.Status.NotCalculated Implements IGraphicObject.Status

        Public Property AutoSize As Boolean = False Implements IGraphicObject.AutoSize

        Public Property Height As Integer = 20 Implements IGraphicObject.Height

        Public Property IsConnector As Boolean = False Implements IGraphicObject.IsConnector

        Public Property Name As String = "" Implements IGraphicObject.Name

        Public Property Tag As String = "" Implements IGraphicObject.Tag

        Public Property Width As Integer = 20 Implements IGraphicObject.Width

        Public Property X As Integer = 0 Implements IGraphicObject.X

        Public Property Y As Integer = 0 Implements IGraphicObject.Y

        Public Property EnergyConnector As IConnectionPoint = New ConnectionPoint() Implements IGraphicObject.EnergyConnector

        Public Property InputConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.InputConnectors

        Public Property OutputConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.OutputConnectors

        Public Property SpecialConnectors As New List(Of IConnectionPoint) Implements IGraphicObject.SpecialConnectors

        Public Property Position As IPoint = New Point() Implements IGraphicObject.Position

        Public Function Clone() As IGraphicObject Implements IGraphicObject.Clone
            Dim instance As GraphicObject = Activator.CreateInstance(Me.GetType)
            instance.LoadData(Me.SaveData)
            Return instance
        End Function

        Public Sub Draw1(surface As Object) Implements IGraphicObject.Draw
            Draw(DirectCast(surface, System.Drawing.Graphics))
        End Sub

        Public Property Selected As Boolean = False Implements IGraphicObject.Selected

        <Xml.Serialization.XmlIgnore> Public Property Owner As ISimulationObject Implements IGraphicObject.Owner

        <Xml.Serialization.XmlIgnore> <NonSerialized> Private _editorform As System.Windows.Forms.Form

        Public Property Editor As Object Implements IGraphicObject.Editor
            Get
                Return _editorform
            End Get
            Set(value As Object)
                _editorform = value
            End Set
        End Property

        Public Function HitTest1(zoomedSelection As Object) As Boolean Implements IGraphicObject.HitTest

            Return HitTest(zoomedSelection)

        End Function

        Public Property DrawOverride As Action(Of Object) Implements IGraphicObject.DrawOverride

        Public Property Extensions As New Dictionary(Of String, IGraphicObjectExtension) Implements IGraphicObject.Extensions

    End Class

End Namespace

