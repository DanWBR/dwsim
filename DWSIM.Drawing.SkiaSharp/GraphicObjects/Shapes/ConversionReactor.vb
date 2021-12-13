﻿Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class ConversionReactorGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.RCT_Conversion
            Me.Description = "Conversion Reactor"
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
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize)
            Me.New(New SKPoint(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height))
        End Sub

#End Region

        Public Overrides Sub PositionConnectors()

            CreateConnectors(0, 0)

        End Sub

        Public Overrides Sub CreateConnectors(InCount As Integer, OutCount As Integer)

            Dim myIC1 As New ConnectionPoint
            myIC1.Position = New Point(X + 0.125 * Width, Y + 0.5 * Height)
            myIC1.Type = ConType.ConIn

            Dim myIC2 As New ConnectionPoint
            myIC2.Position = New Point(X + 0.125 * Width, Y + 0.7 * Height)
            myIC2.Type = ConType.ConEn
            myIC2.Direction = ConDir.Up

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y)
            myOC1.Type = ConType.ConOut

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(X + Width, Y + Height)
            myOC2.Type = ConType.ConOut

            Dim myOC3 As New ConnectionPoint
            myOC3.Position = New Point(X + 0.5 * Width, Y + Height)
            myOC3.Type = ConType.ConEn
            myOC3.Direction = ConDir.Down

            With InputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + (0.25 - 0.14) * Width, Y + 0.5 * Height)
                    .Item(1).Position = New Point(X + 0.25 * Width, Y + Height)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If

                .Item(0).ConnectorName = "Inlet"
                .Item(1).ConnectorName = "Energy Stream" 'only legacy. not in use anymore.

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + (0.75 + 0.14) * Width, Y + (0.1 + 0.14 / 2) * Height)
                    .Item(1).Position = New Point(X + (0.75 + 0.14) * Width, Y + (0.9 - 0.14 / 2) * Height)
                    .Item(2).Position = New Point(X + 0.5 * Width, Y + Height)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                    .Add(myOC3)
                End If

                .Item(0).ConnectorName = "Vapor Outlet"
                .Item(1).ConnectorName = "Liquid Outlet"
                .Item(2).ConnectorName = "Energy Stream"

            End With

            With Me.EnergyConnector
                .Position = New Point(X + 0.5 * Width, Y + Height)
                .Direction = ConDir.Up
                .Active = False
            End With

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            DrawReactor(g, "C")

        End Sub

    End Class

End Namespace