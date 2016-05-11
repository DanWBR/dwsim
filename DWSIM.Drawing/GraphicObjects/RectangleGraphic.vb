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
Imports System.Drawing.Drawing2D

Namespace GraphicObjects

    <Serializable()> _
    Public Class RectangleGraphic

        Inherits GraphicObject

#Region "Constructors"

        Public Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Rectangle
        End Sub

        Public Sub New(ByVal graphicPosition As Point, ByVal text As String)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Text = text
        End Sub

#End Region

        Public Property Font() As Font = System.Drawing.SystemFonts.DefaultFont
        Public Property Text() As String = ""
        Public Property FontColor() As System.Drawing.Color = Drawing.Color.SteelBlue
        Public Property TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default
        Public Property LineWidth As Single = 3
        Public Property GradientMode As Boolean = False
        Public Property LineColor As Color = Color.SteelBlue
        Public Property Fill() As Boolean = False
        Public Property FillColor() As Color = Color.LightSteelBlue
        Public Property GradientColor1 As Color = Color.LightSteelBlue
        Public Property GradientColor2 As Color = Color.SteelBlue
        Public Property RoundEdges As Boolean = True
        Public Property Opacity As Integer = 100

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            Dim gContainer As System.Drawing.Drawing2D.GraphicsContainer
            Dim myMatrix As Drawing2D.Matrix

            gContainer = g.BeginContainer()

            myMatrix = g.Transform()

            If m_Rotation <> 0 Then
                myMatrix.RotateAt(m_Rotation, New PointF(X, Y), Drawing.Drawing2D.MatrixOrder.Append)
                g.Transform = myMatrix
            End If

            If Not Me.TextRenderStyle = -1 Then g.TextRenderingHint = Me.TextRenderStyle

            Dim rect As New System.Drawing.Rectangle(X, Y, Me.Width, Me.Height)

            Dim size = g.MeasureString(Text, Font, 1000)

            Dim pos As New PointF(X + (Width - size.Width) / 2, Y + Height - size.Height - 4)

            Me.LineColor = Color.FromArgb(Me.Opacity, Me.LineColor)
            'Me.FontColor = Color.FromArgb(Me.Opacity, Me.FontColor)
            Me.FillColor = Color.FromArgb(Me.Opacity, Me.FillColor)
            Me.GradientColor1 = Color.FromArgb(Me.Opacity, Me.GradientColor1)
            Me.GradientColor2 = Color.FromArgb(Me.Opacity, Me.GradientColor2)

            'draw borders

            If RoundEdges Then
                Me.DrawRoundRect(g, New Pen(Me.LineColor, Me.LineWidth), X, Y, Width, Height, 3, Brushes.Transparent)
            Else
                g.DrawRectangle(New Pen(Me.LineColor, Me.LineWidth), rect)
            End If

            'draw actual rectangle

            If GradientMode Then
                If RoundEdges Then
                    Me.DrawRoundRect(g, New Pen(Brushes.Transparent, 1), X, Y, Width, Height, 3, New LinearGradientBrush(rect, Me.GradientColor1, Me.GradientColor2, LinearGradientMode.Vertical))
                Else
                    g.FillRectangle(New LinearGradientBrush(rect, Me.GradientColor1, Me.GradientColor2, LinearGradientMode.Vertical), rect)
                End If
            Else
                If RoundEdges Then
                    Me.DrawRoundRect(g, New Pen(Brushes.Transparent, 1), X, Y, Width, Height, 3, New SolidBrush(Me.FillColor))
                Else
                    g.FillRectangle(New SolidBrush(Me.FillColor), rect)
                End If
            End If

            'draw text

            g.DrawString(Text, Font, New SolidBrush(FontColor), pos)

            g.EndContainer(gContainer)

        End Sub

    End Class

End Namespace