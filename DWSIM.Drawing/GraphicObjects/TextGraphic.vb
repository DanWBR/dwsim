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

Namespace GraphicObjects

    <Serializable()> _
    Public Class TextGraphic
        Inherits GraphicObject

        Protected m_Font As Font = System.Drawing.SystemFonts.DefaultFont
        Protected m_Text As String = ""
        Protected m_Color As Color = Drawing.Color.Black
        Protected m_TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default

#Region "Constructors"

        Public Sub New()
            MyBase.New()
            Me.ObjectType = GraphicObjects.ObjectType.GO_Text
        End Sub

        Public Sub New(ByVal graphicPosition As Point, ByVal text As String, _
                ByVal textFont As Font, ByVal textColor As Color)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Font = textFont
            Me.Text = text
            Me.Color = textColor
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, _
            ByVal text As String, ByVal textFont As Font, ByVal textColor As Color)
            Me.New(New Point(posX, posY), text, textFont, textColor)
        End Sub


        Public Sub New(ByVal graphicPosition As Point, _
                ByVal text As String, ByVal textFont As Font, _
                ByVal textColor As Color, ByVal Rotation As Single)
            Me.New(graphicPosition, text, textFont, textColor)
            Me.Rotation = Rotation
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, _
                ByVal text As String, _
                ByVal textFont As Font, _
                ByVal textColor As Color, ByVal Rotation As Single)
            Me.New(New Point(posX, posY), text, textFont, textColor, Rotation)
        End Sub
#End Region

        Public Property Font() As Font
            Get
                Return m_Font
            End Get
            Set(ByVal Value As Font)
                m_Font = Value
            End Set
        End Property

        Public Property Text() As String
            Get
                Return m_Text
            End Get
            Set(ByVal Value As String)
                m_Text = Value
            End Set
        End Property

        Public Property Color() As System.Drawing.Color
            Get
                Return m_Color
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color = Value
            End Set
        End Property

        Public Property TextRenderStyle() As Drawing2D.SmoothingMode
            Get
                Return m_TextRenderStyle
            End Get
            Set(ByVal value As Drawing2D.SmoothingMode)
                m_TextRenderStyle = value
            End Set
        End Property

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
            If Me.AutoSize Then
                Dim mySize As SizeF = g.MeasureString(m_Text, m_Font)
                Me.Width = Convert.ToInt32(mySize.Width)
                Me.Height = Convert.ToInt32(mySize.Height)
                g.DrawString(m_Text, m_Font, New SolidBrush(m_Color), X, Y)
            Else
                Dim rect As New System.Drawing.RectangleF(X, Y, Me.Width, Me.Height)
                g.DrawString(m_Text, m_Font, New SolidBrush(m_Color), rect)
            End If
            g.EndContainer(gContainer)
        End Sub

        Public Sub SetText(ByVal stext As String)
            m_Text = stext
        End Sub

    End Class

End Namespace