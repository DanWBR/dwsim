'    Rectangle Graphic Object
'    Copyright 2018 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports DWSIM.Interfaces

Namespace GraphicObjects.Shapes

    <Serializable()>
    Public Class RectangleGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Rectangle
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal text As String)
            Me.New()
            Height = 200
            Width = 200
            Me.SetPosition(graphicPosition)
            Me.Text = text
        End Sub

#End Region

        Public Property Text() As String = ""
        Public Property FontColor() As SKColor = SKColors.SteelBlue
        Public Property RoundEdges As Boolean = True
        Public Property Opacity As Integer = 100

        Public Overrides Sub Draw(ByVal g As Object)

            Dim sf = GlobalSettings.Settings.UIScalingFactor

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = FontColor
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim bpaint0 As New SKPaint()

            With bpaint0
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Color = FillColor.WithAlpha(50)
            End With

            Dim bpaint As New SKPaint()

            With bpaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Shader = SKShader.CreateLinearGradient(New SKPoint(X, Y), New SKPoint(X, Y + Height),
                                                        New SKColor() {GradientColor1.WithAlpha(Opacity), GradientColor2.WithAlpha(Opacity)},
                                                        New Single() {0, 1}, SKShaderTileMode.Clamp)
            End With

            Dim bpaint2 As New SKPaint()

            With bpaint2
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = True
                .Color = GradientColor2.WithAlpha(Opacity)
                .StrokeWidth = LineWidth
            End With

            Dim rect As New SKRect(X, Y, X + Width, Y + Height)

            Dim size = MeasureString(Text, tpaint)

            Dim pos As New SKPoint(X + (Width - size.Width) / 2, Y + Height - size.Height - 4)

            'draw actual rectangle

            If GradientMode Then
                If RoundEdges Then
                    Me.DrawRoundRect(g, X, Y, Width, Height, 4, bpaint)
                    Me.DrawRoundRect(g, X, Y, Width, Height, 4, bpaint2)
                Else
                    canvas.DrawRect(rect, bpaint)
                    canvas.DrawRect(rect, bpaint2)
                End If
            Else
                If RoundEdges Then
                    Me.DrawRoundRect(g, X, Y, Width, Height, 4, bpaint0)
                    Me.DrawRoundRect(g, X, Y, Width, Height, 4, bpaint2)
                Else
                    canvas.DrawRect(rect, bpaint0)
                    canvas.DrawRect(rect, bpaint2)
                End If
            End If

            'draw text

            canvas.DrawText(Text, pos, tpaint)

        End Sub

        Public Overrides Function SaveData() As List(Of XElement)
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

    End Class

End Namespace