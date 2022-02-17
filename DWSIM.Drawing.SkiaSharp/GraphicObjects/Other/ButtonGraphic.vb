'    Button Graphic Object
'    Copyright 2021 Daniel Wagner O. de Medeiros
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

Namespace GraphicObjects.Shapes

    <Serializable()>
    Public Class ButtonGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Button
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal text As String)
            Me.New()
            Height = 30
            Width = 60
            Me.SetPosition(graphicPosition)
            Me.Text = text
        End Sub

#End Region

        Public Property Text As String = "CLICK ME"

        Public Property Pressed As Boolean = False

        Public Property SelectedScript As String = ""

        Public Overrides Sub Draw(ByVal g As Object)

            Dim sf = GlobalSettings.Settings.UIScalingFactor

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.White
                .IsStroke = False
                .Typeface = BoldTypeFace
            End With

            Dim bpaint0, bpaint1 As New SKPaint()

            With bpaint0
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Color = SKColors.SteelBlue
            End With

            With bpaint1
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Color = SKColors.LightSteelBlue
            End With

            Dim rect As New SKRect(X, Y, X + Width, Y + Height)

            Dim size = MeasureString(Text, tpaint)

            Dim pos As New SKPoint(X + (Width - size.Width) / 2, Y + Height - size.Height - 4)

            Width = size.Width + 10
            Height = size.Height + 10

            'draw actual rectangle

            If Pressed Then
                Me.DrawRoundRect(g, X - 2, Y - 2, Width + 4, Height + 4, 2, bpaint0)
                Me.DrawRoundRect(g, X, Y, Width, Height, 2, bpaint1)
            Else
                Me.DrawRoundRect(g, X - 2, Y - 2, Width + 4, Height + 4, 2, bpaint1)
                Me.DrawRoundRect(g, X, Y, Width, Height, 2, bpaint0)
            End If

            'draw text

            Dim strx As Single = X + (Me.Width - tpaint.MeasureText(Text)) / 2
            Dim stry = Y + Height - (Me.Height - size.Height) / 2

            canvas.DrawText(Text, New SKPoint(strx, stry), tpaint)

        End Sub

        Public Sub Run()

            If SelectedScript <> "" Then
                Try
                    Flowsheet?.RunScript(SelectedScript)
                Catch ex As Exception
                    Flowsheet.ShowMessage(String.Format("Error running script '{0}': {1}", SelectedScript, ex.Message), Interfaces.IFlowsheet.MessageType.GeneralError)
                End Try
            End If

        End Sub


        Public Overrides Function SaveData() As List(Of XElement)
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

    End Class

End Namespace