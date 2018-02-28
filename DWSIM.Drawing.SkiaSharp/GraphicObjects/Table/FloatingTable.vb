'    Table Graphic Object
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports System.Linq
Imports DWSIM.Interfaces
Imports System.Xml
Imports DWSIM.SharedClasses.Extras
Imports DWSIM.Interfaces.Enums
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Tables

    <Serializable()> Public Class FloatingTableGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_FloatingTable
        End Sub

        Public Sub New(ByRef owner As ISimulationObject)
            Me.Owner = owner
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_FloatingTable
        End Sub

        Public Sub New(ByRef owner As ISimulationObject, ByVal graphicPosition As Point)
            Me.New(owner)
            Me.SetPosition(graphicPosition.X, graphicPosition.Y)
        End Sub

        Public Sub New(ByRef owner As ISimulationObject, ByVal posX As Integer, ByVal posY As Integer)
            Me.New(owner, New Point(posX, posY))
        End Sub

#End Region

        Property Padding As Integer = 4

        Public Property TextColor As SKColor = SKColors.Black

        Public Property BorderColor As SKColor = SKColors.Black

        Public Property HeaderText() As String = ""

        Public Overrides Sub Draw(ByVal g As Object)

            Dim zoom As Single = AdditionalInfo

            If zoom = 0 Then Exit Sub

            Padding = 6 / zoom

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = (FontSize + 3) / zoom
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.White
                .IsStroke = False
            End With

            Dim tbpaint As New SKPaint()

            With tbpaint
                .TextSize = (FontSize + 3) / zoom
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.White
                .IsStroke = False
                .Typeface = SKTypeface.FromTypeface(DefaultTypeFace, SKTypefaceStyle.Bold)
            End With

            Dim bpaint As New SKPaint()

            With bpaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.SteelBlue
                .IsStroke = False
                .StrokeWidth = LineWidth
            End With

            Dim spaint As New SKPaint()

            With spaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = SKColors.LightGray.WithAlpha(100)
                .IsStroke = False
            End With

            If Not Me.Owner Is Nothing Then

                If Me.Owner.GetFlowsheet IsNot Nothing Then

                    Dim maxL1, maxL2, maxL3, count As Integer
                    Dim maxH As Integer

                    'determinar comprimento das colunas e altura das linhas

                    maxL1 = 0
                    maxL2 = 0
                    maxL3 = 0
                    maxH = 0
                    count = 1

                    Dim size, size2 As SKSize

                    size = MeasureString(Me.Owner.GraphicObject.Tag.ToUpper, tpaint)
                    If size.Width > maxL1 Then maxL1 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    Dim fs = Owner.GetFlowsheet
                    Dim props As New List(Of String)(fs.FlowsheetOptions.VisibleProperties(Owner.GetType.Name))
                    props.AddRange(DirectCast(Owner.ExtraProperties, IDictionary(Of String, Object)).Keys.ToArray)

                    If Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.CapeOpenUO Then props = Owner.GetProperties(PropertyType.ALL).ToList

                    Dim propstoremove As New List(Of String)

                    If Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                        For Each p In props
                            If Owner.GetPropertyValue(p).Equals(Double.MinValue) Then
                                propstoremove.Add(p)
                            End If
                        Next
                        For i As Integer = 0 To propstoremove.Count - 1
                            props.Remove(propstoremove(i))
                        Next
                    End If

                    Dim propstring, propval, propunit As String, pval0 As Object

                    For Each prop In props
                        propstring = Owner.GetFlowsheet.GetTranslatedString(prop)
                        pval0 = Owner.GetPropertyValue(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        If pval0 Is Nothing Then Exit For
                        If TypeOf pval0 Is Double Then
                            propval = Convert.ToDouble(pval0).ToString(Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                        Else
                            propval = pval0.ToString
                        End If
                        propunit = Owner.GetPropertyUnit(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        size = MeasureString(propstring, tpaint)
                        If size.Width > maxL1 Then maxL1 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        size = MeasureString(propval, tpaint)
                        If size.Width > maxL2 Then maxL2 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        size = MeasureString(propunit, tpaint)
                        If size.Width > maxL3 Then maxL3 = size.Width
                        If size.Height > maxH Then maxH = size.Height
                        count += 1
                    Next

                    'If Not Me.AdditionalInfo Is Nothing Then Me.Padding = 3 / Me.AdditionalInfo

                    If maxH = 0 Then maxH = 20

                    Me.Height = (count + 1) * (maxH + 2 * Me.Padding)
                    size = MeasureString(Me.HeaderText, tpaint)
                    size2 = MeasureString(Owner.GetFlowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), tpaint)

                    If size.Width > size2.Width Then
                        If size.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
                            Me.Width = 2 * Me.Padding + size.Width
                        Else
                            Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
                        End If
                    Else
                        If size2.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
                            Me.Width = 2 * Me.Padding + size2.Width
                        Else
                            Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
                        End If
                    End If

                    Me.Width += 6

                    maxL1 = maxL1 + 2 * Padding
                    maxL2 = maxL2 + 2 * Padding
                    maxL3 = maxL3 + 2 * Padding

                    maxH = maxH + 2 * Padding

                    'draw shadow

                    Me.DrawRoundRect(g, X + 4 / zoom, Y + 4 / zoom, Width, Height, 5 / zoom, spaint)
                    Dim rect0 As SKRect = GetRect(X + 4 / zoom, Y + 4 / zoom, Width, Height)

                    Dim rect As SKRect = GetRect(X, Y, Width, Height)

                    DrawRoundRect(g, X, Y, Width, Height, 5 / zoom, bpaint)

                    size = MeasureString("MEASURE", tpaint)

                    'desenhar textos e retangulos
                    canvas.DrawText(Me.Owner.GraphicObject.Tag.ToUpper, X + Padding + 3, Y + Padding + size.Height, tbpaint)
                    canvas.DrawText(Owner.GetFlowsheet.GetTranslatedString(Me.Owner.GraphicObject.Description), X + Padding + 3, Y + maxH + size.Height, tbpaint)
                    Dim n As Integer = 1
                    For Each prop In props
                        propstring = Owner.GetFlowsheet.GetTranslatedString(prop)
                        pval0 = Owner.GetPropertyValue(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        If pval0 Is Nothing Then Exit For
                        If TypeOf pval0 Is Double Then
                            propval = Convert.ToDouble(pval0).ToString(Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                        Else
                            propval = pval0.ToString
                        End If
                        propunit = Owner.GetPropertyUnit(prop, Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                        canvas.DrawText(propstring, X + Padding + 3, Y + (n + 1) * maxH + Padding + size.Height, tpaint)
                        canvas.DrawText(propval, (maxL2 - MeasureString(propval, tpaint).Width) + X + maxL1 + 3, Y + (n + 1) * maxH + Padding + size.Height, tpaint)
                        canvas.DrawText(propunit, X + maxL1 + maxL2 + Padding + 3, Y + (n + 1) * maxH + Padding + size.Height, tpaint)
                        n += 1
                    Next

                    props.Clear()
                    props = Nothing

                End If

            End If

        End Sub

    End Class

End Namespace

