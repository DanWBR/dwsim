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

Namespace GraphicObjects.Tables

    Public Class TableGraphic

        Inherits ShapeGraphic

        Public Enum SortMode
            AsAdded = 0
            NameAsc = 1
            NameDesc = 2
            FirstParentThenNameAsc = 3
            FirstParentThenNameDesc = 4
        End Enum

        Public Property SortingMode As SortMode = SortMode.AsAdded

        Public Property VisibleProperties As New Dictionary(Of String, List(Of String))

        Public Property Padding As Integer = 4

        Public Property TextColor As SKColor = SKColors.Black

        Public Property BorderColor As SKColor = SKColors.Black

        Public Property TextColorDark As SKColor = SKColors.WhiteSmoke

        Public Property BorderColorDark As SKColor = SKColors.WhiteSmoke

        Public Property HeaderText As String = "PROPERTIES TABLE"

        Public Property ClipboardData As String = ""

        <Xml.Serialization.XmlIgnore> Public Property Flowsheet As IFlowsheet

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            elements.Add(New XElement("VisibleProperties"))

            For Each item In VisibleProperties
                Dim xel2 = New XElement("Object", New XAttribute("Value", item.Key))
                elements(elements.Count - 1).Add(xel2)
                For Each item2 In item.Value
                    xel2.Add(New XElement("PropertyID", New XAttribute("Value", item2)))
                Next
            Next

            Return elements

        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "VisibleProperties").SingleOrDefault

            If Not el Is Nothing Then

                VisibleProperties.Clear()

                For Each xel2 As XElement In el.Elements
                    VisibleProperties.Add(xel2.@Value, New List(Of String))
                    For Each xel3 In xel2.Elements
                        VisibleProperties(xel2.@Value).Add(xel3.@Value)
                    Next
                Next

            End If

            Return MyBase.LoadData(data)

        End Function

        Public Sub New()
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_Table
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint, tpaint2 As New SKPaint()
            Dim bpaint As New SKPaint()

            If DrawMode = 0 Then

                With tpaint
                    .TextSize = FontSize
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    If GlobalSettings.Settings.DarkMode Then
                        .Color = TextColorDark
                    Else
                        .Color = TextColor
                    End If
                    .IsStroke = False
                    .Typeface = GetFont()
                    .FakeBoldText = True
                End With

                With tpaint2
                    .TextSize = FontSize
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    If GlobalSettings.Settings.DarkMode Then
                        .Color = TextColorDark
                    Else
                        .Color = TextColor
                    End If
                    .IsStroke = False
                    .Typeface = GetFont()
                End With

                With bpaint
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    If GlobalSettings.Settings.DarkMode Then
                        .Color = BorderColorDark
                    Else
                        .Color = BorderColor
                    End If
                    .IsStroke = True
                    .StrokeWidth = 1
                End With
            Else

                With tpaint
                    .TextSize = FontSize
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = SKColors.Black
                    .IsStroke = False
                    .Typeface = GetFont()
                    .FakeBoldText = True
                End With

                With tpaint2
                    .TextSize = FontSize
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = SKColors.Black
                    .IsStroke = False
                    .Typeface = GetFont()
                End With

                With bpaint
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = SKColors.Black
                    .IsStroke = True
                    .StrokeWidth = 1
                End With
            End If


            Dim maxL0, maxL1, maxL2, maxL3, count As Integer
            Dim maxH As Integer

            'determinar comprimento das colunas e altura das linhas

            maxL0 = 0
            maxL1 = 0
            maxL2 = 0
            maxL3 = 0
            maxH = 0
            count = 1

            Dim size As SKSize

            Dim propstring, propval, propunit As String, pval0 As Object

            Dim toremove As New List(Of String)
            For Each item In VisibleProperties
                If Not Me.Flowsheet.SimulationObjects.ContainsKey(item.Key) Then toremove.Add(item.Key)
            Next

            For Each item In toremove
                VisibleProperties.Remove(item)
            Next

            toremove.Clear()
            toremove = Nothing

            Dim items As New List(Of Tuple(Of String, String))

            Select Case SortingMode
                Case SortMode.AsAdded
                    For Each item In VisibleProperties
                        For Each value In item.Value
                            items.Add(New Tuple(Of String, String)(item.Key, value))
                        Next
                    Next
                Case SortMode.NameAsc
                    For Each item In VisibleProperties
                        For Each value In item.Value
                            items.Add(New Tuple(Of String, String)(item.Key, value))
                        Next
                    Next
                    items = items.OrderBy(Function(i) Flowsheet.GetTranslatedString(i.Item2)).ToList()
                Case SortMode.NameDesc
                    For Each item In VisibleProperties
                        For Each value In item.Value
                            items.Add(New Tuple(Of String, String)(item.Key, value))
                        Next
                    Next
                    items = items.OrderByDescending(Function(i) Flowsheet.GetTranslatedString(i.Item2)).ToList()
                Case SortMode.FirstParentThenNameAsc
                    For Each item In VisibleProperties
                        Dim values = item.Value.OrderBy(Function(v) Flowsheet.GetTranslatedString(v))
                        For Each v In values
                            items.Add(New Tuple(Of String, String)(item.Key, v))
                        Next
                    Next
                    items = items.OrderBy(Function(i) Flowsheet.SimulationObjects(i.Item1).GraphicObject.Tag).ToList()
                Case SortMode.FirstParentThenNameDesc
                    For Each item In VisibleProperties
                        Dim values = item.Value.OrderByDescending(Function(v) Flowsheet.GetTranslatedString(v))
                        For Each v In values
                            items.Add(New Tuple(Of String, String)(item.Key, v))
                        Next
                    Next
                    items = items.OrderByDescending(Function(i) Flowsheet.SimulationObjects(i.Item1).GraphicObject.Tag).ToList()
            End Select

            If items.Count > 0 Then

                For Each item In items

                    size = MeasureString(Me.Flowsheet.SimulationObjects(item.Item1).GraphicObject.Tag, tpaint)

                    If size.Width > maxL0 Then maxL0 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    propstring = Me.Flowsheet.GetTranslatedString(item.Item2)
                    pval0 = Me.Flowsheet.SimulationObjects(item.Item1).GetPropertyValue(item.Item2, Me.Flowsheet.FlowsheetOptions.SelectedUnitSystem)
                    If TypeOf pval0 Is Double Then
                        propval = Convert.ToDouble(pval0).ToString(Me.Flowsheet.FlowsheetOptions.NumberFormat)
                    Else
                        If pval0 IsNot Nothing Then
                            propval = pval0.ToString
                        Else
                            propval = ""
                        End If
                    End If
                    propunit = Me.Flowsheet.SimulationObjects(item.Item1).GetPropertyUnit(item.Item2, Me.Flowsheet.FlowsheetOptions.SelectedUnitSystem)

                    size = MeasureString(Flowsheet.GetTranslatedString(propstring), tpaint)
                    If size.Width > maxL1 Then maxL1 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    size = MeasureString(propval, tpaint2)
                    If size.Width > maxL2 Then maxL2 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    size = MeasureString(propunit, tpaint)
                    If size.Width > maxL3 Then maxL3 = size.Width
                    If size.Height > maxH Then maxH = size.Height

                    count += 1

                Next

                size = MeasureString(Me.HeaderText, tpaint)
                If size.Width > maxL0 + maxL1 + maxL2 + maxL3 Then maxL1 = size.Width - maxL0 - maxL2 - maxL3
                If size.Height > maxH Then maxH = size.Height

                Me.Height = (count) * (maxH + 2 * Me.Padding)
                Me.Width = 10 * Me.Padding + maxL0 + maxL1 + maxL2 + maxL3

                maxL0 = maxL0 + 2 * Padding
                maxL1 = maxL1 + 2 * Padding
                maxL2 = maxL2 + 2 * Padding
                maxL3 = maxL3 + 2 * Padding
                maxH = maxH + 2 * Padding

                'desenhar textos e retangulos

                size = MeasureString("MEASURE", tpaint)

                ClipboardData = HeaderText + vbCrLf

                canvas.DrawText(Me.HeaderText, X + Padding, Y + Padding + size.Height, tpaint)
                Dim n As Integer = 1

                For Each item In items

                    canvas.DrawText(Me.Flowsheet.SimulationObjects(item.Item1).GraphicObject.Tag, X + Padding, Y + n * maxH + Padding + size.Height, tpaint)

                    propstring = Me.Flowsheet.GetTranslatedString(item.Item2)
                    Try
                        pval0 = Me.Flowsheet.SimulationObjects(item.Item1).GetPropertyValue(item.Item2, Me.Flowsheet.FlowsheetOptions.SelectedUnitSystem)
                    Catch ex As Exception
                        pval0 = ""
                    End Try
                    If TypeOf pval0 Is Double Then
                        propval = Convert.ToDouble(pval0).ToString(Me.Flowsheet.FlowsheetOptions.NumberFormat)
                    Else
                        If pval0 IsNot Nothing Then
                            propval = pval0.ToString
                        Else
                            propval = ""
                        End If
                    End If
                    propunit = Me.Flowsheet.SimulationObjects(item.Item1).GetPropertyUnit(item.Item2, Me.Flowsheet.FlowsheetOptions.SelectedUnitSystem)

                    canvas.DrawText(propstring, X + maxL0 + Padding, Y + n * maxH + Padding + size.Height, tpaint)
                    canvas.DrawText(propval, (maxL2 - MeasureString(propval, tpaint).Width) + X + maxL0 + maxL1, Y + n * maxH + Padding + size.Height, tpaint2)
                    canvas.DrawText(propunit, X + maxL0 + maxL1 + maxL2 + 2 * Padding, Y + n * maxH + Padding + size.Height, tpaint)
                    canvas.DrawLine(X, Y + n * maxH, X + Width, Y + n * maxH, bpaint)

                    ClipboardData += Me.Flowsheet.SimulationObjects(item.Item1).GraphicObject.Tag + vbTab + propstring + vbTab + propval + vbTab + propunit + vbCrLf

                    n += 1

                Next

                canvas.DrawRect(New SKRect(X, Y, X + Width, Y + Height), bpaint)
                canvas.DrawLine(X + maxL0, Y + maxH, X + maxL0, Y + Height, bpaint)
                canvas.DrawLine(X + maxL0 + maxL1, Y + maxH, X + maxL0 + maxL1, Y + Height, bpaint)
                canvas.DrawLine(X + maxL0 + maxL1 + maxL2 + Padding, Y + maxH, X + maxL0 + maxL1 + maxL2 + Padding, Y + Height, bpaint)

            Else

                size = MeasureString(Flowsheet.GetTranslatedString("DoubleClickToEdit"), tpaint)

                Me.Width = 20 + size.Width
                Me.Height = 80 + size.Height

                canvas.DrawText(Flowsheet.GetTranslatedString("DoubleClickToEdit"), X + 10, Y + 40, tpaint)

                canvas.DrawRect(New SKRect(X, Y, X + Width, Y + Height), bpaint)

            End If

            tpaint.Dispose()
            tpaint2.Dispose()
            bpaint.Dispose()

            tpaint = Nothing
            tpaint2 = Nothing
            bpaint = Nothing

        End Sub

    End Class

End Namespace
