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
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Tables

    <Serializable()> Public Class SpreadsheetTableGraphic

        Inherits ShapeGraphic

        <Xml.Serialization.XmlIgnore> Public Property Flowsheet As Interfaces.IFlowsheet

        Public Property ClipboardData As String = ""

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            Return elements

        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Return MyBase.LoadData(data)

        End Function

#Region "Constructors"

        Public Sub New(ByVal graphicPosition As Point)
            Me.New()
            Me.SetPosition(graphicPosition.X, graphicPosition.Y)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New Point(posX, posY))
        End Sub

        Public Sub New()
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable
        End Sub

#End Region

#Region "Properties"

        Public Property Padding As Integer = 4

        Public Property SpreadsheetCellRange As String = ""

        <Xml.Serialization.XmlIgnore> Public Property SpreadsheetData As List(Of String())

        Public Property TextColor As SKColor = SKColors.Black

        Public Property BorderColor As SKColor = SKColors.Black

        Public Property TextColorDark As SKColor = SKColors.WhiteSmoke

        Public Property BorderColorDark As SKColor = SKColors.WhiteSmoke

#End Region

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()
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
                End With

                With bpaint
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    If GlobalSettings.Settings.DarkMode Then
                        .Color = BorderColorDark
                    Else
                        .Color = BorderColor
                    End If
                    .IsStroke = True
                    .StrokeWidth = LineWidth
                End With

            Else
                With tpaint
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
                    .StrokeWidth = LineWidth
                End With

            End If
            Dim maxW As New List(Of Integer)

            'find number of rows and columns by range

            If SpreadsheetCellRange <> "" Then

                Try

                    SpreadsheetData = Flowsheet.GetSpreadsheetData(SpreadsheetCellRange)

                    Dim formats = Flowsheet.GetSpreadsheetFormat(SpreadsheetCellRange)

                    'determinar comprimento das colunas e altura das linhas

                    Dim i, j, k, itemheight, itemwidth, n, m, leftmargin As Integer
                    Dim size As SKSize

                    k = 0
                    For j = 0 To SpreadsheetData(0).Count - 1
                        maxW.Add(1)
                        For i = 0 To SpreadsheetData.Count - 1
                            If formats IsNot Nothing Then
                                If formats(i)(j) <> "" Then
                                    If Double.TryParse(SpreadsheetData(i)(j), New Double) Then
                                        size = MeasureString(Convert.ToDouble(SpreadsheetData(i)(j)).ToString(formats(i)(j)), tpaint)
                                    Else
                                        size = MeasureString(SpreadsheetData(i)(j), tpaint)
                                    End If
                                Else
                                    size = MeasureString(SpreadsheetData(i)(j), tpaint)
                                End If
                            Else
                                size = MeasureString(SpreadsheetData(i)(j), tpaint)
                            End If
                            If size.Width > maxW(k) Then maxW(k) = size.Width
                        Next
                        maxW(k) += 4 * Padding
                        k += 1
                    Next

                    itemheight = MeasureString("AAA", tpaint).Height + 2 * Me.Padding

                    Me.Height = (SpreadsheetData.Count) * itemheight
                    Me.Width = maxW.Sum

                    'Dim rect As SKRect = GetRect(X, Y, Width, Height)
                    'canvas.DrawRect(rect, GetPaint(SKColors.White))

                    size = MeasureString("MEASURE", tpaint)

                    ClipboardData = ""

                    n = 0
                    leftmargin = 0
                    For j = 0 To SpreadsheetData(0).Count - 1
                        m = 0
                        For i = 0 To SpreadsheetData.Count - 1

                            If formats IsNot Nothing Then
                                If formats(i)(j) <> "" Then
                                    If Double.TryParse(SpreadsheetData(i)(j), New Double) Then
                                        Dim val = Convert.ToDouble(SpreadsheetData(i)(j)).ToString(formats(i)(j))
                                        itemwidth = MeasureString(val, tpaint).Width + 2 * Padding
                                        canvas.DrawText(val, X + Padding + leftmargin + maxW(n) - itemwidth, Y + Padding + m * itemheight + size.Height, tpaint)
                                    Else
                                        canvas.DrawText(SpreadsheetData(i)(j), X + Padding + leftmargin, Y + Padding + m * itemheight + size.Height, tpaint)
                                    End If
                                Else
                                    canvas.DrawText(SpreadsheetData(i)(j), X + Padding + leftmargin, Y + Padding + m * itemheight + size.Height, tpaint)
                                End If
                            Else
                                canvas.DrawText(SpreadsheetData(i)(j), X + Padding + leftmargin, Y + Padding + m * itemheight + size.Height, tpaint)
                            End If
                            ClipboardData += SpreadsheetData(i)(j) + vbTab
                            If i < SpreadsheetData.Count - 1 Then canvas.DrawLine(X + leftmargin, Y + (m + 1) * itemheight, X + leftmargin + maxW(n), Y + (m + 1) * itemheight, bpaint)
                            m += 1
                        Next
                        ClipboardData += vbCrLf
                        leftmargin += maxW(n)
                        If j < SpreadsheetData(0).Count - 1 Then canvas.DrawLine(X + leftmargin, Y, X + leftmargin, Y + (SpreadsheetData.Count) * itemheight, bpaint)
                        n += 1
                    Next

                    canvas.DrawRect(GetRect(Me.X, Me.Y, Me.Width, Me.Height), bpaint)

                Catch ex As Exception

                    Dim Size = MeasureString("Error: " + ex.Message.ToString, tpaint)
                    canvas.DrawText("Error: " + ex.Message.ToString, X + 10, Y + 40, tpaint)

                    Me.Width = 20 + Size.Width
                    Me.Height = 80 + Size.Height

                    canvas.DrawRect(GetRect(X, Y, Width, Height), bpaint)

                End Try

            Else

                Dim Size = MeasureString("Double-click to edit", tpaint)
                canvas.DrawText("Double-click to edit", X + 10, Y + 40, tpaint)

                Me.Width = 20 + Size.Width
                Me.Height = 80 + Size.Height

                canvas.DrawRect(GetRect(X, Y, Width, Height), bpaint)

            End If

            tpaint.Dispose()
            bpaint.Dispose()

            tpaint = Nothing
            bpaint = Nothing

        End Sub

    End Class

End Namespace


