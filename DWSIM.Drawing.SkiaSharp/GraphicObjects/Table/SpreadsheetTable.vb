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

#End Region

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = TextColor
                .IsStroke = False
            End With

            Dim bpaint As New SKPaint()

            With bpaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = BorderColor
                .IsStroke = True
                .StrokeWidth = LineWidth
            End With

            Padding = 4

            Dim maxW As New List(Of Integer)

            'find number of rows and columns by range

            If SpreadsheetCellRange <> "" Then

                SpreadsheetData = Flowsheet.GetSpreadsheetData(SpreadsheetCellRange)

                'determinar comprimento das colunas e altura das linhas

                Dim i, j, k, itemheight, n, m, leftmargin As Integer
                Dim size As SKSize

                k = 0
                For j = 0 To SpreadsheetData(0).Count - 1
                    maxW.Add(1)
                    For i = 0 To SpreadsheetData.Count - 1
                        size = MeasureString(SpreadsheetData(i)(j), tpaint)
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

                n = 0
                leftmargin = 0
                For j = 0 To SpreadsheetData(0).Count - 1
                    m = 0
                    For i = 0 To SpreadsheetData.Count - 1
                        canvas.DrawText(SpreadsheetData(i)(j), X + Padding + leftmargin, Y + Padding + m * itemheight + size.Height, tpaint)
                        If i < SpreadsheetData.Count - 1 Then canvas.DrawLine(X + leftmargin, Y + (m + 1) * itemheight, X + leftmargin + maxW(n), Y + (m + 1) * itemheight, bpaint)
                        m += 1
                    Next
                    leftmargin += maxW(n)
                    If j < SpreadsheetData(0).Count - 1 Then canvas.DrawLine(X + leftmargin, Y, X + leftmargin, Y + (SpreadsheetData.Count) * itemheight, bpaint)
                    n += 1
                Next

                canvas.DrawRect(GetRect(Me.X, Me.Y, Me.Width, Me.Height), bpaint)

            Else

                Dim Size = MeasureString("Double-click to edit", tpaint)
                canvas.DrawText("Double-click to edit", X + 10, Y + 40, tpaint)

                Me.Width = 20 + Size.Width
                Me.Height = 80 + Size.Height

                canvas.DrawRect(GetRect(X, Y, Width, Height), bpaint)

            End If

        End Sub

    End Class

End Namespace


