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

    <Serializable()> Public Class MasterTableGraphic

        Inherits ShapeGraphic

        Protected m_objectfamily As Enums.GraphicObjects.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream

        Protected m_objectlist As Dictionary(Of String, Boolean)
        Protected m_propertylist As Dictionary(Of String, Boolean)
        Protected m_sortby As String = ""
        Protected m_sortableitems As ArrayList
        Protected m_sortedlist As List(Of String)

        Protected m_items As Dictionary(Of String, List(Of NodeItem))

        <Xml.Serialization.XmlIgnore> Public Property Flowsheet As Interfaces.IFlowsheet

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Objects").Elements.ToList
                m_objectlist.Add(XmlConvert.DecodeName(xel.Name.LocalName), xel.Value)
            Next
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Properties").Elements.ToList
                m_propertylist.Add(XmlConvert.DecodeName(xel.Name.LocalName), xel.Value)
            Next
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "SortableItems").Elements.ToList
                m_sortableitems.Add(xel.Value)
            Next
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)


            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            With elements

                .Add(New XElement("Objects"))

                For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                    elements(elements.Count - 1).Add(New XElement(XmlConvert.EncodeName(kvp.Key), kvp.Value))
                Next

                .Add(New XElement("Properties"))

                For Each kvp As KeyValuePair(Of String, Boolean) In m_propertylist
                    elements(elements.Count - 1).Add(New XElement(XmlConvert.EncodeName(kvp.Key), kvp.Value))
                Next

                .Add(New XElement("SortableItems"))

                For Each item As String In m_sortableitems
                    elements(elements.Count - 1).Add(New XElement("SortableItem", item))
                Next

                .Add(New XElement("SortedList"))

                For Each item As String In m_sortedlist
                    elements(elements.Count - 1).Add(New XElement("Object", item))
                Next

            End With

            Return elements

        End Function

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = Enums.GraphicObjects.ObjectType.GO_MasterTable
            m_objectlist = New Dictionary(Of String, Boolean)
            m_propertylist = New Dictionary(Of String, Boolean)
            m_sortableitems = New ArrayList
            m_sortedlist = New List(Of String)
        End Sub

        Public Sub New(ByVal graphicPosition As Point)
            Me.New()
            'Me.SetPosition(graphicPosition.ToSKPoint)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New Point(posX, posY))
        End Sub

#End Region

#Region "Properties"

        Property Padding As Integer = 4

        Public Property TextColor As SKColor = SKColors.Black

        Public Property BorderColor As SKColor = SKColors.Black

        Public Property SortBy() As String
            Get
                If m_sortby Is Nothing Then m_sortby = "Name | DESC"
                Return m_sortby
            End Get
            Set(ByVal value As String)
                m_sortby = value
            End Set
        End Property

        Public ReadOnly Property SortableItems() As String()
            Get
                If m_sortableitems Is Nothing Then m_sortableitems = New ArrayList
                m_sortableitems.Clear()
                m_sortableitems.Add("Name | ASC")
                m_sortableitems.Add("Name | DESC")
                For Each kvp As KeyValuePair(Of String, Boolean) In m_propertylist
                    If kvp.Value Then
                        m_sortableitems.Add(Flowsheet.GetTranslatedString(kvp.Key) & " | ASC")
                        m_sortableitems.Add(Flowsheet.GetTranslatedString(kvp.Key) & " | DESC")
                    End If
                Next
                m_sortableitems.Add("Custom")
                Return m_sortableitems.ToArray(Type.GetType("System.String"))
            End Get
        End Property

        Public ReadOnly Property PropertyList() As Dictionary(Of String, Boolean)
            Get
                Return m_propertylist
            End Get
        End Property

        Public ReadOnly Property ObjectList() As Dictionary(Of String, Boolean)
            Get
                Return m_objectlist
            End Get
        End Property

        Public Property SortedList() As List(Of String)
            Get
                Return m_sortedlist
            End Get
            Set(ByVal value As List(Of String))
                m_sortedlist = value
            End Set
        End Property

        Public Property ObjectFamily() As Enums.GraphicObjects.ObjectType
            Get
                Return m_objectfamily
            End Get
            Set(ByVal value As Enums.GraphicObjects.ObjectType)
                m_objectfamily = value
                m_objectlist.Clear()
                m_propertylist.Clear()
                If m_sortby = "" Then m_sortby = "Name | DESC"
            End Set
        End Property

        Public Property HeaderText() As String = ""

#End Region

        Public Sub Update()

            Dim su As IUnitsOfMeasure = Flowsheet.FlowsheetOptions.SelectedUnitSystem
            Dim nf As String = Flowsheet.FlowsheetOptions.NumberFormat

            m_items = New Dictionary(Of String, List(Of NodeItem))

            Dim objectstoremove, propstoremove As New List(Of String)

            For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                If Flowsheet.GetFlowsheetSimulationObject(kvp.Key) Is Nothing Then
                    objectstoremove.Add(kvp.Key)
                End If
            Next

            For i As Integer = 0 To objectstoremove.Count - 1
                m_objectlist.Remove(objectstoremove(i))
            Next

            If m_objectfamily = Enums.GraphicObjects.ObjectType.MaterialStream AndAlso m_objectlist.Count > 0 Then
                For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                    If kvp.Value = True Then
                        Dim myobj As SharedClasses.UnitOperations.BaseClass = Flowsheet.GetFlowsheetSimulationObject(kvp.Key)
                        If myobj.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                            For Each kvp2 As KeyValuePair(Of String, Boolean) In m_propertylist
                                If kvp2.Value = True AndAlso myobj.GetPropertyValue(kvp2.Key).Equals(Double.MinValue) Then
                                    propstoremove.Add(kvp2.Key)
                                End If
                            Next
                        End If
                        For i As Integer = 0 To propstoremove.Count - 1
                            m_propertylist.Remove(propstoremove(i))
                        Next
                        Exit For
                    End If
                Next
            End If

            For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                If kvp.Value = True Then
                    Dim myobj As SharedClasses.UnitOperations.BaseClass = Flowsheet.GetFlowsheetSimulationObject(kvp.Key)
                    m_items.Add(kvp.Key, New List(Of NodeItem))
                    m_items(kvp.Key).Add(New NodeItem(Flowsheet.GetTranslatedString("Objeto"), kvp.Key, "", 0, 0, ""))
                    If Me.HeaderText = "" Then Me.HeaderText = Flowsheet.GetTranslatedString("MasterTable")
                    Dim mypropid As String = ""
                    Dim props() As String = myobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
                    For Each kvp2 As KeyValuePair(Of String, Boolean) In m_propertylist
                        If kvp2.Value = True Then
                            For Each p As String In props
                                If p = kvp2.Key Then
                                    mypropid = p
                                    Dim value As Object = myobj.GetPropertyValue(mypropid, su)
                                    If Double.TryParse(value, New Double) Then
                                        m_items(kvp.Key).Add(New NodeItem(Flowsheet.GetTranslatedString(kvp2.Key), Format(Double.Parse(value), nf), myobj.GetPropertyUnit(mypropid, su), 0, 0, ""))
                                    Else
                                        m_items(kvp.Key).Add(New NodeItem(Flowsheet.GetTranslatedString(kvp2.Key), value, myobj.GetPropertyUnit(mypropid, su), 0, 0, ""))
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                    Next
                End If
            Next

            If m_sortableitems Is Nothing Then m_sortableitems = New ArrayList

            If Not m_sortableitems.Contains(m_sortby) Then m_sortby = "Name | DESC"

            If m_sortedlist Is Nothing Then m_sortedlist = New List(Of String)

            If m_sortby = "Custom" Then

                For i As Integer = 0 To objectstoremove.Count - 1
                    If m_sortedlist.Contains(objectstoremove(i)) Then m_sortedlist.Remove(objectstoremove(i))
                Next

                objectstoremove = New List(Of String)

                For Each s As String In m_sortedlist
                    If m_objectlist(s) = False Then objectstoremove.Add(s)
                Next

                For i As Integer = 0 To objectstoremove.Count - 1
                    If m_sortedlist.Contains(objectstoremove(i)) Then m_sortedlist.Remove(objectstoremove(i))
                Next

            Else

                m_sortedlist.Clear()

                Dim p1, p2 As Object
                Dim j As Integer = 0
                Dim istidx As Integer = 0
                Dim isnotname As Boolean
                For Each s As String In m_items.Keys
                    isnotname = False
                    If j > 0 Then
                        istidx = 0
                        For Each ni As NodeItem In m_items(s)
                            If m_sortby.Contains(ni.Text) Then
                                isnotname = True
                                If Double.TryParse(ni.Value, New Double()) Then
                                    p1 = Double.Parse(ni.Value)
                                    For Each s2 As String In m_sortedlist
                                        p2 = Double.Parse(m_items(s2)(m_items(s).IndexOf(ni)).Value)
                                        If m_sortby.Contains("ASC") Then
                                            'ASC
                                            If p1 >= p2 Then
                                                istidx += 1
                                            End If
                                        Else
                                            'DESC
                                            If p1 <= p2 Then
                                                istidx += 1
                                            End If
                                        End If
                                    Next
                                Else
                                    p1 = ni.Value
                                    For Each s2 As String In m_sortedlist
                                        p2 = m_items(s2)(m_items(s).IndexOf(ni)).Value
                                        If m_sortby.Contains("ASC") Then
                                            'ASC
                                            If p1 >= p2 Then
                                                istidx += 1
                                            End If
                                        Else
                                            'DESC
                                            If p1 <= p2 Then
                                                istidx += 1
                                            End If
                                        End If
                                    Next
                                End If
                                m_sortedlist.Insert(istidx, s)
                                Exit For
                            End If
                        Next
                        If Not isnotname Then
                            'sort by object's name
                            p1 = s
                            istidx = 0
                            For Each s2 As String In m_sortedlist
                                p2 = s2
                                If m_sortby.Contains("ASC") Then
                                    'ASC
                                    If p1 >= p2 Then
                                        istidx += 1
                                    End If
                                Else
                                    'DESC
                                    If p1 <= p2 Then
                                        istidx += 1
                                    End If
                                End If
                            Next
                            m_sortedlist.Insert(istidx, s)
                        End If
                    Else
                        m_sortedlist.Add(s)
                    End If
                    j += 1
                Next
            End If

        End Sub

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

            Update()

            Dim k As Integer = 0
            For Each bo As Boolean In Me.m_objectlist.Values
                If bo Then k += 1
            Next

            Dim maxL1, maxL2(k - 1), maxL2a, maxL3, count, i, maxH, n As Integer

            If SortedList.Count > 0 Then

                'determinar comprimento das colunas e altura das linhas
                maxL1 = 0
                maxL3 = 0
                maxH = 0
                Dim size As SKSize
                Dim ni As NodeItem
                If Not m_items Is Nothing Then
                    i = 0
                    If Not m_sortedlist Is Nothing Then
                        For Each s As String In m_sortedlist
                            maxL2(i) = 0
                            count = 1
                            For Each ni In m_items(s)
                                size = MeasureString(Flowsheet.GetTranslatedString(ni.Text), tpaint)
                                If size.Width > maxL1 Then maxL1 = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                size = MeasureString(ni.Value, tpaint)
                                If size.Width > maxL2(i) Then maxL2(i) = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                size = MeasureString(ni.Unit, tpaint)
                                If size.Width > maxL3 Then maxL3 = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                count += 1
                            Next
                            i += 1
                        Next
                    Else
                        For Each s As String In m_items.Keys
                            maxL2(i) = 0
                            count = 1
                            For Each ni In m_items(s)
                                size = MeasureString(Flowsheet.GetTranslatedString(ni.Text), tpaint)
                                If size.Width > maxL1 Then maxL1 = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                size = MeasureString(ni.Value, tpaint)
                                If size.Width > maxL2(i) Then maxL2(i) = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                size = MeasureString(ni.Unit, tpaint)
                                If size.Width > maxL3 Then maxL3 = size.Width
                                If size.Height > maxH Then maxH = size.Height
                                count += 1
                            Next
                            i += 1
                        Next
                    End If
                Else

                End If

                Me.Height = (count) * (maxH + 2 * Me.Padding)
                If Not m_items Is Nothing Then
                    If maxL2.Length > 0 Then
                        maxL2a = maxL2.Max + 3 * Padding
                        Me.Width = (4 + 2 * m_items.Count) * Me.Padding + maxL1 + (k) * maxL2a + maxL3
                    Else
                        Me.Width = 100
                        maxL2a = 50
                    End If
                Else
                    Me.Width = 100
                    maxL2a = 50
                End If

                maxL1 = maxL1 + 2 * Padding
                maxL3 = maxL3 + 2 * Padding
                maxH = maxH + 2 * Padding

                'Dim rect As SKRect = GetRect(X, Y, Width, Height)
                'canvas.DrawRect(rect, GetPaint(SKColors.White))

                size = MeasureString("MEASURE", tpaint)

                'desenhar textos e retangulos
                canvas.DrawText(Me.HeaderText, X + Padding, Y + Padding + size.Height, tpaint)
                If Not m_items Is Nothing Then
                    If maxL2.Length > 0 Then
                        i = 0
                        If Not m_sortedlist Is Nothing Then
                            For Each s As String In m_sortedlist
                                canvas.DrawLine(X + maxL1 + (i + 1) * maxL2a + Padding, Y + maxH, X + maxL1 + (i + 1) * maxL2a + Padding, Y + Height, bpaint)
                                n = 1
                                For Each ni In m_items(s)
                                    If i = 0 Then canvas.DrawText(Flowsheet.GetTranslatedString(ni.Text), X + Padding, Y + n * maxH + Padding + size.Height, tpaint)
                                    canvas.DrawText(ni.Value, (maxL2a - MeasureString(ni.Value, tpaint).Width) + X + maxL1 + i * maxL2a, Y + n * maxH + Padding + size.Height, tpaint)
                                    If i = m_items.Count - 1 Then canvas.DrawText(ni.Unit, X + maxL1 + (i + 1) * maxL2a + 3 * Padding, Y + n * maxH + Padding + size.Height, tpaint)
                                    n += 1
                                Next
                                i += 1
                            Next
                            For n = 1 To count - 1
                                canvas.DrawLine(X, Y + n * maxH, X + Width, Y + n * maxH, bpaint)
                            Next
                        Else
                            For Each s As String In m_items.Keys
                                canvas.DrawLine(X + maxL1 + (i + 1) * maxL2a + Padding, Y + maxH, X + maxL1 + (i + 1) * maxL2a + Padding, Y + Height, bpaint)
                                n = 2
                                For Each ni In m_items(s)
                                    If i = 0 Then canvas.DrawText(Flowsheet.GetTranslatedString(ni.Text), X + Padding, Y + n * maxH + Padding + size.Height, tpaint)
                                    canvas.DrawText(ni.Value, X + maxL1 + (i + 1) * maxL2a, Y + n * maxH + Padding + size.Height, tpaint)
                                    If i = m_items.Count - 1 Then canvas.DrawText(ni.Unit, X + maxL1 + (i + 1) * maxL2a + 3 * Padding, Y + n * maxH + Padding + size.Height, tpaint)
                                    n += 1
                                Next
                                i += 1
                            Next
                            For n = 1 To count - 1
                                canvas.DrawLine(X, Y + n * maxH, X + Width, Y + n * maxH, bpaint)
                            Next
                        End If
                    Else
                        Me.Height = 40
                    End If
                Else
                    Me.Height = 40
                End If

                canvas.DrawRect(GetRect(Me.X, Me.Y, Me.Width, Me.Height), bpaint)
                canvas.DrawLine(X + maxL1, Y + maxH, X + maxL1, Y + Height, bpaint)

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
