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

Imports System.Drawing
Imports DWSIM.DrawingTools.GraphicObjects
Imports System.Drawing.Drawing2D
Imports DWSIM.DWSIM.Extras
Imports System.Linq
Imports DWSIM.DrawingTools

Namespace DWSIM.DrawingTools.GraphicObjects2

    <Serializable()> Public Class MasterTableGraphic

        Inherits ShapeGraphic

        Protected m_Font_Col1 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_Font_Col2 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_Font_Col3 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_HeaderFont As Font = New Font("Arial", 10, FontStyle.Bold, GraphicsUnit.Pixel, 0, False)

        Protected m_Text As String = ""

        Protected m_Color_Bg As Color = Drawing.Color.White
        Protected m_Color_Gradient_1 As Color = Drawing.Color.LightGray
        Protected m_Color_Gradient_2 As Color = Drawing.Color.WhiteSmoke

        Protected m_bgOpacity As Integer = 255

        Protected m_IsGradientBg As Boolean = False

        Protected m_BorderThickness As Integer = 1
        Protected m_Padding As Integer = 2

        <System.NonSerialized()> Protected m_BorderPen As Drawing.Pen = New Drawing.Pen(Color.Black)
        Protected m_BorderStyle As Drawing2D.DashStyle = DashStyle.Solid
        Protected m_BorderColor As Color = Color.Black

        Protected m_TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default
        Protected m_objectfamily As ObjectType = ObjectType.MaterialStream

        Protected m_objectlist As Dictionary(Of String, Boolean)
        Protected m_propertylist As Dictionary(Of String, Boolean)
        Protected m_sortby As String = ""
        Protected m_sortableitems As ArrayList
        Protected m_sortedlist As List(Of String)

        Protected m_items As Dictionary(Of String, List(Of DWSIM.Extras.NodeItem))

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
            Me.ObjectType = ObjectType.GO_MasterTable
            m_objectlist = New Dictionary(Of String, Boolean)
            m_propertylist = New Dictionary(Of String, Boolean)
            m_sortableitems = New ArrayList
            m_sortedlist = New List(Of String)
        End Sub

        Public Sub New(ByVal graphicPosition As Drawing.Point)
            Me.New()
            Me.SetPosition(graphicPosition.ToDTPoint)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New Drawing.Point(posX, posY))
        End Sub

#End Region

#Region "Properties"

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
                        m_sortableitems.Add(DWSIM.App.GetPropertyName(kvp.Key) & " | ASC")
                        m_sortableitems.Add(DWSIM.App.GetPropertyName(kvp.Key) & " | DESC")
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

        Public Property ObjectFamily() As ObjectType
            Get
                Return m_objectfamily
            End Get
            Set(ByVal value As ObjectType)
                m_objectfamily = value
                m_objectlist.Clear()
                m_propertylist.Clear()
                If m_sortby = "" Then m_sortby = "Name | DESC"
            End Set
        End Property

        Public Property HeaderFont() As Font
            Get
                Return m_HeaderFont
            End Get
            Set(ByVal Value As Font)
                m_HeaderFont = Value
            End Set
        End Property

        Public Property FontCol1() As Font
            Get
                Return m_Font_Col1
            End Get
            Set(ByVal Value As Font)
                m_Font_Col1 = Value
            End Set
        End Property

        Public Property FontCol2() As Font
            Get
                Return m_Font_Col2
            End Get
            Set(ByVal Value As Font)
                m_Font_Col2 = Value
            End Set
        End Property

        Public Property FontCol3() As Font
            Get
                Return m_Font_Col3
            End Get
            Set(ByVal Value As Font)
                m_Font_Col3 = Value
            End Set
        End Property

        Public Property HeaderText() As String
            Get
                Return m_Text
            End Get
            Set(ByVal Value As String)
                m_Text = Value
            End Set
        End Property

        Public Property BorderThickness() As Integer
            Get
                Return m_BorderThickness
            End Get
            Set(ByVal value As Integer)
                m_BorderThickness = value
            End Set
        End Property

        Public Property Padding() As Integer
            Get
                Return m_Padding
            End Get
            Set(ByVal value As Integer)
                m_Padding = value
            End Set
        End Property

        Public Property Opacity() As Integer
            Get
                Return m_bgOpacity
            End Get
            Set(ByVal value As Integer)
                m_bgOpacity = value
            End Set
        End Property

        Public Property BackgroundColor() As System.Drawing.Color
            Get
                Return m_Color_Bg
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Bg = Value
            End Set
        End Property

        Public Property BackgroundGradientColor1() As System.Drawing.Color
            Get
                Return m_Color_Gradient_1
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_1 = Value
            End Set
        End Property

        Public Property BackgroundGradientColor2() As System.Drawing.Color
            Get
                Return m_Color_Gradient_2
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_2 = Value
            End Set
        End Property

        Public Property BorderStyle() As Drawing.Drawing2D.DashStyle
            Get
                Return m_BorderStyle
            End Get
            Set(ByVal value As Drawing.Drawing2D.DashStyle)
                m_BorderStyle = value
            End Set
        End Property

        Public Property BorderColor() As Drawing.Color
            Get
                Return m_BorderColor
            End Get
            Set(ByVal value As Drawing.Color)
                m_BorderColor = value
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

        Property IsGradientBackground() As Boolean
            Get
                Return m_IsGradientBg
            End Get
            Set(ByVal value As Boolean)
                m_IsGradientBg = value
            End Set
        End Property
#End Region

        Public Sub Update(ByRef form As FormFlowsheet)

            Dim su As SystemsOfUnits.Units = form.Options.SelectedUnitSystem
            Dim nf As String = form.Options.NumberFormat

            m_items = New Dictionary(Of String, List(Of DWSIM.Extras.NodeItem))

            Dim objectstoremove As New ArrayList

            For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                If form.GetFlowsheetGraphicObject(kvp.Key) Is Nothing Then
                    objectstoremove.Add(kvp.Key)
                End If
            Next

            For i As Integer = 0 To objectstoremove.Count - 1
                m_objectlist.Remove(objectstoremove(i))
            Next

            For Each kvp As KeyValuePair(Of String, Boolean) In m_objectlist
                If kvp.Value = True Then
                    Dim myobj As SharedClasses.UnitOperations.BaseClass = form.GetFlowsheetSimulationObject(kvp.Key)
                    m_items.Add(kvp.Key, New List(Of NodeItem))
                    m_items(kvp.Key).Add(New NodeItem(DWSIM.App.GetLocalString("Objeto"), kvp.Key, "", 0, 0, ""))
                    If Me.HeaderText = "" Then Me.HeaderText = DWSIM.App.GetLocalString("MasterTable") & " - " & DWSIM.App.GetLocalString(myobj.ComponentDescription)
                    Dim mypropid As String = ""
                    Dim props() As String = myobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
                    For Each kvp2 As KeyValuePair(Of String, Boolean) In m_propertylist
                        If kvp2.Value = True Then
                            For Each p As String In props
                                If p = kvp2.Key Then
                                    mypropid = p
                                    Dim value As Object = myobj.GetPropertyValue(mypropid, su)
                                    If Double.TryParse(value, New Double) Then
                                        m_items(kvp.Key).Add(New NodeItem(kvp2.Key, Format(Double.Parse(value), nf), myobj.GetPropertyUnit(mypropid, su), 0, 0, ""))
                                    Else
                                        m_items(kvp.Key).Add(New NodeItem(kvp2.Key, value, myobj.GetPropertyUnit(mypropid, su), 0, 0, ""))
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

                objectstoremove = New ArrayList

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

        Public Sub CopyToClipboard()

            Dim i As Integer = 0
            Dim j As Integer = 0

            Dim data As String = ""

            Dim refitem = m_items.Values.FirstOrDefault

            If Not m_items Is Nothing Then
                If Not m_sortedlist Is Nothing Then
                    For Each s As String In m_sortedlist
                        data += vbTab & m_items(s)(0).Value
                    Next
                    data += vbCrLf
                    For j = 1 To refitem.Count - 1
                        data += DWSIM.App.GetPropertyName(refitem(j).Text) & vbTab
                        For Each s As String In m_sortedlist
                            data += m_items(s)(j).Value & vbTab
                        Next
                        data += refitem(j).Unit & vbCrLf
                    Next
                End If
            End If

            Clipboard.SetText(data)

        End Sub

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            Dim iopacity As Integer
            If SemiTransparent Then
                iopacity = 50
            Else
                iopacity = Me.Opacity
            End If

            Dim gContainer As System.Drawing.Drawing2D.GraphicsContainer
            Dim myMatrix As Drawing2D.Matrix
            gContainer = g.BeginContainer()
            SetQuality(g)
            myMatrix = g.Transform()
            If Rotation <> 0 Then
                myMatrix.RotateAt(Rotation, New PointF(X, Y), Drawing.Drawing2D.MatrixOrder.Append)
                g.Transform = myMatrix
            End If

            If Not Me.TextRenderStyle = -1 Then g.TextRenderingHint = Me.TextRenderStyle

            Dim k As Integer = 0
            For Each bo As Boolean In Me.m_objectlist.Values
                If bo Then k += 1
            Next

            Dim maxL1, maxL2(k - 1), maxL2a, maxL3, count, i, maxH, n As Integer

            Dim format1 As New StringFormat(StringFormatFlags.NoClip)
            With format1
                .Alignment = StringAlignment.Far
            End With

            Dim format2 As New StringFormat(StringFormatFlags.NoClip)
            With format2
                .Alignment = StringAlignment.Far
            End With

            'determinar comprimento das colunas e altura das linhas
            maxL1 = 0
            maxL3 = 0
            maxH = 0
            Dim size As SizeF
            Dim ni As NodeItem
            If Not m_items Is Nothing Then
                i = 0
                If Not m_sortedlist Is Nothing Then
                    For Each s As String In m_sortedlist
                        maxL2(i) = 0
                        count = 1
                        For Each ni In m_items(s)
                            size = g.MeasureString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            If size.Width > maxL1 Then maxL1 = size.Width
                            If size.Height > maxH Then maxH = size.Height
                            size = g.MeasureString(ni.Value, Me.FontCol2, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            If size.Width > maxL2(i) Then maxL2(i) = size.Width
                            If size.Height > maxH Then maxH = size.Height
                            size = g.MeasureString(ni.Unit, Me.FontCol3, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
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
                            size = g.MeasureString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            If size.Width > maxL1 Then maxL1 = size.Width
                            If size.Height > maxH Then maxH = size.Height
                            size = g.MeasureString(ni.Value, Me.FontCol2, New PointF(0, 0), New StringFormat(StringFormatFlags.DirectionRightToLeft, 0))
                            If size.Width > maxL2(i) Then maxL2(i) = size.Width
                            If size.Height > maxH Then maxH = size.Height
                            size = g.MeasureString(ni.Unit, Me.FontCol3, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            If size.Width > maxL3 Then maxL3 = size.Width
                            If size.Height > maxH Then maxH = size.Height
                            count += 1
                        Next
                        i += 1
                    Next
                End If
            Else

            End If
            'size = g.MeasureString(Me.HeaderText, Me.HeaderFont, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            'If size.Width > maxL1 Then maxL1 = size.Width
            'If size.Height > maxH Then maxH = size.Height

            Me.Height = (count) * (maxH + 2 * Me.Padding)
            If Not m_items Is Nothing Then
                If maxL2.Length > 0 Then
                    maxL2a = MathEx.Common.Max(maxL2) + 3 * Padding
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

            If m_BorderPen Is Nothing Then m_BorderPen = New Drawing.Pen(Color.FromArgb(iopacity, Color.Black))

            With Me.m_BorderPen
                .Color = Color.FromArgb(iopacity, Me.BorderColor)
                .DashStyle = Me.BorderStyle
            End With

            If Width = 0 Then Width = 100
            If Height = 0 Then Height = 100

            Dim rect As New Rectangle(X, Y, Width, Height)
            If Me.IsGradientBackground = False Then
                g.FillRectangle(New SolidBrush(Color.FromArgb(iopacity, Me.BackgroundColor)), rect)
            Else
                g.FillRectangle(New Drawing2D.LinearGradientBrush(rect, Color.FromArgb(iopacity, Me.BackgroundGradientColor2), Color.FromArgb(iopacity, Me.BackgroundGradientColor1), LinearGradientMode.Vertical), rect)
            End If

            'desenhar textos e retangulos
            g.DrawString(Me.HeaderText, Me.HeaderFont, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding, Y + Padding)
            If Not m_items Is Nothing Then
                If maxL2.Length > 0 Then
                    i = 0
                    If Not m_sortedlist Is Nothing Then
                        For Each s As String In m_sortedlist
                            g.DrawLine(Me.m_BorderPen, X + maxL1 + (i + 1) * maxL2a + 2 * Me.Padding, Y + maxH, X + maxL1 + (i + 1) * maxL2a + 2 * Me.Padding, Y + Height)
                            n = 1
                            For Each ni In m_items(s)
                                If i = 0 Then g.DrawString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding, Y + n * maxH + Padding)
                                g.DrawString(ni.Value, Me.FontCol2, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + (i + 1) * maxL2a, Y + n * maxH + Padding, format1)
                                If i = m_items.Count - 1 Then g.DrawString(ni.Unit, Me.FontCol3, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + (i + 1) * maxL2a + 3 * Padding, Y + n * maxH + Padding)
                                n += 1
                            Next
                            i += 1
                        Next
                        For n = 1 To count - 1
                            g.DrawLine(Me.m_BorderPen, X, Y + n * maxH, X + Width, Y + n * maxH)
                        Next
                    Else
                        For Each s As String In m_items.Keys
                            g.DrawLine(Me.m_BorderPen, X + maxL1 + (i + 1) * maxL2a + 2 * Me.Padding, Y + maxH, X + maxL1 + (i + 1) * maxL2a + 2 * Me.Padding, Y + Height)
                            n = 1
                            For Each ni In m_items(s)
                                If i = 0 Then g.DrawString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding, Y + n * maxH + Padding)
                                g.DrawString(ni.Value, Me.FontCol2, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + (i + 1) * maxL2a, Y + n * maxH + Padding, format1)
                                If i = m_items.Count - 1 Then g.DrawString(ni.Unit, Me.FontCol3, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + (i + 1) * maxL2a + 3 * Padding, Y + n * maxH + Padding)
                                n += 1
                            Next
                            i += 1
                        Next
                        For n = 1 To count - 1
                            g.DrawLine(Me.m_BorderPen, X, Y + n * maxH, X + Width, Y + n * maxH)
                        Next
                    End If
                Else
                    Me.Height = 40
                End If
            Else
                Me.Height = 40
            End If

            g.DrawRectangle(Me.m_BorderPen, New Rectangle(Me.X, Me.Y, Me.Width, Me.Height))
            'g.DrawLine(Me.m_BorderPen, X, Y + maxH, X + Width, Y + maxH)
            g.DrawLine(Me.m_BorderPen, X + maxL1, Y + maxH, X + maxL1, Y + Height)

            g.EndContainer(gContainer)

        End Sub

    End Class

    <Serializable()> Public Class TableGraphic

        Inherits ShapeGraphic

        Public BaseOwner As SharedClasses.UnitOperations.BaseClass

        Protected m_Font_Col1 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_Font_Col2 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_Font_Col3 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)
        Protected m_HeaderFont As Font = New Font("Arial", 10, FontStyle.Bold, GraphicsUnit.Pixel, 0, False)

        Protected m_Text As String = ""

        Protected m_Color_Bg As Color = Drawing.Color.White
        Protected m_Color_Gradient_1 As Color = Drawing.Color.LightGray
        Protected m_Color_Gradient_2 As Color = Drawing.Color.WhiteSmoke

        Protected m_bgOpacity As Integer = 255

        Protected m_IsGradientBg As Boolean = False

        Protected m_BorderThickness As Integer = 1
        Protected m_Padding As Integer = 2

        <System.NonSerialized()> Protected m_BorderPen As Drawing.Pen = New Drawing.Pen(Color.Black)
        Protected m_BorderStyle As Drawing2D.DashStyle = DashStyle.Solid
        Protected m_BorderColor As Color = Color.Black

        Protected m_TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            With elements
                If BaseOwner IsNot Nothing Then .Add(New XElement("Owner", BaseOwner.Name))
            End With

            Return elements

        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Return MyBase.LoadData(data)

        End Function

#Region "Constructors"
        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass)
            Me.ObjectType = ObjectType.GO_Table
            Me.BaseOwner = owner
        End Sub

        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass, ByVal graphicPosition As Drawing.Point)
            Me.New(owner)
            Me.SetPosition(graphicPosition.ToDTPoint)
        End Sub

        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass, ByVal posX As Integer, ByVal posY As Integer)
            Me.New(owner, New Drawing.Point(posX, posY))
        End Sub

        Public Sub New()
            Me.ObjectType = ObjectType.GO_Table
        End Sub

#End Region

#Region "Properties"

        Public Property HeaderFont() As Font
            Get
                Return m_HeaderFont
            End Get
            Set(ByVal Value As Font)
                m_HeaderFont = Value
            End Set
        End Property

        Public Property FontCol1() As Font
            Get
                Return m_Font_Col1
            End Get
            Set(ByVal Value As Font)
                m_Font_Col1 = Value
            End Set
        End Property

        Public Property FontCol2() As Font
            Get
                Return m_Font_Col2
            End Get
            Set(ByVal Value As Font)
                m_Font_Col2 = Value
            End Set
        End Property

        Public Property FontCol3() As Font
            Get
                Return m_Font_Col3
            End Get
            Set(ByVal Value As Font)
                m_Font_Col3 = Value
            End Set
        End Property

        Public Property HeaderText() As String
            Get
                If Not Me.BaseOwner Is Nothing Then
                    If Not Me.BaseOwner.GraphicObject Is Nothing Then
                        Return Me.BaseOwner.GraphicObject.Tag
                    Else
                        Return m_Text
                    End If
                Else
                    Return m_Text
                End If
            End Get
            Set(ByVal Value As String)
                m_Text = Value
            End Set
        End Property

        Public Property BorderThickness() As Integer
            Get
                Return m_BorderThickness
            End Get
            Set(ByVal value As Integer)
                m_BorderThickness = value
            End Set
        End Property

        Public Property Padding() As Integer
            Get
                Return m_Padding
            End Get
            Set(ByVal value As Integer)
                m_Padding = value
            End Set
        End Property

        Public Property Opacity() As Integer
            Get
                Return m_bgOpacity
            End Get
            Set(ByVal value As Integer)
                m_bgOpacity = value
            End Set
        End Property

        Public Property BackgroundColor() As System.Drawing.Color
            Get
                Return m_Color_Bg
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Bg = Value
            End Set
        End Property

        Public Property BackgroundGradientColor1() As System.Drawing.Color
            Get
                Return m_Color_Gradient_1
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_1 = Value
            End Set
        End Property

        Public Property BackgroundGradientColor2() As System.Drawing.Color
            Get
                Return m_Color_Gradient_2
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_2 = Value
            End Set
        End Property

        Public Property BorderStyle() As Drawing.Drawing2D.DashStyle
            Get
                Return m_BorderStyle
            End Get
            Set(ByVal value As Drawing.Drawing2D.DashStyle)
                m_BorderStyle = value
            End Set
        End Property

        Public Property BorderColor() As Drawing.Color
            Get
                Return m_BorderColor
            End Get
            Set(ByVal value As Drawing.Color)
                m_BorderColor = value
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

        Property IsGradientBackground() As Boolean
            Get
                Return m_IsGradientBg
            End Get
            Set(ByVal value As Boolean)
                m_IsGradientBg = value
            End Set
        End Property
#End Region

        Public Sub PopulateGrid(ByRef pgrid As PropertyGridEx.PropertyGridEx)

            With pgrid

                .Item.Clear()

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

        End Sub

        Public Sub CopyToClipboard()

            Dim i As Integer = 0
            Dim j As Integer = 0

            Dim data As String = Me.HeaderText & vbCrLf

            Clipboard.SetText(data)

        End Sub

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            Dim iopacity As Integer

            If SemiTransparent Then iopacity = 50 Else iopacity = Me.Opacity

            Dim gContainer As System.Drawing.Drawing2D.GraphicsContainer
            Dim myMatrix As Drawing2D.Matrix
            gContainer = g.BeginContainer()
            SetQuality(g)
            myMatrix = g.Transform()
            If Rotation <> 0 Then
                myMatrix.RotateAt(Rotation, New PointF(X, Y), Drawing.Drawing2D.MatrixOrder.Append)
                g.Transform = myMatrix
            End If

            If Not Me.TextRenderStyle = -1 Then g.TextRenderingHint = Me.TextRenderStyle

            Dim maxL1, maxL2, maxL3, count As Integer
            Dim maxH As Integer

            'determinar comprimento das colunas e altura das linhas
            maxL1 = 0
            maxL2 = 0
            maxL3 = 0
            maxH = 0
            count = 1
            Dim size As SizeF

            'For Each ni In Me.BaseOwner.NodeTableItems.Values
            '    If ni.Checked = True Then
            '        If ni.Level = 0 And ni.ParentNode = "" Or ni.Level > 0 And ni.ParentNode <> "" Then
            '            size = g.MeasureString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            '            If size.Width > maxL1 Then maxL1 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            If Double.TryParse(ni.Value, New Double) Then
            '                size = g.MeasureString(Format(Convert.ToDouble(ni.Value), Me.BaseOwner.FlowSheet.Options.NumberFormat), Me.FontCol2, New PointF(0, 0), New StringFormat(StringFormatFlags.DirectionRightToLeft, 0))
            '            Else
            '                size = g.MeasureString(ni.Value, Me.FontCol2, New PointF(0, 0), New StringFormat(StringFormatFlags.DirectionRightToLeft, 0))
            '            End If
            '            If size.Width > maxL2 Then maxL2 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            size = g.MeasureString(ni.Unit, Me.FontCol3, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            '            If size.Width > maxL3 Then maxL3 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            count += 1
            '        End If
            '    End If
            'Next
            size = g.MeasureString(Me.HeaderText, Me.HeaderFont, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            If size.Width > maxL1 Then maxL1 = size.Width
            If size.Height > maxH Then maxH = size.Height

            Me.Height = (count) * (maxH + 2 * Me.Padding)
            Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3

            maxL1 = maxL1 + 2 * Padding
            maxL2 = maxL2 + 2 * Padding
            maxL3 = maxL3 + 2 * Padding
            maxH = maxH + 2 * Padding

            If m_BorderPen Is Nothing Then m_BorderPen = New Drawing.Pen(Color.FromArgb(iopacity, Color.Black))

            With Me.m_BorderPen
                .Color = Color.FromArgb(iopacity, Me.BorderColor)
                .DashStyle = Me.BorderStyle
            End With

            Dim rect As New Rectangle(X, Y, Width, Height)
            If Me.IsGradientBackground = False Then
                g.FillRectangle(New SolidBrush(Color.FromArgb(iopacity, Me.BackgroundColor)), rect)
            Else
                g.FillRectangle(New Drawing2D.LinearGradientBrush(rect, Color.FromArgb(iopacity, Me.BackgroundGradientColor2), Color.FromArgb(iopacity, Me.BackgroundGradientColor1), LinearGradientMode.Vertical), rect)
            End If

            Dim format1 As New StringFormat(StringFormatFlags.NoClip)
            With format1
                .Alignment = StringAlignment.Far
                '.LineAlignment = StringAlignment.Far
            End With

            'desenhar textos e retangulos
            g.DrawString(Me.HeaderText, Me.HeaderFont, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding, Y + Padding)
            Dim n As Integer = 1
            'For Each ni In Me.BaseOwner.NodeTableItems.Values
            '    If ni.Checked = True Then
            '        If ni.Level = 0 And ni.ParentNode = "" Or ni.Level > 0 And ni.ParentNode <> "" Then
            '            g.DrawString(DWSIM.App.GetPropertyName(ni.Text), Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding, Y + n * maxH + Padding)
            '            If Double.TryParse(ni.Value, New Double) Then
            '                g.DrawString(Format(Convert.ToDouble(ni.Value), Me.BaseOwner.FlowSheet.Options.NumberFormat), Me.FontCol2, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + maxL2, Y + n * maxH + Padding, format1)
            '            Else
            '                g.DrawString(ni.Value, Me.FontCol2, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + maxL2, Y + n * maxH + Padding, format1)
            '            End If
            '            g.DrawString(ni.Unit, Me.FontCol3, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + maxL1 + maxL2 + Padding, Y + n * maxH + Padding)
            '            g.DrawLine(Me.m_BorderPen, X, Y + n * maxH, X + Width, Y + n * maxH)
            '            n += 1
            '        End If
            '    End If
            'Next

            g.DrawRectangle(Me.m_BorderPen, New Rectangle(Me.X, Me.Y, Me.Width, Me.Height))
            g.DrawLine(Me.m_BorderPen, X + maxL1, Y + maxH, X + maxL1, Y + Height)
            g.DrawLine(Me.m_BorderPen, X + maxL1 + maxL2, Y + maxH, X + maxL1 + maxL2, Y + Height)

            g.EndContainer(gContainer)

        End Sub

    End Class

    <Serializable()> Public Class QuickTableGraphic
        Inherits ShapeGraphic

        Public BaseOwner As SharedClasses.UnitOperations.BaseClass

        Protected m_HeaderFont As Font = New Font("Arial", 10, FontStyle.Bold, GraphicsUnit.Pixel, 0, False)

        Protected m_Text As String = ""

        Protected m_Color_Bg As Color = Drawing.Color.Black
        Protected m_Color_Gradient_1 As Color = Drawing.Color.LightSteelBlue
        Protected m_Color_Gradient_2 As Color = Drawing.Color.RoyalBlue

        Protected m_bgOpacity As Integer = 140

        Protected m_IsGradientBg As Boolean = True

        Protected m_BorderThickness As Integer = 0
        Protected m_Padding As Integer = 2

        <System.NonSerialized()> Protected m_BorderPen As Drawing.Pen = New Drawing.Pen(Color.Gray)
        Protected m_BorderStyle As Drawing2D.DashStyle = DashStyle.Solid
        Protected m_BorderColor As Color = Color.Black

        Protected m_TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = ObjectType.GO_FloatingTable
        End Sub

        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass)
            Me.ObjectType = ObjectType.GO_FloatingTable
            Me.BaseOwner = owner
        End Sub

        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass, ByVal graphicPosition As Drawing.Point)
            Me.New(owner)
            Me.SetPosition(graphicPosition.ToDTPoint)
        End Sub

        Public Sub New(ByRef owner As SharedClasses.UnitOperations.BaseClass, ByVal posX As Integer, ByVal posY As Integer)
            Me.New(owner, New Drawing.Point(posX, posY))
        End Sub

#End Region

#Region "Properties"

        Public Property HeaderFont() As Font
            Get
                Return m_HeaderFont
            End Get
            Set(ByVal Value As Font)
                m_HeaderFont = Value
            End Set
        End Property

        Public Property HeaderText() As String
            Get
                Return m_Text
            End Get
            Set(ByVal Value As String)
                m_Text = Value
            End Set
        End Property

        Public Property BorderThickness() As Integer
            Get
                Return m_BorderThickness
            End Get
            Set(ByVal value As Integer)
                m_BorderThickness = value
            End Set
        End Property

        Public Property Padding() As Integer
            Get
                Return m_Padding
            End Get
            Set(ByVal value As Integer)
                m_Padding = value
            End Set
        End Property

        Public Property Opacity() As Integer
            Get
                Return m_bgOpacity
            End Get
            Set(ByVal value As Integer)
                m_bgOpacity = value
            End Set
        End Property

        Public Property BackgroundColor() As System.Drawing.Color
            Get
                Return m_Color_Bg
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Bg = Value
            End Set
        End Property

        Public Property BackgroundGradientColor1() As System.Drawing.Color
            Get
                Return m_Color_Gradient_1
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_1 = Value
            End Set
        End Property

        Public Property BackgroundGradientColor2() As System.Drawing.Color
            Get
                Return m_Color_Gradient_2
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_2 = Value
            End Set
        End Property

        Public Property BorderStyle() As Drawing.Drawing2D.DashStyle
            Get
                Return m_BorderStyle
            End Get
            Set(ByVal value As Drawing.Drawing2D.DashStyle)
                m_BorderStyle = value
            End Set
        End Property

        Public Property BorderColor() As Drawing.Color
            Get
                Return m_BorderColor
            End Get
            Set(ByVal value As Drawing.Color)
                m_BorderColor = value
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

        Property IsGradientBackground() As Boolean
            Get
                Return m_IsGradientBg
            End Get
            Set(ByVal value As Boolean)
                m_IsGradientBg = value
            End Set
        End Property
#End Region

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            'If Not Me.BaseOwner Is Nothing Then

            '    If Me.BaseOwner.ShowQuickTable Then

            '        Me.Opacity = 225

            '        'g.ScaleTransform(1 / Me.AdditionalInfo, 1 / Me.AdditionalInfo)

            '        Dim gContainer As System.Drawing.Drawing2D.GraphicsContainer
            '        Dim myMatrix As Drawing2D.Matrix
            '        gContainer = g.BeginContainer()
            '        SetQuality(g)
            '        myMatrix = g.Transform()
            '        If m_Rotation <> 0 Then
            '            myMatrix.RotateAt(m_Rotation, New PointF(X, Y), Drawing.Drawing2D.MatrixOrder.Append)
            '            g.Transform = myMatrix
            '        End If


            '        'If Not Me.TextRenderStyle = -1 Then g.TextRenderingHint = Me.TextRenderStyle

            '        Dim maxL1, maxL2, maxL3, count As Integer
            '        Dim maxH As Integer

            '        Me.HeaderFont = New Font("Arial", 11 / Me.AdditionalInfo, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)

            '        g.TextRenderingHint = Text.TextRenderingHint.SystemDefault

            '        'determinar comprimento das colunas e altura das linhas
            '        maxL1 = 0
            '        maxL2 = 0
            '        maxL3 = 0
            '        maxH = 0
            '        count = 1

            '        Dim size, size2 As SizeF

            '        size = g.MeasureString(Me.BaseOwner.GraphicObject.Tag.ToUpper, New Font(Me.HeaderFont, FontStyle.Bold))
            '        If size.Width > maxL1 Then maxL1 = size.Width
            '        If size.Height > maxH Then maxH = size.Height

            '        Dim ni As DWSIM.Extras.NodeItem
            '        For Each ni In Me.BaseOwner.QTNodeTableItems.Values
            '            size = g.MeasureString(ni.Text, New Font(Me.HeaderFont, FontStyle.Bold), New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            '            If size.Width > maxL1 Then maxL1 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            size = g.MeasureString(ni.Value, Me.HeaderFont, New PointF(0, 0), New StringFormat(StringFormatFlags.DirectionRightToLeft, 0))
            '            If size.Width > maxL2 Then maxL2 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            size = g.MeasureString(ni.Unit, New Font(Me.HeaderFont, FontStyle.Bold), New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            '            If size.Width > maxL3 Then maxL3 = size.Width
            '            If size.Height > maxH Then maxH = size.Height
            '            count += 1
            '        Next

            '        Me.Padding = 3 / Me.AdditionalInfo

            '        If maxH = 0 Then maxH = 20

            '        Me.Height = (count + 1) * (maxH + 2 * Me.Padding)
            '        size = g.MeasureString(Me.HeaderText, Me.HeaderFont, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            '        size2 = g.MeasureString(DWSIM.App.GetLocalString(Me.BaseOwner.GraphicObject.Description), Me.HeaderFont, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))

            '        If size.Width > size2.Width Then
            '            If size.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
            '                Me.Width = 2 * Me.Padding + size.Width
            '            Else
            '                Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
            '            End If
            '        Else
            '            If size2.Width > (2 * Me.Padding + maxL1 + maxL2 + maxL3) Then
            '                Me.Width = 2 * Me.Padding + size2.Width
            '            Else
            '                Me.Width = 6 * Me.Padding + maxL1 + maxL2 + maxL3
            '            End If
            '        End If

            '        Me.Width += 6

            '        maxL1 = maxL1 + 2 * Padding
            '        maxL2 = maxL2 + 2 * Padding
            '        maxL3 = maxL3 + 2 * Padding

            '        maxH = maxH + 2 * Padding

            '        If m_BorderPen Is Nothing Then m_BorderPen = New Drawing.Pen(Color.Black)

            '        With Me.m_BorderPen
            '            .Color = Color.White
            '            .DashStyle = Me.BorderStyle
            '        End With

            '        Dim rect0 As New Rectangle(X + 4, Y + 4, Width, Height)
            '        'DrawRoundRect(g, Pens.Transparent, rect0.X, rect0.Y, rect0.Width, rect0.Height, 10, New SolidBrush(Color.FromArgb(100, Color.Gray)))
            '        'g.FillRectangle(New SolidBrush(Color.FromArgb(100, Color.DimGray)), rect0)
            '        'Me.DrawRoundRect(g, New Pen(Color.Transparent), X + 4, Y + 4, Width, Height, 10, New SolidBrush(Color.FromArgb(50, Color.DimGray)))


            '        m_Color_Gradient_1 = Color.FromArgb(235, Drawing.Color.SteelBlue)
            '        m_Color_Gradient_2 = Color.FromArgb(235, Drawing.Color.SteelBlue) 'Color.FromArgb(255, 29, 80, 132)
            '        Opacity = 255

            '        'm_Color_Gradient_1 = Drawing.Color.WhiteSmoke
            '        'm_Color_Gradient_2 = Drawing.Color.Gainsboro

            '        Dim rect As New Rectangle(X, Y, Width, Height)
            '        If Me.IsGradientBackground = False Then
            '            'g.FillRectangle(New SolidBrush(Color.FromArgb(Me.Opacity, Me.FillColor)), rect)
            '            DrawRoundRect(g, Pens.Transparent, rect.X, rect.Y, rect.Width, rect.Height, 6, New SolidBrush(Color.FromArgb(Me.Opacity, Me.FillColor)))
            '        Else
            '            'g.FillRectangle(New Drawing2D.LinearGradientBrush(rect, Color.FromArgb(Me.Opacity, Me.BackgroundGradientColor1), Color.FromArgb(Me.Opacity, Me.BackgroundGradientColor2), LinearGradientMode.Vertical), rect)
            '            DrawRoundRect(g, Pens.Transparent, rect.X, rect.Y, rect.Width, rect.Height, 6, New Drawing2D.LinearGradientBrush(rect, Me.m_Color_Gradient_1, Me.m_Color_Gradient_2, LinearGradientMode.Vertical))
            '        End If

            '        Dim format1 As New StringFormat(StringFormatFlags.NoClip)
            '        With format1
            '            .Alignment = StringAlignment.Far
            '            '.LineAlignment = StringAlignment.Far
            '        End With

            '        'desenhar textos e retangulos
            '        g.DrawString(Me.BaseOwner.GraphicObject.Tag.ToUpper, New Font(Me.HeaderFont, FontStyle.Bold), New SolidBrush(Color.White), X + Padding + 3, Y + Padding)
            '        g.DrawString(DWSIM.App.GetLocalString(Me.BaseOwner.GraphicObject.Description), Me.HeaderFont, New SolidBrush(Color.White), X + Padding + 3, Y + maxH)
            '        Dim n As Integer = 1
            '        For Each ni In Me.BaseOwner.QTNodeTableItems.Values
            '            g.DrawString(ni.Text, New Font(Me.HeaderFont, FontStyle.Bold), New SolidBrush(Color.White), X + Padding + 3, Y + (n + 1) * maxH + Padding)
            '            g.DrawString(ni.Value, Me.HeaderFont, New SolidBrush(Color.White), X + maxL1 + maxL2 + 3, Y + (n + 1) * maxH + Padding, format1)
            '            g.DrawString(ni.Unit, New Font(Me.HeaderFont, FontStyle.Bold), New SolidBrush(Color.White), X + maxL1 + maxL2 + Padding + 3, Y + (n + 1) * maxH + Padding)
            '            'g.DrawLine(Me.m_BorderPen, X, Y + n * maxH, X + Width, Y + n * maxH)
            '            n += 1
            '        Next

            '        Me.m_BorderPen.Width = 1 / Me.AdditionalInfo

            '        'g.DrawRectangle(Me.m_BorderPen, New Rectangle(Me.X, Me.Y, Me.Width, Me.Height))
            '        DrawRoundRect(g, Me.m_BorderPen, Me.X, Me.Y, Me.Width, Me.Height, 3, Brushes.Transparent)
            '        'Me.DrawRoundRect(g, Me.m_BorderPen, X, Y, Width, Height, 5, New SolidBrush(Color.FromArgb(Me.Opacity, Color.GhostWhite)))
            '        g.DrawLine(Me.m_BorderPen, X + Padding + 3, Y + 2 * maxH - Padding, X + Width - Padding - 3, Y + 2 * maxH - Padding)
            '        'g.DrawLine(Me.m_BorderPen, X + maxL1, Y + maxH, X + maxL1, Y + Height)
            '        'g.DrawLine(Me.m_BorderPen, X + maxL1 + maxL2, Y + maxH, X + maxL1 + maxL2, Y + Height)

            '        g.EndContainer(gContainer)

            '    End If

            'End If

        End Sub

    End Class

    <Serializable()> Public Class SpreadsheetTableGraphic

        Inherits ShapeGraphic

        <System.NonSerialized()> <Xml.Serialization.XmlIgnore> Public Spreadsheet As SpreadsheetForm

        Protected m_Font_Col1 As Font = New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False)

        Protected m_Text As String = ""

        Protected m_Color_Bg As Color = Drawing.Color.White
        Protected m_Color_Gradient_1 As Color = Drawing.Color.LightGray
        Protected m_Color_Gradient_2 As Color = Drawing.Color.WhiteSmoke

        Protected m_bgOpacity As Integer = 255

        Protected m_IsGradientBg As Boolean = False

        Protected m_BorderThickness As Integer = 1
        Protected m_Padding As Integer = 2

        <System.NonSerialized()> Protected m_BorderPen As Drawing.Pen = New Drawing.Pen(Color.Black)
        Protected m_BorderStyle As Drawing2D.DashStyle = DashStyle.Solid
        Protected m_BorderColor As Color = Color.Black

        Protected m_TextRenderStyle As Drawing2D.SmoothingMode = Drawing2D.SmoothingMode.Default

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            Return elements

        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Return MyBase.LoadData(data)

        End Function

        Public Sub SetSpreadsheet(sheet As SpreadsheetForm)
            Me.Spreadsheet = sheet
        End Sub

#Region "Constructors"

        Public Sub New(ByVal sheet As SpreadsheetForm)
            Me.ObjectType = ObjectType.GO_Table
            Me.Spreadsheet = sheet
        End Sub

        Public Sub New(ByVal sheet As SpreadsheetForm, ByVal graphicPosition As Drawing.Point)
            Me.New(sheet)
            Me.SetPosition(graphicPosition.ToDTPoint)
        End Sub

        Public Sub New(ByVal sheet As SpreadsheetForm, ByVal posX As Integer, ByVal posY As Integer)
            Me.New(sheet, New Drawing.Point(posX, posY))
        End Sub

        Public Sub New()
            Me.ObjectType = ObjectType.GO_Table
        End Sub

#End Region

#Region "Properties"

        Public Property SpreadsheetCellRange As String = ""

        Public Property FontCol1() As Font
            Get
                Return m_Font_Col1
            End Get
            Set(ByVal Value As Font)
                m_Font_Col1 = Value
            End Set
        End Property

        Public Property BorderThickness() As Integer
            Get
                Return m_BorderThickness
            End Get
            Set(ByVal value As Integer)
                m_BorderThickness = value
            End Set
        End Property

        Public Property Padding() As Integer
            Get
                Return m_Padding
            End Get
            Set(ByVal value As Integer)
                m_Padding = value
            End Set
        End Property

        Public Property Opacity() As Integer
            Get
                Return m_bgOpacity
            End Get
            Set(ByVal value As Integer)
                m_bgOpacity = value
            End Set
        End Property

        Public Property BackgroundColor() As System.Drawing.Color
            Get
                Return m_Color_Bg
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Bg = Value
            End Set
        End Property

        Public Property BackgroundGradientColor1() As System.Drawing.Color
            Get
                Return m_Color_Gradient_1
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_1 = Value
            End Set
        End Property

        Public Property BackgroundGradientColor2() As System.Drawing.Color
            Get
                Return m_Color_Gradient_2
            End Get
            Set(ByVal Value As System.Drawing.Color)
                m_Color_Gradient_2 = Value
            End Set
        End Property

        Public Property BorderStyle() As Drawing.Drawing2D.DashStyle
            Get
                Return m_BorderStyle
            End Get
            Set(ByVal value As Drawing.Drawing2D.DashStyle)
                m_BorderStyle = value
            End Set
        End Property

        Public Property BorderColor() As Drawing.Color
            Get
                Return m_BorderColor
            End Get
            Set(ByVal value As Drawing.Color)
                m_BorderColor = value
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

        Property IsGradientBackground() As Boolean
            Get
                Return m_IsGradientBg
            End Get
            Set(ByVal value As Boolean)
                m_IsGradientBg = value
            End Set
        End Property
#End Region

        Public Sub PopulateGrid(ByRef pgrid As PropertyGridEx.PropertyGridEx)

            With pgrid

                .Item.Clear()

                .Item.Add("Range", Me, "SpreadsheetCellRange", False, "", "", True)

                .PropertySort = PropertySort.Alphabetical
                .ShowCustomProperties = True

            End With

        End Sub

        Public Sub CopyToClipboard()

            Dim data As String = ""

            Dim firstcolumn, firstrow, lastcolumn, lastrow As Integer

            'find number of rows and columns by range

            If SpreadsheetCellRange <> "" And Not Spreadsheet Is Nothing Then

                Dim nf As String = Spreadsheet.formc.Options.NumberFormat

                Dim firstcell, lastcell As String

                firstcell = Me.SpreadsheetCellRange.Split(":")(0)
                lastcell = Me.SpreadsheetCellRange.Split(":")(1)

                firstrow = Spreadsheet.GetCellValue(firstcell).RowIndex
                firstcolumn = Spreadsheet.GetCellValue(firstcell).ColumnIndex

                lastrow = Spreadsheet.GetCellValue(lastcell).RowIndex
                lastcolumn = Spreadsheet.GetCellValue(lastcell).ColumnIndex

                Dim grid = Spreadsheet.DataGridView1

                For i = firstrow To lastrow
                    For j = firstcolumn To lastcolumn
                        If Not grid.Rows(i).Cells(j).Value Is Nothing Then
                            If Double.TryParse(grid.Rows(i).Cells(j).Value, New Double()) Then
                                data += Format(Double.Parse(grid.Rows(i).Cells(j).Value), nf) & vbTab
                            Else
                                data += grid.Rows(i).Cells(j).Value.ToString & vbTab
                            End If
                        Else
                            data += "" & vbTab
                        End If
                    Next
                    data += vbCrLf
                Next

            Else

                data = ""

            End If

            Clipboard.SetText(data)

        End Sub

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            Dim iopacity As Integer
            If SemiTransparent Then iopacity = 50 Else iopacity = Me.Opacity

            Dim gContainer As System.Drawing.Drawing2D.GraphicsContainer
            Dim myMatrix As Drawing2D.Matrix
            gContainer = g.BeginContainer()
            SetQuality(g)
            myMatrix = g.Transform()
            If Rotation <> 0 Then
                myMatrix.RotateAt(Rotation, New PointF(X, Y), Drawing.Drawing2D.MatrixOrder.Append)
                g.Transform = myMatrix
            End If

            If Not Me.TextRenderStyle = -1 Then g.TextRenderingHint = Me.TextRenderStyle

            Dim firstcolumn, firstrow, lastcolumn, lastrow As Integer
            Dim maxW As New List(Of Integer)

            'find number of rows and columns by range

            If SpreadsheetCellRange <> "" And Not Spreadsheet Is Nothing Then

                Dim nf As String = Spreadsheet.formc.Options.NumberFormat
                Dim value As Object = Nothing

                Dim firstcell, lastcell As String

                firstcell = Me.SpreadsheetCellRange.Split(":")(0)
                lastcell = Me.SpreadsheetCellRange.Split(":")(1)

                firstrow = Spreadsheet.GetCellValue(firstcell).RowIndex
                firstcolumn = Spreadsheet.GetCellValue(firstcell).ColumnIndex

                lastrow = Spreadsheet.GetCellValue(lastcell).RowIndex
                lastcolumn = Spreadsheet.GetCellValue(lastcell).ColumnIndex

                'determinar comprimento das colunas e altura das linhas

                Dim i, j, k, itemheight, n, m, leftmargin As Integer
                Dim size As SizeF

                Dim grid = Spreadsheet.DataGridView1

                k = 0
                For j = firstcolumn To lastcolumn
                    maxW.Add(10)
                    For i = firstrow To lastrow
                        If Not grid.Rows(i).Cells(j).Value Is Nothing Then
                            If Double.TryParse(grid.Rows(i).Cells(j).Value, New Double()) Then
                                size = g.MeasureString(Format(Double.Parse(grid.Rows(i).Cells(j).Value), nf), Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            Else
                                size = g.MeasureString(grid.Rows(i).Cells(j).Value.ToString, Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            End If
                            If size.Width > maxW(k) Then maxW(k) = size.Width + 4 * Padding
                        Else
                            'maxW(k) = 10
                        End If
                    Next
                    k += 1
                Next

                itemheight = g.MeasureString("AAA", Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0)).Height + 2 * Me.Padding

                Me.Height = (lastrow - firstrow + 1) * itemheight
                Me.Width = maxW.Sum

                If m_BorderPen Is Nothing Then m_BorderPen = New Drawing.Pen(Color.FromArgb(iopacity, Color.Black))

                With Me.m_BorderPen
                    .Color = Color.FromArgb(iopacity, Me.BorderColor)
                    .DashStyle = Me.BorderStyle
                End With

                Dim rect As New Rectangle(X, Y, Width, Height)
                If Me.IsGradientBackground = False Then
                    g.FillRectangle(New SolidBrush(Color.FromArgb(iopacity, Me.BackgroundColor)), rect)
                Else
                    g.FillRectangle(New Drawing2D.LinearGradientBrush(rect, Color.FromArgb(iopacity, Me.BackgroundGradientColor2), Color.FromArgb(iopacity, Me.BackgroundGradientColor1), LinearGradientMode.Vertical), rect)
                End If

                Dim format1 As New StringFormat(StringFormatFlags.NoClip)
                With format1
                    .Alignment = StringAlignment.Far
                    '.LineAlignment = StringAlignment.Far
                End With

                n = 0
                leftmargin = 0
                For j = firstcolumn To lastcolumn
                    m = 0
                    For i = firstrow To lastrow
                        If Not grid.Rows(i).Cells(j).Value Is Nothing Then
                            If Double.TryParse(grid.Rows(i).Cells(j).Value, New Double()) Then
                                g.DrawString(Format(Double.Parse(grid.Rows(i).Cells(j).Value), nf), Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding + leftmargin, Y + Padding + m * itemheight)
                            Else
                                g.DrawString(grid.Rows(i).Cells(j).Value.ToString, Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding + leftmargin, Y + Padding + m * itemheight)
                            End If
                        Else
                            g.DrawString("", Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X + Padding + leftmargin, Y + Padding + m * itemheight)
                        End If
                        If i < lastrow Then g.DrawLine(Me.m_BorderPen, X + leftmargin, Y + (m + 1) * itemheight, X + leftmargin + maxW(n), Y + (m + 1) * itemheight)
                        m += 1
                    Next
                    leftmargin += maxW(n)
                    If j < lastcolumn Then g.DrawLine(Me.m_BorderPen, X + leftmargin, Y, X + leftmargin, Y + (lastrow - firstrow + 1) * itemheight)
                    n += 1
                Next

                g.DrawRectangle(Me.m_BorderPen, New Rectangle(Me.X, Me.Y, Me.Width, Me.Height))

            Else

                g.DrawString("NO_DATA_TO_SHOW", Me.FontCol1, New SolidBrush(Color.FromArgb(iopacity, Me.LineColor)), X, Y)
                Dim size = g.MeasureString("NO_DATA_TO_SHOW", Me.FontCol1, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))

                Me.Height = size.Height
                Me.Width = size.Width

            End If

            g.EndContainer(gContainer)

        End Sub

    End Class


End Namespace
