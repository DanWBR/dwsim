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
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    <Serializable()> _
    Public Class GraphicObjectCollection

        Inherits CollectionBase

        Protected m_HorizRes As Integer = 72
        Protected m_VertRes As Integer = 72

        Public Sub DrawSelectedObject(ByVal g As Graphics, _
                ByVal selectedObject As GraphicObject, _
                ByVal Scale As Single)

            Dim gCon1, gCon2 As Drawing2D.GraphicsContainer

            gCon1 = g.BeginContainer

            g.ScaleTransform(Scale, Scale, _
                Drawing.Drawing2D.MatrixOrder.Append)

            gCon2 = g.BeginContainer

            g.PageUnit = GraphicsUnit.Pixel

            If Not selectedObject Is Nothing Then

                Dim hoverRect As New Rectangle
                With hoverRect
                    Select Case selectedObject.ObjectType
                        Case ObjectType.GO_Animation, ObjectType.GO_MasterTable, ObjectType.GO_Image, ObjectType.GO_Table, ObjectType.GO_FloatingTable, ObjectType.GO_Text, ObjectType.GO_SpreadsheetTable, ObjectType.GO_Rectangle
                            .X = selectedObject.X - 10
                            .Y = selectedObject.Y - 10
                            .Height = selectedObject.Height + 20
                            .Width = selectedObject.Width + 20
                            'Dim strdist As SizeF = g.MeasureString(selectedObject.Tag, New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False), New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                        Case Else
                            .X = selectedObject.X - 10
                            .Y = selectedObject.Y - 10
                            .Height = selectedObject.Height + 30
                            .Width = selectedObject.Width + 20
                            Dim strdist As SizeF = g.MeasureString(selectedObject.Tag, New Font("Arial", 10, FontStyle.Regular, GraphicsUnit.Pixel, 0, False), New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
                            If strdist.Width > selectedObject.Width Then
                                .X = selectedObject.X + (selectedObject.Width - strdist.Width) / 2 - 10
                                .Width = strdist.Width + 20
                            End If
                    End Select
                End With

                Dim myOriginalMatrix As Drawing2D.Matrix
                myOriginalMatrix = g.Transform()
                If selectedObject.Rotation <> 0 Then
                    myOriginalMatrix.RotateAt(selectedObject.Rotation, New PointF(hoverRect.X + hoverRect.Width / 2, hoverRect.Y + hoverRect.Height / 2), _
                       Drawing2D.MatrixOrder.Append)
                    g.Transform = myOriginalMatrix
                End If
                g.PageUnit = GraphicsUnit.Pixel
                Dim color1, color2, color3 As Color
                color1 = Color.FromArgb(50, 130, 185, 200)
                color2 = Color.FromArgb(70, 2, 100, 130)
                color3 = Color.FromArgb(50, 2, 100, 130)
                If hoverRect.Width > 0 Then
                    Dim gbrush As New Drawing2D.LinearGradientBrush(hoverRect, color1, color2, LinearGradientMode.Vertical)
                    DrawRoundRect(g, New Pen(color3, 1), hoverRect.X, hoverRect.Y, hoverRect.Width, hoverRect.Height, 15, gbrush)
                    'g.Transform = myOriginalMatrix
                End If

            End If

            g.EndContainer(gCon2)
            g.EndContainer(gCon1)

        End Sub

        Public Sub DrawRoundRect(ByVal g As Graphics, ByVal p As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal radius As Integer, ByVal myBrush As Brush)

            If width / 2 < radius Then
                radius = width / 2 - 2
            ElseIf height / 2 < radius Then
                radius = height / 2 - 2
            End If

            Dim gp As Drawing2D.GraphicsPath = New Drawing2D.GraphicsPath

            gp.AddLine(x + radius, y, x + width - radius, y)
            gp.AddArc(x + width - radius, y, radius, radius, 270, 90)
            gp.AddLine(x + width, y + radius, x + width, y + height - radius)
            gp.AddArc(x + width - radius, y + height - radius, radius, radius, 0, 90)
            gp.AddLine(x + width - radius, y + height, x + radius, y + height)
            gp.AddArc(x, y + height - radius, radius, radius, 90, 90)
            gp.AddLine(x, y + height - radius, x, y + radius)
            gp.AddArc(x, y, radius, radius, 180, 90)

            gp.CloseFigure()

            g.DrawPath(p, gp)
            g.FillPath(myBrush, gp)

            gp.Dispose()

        End Sub

        Public Sub DrawObjects(ByVal g As Graphics, ByVal Scale As Single, transparent As Boolean)
            Dim drawObj As GraphicObject
            Dim i As Integer
            Dim gCon As Drawing2D.GraphicsContainer
            Dim myOriginalMatrix As Drawing2D.Matrix
            myOriginalMatrix = g.Transform()
            gCon = g.BeginContainer()
            g.PageUnit = GraphicsUnit.Pixel
            g.ScaleTransform(Scale, Scale)
            Dim oldlinecolor, oldfillcolor, oldgradcolor1, oldgradcolor2 As Color
            If Not Me.InnerList Is Nothing AndAlso Me.InnerList.Count > 0 Then
                For i = 0 To Me.InnerList.Count - 1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    If drawObj.ObjectType = ObjectType.GO_Rectangle Then
                        If transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, RectangleGraphic)
                                oldlinecolor = .LineColor
                                oldfillcolor = .FillColor
                                oldgradcolor1 = .GradientColor1
                                oldgradcolor2 = .GradientColor2
                                .LineColor = Color.FromArgb(50, .LineColor)
                                .FillColor = Color.FromArgb(50, .FillColor)
                                .GradientColor1 = Color.FromArgb(50, .GradientColor1)
                                .GradientColor2 = Color.FromArgb(50, .GradientColor2)
                            End With
                        End If
                        drawObj.Draw(g)
                        If transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, RectangleGraphic)
                                .LineColor = oldlinecolor
                                .FillColor = oldfillcolor
                                .GradientColor1 = oldgradcolor1
                                .GradientColor2 = oldgradcolor2
                            End With
                        End If
                    End If
                Next
                For i = 0 To Me.InnerList.Count - 1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    If drawObj.ObjectType = ObjectType.Nenhum Then
                        If TypeOf drawObj Is ShapeGraphic And transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, ShapeGraphic)
                                oldlinecolor = .LineColor
                                oldfillcolor = .FillColor
                                oldgradcolor1 = .GradientColor1
                                oldgradcolor2 = .GradientColor2
                                .LineColor = Color.FromArgb(50, .LineColor)
                                .FillColor = Color.FromArgb(50, .FillColor)
                                .GradientColor1 = Color.FromArgb(50, .GradientColor1)
                                .GradientColor2 = Color.FromArgb(50, .GradientColor2)
                                .SemiTransparent = True
                            End With
                        End If
                        If drawObj.ObjectType <> ObjectType.GO_Rectangle Then drawObj.Draw(g)
                        If TypeOf drawObj Is ShapeGraphic And transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, ShapeGraphic)
                                .LineColor = oldlinecolor
                                .FillColor = oldfillcolor
                                .GradientColor1 = oldgradcolor1
                                .GradientColor2 = oldgradcolor2
                                .SemiTransparent = False
                            End With
                        End If
                    End If
                Next
                For i = 0 To Me.InnerList.Count - 1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    If drawObj.ObjectType <> Interfaces.Enums.GraphicObjects.ObjectType.Nenhum Then
                        If TypeOf drawObj Is ShapeGraphic And transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, ShapeGraphic)
                                oldlinecolor = .LineColor
                                oldfillcolor = .FillColor
                                oldgradcolor1 = .GradientColor1
                                oldgradcolor2 = .GradientColor2
                                .SemiTransparent = True
                            End With
                        End If
                        If drawObj.ObjectType <> ObjectType.GO_Rectangle Then drawObj.Draw(g)
                        If TypeOf drawObj Is ShapeGraphic And transparent And Not drawObj.Selected Then
                            With DirectCast(drawObj, ShapeGraphic)
                                .LineColor = oldlinecolor
                                .FillColor = oldfillcolor
                                .GradientColor1 = oldgradcolor1
                                .GradientColor2 = oldgradcolor2
                                .SemiTransparent = False
                            End With
                        End If
                    End If
                Next
            End If
            g.EndContainer(gCon)
            g.Transform = myOriginalMatrix
        End Sub

        Public Sub PrintObjects(ByVal g As Graphics, ByVal dx As Integer, ByVal dy As Integer)
            Dim drawObj As GraphicObject
            Dim i As Integer
            g.PageUnit = GraphicsUnit.Pixel
            If Not Me.InnerList Is Nothing AndAlso Me.InnerList.Count > 0 Then
                For i = 0 To Me.InnerList.Count - 1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    drawObj.X += dx
                    drawObj.Y += dy
                    drawObj.Draw(g)
                Next
                For i = 0 To Me.InnerList.Count - 1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    drawObj.X -= dx
                    drawObj.Y -= dy
                Next
            End If
        End Sub

        Public Function FindObjectAtPoint(ByVal pt As Point) As GraphicObject

            Dim drawObj As GraphicObject
            Dim i As Integer
            If Not Me.InnerList Is Nothing AndAlso _
                    Me.InnerList.Count > 0 Then
                For i = Me.InnerList.Count - 1 To 0 Step -1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    If drawObj.HitTest(pt.ToSDPoint) Then
                        Return drawObj
                        Exit For
                    End If
                Next
            End If
            Return Nothing

        End Function

        Public Function FindObjectWithName(ByVal name As String) As GraphicObject

            Dim drawObj As GraphicObject
            Dim i As Integer
            If Not Me.InnerList Is Nothing AndAlso _
                    Me.InnerList.Count > 0 Then
                For i = Me.InnerList.Count - 1 To 0 Step -1
                    drawObj = CType(Me.InnerList(i), GraphicObject)
                    If drawObj.Name = name Then
                        Return drawObj
                        Exit For
                    End If
                Next
            End If
            Return Nothing

        End Function

        Public Property HorizontalResolution() As Integer
            Get
                Return m_HorizRes
            End Get
            Set(ByVal Value As Integer)
                m_HorizRes = Value
            End Set
        End Property

        Public Property VerticalResolution() As Integer
            Get
                Return m_VertRes
            End Get
            Set(ByVal Value As Integer)
                m_VertRes = Value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal value As GraphicObjectCollection)
            MyBase.New()
            Me.AddRange(value)
        End Sub

        Public Sub New(ByVal value() As GraphicObject)
            MyBase.New()
            Me.AddRange(value)
        End Sub

        Default Public Property Item(ByVal index As Integer) As GraphicObject
            Get
                Return CType(List(index), GraphicObject)
            End Get
            Set(ByVal Value As GraphicObject)
                List(index) = Value
            End Set
        End Property

        Public Function Add(ByVal value As GraphicObject) As Integer
            Return List.Add(value)
        End Function

        Public Overloads Sub AddRange(ByVal value() As GraphicObject)
            Dim i As Integer = 0
            Do While (i < value.Length)
                Me.Add(value(i))
                i = (i + 1)
            Loop
        End Sub

        Public Overloads Sub AddRange(ByVal value As GraphicObjectCollection)
            Dim i As Integer = 0
            Do While (i < value.Count)
                Me.Add(value(i))
                i = (i + 1)
            Loop
        End Sub

        Public Function Contains(ByVal value As GraphicObject) As Boolean
            Return List.Contains(value)
        End Function

        Public Sub CopyTo(ByVal array() As GraphicObject, ByVal index As Integer)
            List.CopyTo(array, index)
        End Sub

        Public Function IndexOf(ByVal value As GraphicObject) As Integer
            Return List.IndexOf(value)
        End Function

        Public Sub Insert(ByVal index As Integer, ByVal value As GraphicObject)
            List.Insert(index, value)
        End Sub

        'See also 'System.Collections.IEnumerator'
        Public Shadows Function GetEnumerator() As GraphicObjectEnumerator
            Return New GraphicObjectEnumerator(Me)
        End Function

        Public Sub Remove(ByVal value As GraphicObject)
            List.Remove(value)
        End Sub

        Public Class GraphicObjectEnumerator
            Inherits Object
            Implements IEnumerator

            Private baseEnumerator As IEnumerator

            Private temp As IEnumerable

            Public Sub New(ByVal mappings As GraphicObjectCollection)
                MyBase.New()
                Me.temp = CType(mappings, IEnumerable)
                Me.baseEnumerator = temp.GetEnumerator
            End Sub

            Public ReadOnly Property Current() As GraphicObject
                Get
                    Return CType(baseEnumerator.Current, GraphicObject)
                End Get
            End Property

            ReadOnly Property IEnumerator_Current() As Object Implements IEnumerator.Current
                Get
                    Return baseEnumerator.Current
                End Get
            End Property

            Public Function MoveNext() As Boolean
                Return baseEnumerator.MoveNext
            End Function

            Function IEnumerator_MoveNext() As Boolean Implements IEnumerator.MoveNext
                Return baseEnumerator.MoveNext
            End Function

            Public Sub Reset()
                baseEnumerator.Reset()
            End Sub

            Sub IEnumerator_Reset() Implements IEnumerator.Reset
                baseEnumerator.Reset()
            End Sub
        End Class

    End Class

End Namespace
