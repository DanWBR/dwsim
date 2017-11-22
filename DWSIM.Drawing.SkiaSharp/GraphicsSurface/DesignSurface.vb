Imports System.Runtime.Serialization
Imports System.Linq
Imports System.Collections.Generic
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums

Public Class GraphicsSurface

    Private DrawingCanvas As SKCanvas

#Region "Events"

    Public Event SelectionChanged(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)

    Public Event StatusUpdate(ByVal sender As Object, ByVal e As StatusUpdateEventArgs)

#End Region

    Private Const MinimumGridSize As Single = 10
    Private dragOffset As New SKPoint(0, 0)
    Private dragStart As New SKPoint(0, 0)

    Public selectionDragging As Boolean = False
    Private selectable As Boolean = True
    Public dragging As Boolean = False
    Private draggingfs As Boolean
    Private rotating As Boolean = False
    Private hoverdraw As Boolean = False
    Private startingRotation As Single = 0
    Private originalRotation As Single = 0
    Private selectionRect As SKRect
    Private rectp0 As New SKPoint
    Private hoverRect As SKRect
    Private hoverrotation As Integer = 0
    Private Size0 As SKSize

    Private justselected As Boolean = False

    Public DrawingObjects As New List(Of IGraphicObject)

    Private _SelectedObject As IGraphicObject

    Private _FloatingTable As Tables.FloatingTableGraphic

    Private AnimationStart As DateTime
    Private AddedObject As IGraphicObject
    Private DrawAddedAnimation As Boolean = False
    Private Tmax As Integer = 3000
    Private Tfactor As Integer = 1000

    Public InvalidateCallback As Action

    Public Enum AlignDirection
        Lefts
        Centers
        Rights
        Tops
        Middles
        Bottoms
        EqualizeHorizontal
        EqualizeVertical
    End Enum

    Public Property BackgroundColor As SKColor = SKColors.White

    Public Property ResizingMode As Boolean = False

    Public Property ResizingMode_KeepAR As Boolean = True

    Public Property QuickConnect As Boolean = False

    Public Property SurfaceBounds As SKRect

    Public Property SnapToGrid As Boolean = False

    Public Property SurfaceMargins As SKRect

    Public Property Zoom() As Single = 1.0F

    Public Overridable Property ShowGrid() As Boolean = False

    Public Property GridColor() As SKColor

    Public Overridable Property GridLineWidth() As Integer

    Public Overridable Property GridSize() As Single

    Public Property Size As SKSize = New SKSize(1024, 768)

    Public Property DrawFloatingTable As Boolean = True

    Public Property DrawPropertyList As Boolean = False

    Public Property SelectedObject() As IGraphicObject
        Get
            Return _SelectedObject
        End Get
        Set(value As IGraphicObject)
            If Not value Is Nothing AndAlso Not value.IsConnector Then
                _SelectedObject = value
            Else
                _SelectedObject = Nothing
            End If
            RaiseEvent SelectionChanged(Me, New SelectionChangedEventArgs(_SelectedObject))
        End Set
    End Property

    Public Property SelectedObjects() As New Generic.Dictionary(Of String, IGraphicObject)

    Public Sub UpdateSurface(surface As SKSurface)

        DrawingCanvas = surface.Canvas

        'draw the actual objects onto the page, on top of the grid

        If Me.SelectedObject Is Nothing Then Me.SelectedObjects.Clear()

        DrawingCanvas.Clear(BackgroundColor)

        DrawingCanvas.Scale(Me.Zoom, Me.Zoom)

        Dim objects = DrawingObjects.ToArray

        If objects.Count = 0 Then DrawInstructions(DrawingCanvas)

        If DrawAddedAnimation Then
            Dim tstep = (Date.Now - AnimationStart).TotalMilliseconds
            Dim sp As New SKPaint
            With sp
                Dim avalue As Integer = 255 - tstep / Tmax * 255
                If avalue < 0 Then avalue = 0
                .Color = SKColors.SteelBlue.WithAlpha(avalue)
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Shader = SKShader.CreateRadialGradient(New SKPoint(AddedObject.X + AddedObject.Width / 2, AddedObject.Y + AddedObject.Height / 2), Math.Max(AddedObject.Width * (1 + tstep / Tmax), AddedObject.Height * (1 + tstep / Tmax)), New SKColor() {SKColors.LightBlue, SKColors.Transparent}, New Single() {0.05, 0.95}, SKShaderTileMode.Clamp)
            End With
            DrawingCanvas.DrawRoundRect(New SKRect(AddedObject.X - 250 * tstep / Tfactor, AddedObject.Y - 250 * tstep / Tfactor, AddedObject.X + AddedObject.Width + 250 * tstep / Tfactor, AddedObject.Y + AddedObject.Height + 250 * tstep / Tfactor), 10, 10, sp)
        End If

        For Each dobj In objects
            If dobj Is SelectedObject Then
                Dim sp, sp2 As New SKPaint
                With sp
                    .Color = SKColors.LightBlue.WithAlpha(75)
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = False
                End With
                With sp2
                    .Color = SKColors.LightBlue.WithAlpha(175)
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = True
                    .StrokeWidth = 2
                End With
                If dobj.Rotation <> 0 Then
                    DrawingCanvas.Save()
                    DrawingCanvas.RotateDegrees(dobj.Rotation, dobj.X + dobj.Width / 2, dobj.Y + dobj.Height / 2)
                    DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp)
                    DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp2)
                    DrawingCanvas.Restore()
                Else
                    DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp)
                    DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp2)
                End If
            End If

            If dobj.DrawOverride IsNot Nothing Then

                dobj.DrawOverride.Invoke(DrawingCanvas)

            Else

                Dim currmat = DrawingCanvas.TotalMatrix

                DrawingCanvas.Save()

                If dobj.FlippedV And Not dobj.FlippedH Then
                    DrawingCanvas.Scale(-1, 1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                ElseIf dobj.FlippedH And Not dobj.FlippedV Then
                    DrawingCanvas.Scale(1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                ElseIf dobj.FlippedH And dobj.FlippedV Then
                    DrawingCanvas.Scale(-1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                End If

                If dobj.Rotation <> 0 Then DrawingCanvas.RotateDegrees(dobj.Rotation, dobj.X + dobj.Width / 2, dobj.Y + dobj.Height / 2)

                dobj.Draw(DrawingCanvas)

                DrawingCanvas.SetMatrix(currmat)

                If TypeOf dobj Is ShapeGraphic And
                    dobj.ObjectType <> ObjectType.GO_MasterTable And
                    dobj.ObjectType <> ObjectType.GO_SpreadsheetTable And
                    dobj.ObjectType <> ObjectType.GO_Table And
                    dobj.ObjectType <> ObjectType.GO_Animation And
                    dobj.ObjectType <> ObjectType.GO_Chart And
                    dobj.ObjectType <> ObjectType.GO_Image And
                    dobj.ObjectType <> ObjectType.GO_Text And
                    dobj.ObjectType <> ObjectType.GO_FloatingTable Then

                    DirectCast(dobj, ShapeGraphic).DrawTag(DrawingCanvas)

                End If

            End If
        Next

        If DrawPropertyList Then
            For Each dobj In objects
                If dobj.Calculated Then DrawPropertyListBlock(DrawingCanvas, dobj)
            Next
        End If

    End Sub

    Private Sub DrawInstructions(canvas As SKCanvas)

        Dim text As New Text.StringBuilder

        text.AppendLine("How to create and run a new simulation")
        text.AppendLine("(If you've configured the simulation using the Setup Wizard, skip to Step 3)")
        text.AppendLine("")
        text.AppendLine("1. Select Compounds to add to the simulation ('Setup' > 'Compounds')")
        text.AppendLine("2. Add at least one Property Package and one Flash Algorithm ('Setup' > 'Basis')")
        text.AppendLine("3. Add Unit Operations to the Flowsheet ('Objects' > 'Add New Simulation Object')")
        text.AppendLine("4. Add Material and Energy Streams to the Flowsheet ('Objects' > 'Add New Simulation Object')")
        text.AppendLine("5. Connect unit operation blocks to streams (select object, right-click, select 'Edit Connections')")
        text.AppendLine("6. Edit properties of the upstream Material Streams and all Unit Operations (select object, right-click, select 'Edit Properties')")
        text.AppendLine("7. Run the simulation (press F5)")
        text.AppendLine("8. To view the simulation results, go the 'Results' tab and select an object on the list.")
        text.AppendLine("")
        text.AppendLine("Useful shortcuts")
        text.AppendLine("")
        text.AppendLine("Double-click on selected object: edit properties")
        text.AppendLine("SHIFT + Double-click on selected object: edit connections")
        text.AppendLine("ALT + Double-click on selected object: view results")
        text.AppendLine("CTRL + Double-click on selected object: debug object")
        text.AppendLine("Double-click on blank area: zoom flowsheet to fit window")
        text.AppendLine("Mouse wheel: zoom in/out")

        Dim tpaint As New SKPaint()
        With tpaint
            .TextSize = 16
            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            .Color = SKColors.LightSlateGray
            .IsStroke = False
        End With

        Dim lines = text.ToString().Split(vbLf)

        Dim newy As Integer = 50

        For Each l As String In lines
            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath(l, 0, 0).GetBounds(trect)
            newy += trect.Height + 10
            canvas.DrawText(l, 50, newy, tpaint)
        Next

    End Sub

    Public Sub AlignSelectedObjects(direction As AlignDirection)

        If Me.SelectedObjects.Count > 1 Then

            Dim refpos As Integer = 0

            Select Case direction
                Case AlignDirection.Lefts
                    refpos = 10000000
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        If obj.X < refpos Then refpos = obj.X
                    Next
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.X = refpos
                    Next
                Case AlignDirection.Centers
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        refpos += obj.X + obj.Width / 2
                    Next
                    refpos /= Me.SelectedObjects.Count
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.X = refpos - obj.Width / 2
                    Next
                Case AlignDirection.Rights
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        refpos += obj.X + obj.Width
                    Next
                    refpos /= Me.SelectedObjects.Count
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.X = refpos - obj.Width
                    Next
                Case AlignDirection.Tops
                    refpos = 10000000
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        If obj.Y < refpos Then refpos = obj.Y
                    Next
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.Y = refpos
                    Next
                Case AlignDirection.Middles
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        refpos += obj.Y + obj.Height / 2
                    Next
                    refpos /= Me.SelectedObjects.Count
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.Y = refpos - obj.Height / 2
                    Next
                Case AlignDirection.Bottoms
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        refpos += obj.Y + obj.Height
                    Next
                    refpos /= Me.SelectedObjects.Count
                    For Each obj As IGraphicObject In Me.SelectedObjects.Values
                        obj.Y = refpos - obj.Height
                    Next
                Case AlignDirection.EqualizeHorizontal
                    Dim orderedlist As List(Of IGraphicObject) = Me.SelectedObjects.Values.OrderBy(Function(o) o.X).ToList
                    Dim avgdist As Integer, i As Integer
                    For i = 1 To orderedlist.Count - 1
                        avgdist += (orderedlist(i).X) - (orderedlist(i - 1).X + orderedlist(i - 1).Width)
                    Next
                    avgdist /= (orderedlist.Count - 1)
                    For i = 1 To orderedlist.Count - 1
                        orderedlist(i).X = orderedlist(i - 1).X + orderedlist(i - 1).Width + avgdist
                    Next
                Case AlignDirection.EqualizeVertical
                    Dim orderedlist As List(Of IGraphicObject) = Me.SelectedObjects.Values.OrderBy(Function(o) o.Y).ToList
                    Dim avgdist As Integer, i As Integer
                    For i = 1 To orderedlist.Count - 1
                        avgdist += (orderedlist(i).Y) - (orderedlist(i - 1).Y + orderedlist(i - 1).Height)
                    Next
                    avgdist /= (orderedlist.Count - 1)
                    For i = 1 To orderedlist.Count - 1
                        orderedlist(i).Y = orderedlist(i - 1).Y + orderedlist(i - 1).Height + avgdist
                    Next
            End Select

        End If

    End Sub

    Public Sub OffsetAll(deltax As Integer, deltay As Integer)

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If Not gobj.IsConnector Then
                gobj.X += deltax
                gobj.Y += deltay
            End If
        Next

    End Sub

    Public Sub ZoomAll(viewwidth As Integer, viewheight As Integer)

        Size = New SKSize(viewwidth, viewheight)

        Dim minx As Integer = Integer.MaxValue
        Dim miny As Integer = Integer.MaxValue
        Dim maxx As Integer = 0
        Dim maxy As Integer = 0

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If gobj.ObjectType <> ObjectType.Nenhum Then
                If gobj.X <= minx Then minx = gobj.X - 30
                If gobj.X + gobj.Width >= maxx Then maxx = gobj.X + gobj.Width + 30
                If gobj.Y <= miny Then miny = gobj.Y
                If gobj.Y + gobj.Height >= maxy Then maxy = gobj.Y + gobj.Height + 60
            End If
        Next

        Dim newx, newy As Integer

        newx = minx - 30
        newy = miny - 30

        If newx < 0 Then newx = 30
        If newy < 0 Then newy = 30

        Dim zoomx As Double = 1.0#
        Dim zoomy As Double = 1.0#

        zoomx = viewwidth / (maxx - newx)
        zoomy = viewheight / (maxy - newy)

        Dim newzoom As Single = Math.Min(zoomx, zoomy)

        If newzoom > 0.0# And newzoom < 100.0# Then

            Zoom = newzoom

            Dim mindevx, mindevy As Integer

            mindevx = 30
            mindevy = 30

            Dim deltax, deltay As Integer
            deltax = mindevx - minx
            deltay = mindevy - miny

            For Each gobj As IGraphicObject In Me.DrawingObjects
                If Not gobj.IsConnector Then
                    gobj.X += deltax
                    gobj.Y += deltay
                End If
            Next

        End If

    End Sub

    Public Sub Center()

        Dim minx As Integer = Integer.MaxValue
        Dim miny As Integer = Integer.MaxValue
        Dim maxx As Integer = 0
        Dim maxy As Integer = 0
        Dim middlex, middley As Integer

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If gobj.ObjectType <> ObjectType.Nenhum Then
                If gobj.X <= minx Then minx = gobj.X
                If gobj.X + gobj.Width >= maxx Then maxx = gobj.X + gobj.Width + 60
                If gobj.Y <= miny Then miny = gobj.Y
                If gobj.Y + gobj.Height >= maxy Then maxy = gobj.Y + gobj.Height + 60
            End If
        Next

        middlex = (minx + maxx) / 2
        middley = (miny + maxy) / 2

    End Sub

    Public Sub InputMove(x As Integer, y As Integer)

        Dim dx As Integer = -(x - dragStart.X)
        Dim dy As Integer = -(y - dragStart.Y)

        If Not ResizingMode Then

            If draggingfs Then

                Dim dragPoint As New SKPoint(x, y)

                dragPoint.X += dragOffset.X
                dragPoint.Y += dragOffset.Y

                For Each gr As IGraphicObject In Me.DrawingObjects
                    If Not gr.IsConnector Then
                        Dim p As SKPoint = New SKPoint(gr.X, gr.Y)
                        p.X += (x - dragStart.X) / Me.Zoom
                        p.Y += (y - dragStart.Y) / Me.Zoom
                        gr.X = p.X
                        gr.Y = p.Y
                    End If
                Next

                dragStart = New SKPoint(x, y)

            Else

                Dim dragPoint As New SKPoint(x, y)

                If Not SelectedObject Is Nothing Then

                    If Not SelectedObject.IsConnector Then

                        If dragging Then

                            dragPoint.X += dragOffset.X
                            dragPoint.Y += dragOffset.Y

                            For Each gr As IGraphicObject In Me.SelectedObjects.Values
                                Dim p As SKPoint = New SKPoint(gr.X, gr.Y)
                                p.X += (x - dragStart.X) / Me.Zoom
                                p.Y += (y - dragStart.Y) / Me.Zoom
                                gr.X = p.X
                                gr.Y = p.Y
                            Next

                            dragStart = New SKPoint(x, y)

                            RaiseEvent StatusUpdate(Me, New StatusUpdateEventArgs(StatusUpdateType.ObjectMoved, Me.SelectedObject, String.Format("Object Moved to {0}, {1}", dragPoint.X, dragPoint.Y), dragPoint, 0))

                        End If

                    End If

                Else

                    If selectionDragging Then

                        Dim x0, y0, x1, y1 As Integer

                        x0 = rectp0.X
                        y0 = rectp0.Y
                        x1 = dragPoint.X * Me.Zoom
                        y1 = dragPoint.Y * Me.Zoom

                        If x1 > x0 Then
                            selectionRect.Left = x0
                        Else
                            selectionRect.Left = x1
                        End If

                        If y1 > y0 Then
                            selectionRect.Top = y0
                        Else
                            selectionRect.Top = y1
                        End If

                        selectionRect.Right = selectionRect.Left + Math.Abs(x1 - x0)
                        selectionRect.Bottom = selectionRect.Top + Math.Abs(y1 - y0)

                    End If

                End If

                If Not dragging And DrawFloatingTable Then

                    Dim gobj As GraphicObject = FindObjectAtPoint(New SKPoint(x, y))

                    If Not gobj Is Nothing Then

                        If _FloatingTable Is Nothing And Not _
                                gobj.ObjectType = ObjectType.GO_FloatingTable And Not _
                                gobj.ObjectType = ObjectType.GO_MasterTable And Not _
                                gobj.ObjectType = ObjectType.GO_SpreadsheetTable And Not _
                                gobj.ObjectType = ObjectType.GO_Table And Not _
                                gobj.ObjectType = ObjectType.GO_Image And Not _
                                gobj.ObjectType = ObjectType.GO_Text And Not _
                                gobj.ObjectType = ObjectType.GO_Rectangle And Not _
                                gobj.ObjectType = ObjectType.GO_Chart And Not _
                                gobj.ObjectType = ObjectType.Nenhum Then

                            Dim flowsheet = gobj.Owner.GetFlowsheet()

                            If gobj.Calculated Or gobj.ObjectType = ObjectType.OT_Adjust Or
                                gobj.ObjectType = ObjectType.OT_Spec Or gobj.ObjectType = ObjectType.OT_Recycle Or
                                gobj.ObjectType = ObjectType.OT_EnergyRecycle Then

                                If flowsheet.SimulationObjects.ContainsKey(gobj.Name) Then

                                    Dim obj = flowsheet.SimulationObjects(gobj.Name)

                                    Dim tabela As New Tables.FloatingTableGraphic(obj, (x + 25) / Zoom, (y + 25) / Zoom)
                                    tabela.Owner = obj
                                    tabela.Tag = obj.Name
                                    tabela.Name = "QTAB-" & Guid.NewGuid.ToString
                                    tabela.HeaderText = gobj.Tag
                                    tabela.AdditionalInfo = Zoom
                                    _FloatingTable = tabela
                                    DrawingObjects.Add(tabela)

                                End If

                            End If

                        ElseIf gobj.ObjectType = ObjectType.GO_FloatingTable Then

                            If DrawingObjects.Contains(_FloatingTable) Then DrawingObjects.Remove(_FloatingTable)
                            _FloatingTable = Nothing

                        End If

                    Else

                        Try
                            If DrawingObjects.Contains(_FloatingTable) Then DrawingObjects.Remove(_FloatingTable)
                            _FloatingTable = Nothing
                        Catch ex As Exception
                            Console.WriteLine(ex.Message)
                        End Try

                    End If

                Else

                    Try
                        If DrawingObjects.Contains(_FloatingTable) Then DrawingObjects.Remove(_FloatingTable)
                        _FloatingTable = Nothing
                    Catch ex As Exception
                        Console.WriteLine(ex.Message)
                    End Try

                End If

            End If

        Else

            If Not Me.SelectedObject Is Nothing Then

                Dim neww, newh As Integer

                If ResizingMode_KeepAR Then
                    neww = Size0.Width - Convert.ToDouble(Size0.Width / Size0.Height) * Math.Min(dx, dy)
                    newh = Size0.Height - Math.Min(dx, dy)
                Else
                    neww = Size0.Width - dx
                    newh = Size0.Height - dy
                End If

                If neww > 10 And newh > 10 Then
                    Me.SelectedObject.Width = neww
                    Me.SelectedObject.Height = newh
                Else
                    Me.SelectedObject.Width = 10
                    Me.SelectedObject.Height = 10
                End If

            End If

        End If

    End Sub

    Public Sub InputRelease()

        If Not ResizingMode Then

            draggingfs = False
            dragging = False

            If selectionDragging Then

                Dim zoomedSelection As SKRect = DeZoomRectangle(selectionRect)
                Dim graphicObj As IGraphicObject

                For Each graphicObj In Me.DrawingObjects
                    If graphicObj.HitTest(zoomedSelection) Then
                        Me.SelectedObject = graphicObj
                        Exit For
                    End If
                Next

                For Each graphicObj In Me.DrawingObjects
                    If graphicObj.HitTest(zoomedSelection) Then
                        If Not Me.SelectedObjects.ContainsKey(graphicObj.Name) Then Me.SelectedObjects.Add(graphicObj.Name, graphicObj)
                    End If
                Next

                selectionDragging = False
                justselected = True

            End If

        Else

            Size0 = Nothing

        End If

    End Sub

    Public Sub InputPress(x As Integer, y As Integer)

        Dim mousePT As New SKPoint(x, y)

        dragStart = New SKPoint(x, y)

        Me.SelectedObject = FindObjectAtPoint(mousePT)

        If Not SelectedObject Is Nothing Then Size0 = New SKSize(SelectedObject.Width, SelectedObject.Height)

        If Not ResizingMode Then

            If SelectedObject Is Nothing Then
                Me.SelectedObjects.Clear()
                justselected = False
                draggingfs = True
            Else
                If Not justselected Then Me.SelectedObjects.Clear()
                If Not Me.SelectedObjects.ContainsKey(Me.SelectedObject.Name) Then
                    Me.SelectedObjects.Add(Me.SelectedObject.Name, Me.SelectedObject)
                End If
                justselected = False
            End If

            If Not SelectedObject Is Nothing Then
                dragging = True
                dragOffset.X = SelectedObject.X - mousePT.X
                dragOffset.Y = SelectedObject.Y - mousePT.Y
            End If

        End If

    End Sub

    Public Sub DeleteSelectedObject(gobj As IGraphicObject)
        Dim objectToDelete As IGraphicObject = gobj
        If Not objectToDelete Is Nothing Then
            If Me.DrawingObjects.Contains(objectToDelete) Then
                Me.DrawingObjects.Remove(objectToDelete)
                Me.SelectedObject = Nothing
            End If
        End If
    End Sub

    Public Sub DeleteAllObjects()
        Me.DrawingObjects.Clear()
    End Sub

    Private Function AngleToPoint(ByVal Origin As SKPoint, _
            ByVal Target As SKPoint) As Single
        'a cool little utility function, 
        'given two points finds the angle between them....
        'forced me to recall my highschool math, 
        'but the task is made easier by a special overload to
        'Atan that takes X,Y co-ordinates.
        Dim Angle As Single
        Target.X = Target.X - Origin.X
        Target.Y = Target.Y - Origin.Y
        Angle = Math.Atan2(Target.Y, Target.X) / (Math.PI / 180)
        Return Angle
    End Function


    Public Sub ConnectObject(ByVal gObjFrom As GraphicObject, ByVal gObjTo As GraphicObject, Optional ByVal fidx As Integer = -1, Optional ByVal tidx As Integer = -1)

        If gObjFrom.ObjectType <> ObjectType.GO_Image And gObjFrom.ObjectType <> ObjectType.GO_Table And
        gObjFrom.ObjectType <> ObjectType.GO_Table And gObjFrom.ObjectType <> ObjectType.GO_FloatingTable And
        gObjFrom.ObjectType <> ObjectType.Nenhum And
        gObjTo.ObjectType <> ObjectType.GO_Image And gObjTo.ObjectType <> ObjectType.GO_Table And
        gObjTo.ObjectType <> ObjectType.GO_Table And gObjTo.ObjectType <> ObjectType.GO_FloatingTable And
        gObjTo.ObjectType <> ObjectType.Nenhum And gObjTo.ObjectType <> ObjectType.GO_MasterTable Then

            Dim con1OK As Boolean = False
            Dim con2OK As Boolean = False

            'posicionar pontos nos primeiros slots livres
            Dim StartPos, EndPos As New Point
            Dim InConSlot As IConnectionPoint = Nothing
            Dim OutConSlot As IConnectionPoint = Nothing
            If Not gObjFrom Is Nothing Then
                If Not gObjTo Is Nothing Then
                    If gObjFrom.ObjectType = ObjectType.MaterialStream And gObjTo.ObjectType = ObjectType.MaterialStream Then
                        Throw New Exception("This connection is not allowed.")
                    ElseIf gObjFrom.ObjectType = ObjectType.EnergyStream And gObjTo.ObjectType = ObjectType.EnergyStream Then
                        Throw New Exception("This connection is not allowed.")
                    ElseIf Not gObjFrom.ObjectType = ObjectType.MaterialStream And Not gObjFrom.ObjectType = ObjectType.EnergyStream Then
                        If Not gObjTo.ObjectType = ObjectType.EnergyStream And Not gObjTo.ObjectType = ObjectType.MaterialStream Then
                            Throw New Exception("This connection is not allowed.")
                        End If
                    ElseIf gObjFrom.ObjectType = ObjectType.MaterialStream And gObjTo.ObjectType = ObjectType.EnergyStream Then
                        Throw New Exception("This connection is not allowed.")
                    ElseIf gObjFrom.ObjectType = ObjectType.EnergyStream And gObjTo.ObjectType = ObjectType.MaterialStream Then
                        Throw New Exception("This connection is not allowed.")
                    End If
                    If gObjTo.IsEnergyStream = False Then
                        If Not gObjFrom.IsEnergyStream Then
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConIn Then
                                        EndPos.X = InConSlot.Position.X
                                        EndPos.Y = InConSlot.Position.Y
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConIn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos.X = InConSlot.Position.X
                                    EndPos.Y = InConSlot.Position.Y
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                        Else
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConEn Then
                                        EndPos.X = InConSlot.Position.X
                                        EndPos.Y = InConSlot.Position.Y
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConEn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos.X = InConSlot.Position.X
                                    EndPos.Y = InConSlot.Position.Y
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                            If Not con2OK Then
                                Throw New Exception("This connection is not allowed.")
                                Exit Sub
                            End If
                        End If
                        If fidx = -1 Then
                            For Each OutConSlot In gObjFrom.OutputConnectors
                                If Not OutConSlot.IsAttached Then
                                    StartPos.X = OutConSlot.Position.X
                                    StartPos.Y = OutConSlot.Position.Y
                                    OutConSlot.IsAttached = True
                                    If con2OK Then con1OK = True
                                    Exit For
                                End If
                            Next
                        Else
                            If Not gObjFrom.OutputConnectors(fidx).IsAttached Then
                                OutConSlot = gObjFrom.OutputConnectors(fidx)
                                StartPos.X = OutConSlot.Position.X
                                StartPos.Y = OutConSlot.Position.Y
                                OutConSlot.IsAttached = True
                                If con2OK Then con1OK = True
                            End If
                        End If
                    Else
                        Select Case gObjFrom.ObjectType
                            Case ObjectType.Cooler, ObjectType.Pipe, ObjectType.Expander, ObjectType.ShortcutColumn, ObjectType.DistillationColumn, ObjectType.AbsorptionColumn,
                                ObjectType.ReboiledAbsorber, ObjectType.RefluxedAbsorber, ObjectType.OT_EnergyRecycle, ObjectType.ComponentSeparator, ObjectType.SolidSeparator,
                                ObjectType.Filter, ObjectType.CustomUO, ObjectType.CapeOpenUO, ObjectType.FlowsheetUO
                                GoTo 100
                            Case Else
                                Throw New Exception("This connection is not allowed.")
                        End Select
100:                    If gObjFrom.ObjectType <> ObjectType.CapeOpenUO And gObjFrom.ObjectType <> ObjectType.CustomUO And gObjFrom.ObjectType <> ObjectType.DistillationColumn _
                            And gObjFrom.ObjectType <> ObjectType.AbsorptionColumn And gObjFrom.ObjectType <> ObjectType.OT_EnergyRecycle _
                            And gObjFrom.ObjectType <> ObjectType.RefluxedAbsorber And gObjFrom.ObjectType <> ObjectType.ReboiledAbsorber Then
                            If Not gObjFrom.EnergyConnector.IsAttached Then
                                StartPos.X = gObjFrom.EnergyConnector.Position.X
                                StartPos.Y = gObjFrom.EnergyConnector.Position.Y
                                gObjFrom.EnergyConnector.IsAttached = True
                                con1OK = True
                                OutConSlot = gObjFrom.EnergyConnector
                                EndPos.X = gObjTo.InputConnectors(0).Position.X
                                EndPos.Y = gObjTo.InputConnectors(0).Position.Y
                                gObjTo.InputConnectors(0).IsAttached = True
                                con2OK = True
                                InConSlot = gObjTo.InputConnectors(0)
                            End If
                        Else
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConIn Then
                                        EndPos.X = InConSlot.Position.X
                                        EndPos.Y = InConSlot.Position.Y
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConIn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos.X = InConSlot.Position.X
                                    EndPos.Y = InConSlot.Position.Y
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                            If fidx = -1 Then
                                For Each OutConSlot In gObjFrom.OutputConnectors
                                    If Not OutConSlot.IsAttached And OutConSlot.Type = ConType.ConEn Then
                                        StartPos.X = OutConSlot.Position.X
                                        StartPos.Y = OutConSlot.Position.Y
                                        OutConSlot.IsAttached = True
                                        If con2OK Then con1OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjFrom.OutputConnectors(fidx).IsAttached Then
                                    OutConSlot = gObjFrom.OutputConnectors(fidx)
                                    StartPos.X = OutConSlot.Position.X
                                    StartPos.Y = OutConSlot.Position.Y
                                    OutConSlot.IsAttached = True
                                    If con2OK Then con1OK = True
                                End If
                            End If
                        End If
                    End If
                Else
                    'Console.WriteLine(("Nohobjetosaseremcone"), Color.Blue, MessageType.Information)
                    Exit Sub
                End If
            Else
                'Console.WriteLine(("Nohobjetosaseremcone"), Color.Blue, MessageType.Information)
                Exit Sub
            End If
            If con1OK = True And con2OK = True Then
                'desenhar conector
                Dim myCon As New ConnectorGraphic(StartPos.X, StartPos.Y, EndPos.X, EndPos.Y)
                OutConSlot.AttachedConnector = myCon
                InConSlot.AttachedConnector = myCon
                With myCon
                    .IsConnector = True
                    .AttachedFrom = gObjFrom
                    If gObjFrom.IsEnergyStream Then
                        .AttachedFromEnergy = True
                    End If
                    If gObjFrom.OutputConnectors.Contains(OutConSlot) Then
                        .AttachedFromConnectorIndex = gObjFrom.OutputConnectors.IndexOf(OutConSlot)
                        .AttachedFromInput = False
                    Else
                        .AttachedFromConnectorIndex = gObjFrom.InputConnectors.IndexOf(OutConSlot)
                        .AttachedFromInput = True
                    End If
                    .AttachedTo = gObjTo
                    If gObjTo.IsEnergyStream Then
                        .AttachedToEnergy = True
                    End If
                    If gObjTo.InputConnectors.Contains(InConSlot) Then
                        .AttachedToConnectorIndex = gObjTo.InputConnectors.IndexOf(InConSlot)
                        .AttachedToOutput = False
                    Else
                        .AttachedToConnectorIndex = gObjTo.OutputConnectors.IndexOf(InConSlot)
                        .AttachedToOutput = True
                    End If
                    If Not myCon Is Nothing Then
                        DrawingObjects.Add(myCon)
                    End If
                End With
            Else
                Throw New Exception("The requested connection between the given objects cannot be done.")
            End If

        Else


        End If

    End Sub

    Public Sub DisconnectObject(ByVal gObjFrom As GraphicObject, ByVal gObjTo As GraphicObject, Optional ByVal triggercalc As Boolean = False)

        Dim conObj As ConnectorGraphic = Nothing
        Dim SelObj As GraphicObject = gObjFrom
        Dim ObjToDisconnect As GraphicObject = Nothing
        Dim gobj1 As GraphicObject = Nothing
        Dim gobj2 As GraphicObject = Nothing
        ObjToDisconnect = gObjTo
        Dim i1, i2 As Integer
        If Not ObjToDisconnect Is Nothing Then
            Dim conptObj As ConnectionPoint = Nothing
            For Each conptObj In SelObj.InputConnectors
                If conptObj.IsAttached = True Then
                    If Not conptObj.AttachedConnector Is Nothing Then
                        If conptObj.AttachedConnector.AttachedFrom.Name.ToString = ObjToDisconnect.Name.ToString Then
                            i1 = conptObj.AttachedConnector.AttachedFromConnectorIndex
                            i2 = conptObj.AttachedConnector.AttachedToConnectorIndex
                            gobj1 = gObjTo
                            gobj2 = gObjFrom
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).AttachedConnector = Nothing
                            SelectedObjects.Clear()
                            conptObj.IsAttached = False
                            DeleteSelectedObject(conptObj.AttachedConnector)
                        End If
                    End If
                End If
            Next
            For Each conptObj In SelObj.OutputConnectors
                If conptObj.IsAttached = True Then
                    If Not conptObj.AttachedConnector Is Nothing Then
                        If conptObj.AttachedConnector.AttachedTo.Name.ToString = ObjToDisconnect.Name.ToString Then
                            i1 = conptObj.AttachedConnector.AttachedFromConnectorIndex
                            i2 = conptObj.AttachedConnector.AttachedToConnectorIndex
                            gobj1 = gObjFrom
                            gobj2 = gObjTo
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                            conptObj.IsAttached = False
                            DeleteSelectedObject(conptObj.AttachedConnector)
                        End If
                    End If
                End If
            Next
            If SelObj.EnergyConnector.IsAttached = True Then
                If SelObj.EnergyConnector.AttachedConnector.AttachedTo.Name.ToString = ObjToDisconnect.Name.ToString Then
                    i1 = SelObj.EnergyConnector.AttachedConnector.AttachedFromConnectorIndex
                    i2 = SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex
                    gobj1 = SelObj
                    gobj2 = ObjToDisconnect
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                    SelObj.EnergyConnector.IsAttached = False
                    DeleteSelectedObject(SelObj.EnergyConnector.AttachedConnector)
                End If
            End If
        End If

    End Sub

    Public Function FindObjectAtPoint(ByVal pt As SKPoint) As GraphicObject

        Dim objlist As New List(Of GraphicObject)

        Dim drawObj As GraphicObject
        Dim i As Integer
        If Me.DrawingObjects.Count > 0 Then
            For i = Me.DrawingObjects.Count - 1 To 0 Step -1
                drawObj = CType(Me.DrawingObjects(i), GraphicObject)
                If Not drawObj.IsConnector AndAlso drawObj.HitTest(New SKPoint(pt.X / Zoom, pt.Y / Zoom)) Then
                    objlist.Add(drawObj)
                End If
            Next
        End If

        If objlist.Count > 1 Then
            For Each obj In objlist
                Return obj
            Next
        ElseIf objlist.Count = 1 Then
            Return objlist(0)
        Else
            Return Nothing
        End If
        Return Nothing

    End Function

    Public Function FindObjectAtBounds(x As Double, y As Double, w As Double, h As Double) As GraphicObject

        Dim objlist As New List(Of GraphicObject)

        Dim drawObj As GraphicObject
        Dim i As Integer
        If Me.DrawingObjects.Count > 0 Then
            For i = Me.DrawingObjects.Count - 1 To 0 Step -1
                drawObj = CType(Me.DrawingObjects(i), GraphicObject)
                If Not drawObj.IsConnector AndAlso drawObj.HitTest(New SKPoint(x / Zoom, y / Zoom)) Then objlist.Add(drawObj)
                If Not drawObj.IsConnector AndAlso drawObj.HitTest(New SKPoint((x + w) / Zoom, y / Zoom)) Then objlist.Add(drawObj)
                If Not drawObj.IsConnector AndAlso drawObj.HitTest(New SKPoint(x / Zoom, (y + h) / Zoom)) Then objlist.Add(drawObj)
                If Not drawObj.IsConnector AndAlso drawObj.HitTest(New SKPoint((x + w) / Zoom, (y + h) / Zoom)) Then objlist.Add(drawObj)
            Next
        End If

        If objlist.Count > 1 Then
            For Each obj In objlist
                Return obj
            Next
        ElseIf objlist.Count = 1 Then
            Return objlist(0)
        Else
            Return Nothing
        End If
        Return Nothing

    End Function

    Protected Function ZoomRectangle(ByVal originalRect As SKRect) As SKRect
        Dim myNewRect As New SKRect(originalRect.Left * Me.Zoom, originalRect.Top * Me.Zoom, (originalRect.Left + originalRect.Right) * Me.Zoom, (originalRect.Top + originalRect.Bottom) * Me.Zoom)
        Return myNewRect
    End Function

    Protected Function DeZoomRectangle(ByVal originalRect As SKRect) As SKRect
        Dim myNewRect As New SKRect(originalRect.Left / Me.Zoom, originalRect.Top / Me.Zoom, (originalRect.Left + originalRect.Right) / Me.Zoom, (originalRect.Top + originalRect.Bottom) / Me.Zoom)
        Return myNewRect
    End Function

    Public Sub AddObject(ByVal obj As IGraphicObject, Optional ByVal forceposition As Boolean = False)

        'check if an existing object already exists on the desired insertion point

        Dim dpoint = New SKPoint(obj.X, obj.Y)

        Dim pobj = FindObjectAtBounds(dpoint.X, dpoint.Y, obj.Width, obj.Height)

        While pobj IsNot Nothing
            dpoint = New SKPoint(dpoint.X, dpoint.Y + pobj.Height + 30)
            If dpoint.Y + obj.Height >= Size.Height Then
                dpoint.Y = obj.Y
                dpoint.X += pobj.Width + 30
            End If
            pobj = FindObjectAtBounds(dpoint.X, dpoint.Y, obj.Width, obj.Height)
        End While

        obj.X = dpoint.X
        obj.Y = dpoint.Y

        DrawingObjects.Add(obj)

        AddedObject = obj

        Dim t As New Timers.Timer(16)
        t.Enabled = True

        AnimationStart = Date.Now
        DrawAddedAnimation = True

        AddHandler t.Elapsed, Sub()
                                  Dim tstep = (Date.Now - AnimationStart).TotalMilliseconds
                                  If tstep > Tmax Then
                                      t.Stop()
                                      DrawAddedAnimation = True
                                  Else
                                      InvalidateCallback.Invoke()
                                  End If
                              End Sub
        t.Start()

    End Sub

    Private Sub DrawPropertyListBlock(canvas As SKCanvas, gobj As GraphicObject)

        Dim X, Y, Padding, Height, Width As Double

        If gobj.Owner Is Nothing Then Exit Sub

        Padding = gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListPadding

        X = gobj.X + gobj.Width + 5
        Y = gobj.Y + gobj.Height + 5

        Dim tpaint As New SKPaint()

        With tpaint
            .TextSize = gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListFontSize
            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            .IsStroke = False
            Dim colors = New SKColors()
            Try
                .Color = colors.GetType().GetField(gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListFontColor).GetValue(colors)
                .Typeface = SKTypeface.FromFamilyName(gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName, SKFontStyleWeight.Bold, SKFontStyleWidth.Condensed, SKFontStyleSlant.Upright)
            Catch ex As Exception
                .Color = SKColors.DimGray
                .Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold, SKFontStyleWidth.Normal, SKFontStyleSlant.Upright)
            End Try
            colors = Nothing
        End With


        Dim bgpaint As New SKPaint()

        With bgpaint
            .Color = SKColors.White.WithAlpha(100)
            .IsStroke = False
        End With

        If Not gobj.Owner Is Nothing Then

            If gobj.Owner.GetFlowsheet IsNot Nothing Then

                Dim count As Integer

                Dim fs = gobj.Owner.GetFlowsheet
                Dim props As New List(Of String)(fs.FlowsheetOptions.VisibleProperties(gobj.Owner.GetType.Name))
                props.AddRange(DirectCast(gobj.Owner.ExtraProperties, IDictionary(Of String, Object)).Keys.ToArray)

                If gobj.Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.CapeOpenUO Then props = gobj.Owner.GetProperties(PropertyType.ALL).ToList

                Dim propstoremove As New List(Of String)

                If gobj.Owner.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                    For Each p In props
                        If gobj.Owner.GetPropertyValue(p).Equals(Double.MinValue) Then
                            propstoremove.Add(p)
                        End If
                    Next
                    For i As Integer = 0 To propstoremove.Count - 1
                        props.Remove(propstoremove(i))
                    Next
                End If

                count = props.Count

                Dim fsize = MeasureString("MEASURE", tpaint)

                Height = count * (fsize.Height + Padding) + Padding

                Dim propstring, propval, propunit, text As String
                Dim pval0 As Object = Nothing

                Dim texts As New List(Of String)

                Dim n As Integer = 1
                For Each prop In props
                    propstring = gobj.Owner.GetFlowsheet.GetTranslatedString(prop)
                    pval0 = gobj.Owner.GetPropertyValue(prop, gobj.Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                    If pval0 Is Nothing Then Exit For
                    If TypeOf pval0 Is Double Then
                        propval = Convert.ToDouble(pval0).ToString(gobj.Owner.GetFlowsheet.FlowsheetOptions.NumberFormat)
                    Else
                        propval = pval0.ToString
                    End If
                    propunit = gobj.Owner.GetPropertyUnit(prop, gobj.Owner.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem)
                    text = propstring + ": " + propval + " " + propunit
                    If MeasureString(text, tpaint).Width > Width Then Width = MeasureString(text, tpaint).Width
                    texts.Add(text)
                    n += 1
                Next

                canvas.DrawRect(New SKRect(X, Y + 2 * (fsize.Height + Padding), X + Width, Y + 2 * (fsize.Height + Padding) + Height), bgpaint)

                n = 1
                For Each text In texts
                    canvas.DrawText(text, X, Y + (n + 2) * (fsize.Height + Padding), tpaint)
                    n += 1
                Next

                props.Clear()
                props = Nothing

            End If

        End If

    End Sub

    Public Function MeasureString(text As String, paint As SKPaint) As SKSize

        Dim trect As New SKRect(0, 0, 2, 2)
        paint.GetTextPath(text, 0, 0).GetBounds(trect)

        Return New SKSize(trect.Width, trect.Height)

    End Function

End Class
