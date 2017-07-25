Imports System.Runtime.Serialization
Imports System.Linq
Imports System.Collections.Generic
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects

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

    Public Property BackgroundColor As SKColor = SKColors.Black

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

        For Each dobj In objects
            If dobj Is SelectedObject Then
                Dim sp, sp2 As New SKPaint
                With sp
                    .Color = SKColors.LightBlue
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = False
                    .Shader = SKShader.CreateRadialGradient(New SKPoint(dobj.X + dobj.Width / 2, dobj.Y + dobj.Height / 2), Math.Max(dobj.Width, dobj.Height), New SKColor() {SKColors.LightBlue, SKColors.Transparent}, New Single() {0.05, 0.95}, SKShaderTileMode.Clamp)
                End With
                With sp2
                    .Color = SKColors.LightBlue
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = True
                    .StrokeWidth = 2
                End With
                DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp)
                DrawingCanvas.DrawRoundRect(New SKRect(dobj.X - 10, dobj.Y - 10, dobj.X + dobj.Width + 10, dobj.Y + dobj.Height + 10), 4, 4, sp2)
            End If
            dobj.Draw(DrawingCanvas)
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

    Protected Function ZoomRectangle(ByVal originalRect As SKRect) As SKRect
        Dim myNewRect As New SKRect(originalRect.Left * Me.Zoom, originalRect.Top * Me.Zoom, (originalRect.Left + originalRect.Right) * Me.Zoom, (originalRect.Top + originalRect.Bottom) * Me.Zoom)
        Return myNewRect
    End Function

    Protected Function DeZoomRectangle(ByVal originalRect As SKRect) As SKRect
        Dim myNewRect As New SKRect(originalRect.Left / Me.Zoom, originalRect.Top / Me.Zoom, (originalRect.Left + originalRect.Right) / Me.Zoom, (originalRect.Top + originalRect.Bottom) / Me.Zoom)
        Return myNewRect
    End Function

End Class
