Imports System.Runtime.Serialization
Imports System.Linq
Imports System.Collections.Generic
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports DWSIM.DrawingTools.Point
Imports Microsoft.Msagl.Core.Geometry.Curves
Imports Microsoft.Msagl.Core.Layout
Imports Microsoft.Msagl.Layout.Initial
Imports Microsoft.Msagl.Layout.Incremental
Imports System.Security.Cryptography

Public Delegate Sub DrawingEvent(ByVal canvas As SKCanvas)

Public Class GraphicsSurface

#Region "Events"

    Public Event SelectionChanged(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)

    Public Event InputPressed(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)

    Public Event InputMoved(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)

    Public Event InputReleased(ByVal sender As Object, ByVal e As SelectionChangedEventArgs)

    Public Event StatusUpdate(ByVal sender As Object, ByVal e As StatusUpdateEventArgs)

    Public Event ObjectsConnected(sender As Object, e As ObjectConnectionChangedArgs)

    Public Event ObjectsDisconnected(sender As Object, e As ObjectConnectionChangedArgs)

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

    Public ControlPanelMode As Boolean = False

    Public NetworkMode As Boolean = False

    Private PrevPositions As New Dictionary(Of String, Tuple(Of Point, Boolean))

    Public Property Flowsheet As IFlowsheet

    Public Property RegularTypeFace As SKTypeface
    Public Property BoldTypeFace As SKTypeface
    Public Property ItalicTypeFace As SKTypeface
    Public Property BoldItalicTypeFace As SKTypeface

    Public Shared Property RegularFonts As List(Of String)
    Public Shared Property BoldFonts As List(Of String)
    Public Shared Property ItalicFonts As List(Of String)
    Public Shared Property BoldItalicFonts As List(Of String)

    Public DrawAdditionalItems As Boolean = False

    Public OverlayImage As SKBitmap
    Public OverlayImage2 As SKBitmap
    Public OverlayImage3 As SKBitmap
    Public OverlayImage4 As SKBitmap
    Public OverlayImage5 As SKBitmap

    Public MinV, MaxV As Double

    Public Origin As SKPoint

    Public DrawOverlaysAction As Action(Of SKCanvas)

    Public GlobalDrawOverride As Action(Of IGraphicObject, SKCanvas)
    Public GlobalConPositionsOverride As Action(Of IGraphicObject)

    Public Event StartedDrawing As DrawingEvent
    Public Event FinishedDrawing As DrawingEvent

    Public Sub New()

        Dim assm = Me.GetType.Assembly
        If RegularTypeFace Is Nothing Then
            Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.Asap-Regular.ttf")
                RegularTypeFace = SKTypeface.FromStream(filestr)
            End Using
        End If
        If BoldTypeFace Is Nothing Then
            Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.Asap-SemiBold.ttf")
                BoldTypeFace = SKTypeface.FromStream(filestr)
            End Using
        End If
        If ItalicTypeFace Is Nothing Then
            Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.Asap-Italic.ttf")
                ItalicTypeFace = SKTypeface.FromStream(filestr)
            End Using
        End If
        If BoldItalicTypeFace Is Nothing Then
            Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.Asap-SemiBoldItalic.ttf")
                BoldItalicTypeFace = SKTypeface.FromStream(filestr)
            End Using
        End If
        RegularFonts = New List(Of String)
        BoldFonts = New List(Of String)
        ItalicFonts = New List(Of String)
        BoldItalicFonts = New List(Of String)
        Dim value As String = ""
        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.Fonts.txt")
            Using reader As New IO.StreamReader(filestr)
                While Not reader.EndOfStream
                    value = reader.ReadLine.Replace(".ttf", "")
                    If (value.Contains("Bold") Or value.Contains("Medium")) And value.Contains("Italic") Then
                        BoldItalicFonts.Add(value)
                    ElseIf (value.Contains("Bold") Or value.Contains("Medium")) Then
                        BoldFonts.Add(value)
                    ElseIf value.Contains("Italic") Then
                        ItalicFonts.Add(value)
                    Else
                        RegularFonts.Add(value)
                    End If
                End While
            End Using
        End Using

    End Sub

    Public Sub SetRegularFont(fontname As String)
        Using filestr As IO.Stream = Me.GetType().Assembly.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp." + fontname + ".ttf")
            RegularTypeFace = SKTypeface.FromStream(filestr)
        End Using
    End Sub

    Public Sub SetBoldFont(fontname As String)
        Using filestr As IO.Stream = Me.GetType().Assembly.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp." + fontname + ".ttf")
            BoldTypeFace = SKTypeface.FromStream(filestr)
        End Using
    End Sub

    Public Sub SetItalicFont(fontname As String)
        Using filestr As IO.Stream = Me.GetType().Assembly.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp." + fontname + ".ttf")
            ItalicTypeFace = SKTypeface.FromStream(filestr)
        End Using
    End Sub

    Public Sub SetBoldItalicFont(fontname As String)
        Using filestr As IO.Stream = Me.GetType().Assembly.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp." + fontname + ".ttf")
            BoldItalicTypeFace = SKTypeface.FromStream(filestr)
        End Using
    End Sub

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

    Public Shared Property BackgroundColor As SKColor = SKColors.White

    Public Shared Property ForegroundColor As SKColor = SKColors.Black

    Public Property ResizingMode As Boolean = False

    Public Property ResizingMode_KeepAR As Boolean = True

    Public Property QuickConnect As Boolean = False

    Public Property SurfaceBounds As SKRect

    Public Property SnapToGrid As Boolean = False

    Public Property SelectRectangle() As Boolean = False

    Public Property SurfaceMargins As SKRect

    Public Property Zoom() As Single = 1.0

    Public Overridable Property ShowGrid() As Boolean = False

    Public Property GridColor() As SKColor

    Public Overridable Property GridLineWidth() As Integer

    Public Overridable Property GridSize() As Integer = 20

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

    Public Property MultiSelectMode As Boolean = False

    Private Sub DrawGrid(canvas As SKCanvas)

        Dim gpaint As New SKPaint

        With gpaint
            .Color = SKColors.LightSteelBlue.WithAlpha(50)
            .StrokeWidth = 1
            .IsStroke = True
            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
        End With

        Dim i As Integer
        For i = 0 To 10000 Step GridSize
            canvas.DrawLine(i, 0, i, 10000, gpaint)
            canvas.DrawLine(0, i, 10000, i, gpaint)
        Next

    End Sub

    Private Sub DrawSelectionRectangle(DrawingCanvas As SKCanvas, ByVal selectionRect As SKRect)

        Dim spaint As New SKPaint
        With spaint
            .IsStroke = False
            .Color = SKColors.LightSalmon.WithAlpha(25)
        End With

        Dim normalizedRectangle As New SKRect

        'make sure the rectangle's upper left point is
        'up and to the left relative to the other points of the rectangle by
        'ensuring that it has a positive width and height.

        normalizedRectangle.Left = selectionRect.Left
        normalizedRectangle.Right = selectionRect.Right
        normalizedRectangle.Top = selectionRect.Top
        normalizedRectangle.Bottom = selectionRect.Bottom
        If selectionRect.Width < 0 Then
            normalizedRectangle.Left = selectionRect.Left - normalizedRectangle.Width
        Else
            normalizedRectangle.Left = selectionRect.Left
        End If

        If selectionRect.Height < 0 Then
            normalizedRectangle.Top = selectionRect.Top - normalizedRectangle.Height
        Else
            normalizedRectangle.Top = selectionRect.Top
        End If

        DrawingCanvas.DrawRect(normalizedRectangle, spaint)

    End Sub

    Public Sub UpdateCanvas(DrawingCanvas As SKCanvas)

        UpdateColorTheme()

        'draw the actual objects onto the page, on top of the grid

        If Me.SelectedObject Is Nothing Then Me.SelectedObjects.Clear()

        DrawingCanvas.Clear(BackgroundColor)

        If ShowGrid Then DrawGrid(DrawingCanvas)

        DrawingCanvas.Scale(Me.Zoom, Me.Zoom)

        RaiseEvent StartedDrawing(DrawingCanvas)

        'Dim objects = DrawingObjects.ToArray

        If DrawingObjects.Count = 0 AndAlso Not GlobalSettings.Settings.OldUI Then
            If Not NetworkMode Then DrawInstructions(DrawingCanvas)
        End If

        If DrawAddedAnimation Then
            Dim tstep = (Date.Now - AnimationStart).TotalMilliseconds
            Dim spa As New SKPaint
            With spa
                Dim avalue As Integer = 255 - tstep / Tmax * 255
                If avalue < 0 Then avalue = 0
                .Color = SKColors.SteelBlue.WithAlpha(avalue)
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Shader = SKShader.CreateRadialGradient(New SKPoint(AddedObject.X + AddedObject.Width / 2, AddedObject.Y + AddedObject.Height / 2), Math.Max(AddedObject.Width * (1 + tstep / Tmax), AddedObject.Height * (1 + tstep / Tmax)), New SKColor() {SKColors.LightBlue, SKColors.Transparent}, New Single() {0.05, 0.95}, SKShaderTileMode.Clamp)
            End With
            DrawingCanvas.DrawRoundRect(New SKRect(AddedObject.X - 250 * tstep / Tfactor, AddedObject.Y - 250 * tstep / Tfactor, AddedObject.X + AddedObject.Width + 250 * tstep / Tfactor, AddedObject.Y + AddedObject.Height + 250 * tstep / Tfactor), 10, 10, spa)
        End If

        For Each dobj In DrawingObjects
            If TypeOf dobj Is Shapes.RectangleGraphic Then
                dobj.Draw(DrawingCanvas)
            End If
        Next

        For Each dobj In DrawingObjects
            If Not TypeOf dobj Is ConnectorGraphic And Not TypeOf dobj Is Shapes.RectangleGraphic And
           Not TypeOf dobj Is Tables.FloatingTableGraphic Then
                If TypeOf dobj Is ShapeGraphic Then
                    If GlobalConPositionsOverride Is Nothing Then
                        DirectCast(dobj, ShapeGraphic).PositionConnectors()
                    Else
                        GlobalConPositionsOverride.Invoke(dobj)
                    End If
                End If
            End If
        Next

        For Each dobj In DrawingObjects
            If TypeOf dobj Is ConnectorGraphic Then
                If GlobalDrawOverride IsNot Nothing Then
                    GlobalDrawOverride.Invoke(dobj, DrawingCanvas)
                Else
                    dobj.Draw(DrawingCanvas)
                End If
            End If
        Next

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

        If GlobalDrawOverride Is Nothing Then
            For Each dobj As GraphicObject In Me.SelectedObjects.Values
                dobj.Selected = True
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
            Next
        End If

        For Each gr As GraphicObject In Me.DrawingObjects
            If Not Me.SelectedObjects.ContainsKey(gr.Name) Then gr.Selected = False
        Next

        For Each dobj In DrawingObjects

            DirectCast(dobj, GraphicObject).RegularTypeFace = RegularTypeFace
            DirectCast(dobj, GraphicObject).BoldTypeFace = BoldTypeFace
            DirectCast(dobj, GraphicObject).ItalicTypeFace = ItalicTypeFace
            DirectCast(dobj, GraphicObject).BoldItalicTypeFace = BoldItalicTypeFace

            If Not TypeOf dobj Is ConnectorGraphic And Not TypeOf dobj Is Shapes.RectangleGraphic And
               Not TypeOf dobj Is Tables.FloatingTableGraphic Then

                If TypeOf dobj Is ShapeGraphic Then
                    DirectCast(dobj, ShapeGraphic).LineWidth = 1
                    DirectCast(dobj, ShapeGraphic).UpdateStatus()
                End If

                If GlobalDrawOverride IsNot Nothing Then

                    If dobj.FlippedH Or dobj.FlippedV Or dobj.Rotation <> 0 Then

                        Dim currmat = DrawingCanvas.TotalMatrix

                        DrawingCanvas.Save()

                        If dobj.FlippedV And Not dobj.FlippedH Then
                            DrawingCanvas.Scale(1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                        ElseIf dobj.FlippedH And Not dobj.FlippedV Then
                            DrawingCanvas.Scale(-1, 1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                        ElseIf dobj.FlippedH And dobj.FlippedV Then
                            DrawingCanvas.Scale(-1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                        End If

                        If dobj.Rotation <> 0.0 Then
                            DrawingCanvas.RotateDegrees(dobj.Rotation, dobj.X + dobj.Width / 2, dobj.Y + dobj.Height / 2)
                        End If

                        GlobalDrawOverride.Invoke(dobj, DrawingCanvas)

                        DrawingCanvas.SetMatrix(currmat)

                    Else

                        GlobalDrawOverride.Invoke(dobj, DrawingCanvas)

                    End If

                Else

                    If dobj.DrawOverride IsNot Nothing Then

                        dobj.DrawOverride.Invoke(DrawingCanvas)

                    Else

                        If dobj.FlippedH Or dobj.FlippedV Or dobj.Rotation <> 0 Then

                            Dim currmat = DrawingCanvas.TotalMatrix

                            DrawingCanvas.Save()

                            If dobj.FlippedV And Not dobj.FlippedH Then
                                DrawingCanvas.Scale(1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                            ElseIf dobj.FlippedH And Not dobj.FlippedV Then
                                DrawingCanvas.Scale(-1, 1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                            ElseIf dobj.FlippedH And dobj.FlippedV Then
                                DrawingCanvas.Scale(-1, -1, (dobj.X + dobj.Width / 2), (dobj.Y + dobj.Height / 2))
                            End If

                            If dobj.Rotation <> 0.0 Then
                                DrawingCanvas.RotateDegrees(dobj.Rotation, dobj.X + dobj.Width / 2, dobj.Y + dobj.Height / 2)
                            End If

                            dobj.Draw(DrawingCanvas)

                            DrawingCanvas.SetMatrix(currmat)

                        Else

                            Try
                                dobj.Draw(DrawingCanvas)
                            Catch ex As Exception
                                Using p As New SKPaint
                                    p.Color = SKColors.Black
                                    p.StrokeWidth = 1
                                    p.IsStroke = True
                                    DrawingCanvas.DrawText(String.Format("Error drawing {0}: {1}", dobj.Tag, ex.Message), dobj.X, dobj.Y, p)
                                End Using
                            End Try

                        End If

                        'If TypeOf dobj Is ShapeGraphic And dobj.Status = Status.Calculating Then
                        '    DirectCast(dobj, ShapeGraphic).DrawCalculatingMode(DrawingCanvas)
                        'End If

                        If TypeOf dobj Is ShapeGraphic And
                        dobj.ObjectType <> ObjectType.GO_MasterTable And
                        dobj.ObjectType <> ObjectType.GO_SpreadsheetTable And
                        dobj.ObjectType <> ObjectType.GO_Table And
                        dobj.ObjectType <> ObjectType.GO_Animation And
                        dobj.ObjectType <> ObjectType.GO_Chart And
                        dobj.ObjectType <> ObjectType.GO_Rectangle And
                        dobj.ObjectType <> ObjectType.GO_Image And
                        dobj.ObjectType <> ObjectType.GO_Text And
                        dobj.ObjectType <> ObjectType.GO_Button And
                        dobj.ObjectType <> ObjectType.GO_FloatingTable Then

                            DirectCast(dobj, ShapeGraphic).DrawTag(DrawingCanvas)

                            If Not dobj.Owner?.SupportsDynamicMode And dobj.Owner?.GetFlowsheet?.DynamicMode Then

                                dobj.DrawNotDynamicsCompatible(DrawingCanvas)

                            ElseIf dobj.Owner?.GetFlowsheet?.DynamicMode And
                                dobj.Owner?.GetFlowsheet()?.FlowsheetOptions.DisplayDynamicPropertyValues Then

                                dobj.DrawDynamicProperties(DrawingCanvas)

                            End If

                        End If

                        If dobj.Owner?.GetFlowsheet.DynamicMode Then

                            If dobj.ObjectType = ObjectType.MaterialStream Then

                                If Not dobj.InputConnectors(0).IsAttached Or
                                Not dobj.OutputConnectors(0).IsAttached Or
                                dobj.InputConnectors(0).IsAttached AndAlso
                                (dobj.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.Valve Or
                                dobj.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_Recycle) Or
                                dobj.OutputConnectors(0).IsAttached AndAlso
                                (dobj.OutputConnectors(0).AttachedConnector.AttachedTo.ObjectType = ObjectType.Valve Or
                                dobj.OutputConnectors(0).AttachedConnector.AttachedTo.ObjectType = ObjectType.OT_Recycle) Then

                                    'draw dyn spec

                                    dobj.DrawDynSpec(DrawingCanvas, dobj.Owner.DynamicsSpec)

                                End If

                            End If

                            End If


                    End If

                End If

            End If

        Next

        If DrawPropertyList Then
            For Each dobj In DrawingObjects
                If dobj.Calculated Then DrawPropertyListBlock(DrawingCanvas, dobj)
            Next
        End If

        'draw selection rectangle (click and drag to select interface)
        'on top of everything else, but transparent
        If selectionDragging Then
            DrawSelectionRectangle(DrawingCanvas, selectionRect)
        End If

        If DrawAdditionalItems Then
            DrawOverlaysAction.Invoke(DrawingCanvas)
        End If

        RaiseEvent FinishedDrawing(DrawingCanvas)

        For Each dobj As GraphicObject In Me.DrawingObjects
            If TypeOf dobj Is Tables.FloatingTableGraphic Then
                dobj.Draw(DrawingCanvas)
            End If
        Next

    End Sub

    Public Sub UpdateSurface(surface As SKSurface)

        UpdateCanvas(surface.Canvas)

    End Sub

    Public Function GetCanvasTopLeftCorner() As SKPoint

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

        Return New SKPoint(minx, miny)

    End Function

    Public Function GetCanvasSize() As SKSize

        Dim minx As Integer = Integer.MaxValue
        Dim miny As Integer = Integer.MaxValue
        Dim maxx As Integer = 0
        Dim maxy As Integer = 0

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If gobj.ObjectType <> ObjectType.Nenhum Then
                If gobj.X <= minx Then minx = gobj.X - 30
                If gobj.X + gobj.Width >= maxx Then maxx = gobj.X + gobj.Width + 30
                If gobj.Y <= miny Then miny = gobj.Y
                If gobj.Y + gobj.Height >= maxy Then maxy = gobj.Y + gobj.Height + 30
            End If
        Next

        Dim width = maxx - minx
        Dim height = maxy - miny

        Return New SKSize(width, height)

    End Function

    Private Sub DrawInstructions(canvas As SKCanvas)

        Dim text As New Text.StringBuilder

        text.AppendLine("How to create and run a new simulation")
        text.AppendLine("(If you've configured the simulation using the Setup Wizard, skip to Step 3)")
        text.AppendLine("")
        text.AppendLine("1. Select Compounds to add to the simulation ('Setup' > 'Compounds')")
        text.AppendLine("2. Add at least one Property Package and one Flash Algorithm ('Setup' > 'Basis')")
        text.AppendLine("3. Add Unit Operations to the Flowsheet ('Objects' > 'Add New Simulation Object')")
        text.AppendLine("4. Add Material and Energy Streams to the Flowsheet ('Objects' > 'Add New Simulation Object')")
        text.AppendLine("5. Select an object to display its editing panels on the Object Editor area.")
        text.AppendLine("6. Connect unit operation blocks to streams (select object, go to Object Editor and select the 'Connections' tab)")
        text.AppendLine("7. Edit properties of the upstream Material Streams and all Unit Operations (select object, go to Object Editor, select the 'Properties' tab)")
        text.AppendLine("8. Run the simulation (press F5)")
        text.AppendLine("9. To view the simulation results, go the 'Results' tab and select an object on the list.")
        text.AppendLine("")
        text.AppendLine("Useful shortcuts")
        text.AppendLine("")
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
            .Typeface = RegularTypeFace
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

    Public Sub OffsetAll(deltax As Single, deltay As Single)

        If deltax > 10000000000.0 Or deltay > 10000000000.0 Then Exit Sub

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If Not gobj.IsConnector Then
                gobj.X += deltax
                gobj.Y += deltay
            End If
        Next

        Origin.Offset(deltax, deltay)

    End Sub

    Public Sub ZoomAll(viewwidth As Integer, viewheight As Integer)

        If Me.DrawingObjects.Count = 0 Then Exit Sub

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

            Origin.Offset(deltax, deltay)

            For Each gobj As IGraphicObject In Me.DrawingObjects
                If Not gobj.IsConnector Then
                    gobj.X += deltax
                    gobj.Y += deltay
                End If
            Next

        End If

    End Sub

    Public Sub Center(viewwidth As Integer, viewheight As Integer)

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

        Dim deltax = (viewwidth / 2 / Zoom - middlex)
        Dim deltay = (viewheight / 2 / Zoom - middley)

        'Origin.Offset(deltax, deltay)

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If Not gobj.IsConnector Then
                gobj.X += deltax
                gobj.Y += deltay
            End If
        Next

    End Sub

    Public Sub CenterTo(oldzoom As Double, mx As Integer, my As Integer, viewwidth As Integer, viewheight As Integer)

        Dim x1 = mx / oldzoom
        Dim y1 = my / oldzoom

        Dim x2 = mx / Zoom
        Dim y2 = my / Zoom

        Dim deltax = x2 - x1
        Dim deltay = y2 - y1

        'Origin.Offset(deltax, deltay)

        For Each gobj As IGraphicObject In Me.DrawingObjects
            If Not gobj.IsConnector Then
                gobj.X += deltax
                gobj.Y += deltay
            End If
        Next

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

                Origin.Offset(New SKPoint(-dx / Zoom, -dy / Zoom))

            Else

                Dim dragPoint As New SKPoint(x, y)

                If Not SelectedObject Is Nothing Then

                    If Not SelectedObject.IsConnector Then

                        If dragging Then

                            dragPoint.X += dragOffset.X
                            dragPoint.Y += dragOffset.Y

                            For Each gr As IGraphicObject In Me.SelectedObjects.Values
                                Dim p As SKPoint = New SKPoint(gr.X, gr.Y)
                                p.X += (x - dragStart.X) / Zoom
                                p.Y += (y - dragStart.Y) / Zoom
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

                        x0 = rectp0.X / Zoom
                        y0 = rectp0.Y / Zoom
                        x1 = dragPoint.X / Zoom
                        y1 = dragPoint.Y / Zoom

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

                            Dim flowsheet = gobj.Owner?.GetFlowsheet()

                            If flowsheet IsNot Nothing Then

                                If gobj.Calculated Or gobj.ObjectType = ObjectType.OT_Adjust Or
                                gobj.ObjectType = ObjectType.OT_Spec Or gobj.ObjectType = ObjectType.OT_Recycle Or
                                gobj.ObjectType = ObjectType.OT_EnergyRecycle Then

                                    If gobj.Owner IsNot Nothing Then

                                        Dim obj = gobj.Owner

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

        RaiseEvent InputMoved(Me, New SelectionChangedEventArgs(SelectedObject))

    End Sub

    Public Sub InputRelease()

        If ControlPanelMode Then

            If TypeOf SelectedObject Is Shapes.ButtonGraphic Then
                With DirectCast(SelectedObject, Shapes.ButtonGraphic)
                    .Pressed = False
                    .Run()
                End With
            End If

            Exit Sub

        End If

        If Not ResizingMode Then

            draggingfs = False

            If dragging And SnapToGrid Then
                Dim oc As SKPoint
                Dim snapx, snapy As Integer
                For Each go As GraphicObject In Me.SelectedObjects.Values
                    oc = New SKPoint(go.X + go.Width / 2, go.Y + go.Height / 2)
                    snapx = Math.Round(oc.X / GridSize) * GridSize - go.Width / 2
                    snapy = Math.Round(oc.Y / GridSize) * GridSize - go.Height / 2
                    go.SetPosition(New SKPoint(snapx, snapy))
                Next
            End If

            dragging = False

            If selectionDragging Then

                Dim zoomedSelection As SKRect = selectionRect
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

            Else

                SelectedObject?.DisplayControlPanelModeEditor()

            End If

        Else

            Size0 = Nothing

        End If

        RaiseEvent InputReleased(Me, New SelectionChangedEventArgs(SelectedObject))

    End Sub

    Public Sub InputPress(x As Integer, y As Integer)

        If ControlPanelMode Then

            SelectedObject = FindObjectAtPoint(New SKPoint(x, y))

            SelectedObject?.DisplayControlPanelModeEditor()

            If TypeOf SelectedObject Is SwitchGraphic Then
                Dim switchobj = DirectCast(SelectedObject.Owner, ISwitch)
                switchobj.IsOn = Not switchobj.IsOn
                DirectCast(switchobj, ISimulationObject).Calculate()
            End If

            If TypeOf SelectedObject Is Shapes.ButtonGraphic Then
                With DirectCast(SelectedObject, Shapes.ButtonGraphic)
                    .Pressed = True
                End With
            End If

        Else

            If MultiSelectMode Then
                SelectRectangle = Not My.Computer.Keyboard.ShiftKeyDown
            Else
                SelectRectangle = My.Computer.Keyboard.ShiftKeyDown
            End If

            Dim mousePT As New SKPoint(x, y)

            dragStart = New SKPoint(x, y)

            SelectedObject = FindObjectAtPoint(mousePT)

            If Not SelectedObject Is Nothing Then Size0 = New SKSize(SelectedObject.Width, SelectedObject.Height)

            If Not ResizingMode Then

                If Me.SelectedObject Is Nothing Then
                    Me.SelectedObjects.Clear()
                    justselected = False
                    draggingfs = Not SelectRectangle
                Else
                    If My.Computer.Keyboard.CtrlKeyDown And MultiSelectMode Then
                        If Not Me.SelectedObjects.ContainsKey(Me.SelectedObject.Name) Then
                            Me.SelectedObjects.Add(Me.SelectedObject.Name, Me.SelectedObject)
                        Else
                            Me.SelectedObjects.Remove(Me.SelectedObject.Name)
                        End If
                        justselected = True
                    Else
                        If Not justselected Then Me.SelectedObjects.Clear()
                        If Not Me.SelectedObjects.ContainsKey(Me.SelectedObject.Name) Then
                            Me.SelectedObjects.Add(Me.SelectedObject.Name, Me.SelectedObject)
                        End If
                        justselected = False
                    End If
                End If

                If Not SelectedObject Is Nothing Then
                    dragging = True
                    dragOffset.X = SelectedObject.X - mousePT.X
                    dragOffset.Y = SelectedObject.Y - mousePT.Y
                Else
                    If Me.SelectRectangle Then
                        selectionDragging = True
                        rectp0.X = mousePT.X
                        rectp0.Y = mousePT.Y
                        selectionRect.Left = mousePT.X
                        selectionRect.Top = mousePT.Y
                        selectionRect.Right = mousePT.X + 1
                        selectionRect.Bottom = mousePT.Y + 1
                    End If
                End If

            End If

        End If

        RaiseEvent InputPressed(Me, New SelectionChangedEventArgs(SelectedObject))

    End Sub

    Public Sub DeleteSelectedObject(gobj As IGraphicObject)
        Dim objectToDelete As IGraphicObject = gobj
        If Not objectToDelete Is Nothing Then
            If Me.DrawingObjects.Contains(objectToDelete) Then
                Me.DrawingObjects.Remove(objectToDelete)
                Me.SelectedObject = Nothing
                RaiseEvent StatusUpdate(Me, New StatusUpdateEventArgs(StatusUpdateType.ObjectDeleted, Nothing, "", Nothing, 0))
            End If
        End If
    End Sub

    Public Sub DeleteAllObjects()
        Me.DrawingObjects.Clear()
        RaiseEvent StatusUpdate(Me, New StatusUpdateEventArgs(StatusUpdateType.ObjectDeleted, Nothing, "", Nothing, 0))
    End Sub

    Private Function AngleToPoint(ByVal Origin As SKPoint,
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
                    If gObjFrom.ObjectType = ObjectType.MaterialStream AndAlso gObjFrom.InputConnectors(0).IsAttached Then
                        If gObjTo.Name = gObjFrom.InputConnectors(0).AttachedConnector.AttachedFrom.Name Then
                            Throw New Exception("This connection is not allowed.")
                        End If
                    End If
                    If gObjFrom.ObjectType = ObjectType.EnergyStream AndAlso gObjFrom.InputConnectors(0).IsAttached Then
                        If gObjTo.Name = gObjFrom.InputConnectors(0).AttachedConnector.AttachedFrom.Name Then
                            Throw New Exception("This connection is not allowed.")
                        End If
                    End If
                    If gObjTo.ObjectType = ObjectType.MaterialStream AndAlso gObjTo.OutputConnectors(0).IsAttached Then
                        If gObjFrom.Name = gObjTo.OutputConnectors(0).AttachedConnector.AttachedTo.Name Then
                            Throw New Exception("This connection is not allowed.")
                        End If
                    End If
                    If gObjTo.ObjectType = ObjectType.EnergyStream AndAlso gObjTo.OutputConnectors(0).IsAttached Then
                        If gObjFrom.Name = gObjTo.OutputConnectors(0).AttachedConnector.AttachedTo.Name Then
                            Throw New Exception("This connection is not allowed.")
                        End If
                    End If
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
                            Case ObjectType.Cooler, ObjectType.Heater, ObjectType.Pipe, ObjectType.Expander, ObjectType.ShortcutColumn, ObjectType.DistillationColumn, ObjectType.AbsorptionColumn,
                            ObjectType.ReboiledAbsorber, ObjectType.RefluxedAbsorber, ObjectType.OT_EnergyRecycle, ObjectType.ComponentSeparator, ObjectType.SolidSeparator,
                            ObjectType.Filter, ObjectType.CustomUO, ObjectType.CapeOpenUO, ObjectType.FlowsheetUO, ObjectType.External, ObjectType.RCT_Conversion
                                GoTo 100
                            Case Else
                                Throw New Exception("This connection is not allowed.")
                        End Select
100:                    If gObjFrom.ObjectType <> ObjectType.CapeOpenUO And gObjFrom.ObjectType <> ObjectType.CustomUO And gObjFrom.ObjectType <> ObjectType.DistillationColumn _
                        And gObjFrom.ObjectType <> ObjectType.AbsorptionColumn And gObjFrom.ObjectType <> ObjectType.OT_EnergyRecycle And gObjFrom.ObjectType <> ObjectType.External _
                                                    And gObjFrom.ObjectType <> ObjectType.RefluxedAbsorber And gObjFrom.ObjectType <> ObjectType.ReboiledAbsorber _
                                                    And gObjFrom.ObjectType <> ObjectType.RCT_Conversion Then
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
                RaiseEvent ObjectsConnected(Me, New ObjectConnectionChangedArgs(gObjFrom, gObjTo))
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
            RaiseEvent ObjectsDisconnected(Me, New ObjectConnectionChangedArgs(gObjFrom, gObjTo))
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
                If obj.ObjectType <> ObjectType.GO_Rectangle Then Return obj
            Next
        ElseIf objlist.Count = 1 Then
            Return objlist(0)
        Else
            Return Nothing
        End If
        Return Nothing

    End Function

    Public Function FindObjectAtPointAbsolute(ByVal pt As SKPoint) As GraphicObject

        Dim objlist As New List(Of GraphicObject)

        Dim drawObj As GraphicObject
        Dim i As Integer
        If Me.DrawingObjects.Count > 0 Then
            For i = Me.DrawingObjects.Count - 1 To 0 Step -1
                drawObj = CType(Me.DrawingObjects(i), GraphicObject)
                If drawObj.HitTest(New SKPoint(pt.X, pt.Y)) Then
                    objlist.Add(drawObj)
                End If
            Next
        End If

        If objlist.Count > 1 Then
            For Each obj In objlist
                If obj.ObjectType <> ObjectType.GO_Rectangle Then Return obj
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

    Public Function FindObjectsAtBounds(x As Double, y As Double, w As Double, h As Double) As List(Of GraphicObject)

        Dim objlist As New List(Of GraphicObject)

        Dim drawObj As GraphicObject
        Dim i As Integer
        If Me.DrawingObjects.Count > 0 Then
            For i = Me.DrawingObjects.Count - 1 To 0 Step -1
                drawObj = CType(Me.DrawingObjects(i), GraphicObject)
                If Not drawObj.IsConnector AndAlso drawObj.IsInRect(New SKRect(x, y, x + w, y + h)) Then
                    objlist.Add(drawObj)
                End If
            Next
        End If

        Return objlist

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
                                      InvalidateCallback?.Invoke()
                                  End If
                              End Sub
        t.Start()

        RaiseEvent StatusUpdate(Me, New StatusUpdateEventArgs(StatusUpdateType.ObjectAdded, AddedObject, "", Nothing, 0))

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
                If GlobalSettings.Settings.DarkMode Then
                    .Color = ForegroundColor
                Else
                    .Color = colors.GetType().GetField(gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListFontColor).GetValue(colors)
                End If
                .Typeface = SKTypeface.FromFamilyName(gobj.Owner.GetFlowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName, SKFontStyleWeight.Bold, SKFontStyleWidth.Condensed, SKFontStyleSlant.Upright)
            Catch ex As Exception
                .Color = SKColors.DimGray
                .Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold, SKFontStyleWidth.Normal, SKFontStyleSlant.Upright)
            End Try
            colors = Nothing
        End With


        'Dim bgpaint As New SKPaint()

        'With bgpaint
        '    .Color = ForegroundColor.WithAlpha(100)
        '    .IsStroke = False
        'End With

        If Not gobj.Owner Is Nothing Then

            If gobj.Owner.GetFlowsheet IsNot Nothing Then

                Dim fs = gobj.Owner.GetFlowsheet

                If Not fs.FlowsheetOptions.VisibleProperties.ContainsKey(gobj.Owner.GetType.Name) Then
                    Exit Sub
                End If

                Dim count As Integer

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

                'canvas.DrawRect(New SKRect(X, Y + 2 * (fsize.Height + Padding), X + Width, Y + 2 * (fsize.Height + Padding) + Height), bgpaint)

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

    Public Sub AutoArrange(Optional ByVal DistanceFactor As Double = 1.0)

        Dim graph As New GeometryGraph
        Dim rec1 As ICurve

        PrevPositions = New Dictionary(Of String, Tuple(Of Point, Boolean))

        For Each obj In DrawingObjects
            If Not obj.IsConnector Then
                Select Case obj.ObjectType
                    Case ObjectType.GO_Rectangle,
                           ObjectType.GO_FloatingTable
                    Case Else
                        rec1 = Microsoft.Msagl.Core.Geometry.Curves.CurveFactory.CreateRectangle(obj.Width, obj.Height, New Microsoft.Msagl.Core.Geometry.Point(obj.X + obj.Width / 2, obj.Y + obj.Height / 2))
                        Dim n1 = New Node(rec1, obj.Name)
                        graph.Nodes.Add(n1)
                        PrevPositions.Add(obj.Name, New Tuple(Of Point, Boolean)(New Point(obj.X, obj.Y), obj.FlippedH))
                End Select
            End If
        Next

        For Each obj In DrawingObjects
            If TypeOf obj Is IConnectorGraphicObject Then
                Dim con = DirectCast(obj, IConnectorGraphicObject)
                Dim n1 = graph.Nodes.Where(Function(x) x.UserData = con.AttachedFrom.Name).FirstOrDefault
                Dim n2 = graph.Nodes.Where(Function(x) x.UserData = con.AttachedTo.Name).FirstOrDefault
                Dim e1 = New Edge(n1, n2)
                graph.Edges.Add(e1)
            ElseIf TypeOf obj Is Shapes.AdjustGraphic Then
                Dim adj = DirectCast(obj, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.AdjustGraphic)
                Dim n0 = graph.Nodes.Where(Function(x) x.UserData = adj.Name).FirstOrDefault
                Dim n1 = graph.Nodes.Where(Function(x) x.UserData = adj.ConnectedToCv?.Name).FirstOrDefault
                Dim n2 = graph.Nodes.Where(Function(x) x.UserData = adj.ConnectedToMv?.Name).FirstOrDefault
                If n1 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n1)
                    graph.Edges.Add(e1)
                End If
                If n2 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n2)
                    graph.Edges.Add(e1)
                End If
            ElseIf TypeOf obj Is Shapes.PIDControllerGraphic Then
                Dim pid = DirectCast(obj, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.PIDControllerGraphic)
                Dim n0 = graph.Nodes.Where(Function(x) x.UserData = pid.Name).FirstOrDefault
                Dim n1 = graph.Nodes.Where(Function(x) x.UserData = pid.ConnectedToCv?.Name).FirstOrDefault
                Dim n2 = graph.Nodes.Where(Function(x) x.UserData = pid.ConnectedToMv?.Name).FirstOrDefault
                If n1 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n1)
                    graph.Edges.Add(e1)
                End If
                If n2 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n2)
                    graph.Edges.Add(e1)
                End If
            ElseIf TypeOf obj Is Shapes.SpecGraphic Then
                Dim spec = DirectCast(obj, DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.SpecGraphic)
                Dim n0 = graph.Nodes.Where(Function(x) x.UserData = spec.Name).FirstOrDefault
                Dim n1 = graph.Nodes.Where(Function(x) x.UserData = spec.ConnectedToSv?.Name).FirstOrDefault
                Dim n2 = graph.Nodes.Where(Function(x) x.UserData = spec.ConnectedToTv?.Name).FirstOrDefault
                If n1 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n1)
                    graph.Edges.Add(e1)
                End If
                If n2 IsNot Nothing Then
                    Dim e1 = New Edge(n0, n2)
                    graph.Edges.Add(e1)
                End If
            ElseIf obj.Owner IsNot Nothing Then
                Select Case obj.ObjectType
                    Case ObjectType.Input
                        Dim owneri = DirectCast(obj.Owner, IInput)
                        Dim SelectedObject = obj.Owner.GetFlowsheet?.SimulationObjects.Values.Where(Function(x) x.Name = owneri.SelectedObjectID).FirstOrDefault
                        If SelectedObject IsNot Nothing Then
                            Dim n0 = graph.Nodes.Where(Function(x) x.UserData = obj.Name).FirstOrDefault
                            Dim n1 = graph.Nodes.Where(Function(x) x.UserData = SelectedObject.Name).FirstOrDefault
                            Dim e1 = New Edge(n0, n1)
                            graph.Edges.Add(e1)
                        End If
                    Case ObjectType.LevelGauge, ObjectType.AnalogGauge, ObjectType.DigitalGauge
                        Dim owneri = DirectCast(obj.Owner, IIndicator)
                        Dim SelectedObject = obj.Owner.GetFlowsheet?.SimulationObjects.Values.Where(Function(x) x.Name = owneri.SelectedObjectID).FirstOrDefault
                        If SelectedObject IsNot Nothing Then
                            Dim n0 = graph.Nodes.Where(Function(x) x.UserData = obj.Name).FirstOrDefault
                            Dim n1 = graph.Nodes.Where(Function(x) x.UserData = SelectedObject.Name).FirstOrDefault
                            Dim e1 = New Edge(n0, n1)
                            graph.Edges.Add(e1)
                        End If
                End Select
            End If
        Next

        Dim settings = New FastIncrementalLayoutSettings()
        settings.AvoidOverlaps = True
        settings.PackingMethod = PackingMethod.Compact
        settings.NodeSeparation = 50 * DistanceFactor
        settings.RouteEdges = True
        settings.RespectEdgePorts = True
        Dim eset = New Microsoft.Msagl.Core.Routing.EdgeRoutingSettings()
        eset.EdgeRoutingMode = Microsoft.Msagl.Core.Routing.EdgeRoutingMode.SplineBundling
        settings.EdgeRoutingSettings = eset
        settings.Reporting = False

        Dim layout = New InitialLayout(graph, settings)
        layout.Run()

        graph.UpdateBoundingBox()

        For Each node In graph.Nodes
            Dim gobj = DrawingObjects.Where(Function(x) x.Name = node.UserData).FirstOrDefault
            gobj.X = node.Center.X - node.Width / 2
            gobj.Y = node.Center.Y - node.Height / 2
        Next

        settings = Nothing
        layout = Nothing
        graph = Nothing

        For Each obj In DrawingObjects
            If TypeOf obj Is ShapeGraphic And Not obj.IsConnector Then
                DirectCast(obj, ShapeGraphic).PositionConnectors()
            End If
        Next

        For Each obj In DrawingObjects
            If TypeOf obj Is ShapeGraphic And Not obj.IsConnector Then
                If obj.InputConnectors.Count = 1 And obj.OutputConnectors.Count = 1 Then
                    If obj.InputConnectors(0).IsAttached Then
                        If obj.InputConnectors(0).AttachedConnector.AttachedFrom.X > obj.X Then
                            obj.FlippedH = True
                        Else
                            obj.FlippedH = False
                        End If
                    ElseIf obj.ObjectType = ObjectType.EnergyStream Then
                        If obj.OutputConnectors(0).IsAttached Then
                            Dim dest = obj.OutputConnectors(0).AttachedConnector.AttachedTo
                            If dest.FlippedH Then
                                obj.X += dest.Width * 1.3
                            Else
                                obj.X -= dest.Width * 1.3
                            End If
                            If dest.X < obj.X Then
                                obj.FlippedH = True
                            Else
                                obj.FlippedH = False
                            End If
                        End If
                    End If
                End If
            End If
        Next

    End Sub

    Public Sub ApplyNaturalLayout(orderedIds As List(Of String), deltaX As Integer)

        PrevPositions = New Dictionary(Of String, Tuple(Of Point, Boolean))
        For Each objID In orderedIds
            Dim gobj = DrawingObjects.Where(Function(o) o.Name = objID).FirstOrDefault()
            PrevPositions.Add(gobj.Name, New Tuple(Of Point, Boolean)(New Point(gobj.X, gobj.Y), gobj.FlippedH))
        Next

        Dim nrecycles = DrawingObjects.Where(Function(o) o.ObjectType = ObjectType.OT_Recycle).Count()

        For i = 0 To nrecycles

            Dim x = 50
            Dim y = 50

            For Each objID In orderedIds
                Dim gobj = DrawingObjects.Where(Function(o) o.Name = objID).FirstOrDefault()
                gobj.FlippedH = False
                If gobj.ObjectType <> ObjectType.EnergyStream Then
                    If gobj.InputConnectors(0).IsAttached Then
                        Dim conFrom = gobj.InputConnectors(0).AttachedConnector.AttachedFrom
                        Dim conFromIdx = gobj.InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                        Dim x2 = conFrom.OutputConnectors(conFromIdx).Position.X + deltaX
                        Dim y2 = conFrom.OutputConnectors(conFromIdx).Position.Y
                        gobj.X = x2
                        gobj.Y = y2
                    Else
                        gobj.X = x
                        gobj.Y = y
                    End If
                    x += deltaX
                ElseIf gobj.ObjectType = ObjectType.MaterialStream Then
                    If gobj.InputConnectors(0).IsAttached Then
                        Dim conFrom = gobj.InputConnectors(0).AttachedConnector.AttachedFrom
                        Dim conFromIdx = gobj.InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                        Dim x2 = conFrom.OutputConnectors(conFromIdx).Position.X + deltaX
                        Dim y2 = conFrom.OutputConnectors(conFromIdx).Position.Y
                        gobj.X = x2
                        gobj.Y = y2
                    Else
                        gobj.X = x
                        gobj.Y = y
                    End If
                End If
                gobj.PositionConnectors()
            Next

            For Each objID In orderedIds
                Dim gobj = DrawingObjects.Where(Function(o) o.Name = objID).FirstOrDefault()
                gobj.FlippedH = False
                If gobj.ObjectType = ObjectType.EnergyStream Then
                    If gobj.InputConnectors(0).IsAttached Then
                        Dim conn = gobj.InputConnectors(0).AttachedConnector
                        Dim conFrom = conn.AttachedFrom
                        Dim conFromIdx = conn.AttachedFromConnectorIndex
                        If conFromIdx >= 0 Then
                            Dim x2 = conFrom.OutputConnectors(conFromIdx).Position.X + deltaX
                            Dim y2 = conFrom.OutputConnectors(conFromIdx).Position.Y
                            gobj.X = x2
                            gobj.Y = y2
                        Else
                            Dim x2 = conFrom.EnergyConnector.Position.X + deltaX
                            Dim y2 = conFrom.EnergyConnector.Position.Y
                            gobj.X = x2
                            gobj.Y = y2
                        End If
                    ElseIf gobj.OutputConnectors(0).IsAttached Then
                        Dim conTo = gobj.OutputConnectors(0).AttachedConnector.AttachedTo
                        Dim conToIdx = gobj.OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                        If conToIdx >= 0 Then
                            Dim x2 = conTo.InputConnectors(conToIdx).Position.X - deltaX
                            Dim y2 = conTo.InputConnectors(conToIdx).Position.Y + deltaX
                            gobj.X = x2
                            gobj.Y = y2
                        Else
                            Dim x2 = conTo.EnergyConnector.Position.X - deltaX
                            Dim y2 = conTo.EnergyConnector.Position.Y + deltaX
                            gobj.X = x2
                            gobj.Y = y2
                        End If
                    End If
                End If
                gobj.PositionConnectors()
            Next

        Next


    End Sub

    Public Sub RestoreLayout()

        If PrevPositions.Count > 0 Then

            For Each item In PrevPositions
                Dim obj = DrawingObjects.Where(Function(x) x.Name = item.Key).FirstOrDefault()
                If obj IsNot Nothing Then
                    obj.X = item.Value.Item1.X
                    obj.Y = item.Value.Item1.Y
                    obj.FlippedH = item.Value.Item2
                End If
            Next

        End If

    End Sub

    Public Sub UpdateColorTheme()

        If Flowsheet Is Nothing Then Exit Sub

        For Each obj In DrawingObjects
            obj.DrawMode = Flowsheet.FlowsheetOptions.FlowsheetColorTheme
        Next

    End Sub

End Class
