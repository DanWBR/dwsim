Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects

    Public Class AnalogGaugeGraphic

        Inherits ShapeGraphic

        <Xml.Serialization.XmlIgnore> Public Property PointList As New List(Of Point)

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal w As Double, h As Double)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.SetSize(New SKSize(w, h))
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal w As Double, h As Double)
            Me.New()
            Me.SetPosition(New SKPoint(posX, posY))
            Me.SetSize(New SKSize(w, h))
        End Sub

#End Region

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim w = Width
            Dim h = Height

            Dim center = New SKPoint(X + w / 2, Y + h / 2)

            Dim radius = w / 2

            Dim f = Width / 100.0

            'draw circle
            Using paint As New SKPaint With {.Color = GetForeColor(), .IsStroke = True, .StrokeWidth = 5 * f, .IsAntialias = True}
                canvas.DrawCircle(center.X, center.Y, radius, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Green.WithAlpha(100), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -225, 90)
                canvas.DrawPath(p, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Yellow.WithAlpha(100), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -135, 90)
                canvas.DrawPath(p, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Red.WithAlpha(100), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -45, 90)
                canvas.DrawPath(p, paint)
            End Using

            Dim needle = SKPath.ParseSvgPathData("M13.886,84.243L2.83,83.875c0,0,3.648-70.77,3.956-74.981C7.104,4.562,7.832,0,8.528,0c0.695,0,1.752,4.268,2.053,8.894C10.883,13.521,13.886,84.243,13.886,84.243z")
            needle.AddPath(SKPath.ParseSvgPathData("M16.721,85.475c0,4.615-3.743,8.359-8.36,8.359S0,90.09,0,85.475c0-4.62,3.743-8.363,8.36-8.363S16.721,80.855,16.721,85.475z"))
            Dim bounds As SKRect = SKRect.Empty
            needle.GetBounds(bounds)
            Dim sf = radius / bounds.Height * 0.9
            needle.Transform(SKMatrix.MakeScale(sf, sf))
            needle.GetBounds(bounds)
            needle.Offset(center)
            needle.Offset(-bounds.Width / 2, -bounds.Height + 5 * f)

            Dim mintick = -135.0
            Dim maxtick = 135.0
            Dim nstep = 10

            Using paint As New SKPaint With {.Color = GetForeColor(), .IsStroke = True, .StrokeWidth = 2 * f, .IsAntialias = True}
                For i As Integer = mintick To maxtick Step nstep
                    Dim angle = (i - 90) * Math.PI / 180
                    Dim p1 = New SKPoint(center.X + radius * Math.Cos(angle), center.Y + radius * Math.Sin(angle))
                    Dim p2 = New SKPoint(center.X + (radius - 10.0 * f) * Math.Cos(angle), center.Y + (radius - 10.0 * f) * Math.Sin(angle))
                    canvas.DrawLine(p1, p2, paint)
                Next
            End Using

            Dim owneri = DirectCast(Owner, IIndicator)

            Dim multiplier As Integer = IIf(owneri.DisplayInPercent, 100, 1)

            Dim minvalue = owneri.MinimumValue * multiplier
            Dim maxvalue = owneri.MaximumValue * multiplier
            Dim currentvalue As Double

            Dim SelectedObject = Owner?.GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = owneri.SelectedObjectID).FirstOrDefault

            If Not SelectedObject Is Nothing Then
                Using aPen As New SKPaint()
                    With aPen
                        .Color = SKColors.Green.WithAlpha(100)
                        .StrokeWidth = 0.5
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With
                    SetupPositioning(ConDir.Down, ConDir.Down)
                    Dim path As New SKPath()
                    Dim points() As SKPoint = PointList.Select(Function(x) New SKPoint(x.X, x.Y)).ToArray
                    path.MoveTo(points(0).X, points(0).Y)
                    For i As Integer = 1 To points.Length - 1
                        path.LineTo(points(i).X, points(i).Y)
                    Next
                    canvas.DrawPath(path, aPen)
                End Using
            End If

            If Not SelectedObject Is Nothing Then
                Try
                    currentvalue = multiplier * SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(owneri.SelectedPropertyUnits, SelectedObject.GetPropertyValue(owneri.SelectedProperty))
                    owneri.CurrentValue = currentvalue / multiplier
                Catch ex As Exception
                End Try
            End If

            Dim currenttick = mintick + (currentvalue - minvalue) / (maxvalue - minvalue) * (maxtick - mintick)

            needle.Transform(SKMatrix.MakeRotationDegrees(currenttick, center.X, center.Y))

            Using paint As New SKPaint With {.Color = GetForeColor(), .IsStroke = False, .IsAntialias = True}
                canvas.DrawPath(needle, paint)
            End Using

            Dim valtext = currentvalue.ToString("N0")

            Using paint As New SKPaint With {.TextSize = 15.0 * f, .Color = GetForeColor(), .IsAntialias = True}
                Select Case GlobalSettings.Settings.RunningPlatform
                    Case GlobalSettings.Settings.Platform.Windows
                        paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                    Case GlobalSettings.Settings.Platform.Linux
                        paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                    Case GlobalSettings.Settings.Platform.Mac
                        paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                End Select
                Dim trect As New SKRect(0, 0, 2, 2)
                paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
                Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)
                Dim strx As Single = (w - paint.MeasureText(valtext)) / 2
                Dim stry As Single = h - tsize.Height - 25.0 * f
                canvas.DrawText(valtext, X + strx, Y + stry, paint)
            End Using

            Owner.Calculate()

            If owneri.ShowAlarms Then

                Dim tll = "LL"
                Dim tl = "L"
                Dim th = "H"
                Dim thh = "HH"

                Dim r1 = 1

                Using paint As New SKPaint With {.TextSize = 10 * f, .Color = SKColors.White, .IsAntialias = True}
                    Select Case GlobalSettings.Settings.RunningPlatform
                        Case GlobalSettings.Settings.Platform.Windows
                            paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Linux
                            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Mac
                            paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                    End Select
                    Dim trll As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(thh, 0, 0).GetBounds(trll)
                    r1 = Math.Max(trll.Width, trll.Height)
                End Using

                Dim delta = -3 * r1 * f

                Using paint As New SKPaint With {.TextSize = 10 * f, .Color = SKColors.White, .IsAntialias = True}
                    Select Case GlobalSettings.Settings.RunningPlatform
                        Case GlobalSettings.Settings.Platform.Windows
                            paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Linux
                            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Mac
                            paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                    End Select
                    Dim trll As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(tll, 0, 0).GetBounds(trll)
                    Using bpaint As New SKPaint With {.Color = SKColors.Gray, .IsStroke = False, .IsAntialias = True}
                        If owneri.VeryLowAlarmEnabled Then
                            If owneri.VeryLowAlarmActive Then
                                bpaint.Color = SKColors.Red
                            Else
                                bpaint.Color = SKColors.Black
                            End If
                        End If
                        canvas.DrawOval(X, Y + delta - trll.Height / 2, r1, r1, bpaint)
                    End Using
                    canvas.DrawText(tll, X - trll.Width / 2, Y + delta, paint)
                End Using

                Using paint As New SKPaint With {.TextSize = 10 * f, .Color = SKColors.White, .IsAntialias = True}
                    Select Case GlobalSettings.Settings.RunningPlatform
                        Case GlobalSettings.Settings.Platform.Windows
                            paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Linux
                            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Mac
                            paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                    End Select
                    Dim trl As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(tl, 0, 0).GetBounds(trl)
                    Using bpaint As New SKPaint With {.Color = SKColors.Gray, .IsStroke = False, .IsAntialias = True}
                        If owneri.LowAlarmEnabled Then
                            If owneri.LowAlarmActive Then
                                bpaint.Color = SKColors.Red
                            Else
                                bpaint.Color = SKColors.Black
                            End If
                        End If
                        canvas.DrawOval(X + 0.33 * w, Y + delta - trl.Height / 2, r1, r1, bpaint)
                    End Using
                    canvas.DrawText(tl, X + 0.33 * w - trl.Width / 2, Y + delta, paint)
                End Using

                Using paint As New SKPaint With {.TextSize = 10 * f, .Color = SKColors.White, .IsAntialias = True}
                    Select Case GlobalSettings.Settings.RunningPlatform
                        Case GlobalSettings.Settings.Platform.Windows
                            paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Linux
                            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Mac
                            paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                    End Select
                    Dim trh As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(th, 0, 0).GetBounds(trh)
                    Using bpaint As New SKPaint With {.Color = SKColors.Gray, .IsStroke = False, .IsAntialias = True}
                        If owneri.HighAlarmEnabled Then
                            If owneri.HighAlarmActive Then
                                bpaint.Color = SKColors.Red
                            Else
                                bpaint.Color = SKColors.Black
                            End If
                        End If
                        canvas.DrawOval(X + 0.66 * w, Y + delta - trh.Height / 2, r1, r1, bpaint)
                    End Using
                    canvas.DrawText(th, X + 0.66 * w - trh.Width / 2, Y + delta, paint)
                End Using

                Using paint As New SKPaint With {.TextSize = 10 * f, .Color = SKColors.White, .IsAntialias = True}
                    Select Case GlobalSettings.Settings.RunningPlatform
                        Case GlobalSettings.Settings.Platform.Windows
                            paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Linux
                            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                        Case GlobalSettings.Settings.Platform.Mac
                            paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                    End Select
                    Dim trhh As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(thh, 0, 0).GetBounds(trhh)
                    Using bpaint As New SKPaint With {.Color = SKColors.Gray, .IsStroke = False, .IsAntialias = True}
                        If owneri.VeryHighAlarmEnabled Then
                            If owneri.VeryHighAlarmActive Then
                                bpaint.Color = SKColors.Red
                            Else
                                bpaint.Color = SKColors.Black
                            End If
                        End If
                        canvas.DrawOval(X + w, Y + delta - trhh.Height / 2, r1, r1, bpaint)
                    End Using
                    canvas.DrawText(thh, X + w - trhh.Width / 2, Y + delta, paint)
                End Using

            End If

            SelectedObject = Nothing

        End Sub

        Public Sub SetupPositioning(StartDir As ConDir, EndDir As ConDir)

            Dim SimObject = Owner?.GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = DirectCast(Owner, IIndicator).SelectedObjectID).FirstOrDefault

            'posicionar pontos nos primeiros slots livres

            PointList = New List(Of Point)

            Dim StartPos, EndPos As New Point

            StartPos = New Point(X + Width / 2, Y + Height)

            EndPos = New Point(SimObject.GraphicObject.X + SimObject.GraphicObject.Width / 2, SimObject.GraphicObject.Y)

            Dim DeltaX, DeltaY As Integer
            DeltaX = 10
            DeltaY = 10

            Dim XM, YM As Double

            Dim LeftTop1, RightBottom1, LeftTop2, RightBottom2 As New Point
            LeftTop1.X = Me.X
            LeftTop1.Y = Me.Y
            RightBottom1.X = Me.X + Me.Width
            RightBottom1.Y = Me.Y + Me.Height
            LeftTop2.X = SimObject.GraphicObject.X
            LeftTop2.Y = SimObject.GraphicObject.Y
            RightBottom2.X = SimObject.GraphicObject.X + SimObject.GraphicObject.Width
            RightBottom2.Y = SimObject.GraphicObject.Y + SimObject.GraphicObject.Height


            'Check Rotation

            If Me.Rotation >= 90 And Me.Rotation < 180 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Left
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Down
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Right
                End If
            ElseIf Me.Rotation >= 180 And Me.Rotation < 270 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Right
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Left
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Down
                End If
            ElseIf Me.Rotation >= 270 And Me.Rotation < 360 Then
                If StartDir = ConDir.Left Then
                    StartDir = ConDir.Down
                ElseIf StartDir = ConDir.Down Then
                    StartDir = ConDir.Right
                ElseIf StartDir = ConDir.Right Then
                    StartDir = ConDir.Up
                ElseIf StartDir = ConDir.Up Then
                    StartDir = ConDir.Left
                End If
            End If

            If SimObject.GraphicObject.Rotation >= 90 And SimObject.GraphicObject.Rotation < 180 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Left
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Down
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Right
                End If
            ElseIf SimObject.GraphicObject.Rotation >= 180 And SimObject.GraphicObject.Rotation < 270 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Right
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Left
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Down
                End If
            ElseIf SimObject.GraphicObject.Rotation >= 270 And SimObject.GraphicObject.Rotation < 360 Then
                If EndDir = ConDir.Left Then
                    EndDir = ConDir.Down
                ElseIf EndDir = ConDir.Down Then
                    EndDir = ConDir.Right
                ElseIf EndDir = ConDir.Right Then
                    EndDir = ConDir.Up
                ElseIf EndDir = ConDir.Up Then
                    EndDir = ConDir.Left
                End If
            End If

            'Apply Rotation

            If Me.Rotation <> 0 Then
                Dim angle_rad As Double = Me.Rotation * Math.PI / 180
                Dim center As New Point(Me.X + Me.Width / 2, Me.Y + Me.Height / 2)
                Dim x As Double = StartPos.X - center.X
                Dim y As Double = StartPos.Y - center.Y
                StartPos.X = center.X + (x * Math.Cos(angle_rad) + y * Math.Sin(angle_rad))
                StartPos.Y = center.Y - (x * -Math.Sin(angle_rad) + y * Math.Cos(angle_rad))
            End If
            If SimObject.GraphicObject.Rotation <> 0 Then
                Dim angle_rad As Double = SimObject.GraphicObject.Rotation * Math.PI / 180
                Dim center As New Point(SimObject.GraphicObject.X + SimObject.GraphicObject.Width / 2, SimObject.GraphicObject.Y + SimObject.GraphicObject.Height / 2)
                Dim x As Double = EndPos.X - center.X
                Dim y As Double = EndPos.Y - center.Y
                EndPos.X = center.X + (x * Math.Cos(angle_rad) + y * Math.Sin(angle_rad))
                EndPos.Y = center.Y - (x * -Math.Sin(angle_rad) + y * Math.Cos(angle_rad))
            End If

            'Check Flipping

            If Me.FlippedV Then
                Dim center As New Point(Me.X + Me.Width / 2, Me.Y + Me.Height / 2)
                Dim y As Double = StartPos.Y - center.Y
                StartPos.Y = center.Y - y
                If StartDir = ConDir.Down Then StartDir = ConDir.Up
                If StartDir = ConDir.Up Then StartDir = ConDir.Down
            End If
            If Me.FlippedH Then
                Dim center As New Point(Me.X + Me.Width / 2, Me.Y + Me.Height / 2)
                Dim x As Double = StartPos.X - center.X
                StartPos.X = center.X - x
                If StartDir = ConDir.Left Then StartDir = ConDir.Right
                If StartDir = ConDir.Right Then StartDir = ConDir.Left
            End If
            If SimObject.GraphicObject.FlippedV Then
                Dim center As New Point(SimObject.GraphicObject.X + SimObject.GraphicObject.Width / 2, SimObject.GraphicObject.Y + SimObject.GraphicObject.Height / 2)
                Dim y As Double = EndPos.Y - center.Y
                EndPos.Y = center.Y - y
                If EndDir = ConDir.Down Then EndDir = ConDir.Up
                If EndDir = ConDir.Up Then EndDir = ConDir.Down
            End If
            If SimObject.GraphicObject.FlippedH Then
                Dim center As New Point(SimObject.GraphicObject.X + SimObject.GraphicObject.Width / 2, SimObject.GraphicObject.Y + SimObject.GraphicObject.Height / 2)
                Dim x As Double = EndPos.X - center.X
                EndPos.X = center.X - x
                If EndDir = ConDir.Left Then EndDir = ConDir.Right
                If EndDir = ConDir.Right Then EndDir = ConDir.Left
            End If

            'Construct path of stream
            PointList.Add(New Point(StartPos.X, StartPos.Y))

            '================== EndDir Right =======================

            If StartDir = ConDir.Down And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    If EndPos.Y >= StartPos.Y + DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))

                        XM = (RightBottom1.X + LeftTop2.X) / 2
                        If XM < RightBottom1.X + DeltaX Then XM = LeftTop1.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X - DeltaX
                    If XM > LeftTop1.X - DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = LeftTop1.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < StartPos.Y + DeltaY Then YM = StartPos.Y + DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            End If

            If StartDir = ConDir.Up And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    If EndPos.Y <= StartPos.Y - DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))

                        XM = (RightBottom1.X + LeftTop2.X) / 2
                        If XM < RightBottom1.X + DeltaX Then XM = LeftTop1.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X - DeltaX
                    If XM > LeftTop1.X - DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = LeftTop1.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                    If YM > StartPos.Y - DeltaY Then YM = StartPos.Y - DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            End If

            If StartDir = ConDir.Right And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) >= (StartPos.X + DeltaX) Then
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, StartPos.Y))
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, EndPos.Y))
                Else
                    PointList.Add(New Point((StartPos.X + DeltaX), StartPos.Y))

                    XM = EndPos.X - DeltaX

                    YM = (LeftTop2.Y + RightBottom1.Y) / 2
                    If RightBottom2.Y + DeltaY > LeftTop1.Y - DeltaY Then YM = RightBottom1.Y + DeltaY
                    If YM < RightBottom2.Y + DeltaY And YM > LeftTop2.Y - DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < (RightBottom1.Y + LeftTop2.Y) / 2 Then YM = (RightBottom1.Y + LeftTop2.Y) / 2

                    PointList.Add(New Point((StartPos.X + DeltaX), YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))

                End If
            End If

            If StartDir = ConDir.Left And EndDir = ConDir.Right Then
                If (EndPos.X - DeltaX) > StartPos.X Then
                    PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                    If EndPos.Y > LeftTop1.Y - DeltaY And EndPos.Y < RightBottom1.Y + DeltaY Then
                        If StartPos.Y < EndPos.Y Then
                            YM = LeftTop1.Y - DeltaY
                        Else
                            YM = RightBottom1.Y + DeltaY
                        End If

                        PointList.Add(New Point(StartPos.X - DeltaX, YM))
                        PointList.Add(New Point((RightBottom1.X + LeftTop2.X) / 2, YM))
                        PointList.Add(New Point((RightBottom1.X + LeftTop2.X) / 2, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X - DeltaX, EndPos.Y))
                    End If
                Else
                    XM = StartPos.X - DeltaX
                    If XM > EndPos.X - DeltaX Then XM = EndPos.X - DeltaX

                    If StartPos.Y > LeftTop2.Y - DeltaY And StartPos.Y < RightBottom2.Y + DeltaY Then
                        PointList.Add(New Point((StartPos.X + RightBottom2.X) / 2, StartPos.Y))
                        If StartPos.Y < EndPos.Y Then
                            YM = LeftTop2.Y - DeltaY
                        Else
                            YM = RightBottom2.Y + DeltaY
                        End If
                        PointList.Add(New Point((StartPos.X + RightBottom2.X) / 2, YM))
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(XM, EndPos.Y))
                    Else
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                End If
            End If

            '================== EndDir Down  =======================

            If StartDir = ConDir.Right And EndDir = ConDir.Down Then
                If (EndPos.Y - DeltaY) > StartPos.Y Then
                    If EndPos.X >= StartPos.X + DeltaX Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        YM = (StartPos.Y + EndPos.Y) / 2
                        If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                        If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                        PointList.Add(New Point(StartPos.X + DeltaX, StartPos.Y))
                        PointList.Add(New Point(StartPos.X + DeltaX, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    XM = StartPos.X + DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    YM = EndPos.Y - DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            End If

            If StartDir = ConDir.Left And EndDir = ConDir.Down Then
                If (EndPos.Y - DeltaY) > StartPos.Y Then
                    If EndPos.X <= StartPos.X - DeltaX Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        YM = (StartPos.Y + EndPos.Y) / 2
                        If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                        If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                        PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                        PointList.Add(New Point(StartPos.X - DeltaX, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    XM = StartPos.X - DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = LeftTop2.X - DeltaX
                    YM = EndPos.Y - DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = LeftTop1.Y - DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            End If

            If StartDir = ConDir.Up And EndDir = ConDir.Down Then
                YM = StartPos.Y - DeltaY
                If YM < EndPos.Y - DeltaY Then
                    XM = EndPos.X
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = (RightBottom1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    YM = EndPos.Y - DeltaY
                    If StartPos.X > LeftTop2.X - DeltaX And StartPos.X < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        YM = (RightBottom2.Y + StartPos.Y) / 2
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = EndPos.Y - DeltaY
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                End If
            End If

            If StartDir = ConDir.Down And EndDir = ConDir.Down Then
                YM = (StartPos.Y + EndPos.Y) / 2
                If YM < StartPos.Y + DeltaY Then
                    XM = (RightBottom1.X + LeftTop2.X) / 2
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    End If
                    PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))
                    PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                    YM = EndPos.Y - DeltaY
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                Else
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            End If

            '================== EndDir Left =======================

            If StartDir = ConDir.Right And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) > (StartPos.X + DeltaX) Then
                    If EndPos.Y < RightBottom1.Y + DeltaY And EndPos.Y > LeftTop1.Y - DeltaY Then
                        If EndPos.Y < (LeftTop1.Y + RightBottom1.Y) / 2 Then
                            YM = RightBottom2.Y + DeltaY
                        Else
                            YM = LeftTop2.Y - DeltaY
                        End If
                        PointList.Add(New Point((StartPos.X + LeftTop2.X) / 2, StartPos.Y))
                        PointList.Add(New Point((StartPos.X + LeftTop2.X) / 2, YM))
                        PointList.Add(New Point(EndPos.X + DeltaX, YM))
                        PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                    Else
                        PointList.Add(New Point(EndPos.X + DeltaX, StartPos.Y))
                        PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                    End If
                Else
                    PointList.Add(New Point(StartPos.X + DeltaX, StartPos.Y))
                    If EndPos.Y < RightBottom1.Y + DeltaY And EndPos.Y > LeftTop1.Y - DeltaY Then
                        If EndPos.Y < (LeftTop1.Y + RightBottom1.Y) / 2 Then
                            YM = LeftTop1.Y - DeltaY
                        Else
                            YM = RightBottom1.Y + DeltaY
                        End If
                        PointList.Add(New Point(StartPos.X + DeltaX, YM))
                        PointList.Add(New Point((RightBottom2.X + LeftTop1.X) / 2, YM))
                        PointList.Add(New Point((RightBottom2.X + LeftTop1.X) / 2, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X + DeltaX, EndPos.Y))
                    End If

                End If
            End If

            If StartDir = ConDir.Left And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) > (StartPos.X - DeltaX) Then
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < RightBottom1.Y + DeltaY And YM > LeftTop1.Y - DeltaY Then YM = LeftTop1.Y - DeltaY
                    If YM < RightBottom2.Y + DeltaY And YM > LeftTop2.Y - DeltaY Then YM = LeftTop2.Y - DeltaY
                    PointList.Add(New Point(StartPos.X - DeltaX, StartPos.Y))
                    PointList.Add(New Point(StartPos.X - DeltaX, YM))
                    PointList.Add(New Point(EndPos.X + DeltaX, YM))
                    PointList.Add(New Point(EndPos.X + DeltaX, EndPos.Y))
                Else
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, StartPos.Y))
                    PointList.Add(New Point((StartPos.X + EndPos.X) / 2, EndPos.Y))
                End If
            End If

            If StartDir = ConDir.Down And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) < StartPos.X Then
                    If EndPos.Y >= StartPos.Y + DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y + DeltaY))

                        XM = (LeftTop1.X + RightBottom2.X) / 2
                        If XM > LeftTop1.X - DeltaX Then XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y + DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X + DeltaX
                    If XM < RightBottom1.X + DeltaX And EndPos.Y < StartPos.Y + DeltaY Then XM = RightBottom1.X + DeltaX
                    YM = (StartPos.Y + LeftTop2.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = RightBottom2.Y + DeltaY
                    If YM < StartPos.Y + DeltaY Then YM = StartPos.Y + DeltaY
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            End If

            If StartDir = ConDir.Up And EndDir = ConDir.Left Then
                If (EndPos.X + DeltaX) < StartPos.X Then
                    If EndPos.Y <= StartPos.Y - DeltaY Then
                        PointList.Add(New Point(StartPos.X, EndPos.Y))
                    Else
                        PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))

                        XM = (LeftTop1.X + RightBottom2.X) / 2
                        If XM > LeftTop1.X - DeltaX Then XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                        PointList.Add(New Point(XM, EndPos.Y))
                    End If
                Else
                    XM = EndPos.X + DeltaX
                    If XM < RightBottom1.X + DeltaX And EndPos.Y > StartPos.Y - DeltaY Then XM = RightBottom1.X + DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM > LeftTop2.Y - DeltaY And YM < RightBottom2.Y + DeltaY Then YM = LeftTop2.Y - DeltaY
                    If YM > StartPos.Y - DeltaY Then YM = StartPos.Y - DeltaY

                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(XM, EndPos.Y))
                End If
                PointList.Add(New Point(EndPos.X, EndPos.Y))
            End If

            '================== EndDir Up =======================

            If StartDir = ConDir.Left And EndDir = ConDir.Up Then
                If EndPos.X < StartPos.X - DeltaX Then
                    If StartPos.Y > EndPos.Y + DeltaY Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        XM = (StartPos.X + EndPos.X) / 2
                        If XM < RightBottom2.X + DeltaX Then XM = LeftTop2.X - DeltaX
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                        PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                    End If

                Else
                    XM = StartPos.X - DeltaX
                    If XM > LeftTop2.X - DeltaX Then XM = LeftTop2.X - DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < RightBottom2.Y + DeltaY Then YM = EndPos.Y + DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = RightBottom1.Y + DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If

            End If

            If StartDir = ConDir.Right And EndDir = ConDir.Up Then
                If EndPos.X > StartPos.X + DeltaX Then
                    If StartPos.Y > EndPos.Y + DeltaY Then
                        PointList.Add(New Point(EndPos.X, StartPos.Y))
                    Else
                        XM = (StartPos.X + EndPos.X) / 2
                        If XM > LeftTop2.X - DeltaX Then XM = RightBottom2.X + DeltaX
                        PointList.Add(New Point(XM, StartPos.Y))
                        PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                        PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                    End If

                Else
                    XM = StartPos.X + DeltaX
                    If XM < RightBottom2.X + DeltaX Then XM = RightBottom2.X + DeltaX
                    YM = (StartPos.Y + EndPos.Y) / 2
                    If YM < EndPos.Y + DeltaY Then YM = EndPos.Y + DeltaY
                    If YM > LeftTop1.Y - DeltaY And YM < RightBottom1.Y + DeltaY Then YM = RightBottom1.Y + DeltaY
                    PointList.Add(New Point(XM, StartPos.Y))
                    PointList.Add(New Point(XM, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                End If
            End If

            If StartDir = ConDir.Up And EndDir = ConDir.Up Then
                If EndPos.Y + DeltaY < StartPos.Y - DeltaY Then
                    YM = (StartPos.Y + EndPos.Y) / 2
                    PointList.Add(New Point(StartPos.X, YM))
                    PointList.Add(New Point(EndPos.X, YM))
                Else
                    XM = (StartPos.X + EndPos.X) / 2
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then XM = RightBottom1.X + DeltaX
                    If XM > LeftTop2.X - DeltaX And XM < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then XM = RightBottom1.X + DeltaX
                    End If
                    PointList.Add(New Point(StartPos.X, StartPos.Y - DeltaY))
                    PointList.Add(New Point(XM, StartPos.Y - DeltaY))
                    PointList.Add(New Point(XM, EndPos.Y + DeltaY))
                    PointList.Add(New Point(EndPos.X, EndPos.Y + DeltaY))
                End If
            End If

            If StartDir = ConDir.Down And EndDir = ConDir.Up Then
                YM = StartPos.Y + DeltaY
                XM = EndPos.X
                If YM > EndPos.Y + DeltaY Then
                    If XM > LeftTop1.X - DeltaX And XM < RightBottom1.X + DeltaX Then
                        XM = RightBottom1.X + DeltaX
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = (LeftTop1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                Else
                    YM = EndPos.Y + DeltaY
                    If StartPos.X > LeftTop2.X - DeltaX And StartPos.X < RightBottom2.X + DeltaX Then
                        XM = RightBottom2.X + DeltaX
                        YM = (LeftTop1.Y + EndPos.Y) / 2
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(XM, YM))
                        YM = EndPos.Y + DeltaY
                        PointList.Add(New Point(XM, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    Else
                        PointList.Add(New Point(StartPos.X, YM))
                        PointList.Add(New Point(EndPos.X, YM))
                    End If
                End If
            End If

            'finish path

            PointList.Add(New Point(EndPos.X, EndPos.Y))

        End Sub


    End Class

End Namespace
