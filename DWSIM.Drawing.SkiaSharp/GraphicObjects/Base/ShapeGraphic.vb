Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports s = DWSIM.GlobalSettings.Settings

Namespace GraphicObjects

    Public MustInherit Class ShapeGraphic

        Inherits GraphicObject

        Public Sub UpdateStatus()
            Dim alpha As Integer = 255

            If SemiTransparent Then alpha = 50

            If Not OverrideColors Then
                If Me.Active Then
                    Select Case Status
                        Case Status.Calculated
                            LineColor = SKColors.SteelBlue
                        Case Status.Calculating
                            LineColor = SKColors.YellowGreen
                        Case Status.ErrorCalculating
                            LineColor = SKColors.Salmon
                        Case Status.Idle
                            LineColor = SKColors.SteelBlue
                        Case Status.Inactive
                            LineColor = SKColors.Gray
                        Case Status.NotCalculated
                            LineColor = SKColors.Salmon
                        Case Status.Modified
                            LineColor = SKColors.LightGreen
                    End Select
                Else
                    LineColor = SKColors.Gray
                End If
            End If

        End Sub

        Public Property DefaultTypeFace As SKTypeface
        Public Property RegularTypeFace As SKTypeface
        Public Property SemiTransparent As Boolean = False
        Public Overridable Property LineWidth As Integer = 1
        Public Overridable Property GradientMode As Boolean = True
        Public Overridable Property LineColor As SKColor = SKColors.Black
        Public Overridable Property LineColorDark As SKColor = SKColors.SteelBlue
        Public Overridable Property Fill As Boolean = False
        Public Overridable Property FillColor As SKColor = SKColors.LightGray
        Public Overridable Property FillColorDark As SKColor = SKColors.Black
        Public Overridable Property GradientColor1 As SKColor = SKColors.LightGray
        Public Overridable Property GradientColor2 As SKColor = SKColors.White
        Public Overridable Property FontSize As Double = 10.0#
        Public Overridable Property OverrideColors As Boolean = False

        Public Overrides Sub Draw(ByVal g As Object)
            MyBase.Draw(g)

        End Sub

        Public Overridable Sub DrawTag(ByVal g As SKCanvas)
            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(s.DarkMode, LineColorDark, LineColor)
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath(Me.Tag, 0, 0).GetBounds(trect)
            Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)

            Dim strx As Single = (Me.Width - tpaint.MeasureText(Me.Tag)) / 2

            Dim bpaint As New SKPaint()

            With bpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Typeface = DefaultTypeFace
                .Color = If(s.DarkMode, SKColors.Transparent, SKColors.White.WithAlpha(200))
                .IsStroke = True
                .StrokeWidth = 2
                .BlendMode = SKBlendMode.Overlay
            End With

            'g.DrawRect(New SKRect(X + strx - 2, Y + Height + 20 - trect.Height - 2, X + strx + trect.Width + 2, Y + Height + 20 + 2), bpaint)

            g.DrawText(Me.Tag, X + strx, Y + Height + 20, bpaint)
            g.DrawText(Me.Tag, X + strx, Y + Height + 20, tpaint)

        End Sub

        Public Sub DrawReactor(ByVal g As SKCanvas, ByVal TypeName As String)
            MyBase.Draw(g)

            Dim rect2 As New SKRect(X + (0.25 - 0.14) * Width, Y + (0.5 - 0.14 / 2) * Height, X + 0.25 * Width, Y + (0.5 - 0.14 / 2) * Height + 0.14 * Height)
            Dim rect3 As New SKRect(X + 0.75 * Width, Y + 0.1 * Height, X + 0.75 * Width + 0.14 * Width, Y + 0.1 * Height + 0.14 * Height)
            Dim rect4 As New SKRect(X + 0.75 * Width, Y + (0.9 - 0.14) * Height, X + 0.75 * Width + 0.14 * Width, Y + (0.9 - 0.14) * Height + 0.14 * Height)
            If Me.FlippedH = True Then
                rect2 = New SKRect(X + 0.75 * Width, Y + (0.5 - 0.14 / 2) * Height, X + 0.75 * Width + 0.14 * Width, Y + (0.5 - 0.14 / 2) * Height + 0.14 * Height)
                rect3 = New SKRect(X + (0.25 - 0.14) * Width, Y + 0.1 * Height, X + (0.25 - 0.14) * Width + 0.14 * Width, Y + 0.1 * Height + 0.14 * Height)
                rect4 = New SKRect(X + (0.25 - 0.14) * Width, Y + (0.9 - 0.14) * Height, X + (0.25 - 0.14) * Width + 0.14 * Width, Y + (0.9 - 0.14) * Height + 0.14 * Height)
            End If

            Dim myPen As New SKPaint()
            With myPen
                .Color = If(s.DarkMode, LineColorDark, LineColor)
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect As New SKRect(X, Y, Width, Height)

            If GradientMode Then

                Dim r0 As New SKRect(X, Y, X + Width, Y + Height)

                Dim radius2 = 0.8F * Math.Min(Width, Height)
                Dim center = New SKPoint(r0.MidX, r0.MidY)
                Dim offCenter = center - New SKPoint(radius2 / 2, radius2 / 2)

                Dim gradPen As New SKPaint()
                With gradPen
                    .Color = LineColor
                    .StrokeWidth = LineWidth
                    .IsStroke = False
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Shader = SKShader.CreateTwoPointConicalGradient(
                                    offCenter, 1, center, radius2,
                                    New SKColor() {SKColors.White, LineColor},
                                    Nothing, SKShaderTileMode.Clamp)
                End With

                g.DrawRoundRect(New SKRect(X + 0.25 * Width, Y, X + 0.75 * Width, Y + Height), 5, 5, gradPen)
                g.DrawRect(rect2, gradPen)
                g.DrawRect(rect3, gradPen)
                g.DrawRect(rect4, gradPen)

            End If

            g.DrawRoundRect(New SKRect(X + 0.25 * Width, Y, X + 0.75 * Width, Y + Height), 5, 5, myPen)
            g.DrawRect(rect2, myPen)
            g.DrawRect(rect3, myPen)
            g.DrawRect(rect4, myPen)

            'Draw signature of object type

            Dim tPen As New SKPaint()
            With tPen
                .TextSize = FontSize
                .TextEncoding = SKTextEncoding.Utf8
                .Color = If(s.DarkMode, LineColorDark, LineColor)
                .Typeface = DefaultTypeFace
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tPen.GetTextPath(TypeName, 0, 0).GetBounds(trect)
            Dim size As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)

            Dim ax, ay As Integer
            If Me.FlippedH Then
                ax = Me.X + (Me.Width - size.Width) / 2
                ay = Me.Y + Me.Height * 0.8 - size.Height
            Else
                ax = Me.X + (Me.Width - size.Width) / 2
                ay = Me.Y + Me.Height * 0.8 - size.Height
            End If

            If FlippedH Or FlippedV Or Rotation <> 0 Then

                Dim currmat = g.TotalMatrix

                g.Save()

                If FlippedV And Not FlippedH Then
                    g.Scale(1, -1, (X + Width / 2), (Y + Height / 2))
                ElseIf FlippedH And Not FlippedV Then
                    g.Scale(-1, 1, (X + Width / 2), (Y + Height / 2))
                ElseIf FlippedH And FlippedV Then
                    g.Scale(-1, -1, (X + Width / 2), (Y + Height / 2))
                End If

                If Rotation <> 0.0 Then g.RotateDegrees(Rotation, X + Width / 2, Y + Height / 2)

                g.DrawText(TypeName, ax, ay, tPen)

                g.SetMatrix(currmat)

            Else

                g.DrawText(TypeName, ax, ay, tPen)

            End If

            'Draw interior packing

            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)

            'DrawTag(g)

        End Sub

        Public Sub New()

            MyBase.New()

            Select Case GlobalSettings.Settings.RunningPlatform
                Case GlobalSettings.Settings.Platform.Windows
                    Me.DefaultTypeFace = SKTypeface.FromFamilyName("Segoe UI", SKTypefaceStyle.Bold)
                    Me.RegularTypeFace = SKTypeface.FromFamilyName("Segoe UI", SKTypefaceStyle.Normal)
                Case GlobalSettings.Settings.Platform.Linux
                    Me.DefaultTypeFace = SKTypeface.FromFamilyName("Ubuntu", SKTypefaceStyle.Bold)
                    Me.RegularTypeFace = SKTypeface.FromFamilyName("Ubuntu", SKTypefaceStyle.Normal)
                Case GlobalSettings.Settings.Platform.Mac
                    Me.DefaultTypeFace = SKTypeface.FromFamilyName("Helvetica Neue", SKTypefaceStyle.Bold)
                    Me.RegularTypeFace = SKTypeface.FromFamilyName("Helvetica Neue", SKTypefaceStyle.Normal)
            End Select

        End Sub

    End Class


End Namespace