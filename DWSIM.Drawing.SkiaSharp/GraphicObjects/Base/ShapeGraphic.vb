Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    Public MustInherit Class ShapeGraphic

        Inherits GraphicObject

        Public Sub UpdateStatus()

            Dim alpha As Integer = 255

            If SemiTransparent Then alpha = 50

            If Me.Active Then
                Select Case Status
                    Case Status.Calculated
                        LineColor = SKColors.SteelBlue
                    Case Status.Calculating
                        LineColor = SKColors.YellowGreen
                    Case Status.ErrorCalculating
                        LineColor = SKColors.Red
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

        End Sub

        Public Property DefaultTypeFace As SKTypeface

        Public Property SemiTransparent As Boolean = False

        Public Overridable Property LineWidth As Integer = 1

        Public Overridable Property GradientMode As Boolean = True

        Public Overridable Property LineColor As SKColor = SKColors.Black

        Public Overridable Property Fill As Boolean = False

        Public Overridable Property FillColor As SKColor = SKColors.LightGray

        Public Overridable Property GradientColor1 As SKColor = SKColors.LightGray

        Public Overridable Property GradientColor2 As SKColor = SKColors.White

        Public Overridable Property FontSize As Double = 10.0#

        Public Overrides Sub Draw(ByVal g As Object)

            MyBase.Draw(g)

            DrawTag(g)

        End Sub

        Public Overridable Sub DrawTag(ByVal g As SKCanvas)

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = LineColor
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath(Me.Tag, 0, 0).GetBounds(trect)
            Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)

            Dim strx As Single = (Me.Width - tpaint.MeasureText(Me.Tag)) / 2

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
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect As New SKRect(X, Y, Width, Height)

            g.DrawRoundRect(New SKRect(X + 0.25 * Width, Y, X + 0.75 * Width, Y + Height), 5, 5, myPen)
            g.DrawRect(rect2, myPen)
            g.DrawRect(rect3, myPen)
            g.DrawRect(rect4, myPen)

            'Draw signature of object type

            Dim tPen As New SKPaint()
            With tPen
                .TextSize = FontSize
                .TextEncoding = SKTextEncoding.Utf8
                .Color = LineColor
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

            g.DrawText(TypeName, ax, ay, tPen)

            'Draw interior packing

            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
            g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)

            DrawTag(g)

        End Sub

        Public Sub New()

            MyBase.New()

            Me.DefaultTypeFace = SKTypeface.FromFamilyName("Helvetica", SKTypefaceStyle.Bold)

        End Sub

    End Class


End Namespace