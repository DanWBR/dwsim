Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class CAPEOPENGraphic

        Inherits ShapeGraphic

        Public Property ChemSep As Boolean = False

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.CapeOpenUO
            Me.Description = "CAPE-OPEN Unit Operation Block"
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize)
            Me.New(graphicPosition)
            Me.SetSize(graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize)
            Me.New(New SKPoint(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height))
        End Sub

#End Region

        Public Overrides Sub PositionConnectors()

            If Not Me.AdditionalInfo Is Nothing Then

                Dim obj1(Me.InputConnectors.Count), obj2(Me.InputConnectors.Count) As Double
                Dim obj3(Me.OutputConnectors.Count), obj4(Me.OutputConnectors.Count) As Double
                obj1 = Me.AdditionalInfo(0)
                obj2 = Me.AdditionalInfo(1)
                obj3 = Me.AdditionalInfo(2)
                obj4 = Me.AdditionalInfo(3)

                Try
                    Dim i As Integer = 0
                    For Each ic As IConnectionPoint In Me.InputConnectors
                        ic.Position = New Point(Me.X + obj1(i), Me.Y + obj2(i))
                        i = i + 1
                    Next
                    i = 0
                    For Each oc As IConnectionPoint In Me.OutputConnectors
                        oc.Position = New Point(Me.X + obj3(i), Me.Y + obj4(i))
                        i = i + 1
                    Next
                Catch ex As Exception

                End Try

            Else

                Try
                    Dim i As Integer = 0
                    For Each ic As IConnectionPoint In Me.InputConnectors
                        ic.Position = New Point(Me.X, Me.Y + (i + 1) / InputConnectors.Count * Height)
                        i = i + 1
                    Next
                    i = 0
                    For Each oc As IConnectionPoint In Me.OutputConnectors
                        oc.Position = New Point(Me.X + Width, Me.Y + (i + 1) / OutputConnectors.Count * Height)
                        i = i + 1
                    Next
                Catch ex As Exception

                End Try

            End If

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            MyBase.Draw(g)

            PositionConnectors()
            UpdateStatus()

            If Owner IsNot Nothing AndAlso Owner.UseEmbeddedImage = True AndAlso Owner.EmbeddedImageData <> "" Then

                Dim p As New SKPaint
                With p
                    p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    p.FilterQuality = SKFilterQuality.High
                End With

                Using image As SKImage = EmbeddedImageGraphic.Base64ToImage(Owner.EmbeddedImageData)
                    canvas.DrawImage(image, New SKRect(X, Y, X + Width, Y + Height), p)
                End Using

            Else

                Dim myPen As New SKPaint()
                With myPen
                    .Color = LineColor
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = True
                    .StrokeWidth = LineWidth
                End With

                If ChemSep Then

                    If GradientMode Then

                        Dim r0 As New SKRect(X, Y, X + Width, Y + Height)

                        Dim radius2 = 0.9F * Math.Min(Width, Height)
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

                        canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, gradPen)

                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.1 * Height), New SKPoint(X + 0.175 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.1 * Height)}, gradPen)
                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.9 * Height), New SKPoint(X + 0.175 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.9 * Height)}, gradPen)

                        canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), gradPen)
                        canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), gradPen)

                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.25 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.3 * Height)}, gradPen)
                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.75 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.7 * Height)}, gradPen)

                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.3 * Height), gradPen)
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.7 * Height), gradPen)

                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.98 * Height), (X + Width), (Y + 0.98 * Height), gradPen)

                        If Me.Shape = 1 Then
                            canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), gradPen)
                            canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), gradPen)
                        Else
                            canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), gradPen)
                        End If

                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.125 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.225 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.175 * Height)}, gradPen)
                        canvas.DrawLine((X + 0.65 * 1.25 * Width), (Y + 0.175 * Height), (X + Width), (Y + 0.175 * Height), gradPen)

                        canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.875 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.775 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + Width, Y + 0.825 * Height)}, gradPen)
                        canvas.DrawLine((X + 0.5 * 1.25 * Width), (Y + 0.825 * Height), (X + 0.4 * 1.25 * Width), (Y + 0.825 * Height), gradPen)

                        canvas.DrawLine((X), (Y + 0.5 * Height), (X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), gradPen)

                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.2 * Height), (X + 0.31 * Width), (Y + 0.2 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.31 * Width), (Y + 0.3 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.4 * Height), (X + 0.31 * Width), (Y + 0.4 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), (X + 0.31 * Width), (Y + 0.5 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.6 * Height), (X + 0.31 * Width), (Y + 0.6 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.31 * Width), (Y + 0.7 * Height), gradPen)
                        canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.8 * Height), (X + 0.31 * Width), (Y + 0.8 * Height), gradPen)

                    End If

                    canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)

                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.1 * Height), New SKPoint(X + 0.175 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.1 * Height)}, myPen)
                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.9 * Height), New SKPoint(X + 0.175 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.9 * Height)}, myPen)

                    canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
                    canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.25 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.3 * Height)}, myPen)
                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.75 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.7 * Height)}, myPen)

                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.3 * Height), myPen)
                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.7 * Height), myPen)

                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.98 * Height), (X + Width), (Y + 0.98 * Height), myPen)

                    If Me.Shape = 1 Then
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                    Else
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                    End If

                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.125 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.225 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.175 * Height)}, myPen)
                    canvas.DrawLine((X + 0.65 * 1.25 * Width), (Y + 0.175 * Height), (X + Width), (Y + 0.175 * Height), myPen)

                    canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.875 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.775 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + Width, Y + 0.825 * Height)}, myPen)
                    canvas.DrawLine((X + 0.5 * 1.25 * Width), (Y + 0.825 * Height), (X + 0.4 * 1.25 * Width), (Y + 0.825 * Height), myPen)

                    canvas.DrawLine((X), (Y + 0.5 * Height), (X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), myPen)

                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.2 * Height), (X + 0.31 * Width), (Y + 0.2 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.31 * Width), (Y + 0.3 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.4 * Height), (X + 0.31 * Width), (Y + 0.4 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), (X + 0.31 * Width), (Y + 0.5 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.6 * Height), (X + 0.31 * Width), (Y + 0.6 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.31 * Width), (Y + 0.7 * Height), myPen)
                    canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.8 * Height), (X + 0.31 * Width), (Y + 0.8 * Height), myPen)

                Else

                    Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)

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

                        canvas.DrawRoundRect(rect1, 2, 2, gradPen)

                    End If

                    canvas.DrawRoundRect(rect1, 2, 2, myPen)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 10.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = LineColor
                        .IsStroke = False
                        .Typeface = DefaultTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("CO", 0, 0).GetBounds(trect)

                    Dim ax, ay As Integer
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    If FlippedH Or FlippedV Or Rotation <> 0 Then

                        Dim currmat = canvas.TotalMatrix

                        canvas.Save()

                        If FlippedV And Not FlippedH Then
                            canvas.Scale(1, -1, (X + Width / 2), (Y + Height / 2))
                        ElseIf FlippedH And Not FlippedV Then
                            canvas.Scale(-1, 1, (X + Width / 2), (Y + Height / 2))
                        ElseIf FlippedH And FlippedV Then
                            canvas.Scale(-1, -1, (X + Width / 2), (Y + Height / 2))
                        End If

                        If Rotation <> 0.0 Then canvas.RotateDegrees(Rotation, X + Width / 2, Y + Height / 2)

                        canvas.DrawText("CO", ax, ay, tpaint)

                        canvas.SetMatrix(currmat)

                    Else

                        canvas.DrawText("CO", ax, ay, tpaint)

                    End If

                End If

            End If

        End Sub

    End Class

End Namespace