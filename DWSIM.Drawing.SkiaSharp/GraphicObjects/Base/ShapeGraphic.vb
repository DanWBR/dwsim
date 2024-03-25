Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports s = DWSIM.GlobalSettings.Settings
Imports DWSIM.ExtensionMethods
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects

    Public MustInherit Class ShapeGraphic

        Inherits GraphicObject

        Private AttentionImage, CalculatingImage, WeightImage, TemperatureImage, PressureImage,
            FlowImage, LightningImage, VolumeImage, ResidenceTimeImage As SKImage

        Public Sub UpdateStatus()

            Dim alpha As Integer = 255

            If SemiTransparent Then alpha = 50

            If Not OverrideColors Then
                If Me.Active Then
                    Select Case Status
                        Case Status.Calculated
                            If s.DarkMode Then
                                LineColor = SKColors.WhiteSmoke
                            Else
                                LineColor = SKColors.SteelBlue
                            End If
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

        Public Property SemiTransparent As Boolean = False
        Public Overridable Property LineWidth As Integer = 1
        Public Overridable Property GradientMode As Boolean = True
        Public Overridable Property LineColor As SKColor = SKColors.Black
        Public Overridable Property LineColorDark As SKColor = SKColors.WhiteSmoke
        Public Overridable Property Fill As Boolean = False
        Public Overridable Property FillColor As SKColor = SKColors.LightGray
        Public Overridable Property FillColorDark As SKColor = SKColors.White
        Public Overridable Property GradientColor1 As SKColor = SKColors.LightGray
        Public Overridable Property GradientColor2 As SKColor = SKColors.White
        Public Overridable Property FontSize As Double = 10.0#
        Public Overridable Property OverrideColors As Boolean = False

        Public Overrides Sub Draw(ByVal g As Object)

            MyBase.Draw(g)

        End Sub

        Friend Sub DrawIcon(canvas As SKCanvas)

            If Image Is Nothing Then

                Using streamBG = GetIconAsStream()
                    Using bitmap = SKBitmap.Decode(streamBG)
                        Image = SKImage.FromBitmap(bitmap)
                    End Using
                End Using

            End If

            Using p As New SKPaint With {.IsAntialias = False, .FilterQuality = SKFilterQuality.High}
                canvas.DrawImage(Image, New SKRect(X, Y, X + Width, Y + Height), p)
            End Using

            If Not Calculated Then
                Using p As New SKPaint With {.IsAntialias = False, .FilterQuality = SKFilterQuality.None}
                    p.BlendMode = SKBlendMode.Color
                    p.ColorFilter = SKColorFilter.CreateBlendMode(LineColor, SKBlendMode.SrcIn)
                    canvas.DrawImage(Image, New SKRect(X, Y, X + Width, Y + Height), p)
                End Using
            End If

            If Not Active Then
                Using p As New SKPaint With {.IsAntialias = False, .FilterQuality = SKFilterQuality.None}
                    p.BlendMode = SKBlendMode.Color
                    p.ColorFilter = SKColorFilter.CreateBlendMode(SKColors.Gray, SKBlendMode.SrcIn)
                    canvas.DrawImage(Image, New SKRect(X, Y, X + Width, Y + Height), p)
                End Using
            End If

        End Sub

        Public Sub DrawCalculatingMode(ByVal canvas As SKCanvas)

            If CalculatingImage Is Nothing Then

                Dim assm = Me.GetType.Assembly
                Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.clock.png")
                    Using bitmap = SKBitmap.Decode(filestr)
                        CalculatingImage = SKImage.FromBitmap(bitmap)
                    End Using
                End Using

            End If

            Dim x0, y0, w0, h0 As Double

            x0 = X + Width / 2 - Width / 4
            y0 = Y + Height / 2 - Height / 4
            w0 = Width / 2
            h0 = Width / 2

            Using p As New SKPaint With {.IsStroke = False, .Color = SKColors.White.WithAlpha(100)}
                canvas.DrawRect(X, Y, X + Width, Y + Height, p)
            End Using

            Using p As New SKPaint With {.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                canvas.DrawImage(CalculatingImage, New SKRect(x0, y0, x0 + w0, y0 + w0), p)
            End Using

        End Sub

        Public Sub DrawNotDynamicsCompatible(ByVal canvas As SKCanvas)

            If AttentionImage Is Nothing Then

                Dim assm = Me.GetType.Assembly
                Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.attention.png")
                    Using bitmap = SKBitmap.Decode(filestr)
                        AttentionImage = SKImage.FromBitmap(bitmap)
                    End Using
                End Using

            End If

            Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                canvas.DrawImage(AttentionImage, New SKRect(X + Width / 2 - 10, Y - 25, X + Width / 2 + 10, Y - 5), p)
            End Using

        End Sub

        Public Sub DrawDynamicProperties(ByVal g As SKCanvas)

            If Flowsheet IsNot Nothing Then

                If TypeOf Owner Is IUnitOperation Then

                    If VolumeImage Is Nothing Then
                        Dim assm = Me.GetType.Assembly
                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.volume.png")
                            Using bitmap = SKBitmap.Decode(filestr)
                                VolumeImage = SKImage.FromBitmap(bitmap)
                            End Using
                        End Using
                    End If
                    If WeightImage Is Nothing Then
                        Dim assm = Me.GetType.Assembly
                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.weight.png")
                            Using bitmap = SKBitmap.Decode(filestr)
                                WeightImage = SKImage.FromBitmap(bitmap)
                            End Using
                        End Using
                    End If
                    If ResidenceTimeImage Is Nothing Then
                        Dim assm = Me.GetType.Assembly
                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.clock2.png")
                            Using bitmap = SKBitmap.Decode(filestr)
                                ResidenceTimeImage = SKImage.FromBitmap(bitmap)
                            End Using
                        End Using
                    End If

                    Dim t = Owner.GetDynamicResidenceTime()
                    Dim v = Owner.GetDynamicVolume()
                    Dim m = Owner.GetDynamicContents()

                    If Double.IsNaN(t) Then Exit Sub

                    Dim nf = "N2"
                    Dim su = Flowsheet.FlowsheetOptions.SelectedUnitSystem

                    Dim text1 = v.ConvertFromSI(su.volume).ToString(nf) + " " + su.volume
                    Dim text2 = m.ConvertFromSI(su.mass).ToString(nf) + " " + su.mass
                    Dim text3 = t.ConvertFromSI(su.time).ToString(nf) + " " + su.time

                    Dim trect As New SKRect(0, 0, 2, 2)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = FontSize
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = If(s.DarkMode, LineColorDark, LineColor)
                        .IsStroke = False
                        .Typeface = GetFont()
                    End With

                    Dim bpaint As SKPaint = Nothing
                    Dim framepaint As SKPaint = Nothing

                    Select Case DrawMode

                        Case 1

                        Case Else

                            bpaint = New SKPaint()
                            With bpaint
                                .TextSize = FontSize
                                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                                .Typeface = BoldTypeFace
                                .Color = If(s.DarkMode, SKColors.Transparent, SKColors.White.WithAlpha(200))
                                .IsStroke = True
                                .StrokeWidth = 2
                                .BlendMode = SKBlendMode.Overlay
                            End With

                            framepaint = New SKPaint()
                            With framepaint
                                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                                .Color = If(s.DarkMode, SKColors.Transparent, SKColors.LightGray)
                                .IsStroke = False
                                .BlendMode = SKBlendMode.Luminosity
                            End With

                    End Select

                    Dim strx = (Me.Width - tpaint.MeasureText(text1)) / 2

                    tpaint.GetTextPath(text1, 0, 0).GetBounds(trect)
                    Dim tsize1 As New SKSize(trect.Right - trect.Left, trect.Bottom - trect.Top)
                    trect = New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath(text2, 0, 0).GetBounds(trect)
                    Dim tsize2 As New SKSize(trect.Right - trect.Left, trect.Bottom - trect.Top)
                    trect = New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath(text3, 0, 0).GetBounds(trect)
                    Dim tsize3 As New SKSize(trect.Right - trect.Left, trect.Bottom - trect.Top)

                    Dim imgsize = New SKSize(Math.Abs(tsize1.Height + 4), Math.Abs(tsize1.Height + 4))

                    Dim y0 = Y + Height + 2 * tsize1.Height + 5
                    Dim yf = y0 + (tsize1.Height + 4) * 3
                    Dim midx = X + Width / 2
                    Dim x0 = midx - New Double() {tpaint.MeasureText(text1),
                        tpaint.MeasureText(text2),
                        tpaint.MeasureText(text3)}.Max / 2 - imgsize.Width
                    Dim xf = midx + New Double() {tpaint.MeasureText(text1),
                        tpaint.MeasureText(text2),
                        tpaint.MeasureText(text3)}.Max / 2 + imgsize.Width

                    If framepaint IsNot Nothing Then
                        g.DrawRoundRect(New SKRoundRect(New SKRect(x0, y0, xf, yf), 2), framepaint)
                    End If

                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                        g.DrawImage(VolumeImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, y0), imgsize), p)
                    End Using

                    g.DrawText(text1, X + strx + 0.6 * imgsize.Width, y0 + tsize1.Height + 2, tpaint)

                    y0 += tsize1.Height + 4

                    strx = (Me.Width - tpaint.MeasureText(text2)) / 2

                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                        g.DrawImage(WeightImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, y0), imgsize), p)
                    End Using

                    g.DrawText(text2, X + strx + 0.6 * imgsize.Width, y0 + tsize1.Height + 2, tpaint)

                    strx = (Me.Width - tpaint.MeasureText(text3)) / 2

                    y0 += tsize1.Height + 4

                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                        g.DrawImage(ResidenceTimeImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, y0), imgsize), p)
                    End Using

                    g.DrawText(text3, X + strx + 0.6 * imgsize.Width, y0 + tsize1.Height + 2, tpaint)

                End If

            End If

        End Sub

        Public Overridable Sub DrawDynSpec(ByVal g As SKCanvas, ByVal DynSpec As Enums.Dynamics.DynamicsSpecType)

            Dim tpaint As New SKPaint()

            Dim text As String = "P"

            If DynSpec = Enums.Dynamics.DynamicsSpecType.Flow Then text = "F"

            With tpaint
                .TextSize = FontSize
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = If(DynSpec = Enums.Dynamics.DynamicsSpecType.Flow, SKColors.Brown, SKColors.Blue)
                .IsStroke = False
                .Typeface = BoldTypeFace
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath(text, 0, 0).GetBounds(trect)
            Dim tsize As New SKSize(trect.Right - trect.Left, trect.Bottom - trect.Top)

            Dim strx As Single = (Me.Width - tpaint.MeasureText(text)) / 2

            Dim bpaint As New SKPaint()

            With bpaint
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = False
                .Color = SKColors.LightGray
            End With

            g.DrawRoundRect(X - 3 + strx, Y - 8 - tsize.Height, tsize.Width + 6, tsize.Height + 6, 2, 2, bpaint)
            g.DrawText(text, X + strx, Y - 5, tpaint)

        End Sub

        Public Overridable Sub DrawTag(ByVal g As SKCanvas)

            If DrawLabel Then

                Dim tpaint As New SKPaint()

                With tpaint
                    .TextSize = FontSize
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = If(s.DarkMode, LineColorDark, LineColor)
                    .IsStroke = False
                    .Typeface = GetFont()
                End With

                Dim trect As New SKRect(0, 0, 2, 2)
                tpaint.GetTextPath(Me.Tag, 0, 0).GetBounds(trect)
                Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)

                Dim strx As Single = (Me.Width - tsize.Width) / 2

                Dim bpaint As SKPaint = Nothing

                Select Case DrawMode

                    Case 1

                        With tpaint
                            .TextSize = FontSize
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .Color = SKColors.Black
                            .IsStroke = False
                            .Typeface = GetFont()
                        End With

                    Case Else

                        bpaint = New SKPaint()
                        With bpaint
                            .TextSize = FontSize
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .Typeface = BoldTypeFace
                            .Color = If(s.DarkMode, SKColors.Transparent, SKColors.White.WithAlpha(200))
                            .IsStroke = True
                            .StrokeWidth = 2
                            .BlendMode = SKBlendMode.Overlay
                        End With

                        g.DrawText(Me.Tag, X + strx, Y + Height + 14, bpaint)

                End Select

                g.DrawText(Me.Tag, X + strx, Y + Height + 14, tpaint)

                If Flowsheet IsNot Nothing Then
                    Dim fo = Flowsheet.FlowsheetOptions
                    If fo.DisplayMaterialStreamTemperatureValue Or
                        fo.DisplayMaterialStreamEnergyFlowValue Or
                        fo.DisplayMaterialStreamMassFlowValue Or
                        fo.DisplayMaterialStreamMolarFlowValue Or
                        fo.DisplayMaterialStreamPressureValue Or
                        fo.DisplayMaterialStreamVolFlowValue Or
                        fo.DisplayEnergyStreamPowerValue Then
                        If Flowsheet.SimulationObjects.ContainsKey(Name) Then
                            Dim eformat = "N2"
                            If ObjectType = ObjectType.MaterialStream Then
                                Dim deltay = tsize.Height
                                Dim mstr = Flowsheet.SimulationObjects(Name)
                                If fo.DisplayMaterialStreamTemperatureValue Then
                                    If TemperatureImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.temperature.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                TemperatureImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_0"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.temperature
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(TemperatureImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                                If fo.DisplayMaterialStreamPressureValue Then
                                    If PressureImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.pressure.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                PressureImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_1"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.pressure
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(PressureImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                                If fo.DisplayMaterialStreamMassFlowValue Then
                                    If FlowImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.flow.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                FlowImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_2"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.massflow
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(FlowImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                                If fo.DisplayMaterialStreamMolarFlowValue Then
                                    If FlowImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.flow.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                FlowImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_3"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.molarflow
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(FlowImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                                If fo.DisplayMaterialStreamVolFlowValue Then
                                    If FlowImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.flow.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                FlowImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_4"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.volumetricFlow
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(FlowImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                                If fo.DisplayMaterialStreamEnergyFlowValue Then
                                    If LightningImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.lightning_bolt.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                LightningImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim eval = Convert.ToDouble(mstr.GetPropertyValue("PROP_MS_154"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.heatflow
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2
                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(LightningImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 - deltay + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - deltay - tsize.Height + 4, tpaint)
                                    deltay += tsize.Height - 4
                                End If
                            ElseIf ObjectType = ObjectType.EnergyStream Then
                                If fo.DisplayEnergyStreamPowerValue Then
                                    If LightningImage Is Nothing Then
                                        Dim assm = Me.GetType.Assembly
                                        Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.lightning_bolt.png")
                                            Using bitmap = SKBitmap.Decode(filestr)
                                                LightningImage = SKImage.FromBitmap(bitmap)
                                            End Using
                                        End Using
                                    End If
                                    Dim estr = Flowsheet.SimulationObjects(Name)
                                    Dim eval = Convert.ToDouble(estr.GetPropertyValue("PROP_ES_0"))
                                    Dim eunit = Flowsheet.FlowsheetOptions.SelectedUnitSystem.heatflow
                                    Dim estring = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(eunit, eval).ToString(eformat) + " " + eunit
                                    strx = (Me.Width - tpaint.MeasureText(estring)) / 2

                                    Dim imgsize = New SKSize(Math.Abs(tsize.Height) + 4, Math.Abs(tsize.Height) + 4)
                                    Using p As New SKPaint With {.IsAntialias = s.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                                        g.DrawImage(LightningImage, SKRect.Create(New SKPoint(X + strx - 0.6 * imgsize.Width, Y + Height + 14 + 2), imgsize), p)
                                    End Using
                                    If bpaint IsNot Nothing Then
                                        g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - tsize.Height + 4, bpaint)
                                    End If
                                    g.DrawText(estring, X + strx + 0.6 * imgsize.Width, Y + Height + 14 - tsize.Height + 4, tpaint)
                                End If
                            End If
                        End If
                    End If
                End If

            End If

        End Sub

        Public Sub DrawReactor(ByVal g As SKCanvas, ByVal TypeName As String)

            MyBase.Draw(g)

            Dim rect As New SKRect(X, Y, Width, Height)

            Dim tPen As New SKPaint()

            Dim myPen As New SKPaint()
            Select Case DrawMode

                Case 0

                    'default

                    With myPen
                        .Color = If(s.DarkMode, LineColorDark, LineColor)
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    'Draw signature of object type

                    With tPen
                        .TextSize = FontSize
                        .TextEncoding = SKTextEncoding.Utf8
                        .Color = If(s.DarkMode, LineColorDark, LineColor)
                        .Typeface = BoldTypeFace
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                Case 1

                    'b/w

                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    'Draw signature of object type

                    With tPen
                        .TextSize = FontSize
                        .TextEncoding = SKTextEncoding.Utf8
                        .Color = SKColors.Black
                        .Typeface = BoldTypeFace
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                Case 2

                    DrawIcon(g)

                Case 3

                    'Temperature Gradients

                Case 4

                    'Pressure Gradients

                Case 5

                    'Temperature/Pressure Gradients

            End Select

            If DrawMode <> 2 Then

                Dim rect2 As New SKRect(X + (0.25 - 0.14) * Width, Y + (0.5 - 0.14 / 2) * Height, X + 0.25 * Width, Y + (0.5 - 0.14 / 2) * Height + 0.14 * Height)
                Dim rect3 As New SKRect(X + 0.75 * Width, Y + 0.1 * Height, X + 0.75 * Width + 0.14 * Width, Y + 0.1 * Height + 0.14 * Height)
                Dim rect4 As New SKRect(X + 0.75 * Width, Y + (0.9 - 0.14) * Height, X + 0.75 * Width + 0.14 * Width, Y + (0.9 - 0.14) * Height + 0.14 * Height)

                If DrawMode = 0 Then

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
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

                Dim trect As New SKRect(0, 0, 2, 2)
                tPen.GetTextPath(TypeName, 0, 0).GetBounds(trect)
                Dim size As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)

                Dim ax, ay As Integer
                If Me.FlippedH Then
                    ax = Me.X + (Me.Width - size.Width) / 2 - 1.0
                    ay = Me.Y + Me.Height * 0.8 - size.Height
                Else
                    ax = Me.X + (Me.Width - size.Width) / 2 - 1.0
                    ay = Me.Y + Me.Height * 0.8 - size.Height
                End If

                Using New SKAutoCanvasRestore(g)
                    StraightCanvas(g)
                    g.DrawText(TypeName, ax, ay, tPen)
                End Using

                'Draw interior packing

                g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)
                g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
                g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.3 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.7 * Height)}, myPen)
                g.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.25 * Width, Y + 0.7 * Height), New SKPoint(X + 0.75 * Width, Me.Y + 0.3 * Height)}, myPen)

            End If

        End Sub

        Public Sub New()

            MyBase.New()

            FontStyle = FontStyle.Bold

        End Sub

        Public Overrides Function GetPointValue(type As PointValueType, X As Integer, Y As Integer, args As List(Of Object)) As Double

            If X >= 0 And X <= Width And Y >= 0 And Y <= Height Then
                Dim value As Double
                Dim icl = InputConnectors.Where(Function(c) c.Type <> ConType.ConEn And c.IsAttached).ToList()
                Dim ocl = OutputConnectors.Where(Function(c) c.Type <> ConType.ConEn And c.IsAttached).ToList()
                If icl.Count = 0 And ocl.Count = 0 Then Return Double.NaN
                icl.AddRange(ocl)
                Dim points As New List(Of Tuple(Of Point, Double))
                For Each ic In icl
                    Dim im = TryCast(ic.AttachedConnector.AttachedFrom.Owner, IMaterialStream)
                    If im IsNot Nothing Then
                        Dim v As Double
                        Select Case type
                            Case PointValueType.Temperature
                                v = im.GetTemperature()
                            Case PointValueType.Pressure
                                v = im.GetPressure()
                            Case PointValueType.Flow
                                v = im.GetMassFlow()
                            Case PointValueType.EnergyFlow
                                v = im.GetEnergyFlow()
                            Case PointValueType.Concentration
                                v = im.GetCompoundMassConcentration(args(0))
                        End Select
                        points.Add(New Tuple(Of Point, Double)(New Point(ic.Position.X - X, ic.Position.Y - Y), v))
                    End If
                Next
                value = MathOps.BilinearInterpolation.Interpolate(New Point(X, Y), points)
                Return value
            Else
                Return Double.NaN
            End If

        End Function

    End Class


End Namespace