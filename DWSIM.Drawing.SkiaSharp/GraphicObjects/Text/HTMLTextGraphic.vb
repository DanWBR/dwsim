Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports SkiaSharp.Views.Desktop.Extensions
Imports TheArtOfDev.HtmlRenderer.WinForms

Namespace GraphicObjects

    Public Class HTMLTextGraphic

        Inherits GraphicObject

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_HTMLText

            Me.Height = 300
            Me.Width = 400

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Text = Text
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

#End Region

        Public Property Text As String = "<html><body><p>Double-click to edit this text.</p></body></html>"

        Public Property Size As Double = 14.0#

        Public Property Color As SKColor = SKColors.Black

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            If DrawMode = 0 Then
                With tpaint
                    .TextSize = Size
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = If(GlobalSettings.Settings.DarkMode, SKColors.LightSteelBlue, Color)
                    .IsStroke = False
                    .Typeface = GetFont()
                End With
            Else
                With tpaint
                    .TextSize = Size
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = SKColors.Black
                    .IsStroke = False
                    .Typeface = GetFont()
                End With
            End If

            Dim newtext = ReplaceVars(Text)

            Dim p As New SKPaint
            With p
                p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                p.FilterQuality = SKFilterQuality.High
            End With

            Dim scaling = GlobalSettings.Settings.DpiScale

            If GlobalSettings.Settings.IsRunningOnMono() Then
                Using img = HtmlRender.RenderToImage(newtext, Width, Height, System.Drawing.Color.White)
                    Using bmp = New System.Drawing.Bitmap(img)
                        Using skbmp = bmp.ToSKBitmap()
                            canvas.DrawBitmap(skbmp, X, Y, p)
                        End Using
                    End Using
                End Using
            Else
                Using img = HtmlRender.RenderToImageGdiPlus(newtext, Width, Height,
                                                            System.Drawing.Text.TextRenderingHint.SystemDefault)
                    Using bmp = New System.Drawing.Bitmap(img)
                        Using skbmp = bmp.ToSKBitmap()
                            canvas.DrawBitmap(skbmp, X, Y, p)
                        End Using
                    End Using
                End Using
            End If

        End Sub

        Private Function ReplaceVars(oldtext As String) As String

            Dim newtext As String = oldtext
            Dim i As Integer = 0

            If Flowsheet IsNot Nothing Then
                If Text.Contains("{") And Text.Contains("}") Then
                    For i = 1 To Flowsheet.WatchItems.Count
                        Dim objID = Flowsheet.WatchItems(i - 1).ObjID
                        Dim propID = Flowsheet.WatchItems(i - 1).PropID
                        If Flowsheet.SimulationObjects.ContainsKey(objID) Then
                            Dim units = Flowsheet.SimulationObjects(objID).GetPropertyUnit(propID)
                            Dim name = Flowsheet.GetTranslatedString(propID)
                            newtext = newtext.Replace("{" + i.ToString() + ":N}", name)
                            newtext = newtext.Replace("{" + i.ToString() + ":U}", units)
                            Dim value = Flowsheet.SimulationObjects(objID).GetPropertyValue(propID).ToString()
                            If Double.TryParse(value, New Double) Then
                                Dim dval = Double.Parse(value).ToString(Flowsheet.FlowsheetOptions.NumberFormat)
                                newtext = newtext.Replace("{" + i.ToString() + ":V}", dval)
                            Else
                                newtext = newtext.Replace("{" + i.ToString() + ":V}", value)
                            End If
                        End If
                    Next
                End If
            End If

            Return newtext

        End Function

    End Class

End Namespace