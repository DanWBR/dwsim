Imports System.IO
Imports Eto.Drawing
Imports Eto.Forms
Imports SkiaSharp
Imports c = DWSIM.UI.Shared.Common

Public Class PerfAn

    Public Shared Function GetPerfAnWindow(solutionID As String) As TableLayout

        'Layout

        Dim content As New TableLayout With {.Padding = New Padding(10, 10, 10, 10), .Spacing = New Size(10, 10)}

        Dim InspectorLabel = New Label With {.Text = "Solution Inspector / Performance Analyzer: " & Date.FromBinary(solutionID).ToString, .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim InspectorDescription = New Label With {.Text = "The Performance Analyzer displays information about the methods called by the models, including how many times they were called and the time spent on each call.", .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorDescription.Font = New Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim cbType, cbSubType As New DropDown

        cbType.Items.Add("1")
        cbSubType.Items.Add("2")

        Dim cbTypeLabel = New Label With {.Text = "View", .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        cbTypeLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim cbSubTypeLabel = New Label With {.Text = "Type", .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        cbSubTypeLabel.Font = New Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim tb As New TableLayout With {.Spacing = New Size(10, 10)}
        tb.Rows.Add(New TableRow(cbTypeLabel, cbType, cbSubTypeLabel, cbSubType, Nothing))

        content.BackgroundColor = New Color(0.051F, 0.447F, 0.651F)

        content.Rows.Add(New TableRow(InspectorLabel))
        content.Rows.Add(New TableRow(InspectorDescription))

        content.Rows.Add(New TableRow(tb))

        Dim skview As New Eto.Forms.Controls.SkiaSharp.Shared.SKControl
        skview.Width = 1000
        skview.Height = 1000

        skview.PaintSurfaceAction = Sub(surface)
                                        surface.Canvas.Clear(SKColors.White)
                                    End Sub

        content.Rows.Add(New TableRow(skview))

        Return content

    End Function

End Class
