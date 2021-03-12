Public Class FormChart

    Public px, py1, py2, py3, py4 As ArrayList
    Public xtitle, ytitle, title, y1ctitle, y2ctitle, y3ctitle, y4ctitle As String
    Public ycurvetypes As ArrayList
    Public xformat, yformat As Integer
    Public tbtext As String

    'xformat:
    '1 - double number
    '2 - integer
    '3 - date (dd/MM)

    'yformat:
    '1 - linear
    '2 - logarithmic

    'ycurvetypes:
    '1 - points only
    '2 - points and line
    '3 - line only
    '4 - dashed line
    '5 - dashed line with points
    '6 - non-smoothed line

    Private Sub FormChart_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        DrawChart()

        Me.tbinfo.Text = tbtext

    End Sub

    Public Sub DrawChart()

        Dim rnd As New Random()

        With Me.graph.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .YAxisList.Clear()
            If py1.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis(ytitle)
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 11
                .YAxisList.Add(ya0)
                With .AddCurve(y1ctitle, px.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                    Dim ppl As ZedGraph.PointPairList = .Points
                    ppl.Sort(ZedGraph.SortType.XValues)
                    Select Case ycurvetypes(0)
                        '1 - somente pontos
                        '2 - pontos e linha
                        '3 - somente linha
                        '4 - linha tracejada
                        '5 - linha tracejada com pontos
                        Case 1
                            .Line.IsVisible = False
                            .Line.IsSmooth = False
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = c1
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 2
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = c1
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 3
                            .Line.IsVisible = True
                            .Line.IsSmooth = True
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.IsVisible = False
                        Case 4
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Line.Style = Drawing2D.DashStyle.Dash
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.IsVisible = False
                        Case 5
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Line.Style = Drawing2D.DashStyle.Dash
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = c1
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 6
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = c1
                            .Symbol.IsVisible = False
                    End Select
                    .YAxisIndex = 0
                End With
            End If
            If Not py2 Is Nothing Then
                If py2.Count > 0 Then
                    With .AddCurve(y2ctitle, px.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.Black)
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(1)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = True
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If
            If Not py3 Is Nothing Then
                If py3.Count > 0 Then
                    With .AddCurve(y3ctitle, px.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Black)
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(2)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = True
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If
            If Not py4 Is Nothing Then
                If py4.Count > 0 Then
                    With .AddCurve(y4ctitle, px.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Black)
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(3)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = True
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = c1
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                .Color = c1
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If

            With .Legend
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
                .IsHStack = True
                .FontSpec.Size = 10
            End With


            With .XAxis
                .Title.Text = xtitle
                .Title.FontSpec.Size = 11
                .Scale.MinAuto = True
                .Scale.MaxAuto = True
                .Scale.FontSpec.Size = 10
                Select Case xformat
                    Case 1
                        .Type = ZedGraph.AxisType.Linear
                    Case 2
                        .Type = ZedGraph.AxisType.Linear
                    Case 3
                        .Type = ZedGraph.AxisType.DateAsOrdinal
                        .Scale.Format = "dd/MM/yy"
                End Select
            End With

            Select Case yformat
                Case 1
                    .YAxis.Type = ZedGraph.AxisType.Linear
                Case 2
                    .YAxis.Type = ZedGraph.AxisType.Log
            End Select

            With .Legend
                .Border.IsVisible = False
                .IsVisible = True
                .Position = ZedGraph.LegendPos.TopCenter
                .FontSpec.Size = 11
            End With

            .Margin.All = 10

            With .Title
                .IsVisible = True
                .Text = title
                .FontSpec.Size = 12
            End With

            Me.graph.IsAntiAlias = True
            .AxisChange(Me.CreateGraphics)
            .AxisChange(Me.CreateGraphics)
            Me.graph.Invalidate()

        End With

    End Sub

End Class