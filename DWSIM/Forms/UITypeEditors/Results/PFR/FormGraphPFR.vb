Public Class FormGraphPFR
    Inherits System.Windows.Forms.Form

    Protected m_results As ArrayList
    Protected m_conv As Dictionary(Of String, Double)

    Protected vx As Double()
    Protected vy As Double()
    Protected vn As String()
    Protected vya As ArrayList
    Protected m_ytitle As String
    Protected m_xtitle As String
    Public form As FormFlowsheet

    Public Property Points() As ArrayList
        Get
            Return m_results
        End Get
        Set(ByVal value As ArrayList)
            m_results = value
        End Set
    End Property

    Private Sub FormGraph_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        m_conv = New Dictionary(Of String, Double)
        vya = New ArrayList
        ReDim vx(Me.Points.Count - 1)
        ReDim vy(Me.Points.Count - 1)
        Me.m_conv = CType(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), DWSIM.SimulationObjects.Reactors.Reactor).ComponentConversions

        Me.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & DWSIM.App.GetLocalString("VisualizarResultados")

        Dim i As Integer = 0
        Dim j As Integer
        For Each obj As Object In Me.Points
            vx(i) = obj(0)
            i += 1
        Next

        j = 1
        Do
            i = 0
            For Each obj As Object In Me.Points
                vy(i) = obj(j)
                i += 1
            Next
            j += 1
            vya.Add(vy.Clone)
        Loop Until j = Me.m_conv.Count + 3

        ReDim vn(CType(Me.Points(0), Double()).Length - 3)

        i = 0
        For Each s As String In m_conv.Keys
            vn(i) = Global.DWSIM.DWSIM.App.GetComponentName(s)
            i += 1
        Next

        DrawGraph()

    End Sub

    Private Sub DrawGraph()


        Dim rnd As New Random(231)

        Dim i As Integer
        Dim color1 As System.Drawing.Color

        With Me.ZedGraphControl1.GraphPane
            .CurveList.Clear()
            .AddY2Axis("T (K)")
            .AddY2Axis("P (Pa)")
            With .Y2AxisList(0)
                .IsVisible = True
                .Title.Text = "T (K)"
                .Scale.FontSpec.Size = 10
            End With
            With .Y2AxisList(1)
                .IsVisible = True
                .Title.Text = "P (Pa)"
                .Scale.FontSpec.Size = 10
            End With
            i = 0
            Do
                color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                With .AddCurve(vn(i), vx, vya(i), color1, ZedGraph.SymbolType.Circle)
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Size = 5
                End With
                i += 1
            Loop Until i = vn.Length - 1
            color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
            With .AddCurve("T (K)", vx, vya(i), color1, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                .Symbol.Size = 5
                .IsY2Axis = True
                .YAxisIndex = 0
            End With
            color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
            With .AddCurve("P (Pa)", vx, vya(i + 1), color1, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                .Symbol.Size = 5
                .IsY2Axis = True
                .YAxisIndex = 1
            End With
            .Title.Text = DWSIM.App.GetLocalString("PerfildeConcentracoesnoReator")
            .XAxis.Title.Text = DWSIM.App.GetLocalString("V (m3)")
            .XAxis.Scale.FontSpec.Size = 10
            .YAxis.Title.Text = DWSIM.App.GetLocalString("C (mol/m3)")
            .YAxis.Scale.FontSpec.Size = 10
            With .Legend
                .Position = ZedGraph.LegendPos.TopCenter
                .IsHStack = True
                .Border.IsVisible = False
                .FontSpec.Size = 10
                .FontSpec.IsDropShadow = False
            End With
            .AxisChange(Me.CreateGraphics)
        End With
        Me.ZedGraphControl1.Invalidate()

    End Sub

End Class