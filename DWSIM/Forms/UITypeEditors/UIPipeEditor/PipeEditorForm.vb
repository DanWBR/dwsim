Public Class PipeEditorForm
    Inherits System.Windows.Forms.Form

    Dim px, py As New ArrayList
    Dim loaded As Boolean = False

    Private Sub PipeEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Button1.Text = DWSIM.App.GetLocalString("Detalhes31")
        Me.Height = 373
    End Sub

    Private Sub PipeEditor1_StatusChanged(ByVal e As System.EventArgs, ByVal statuscode As PipeEditorStatus) Handles PipeEditor1.StatusChanged

        If statuscode = PipeEditorStatus.OK Then
            px.Clear()
            py.Clear()
            If Me.KryptonRadioButton1.Checked Then
                With Me.PipeEditor1.Profile
                    Dim i As Integer = 1
                    Do
                        If .Sections(i).ObjectType = "Tubulaosimples" Then
                            If i >= 2 Then
                                px.Add(px(px.Count - 1) + Convert.ToDouble(.Sections(i).Comprimento))
                                py.Add(py(py.Count - 1) + Convert.ToDouble(.Sections(i).Elevacao))
                            Else
                                px.Add(Convert.ToDouble(.Sections(i).Comprimento))
                                py.Add(Convert.ToDouble(.Sections(i).Elevacao))
                            End If
                        Else
                            If i >= 2 Then
                                px.Add(px(px.Count - 1))
                                py.Add(py(py.Count - 1))
                            Else
                                px.Add(0.0#)
                                py.Add(0.0#)
                            End If
                        End If
                        i = i + 1
                    Loop Until .Sections.ContainsKey(i) = False
                End With

                With Me.GraphControl.GraphPane
                    .XAxis.Title.Text = DWSIM.App.GetLocalString("Comprimentom")
                    .YAxis.Title.Text = DWSIM.App.GetLocalString("Elevaom")
                End With

            Else
                With Me.PipeEditor1.Profile
                    Dim i As Integer = 1
                    Do
                        If .Sections(i).ObjectType = "Tubulaosimples" Then
                            If i >= 2 Then
                                px.Add(px(px.Count - 1) + (Convert.ToDouble(.Sections(i).Comprimento ^ 2 - .Sections(i).Elevacao ^ 2) ^ 0.5))
                                py.Add(py(py.Count - 1) + Convert.ToDouble(.Sections(i).Elevacao))
                            Else
                                px.Add((Convert.ToDouble(.Sections(i).Comprimento ^ 2 - .Sections(i).Elevacao ^ 2) ^ 0.5))
                                py.Add(Convert.ToDouble(.Sections(i).Elevacao))
                            End If
                        Else
                            If i >= 2 Then
                                px.Add(px(px.Count - 1))
                                py.Add(py(py.Count - 1))
                            Else
                                px.Add(0.0#)
                                py.Add(0.0#)
                            End If
                        End If
                        i = i + 1
                    Loop Until .Sections.ContainsKey(i) = False
                End With

                With Me.GraphControl.GraphPane
                    .XAxis.Title.Text = DWSIM.App.GetLocalString("DistnciaHorizontalm")
                    .YAxis.Title.Text = DWSIM.App.GetLocalString("Elevaom")
                End With

            End If

        End If

        With Me.GraphControl.GraphPane
            .Title.IsVisible = False
            .CurveList.Clear()
            With .AddCurve(DWSIM.App.GetLocalString("Perfil"), px.ToArray(GetType(Double)), py.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            Me.GraphControl.IsAutoScrollRange = True
            .XAxis.Title.FontSpec.Size = 22
            .YAxis.Title.FontSpec.Size = 22
            .XAxis.Scale.FontSpec.Size = 18
            .YAxis.Scale.FontSpec.Size = 18
            .AxisChange(Me.CreateGraphics)
            Me.GraphControl.Invalidate()
        End With

        Me.GraphControl.GraphPane.Legend.IsVisible = False

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If Me.Height = 373 Then
            Me.Button1.Text = DWSIM.App.GetLocalString("Detalhes21")
            Me.Height = 592
        Else
            Me.Button1.Text = DWSIM.App.GetLocalString("Detalhes31")
            Me.Height = 373
        End If
    End Sub

    Private Sub KryptonRadioButton1_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles KryptonRadioButton1.CheckedChanged, KryptonRadioButton2.CheckedChanged

        If loaded Then Call Me.PipeEditor1_StatusChanged(e, PipeEditorStatus.OK)

    End Sub

    Private Sub PipeEditorForm_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        loaded = True
    End Sub
End Class