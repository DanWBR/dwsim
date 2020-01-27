Imports System.Drawing
Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.PumpOps

Public Class EditingForm_CompressorExpander_Curves

    Public simobj As ISimulationObject

    Public loaded As Boolean = False

    Private Sub EditingForm_CompressorExpander_Curves_Load(sender As Object, e As EventArgs) Handles MyBase.Shown

        Populate()

    End Sub

    Sub Populate()

        TabControl1.TabPages.Clear()

        If TypeOf simobj Is Compressor Then

            Dim uo = DirectCast(simobj, Compressor)

            For Each item In uo.Curves

                Dim editor As New ComprExprCurveSet

                editor.curvedata = item.Value
                editor.speed = item.Key
                editor.Dock = DockStyle.Fill
                editor.Populate()

                Dim tab1 As New TabPage(item.Key.ToString & " rpm")
                tab1.Controls.Add(editor)

                AddHandler editor.tbRotation.TextChanged, Sub()
                                                              tab1.Text = editor.tbRotation.Text & " rpm"
                                                          End Sub

                TabControl1.TabPages.Add(tab1)

            Next

        Else

            Dim uo = DirectCast(simobj, Expander)

            For Each item In uo.Curves

                Dim editor As New ComprExprCurveSet

                editor.curvedata = item.Value
                editor.speed = item.Key
                editor.Dock = DockStyle.Fill
                editor.Populate()

                Dim tab1 As New TabPage(item.Key.ToString & " rpm")
                tab1.Controls.Add(editor)

                AddHandler editor.tbRotation.TextChanged, Sub()
                                                              tab1.Text = editor.tbRotation.Text & " rpm"
                                                          End Sub

                TabControl1.TabPages.Add(tab1)

            Next

        End If


    End Sub

    Private Sub EditingForm_CompressorExpander_Curves_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing


        Try

            Dim newcurves As New Dictionary(Of Integer, Dictionary(Of String, Curve))

            For Each t As TabPage In TabControl1.TabPages

                Dim editor As ComprExprCurveSet = t.Controls.Item(0)
                Dim data = editor.curvedata
                Dim speed = editor.speed

                newcurves.Add(speed, data)

            Next

            If TypeOf simobj Is Compressor Then

                Dim uo = DirectCast(simobj, Compressor)
                uo.Curves = newcurves

            Else

                Dim uo = DirectCast(simobj, Expander)
                uo.Curves = newcurves

            End If

        Catch ex As Exception

            MessageBox.Show(ex.Message,
                            simobj.GetFlowsheet.GetTranslatedString("Erro"),
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        If TypeOf simobj Is Compressor Then

            Dim uo = DirectCast(simobj, Compressor)

            Dim curves = uo.CreateCurves

            Dim speed = uo.Curves.Keys.Max + 1

            Dim editor As New ComprExprCurveSet

            editor.curvedata = curves
            editor.speed = speed
            editor.Dock = DockStyle.Fill
            editor.Populate()

            Dim tab1 As New TabPage(speed & " rpm")
            tab1.Controls.Add(editor)

            AddHandler editor.tbRotation.TextChanged, Sub()
                                                          tab1.Text = editor.tbRotation.Text & " rpm"
                                                      End Sub

            TabControl1.TabPages.Add(tab1)

        Else

            Dim uo = DirectCast(simobj, Expander)

            Dim curves = uo.CreateCurves

            Dim speed = uo.Curves.Keys.Max + 1

            Dim editor As New ComprExprCurveSet
            editor.curvedata = curves
            editor.speed = speed
            editor.Populate()

            editor.Dock = DockStyle.Fill

            Dim tab1 As New TabPage(speed & " rpm")
            tab1.Controls.Add(editor)

            TabControl1.TabPages.Add(tab1)

        End If

    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click

        If TabControl1.TabPages.Count = 1 Then
            Exit Sub
        End If

        If MessageBox.Show(simobj.GetFlowsheet.GetTranslatedString("ConfirmOperation"),
                           simobj.GetFlowsheet.GetTranslatedString("Attention"),
                           MessageBoxButtons.YesNo,
                           MessageBoxIcon.Question) = DialogResult.Yes Then
            TabControl1.TabPages.Remove(TabControl1.SelectedTab)
        End If

    End Sub

    Private Sub tsbImport_Click(sender As Object, e As EventArgs) Handles tsbImport.Click

        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then

            Try
                Dim data = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Integer, Dictionary(Of String, Curve)))(File.ReadAllText(OpenFileDialog1.FileName))
                If TypeOf simobj Is Compressor Then
                    Dim uo = DirectCast(simobj, Compressor)
                    uo.Curves = data
                Else
                    Dim uo = DirectCast(simobj, Expander)
                    uo.Curves = data
                End If
                Populate()
            Catch ex As Exception
                MessageBox.Show(simobj.GetFlowsheet.GetTranslatedString("ErrorAddingComponent") & " " & ex.Message,
                                simobj.GetFlowsheet.GetTranslatedString("Erro"),
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try

        End If

    End Sub

    Private Sub tsbExport_Click(sender As Object, e As EventArgs) Handles tsbExport.Click

        If SaveFileDialog1.ShowDialog() = DialogResult.OK Then

            Try
                If TypeOf simobj Is Compressor Then
                    Dim uo = DirectCast(simobj, Compressor)
                    Dim data = Newtonsoft.Json.JsonConvert.SerializeObject(uo.Curves, Newtonsoft.Json.Formatting.Indented)
                    File.WriteAllText(SaveFileDialog1.FileName, data)
                Else
                    Dim uo = DirectCast(simobj, Expander)
                    Dim data = Newtonsoft.Json.JsonConvert.SerializeObject(uo.Curves, Newtonsoft.Json.Formatting.Indented)
                    File.WriteAllText(SaveFileDialog1.FileName, data)
                End If
            Catch ex As Exception
                MessageBox.Show(simobj.GetFlowsheet.GetTranslatedString("Erroaosalvararquivo") & " " & ex.Message,
                                simobj.GetFlowsheet.GetTranslatedString("Erro"),
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try

        End If

    End Sub

End Class