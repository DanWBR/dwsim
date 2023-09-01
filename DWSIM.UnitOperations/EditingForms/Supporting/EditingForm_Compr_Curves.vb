Imports System.Drawing
Imports System.IO
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.PumpOps

Public Class EditingForm_CompressorExpander_Curves

    Public simobj As ISimulationObject

    Public loaded As Boolean = False

    Private Sub EditingForm_CompressorExpander_Curves_Load(sender As Object, e As EventArgs) Handles MyBase.Shown

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()

        End Using

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

            Dim curves = uo.CreateCurves()

            uo.Curves.Add(uo.Speed, curves)

            Dim speed = uo.Speed

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

            uo.Curves.Add(uo.Speed, curves)

            Dim speed = uo.Speed

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

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Dim text = handler.ReadAllText()
            Try
                Dim data = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Integer, Dictionary(Of String, Curve)))(text)
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

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Try
                If TypeOf simobj Is Compressor Then
                    Dim uo = DirectCast(simobj, Compressor)
                    Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(simobj.Curves, Newtonsoft.Json.Formatting.Indented)
                    Using stream As New IO.MemoryStream()
                        Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                            writer.Write(jsondata)
                            handler.Write(stream)
                        End Using
                    End Using
                Else
                    Dim uo = DirectCast(simobj, Expander)
                    Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(simobj.Curves, Newtonsoft.Json.Formatting.Indented)
                    Using stream As New IO.MemoryStream()
                        Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                            writer.Write(jsondata)
                            handler.Write(stream)
                        End Using
                    End Using
                End If
            Catch ex As Exception
                MessageBox.Show(simobj.GetFlowsheet.GetTranslatedString("Erroaosalvararquivo") & " " & ex.Message,
                                simobj.GetFlowsheet.GetTranslatedString("Erro"),
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub EditingForm_CompressorExpander_Curves_Load_1(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

    End Sub
End Class