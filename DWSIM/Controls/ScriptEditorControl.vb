Imports System.IO
Imports System.Text
Imports Microsoft.Scripting.Hosting
Imports System.Drawing.Text
Imports System.Reflection
Imports System.ComponentModel
Imports Aga.Controls.Tree
Imports System.Linq
Imports System.Threading.Tasks
Imports System.Net.Http
Imports System.Net

Public Class ScriptEditorControl

    Public form As FormFlowsheet

    Private linkAPI As String = ""
    Private linkSC As String = ""

    Private loaded As Boolean = False

    Private Sub chkLink_CheckedChanged(sender As Object, e As EventArgs) Handles chkLink.CheckedChanged
        Label1.Enabled = chkLink.Checked
        Label2.Enabled = chkLink.Checked
        cbLinkedEvent.Enabled = chkLink.Checked
        cbLinkedObject.Enabled = chkLink.Checked
    End Sub

    Private Sub ScriptEditorControl_Load(sender As Object, e As EventArgs) Handles Me.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        tvVariables.Columns(0).Width = 100 * Settings.DpiScale
        tvVariables.Columns(1).Width = 150 * Settings.DpiScale
        tvVariables.Columns(2).Width = 200 * Settings.DpiScale
        tvVariables.RowHeight = 20 * Settings.DpiScale

        Me.txtScript.Tag = 1

        cbLinkedObject.Items.AddRange(New String() {"Simulation", "Solver", "Integrator"})
        cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})

        For Each obj In form.SimulationObjects.Values
            cbLinkedObject.Items.Add(obj.GraphicObject.Tag)
        Next

        loaded = True

    End Sub

    Private Sub cbLinkedObject_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbLinkedObject.SelectedIndexChanged

        Select Case cbLinkedObject.SelectedIndex
            Case 0
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})
            Case 1
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Solver Started", "Solver Finished", "Recycle Loop"})
            Case 2
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Integrator Started", "Integrator Finished", "Integrator Error", "Integrator Post-Step", "Integrator Pre-Step"})
            Case Else
                cbLinkedEvent.Items.Clear()
                cbLinkedEvent.Items.AddRange(New String() {"Object Calculation Started", "Object Calculation Finished", "Object Calculation Error"})
        End Select

        cbLinkedEvent.SelectedIndex = 0

    End Sub

    Private Sub txtScript_TextChanged(sender As Object, e As EventArgs) Handles txtScript.TextChanged

        txtScript.SetColumnMargins()

        ShowAutoComplete(txtScript)

        ShowToolTip(txtScript)

    End Sub

    Private Function GetHTML(website As String) As HtmlAgilityPack.HtmlDocument

        Try

            Dim siteUri As Uri = New Uri(website)
            Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

            Dim handler As New HttpClientHandler()

            If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
                Dim proxyObj As New WebProxy(proxyUri)
                proxyObj.Credentials = CredentialCache.DefaultCredentials
                handler.Proxy = proxyObj
            End If

            Dim http As New HttpClient(handler)

            Dim response = http.GetByteArrayAsync(website)
            response.Wait()

            Dim source As [String] = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage As New HtmlAgilityPack.HtmlDocument()

            htmlpage.LoadHtml(source)

            Return htmlpage

        Catch ex As Exception

            Return Nothing

        End Try

    End Function

End Class
