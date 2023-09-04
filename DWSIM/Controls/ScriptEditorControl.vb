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

    Public xmlref As XDocument

    Private loaded As Boolean = False

    Private Sub chkLink_CheckedChanged(sender As Object, e As EventArgs) Handles chkLink.CheckedChanged
        Label1.Enabled = chkLink.Checked
        Label2.Enabled = chkLink.Checked
        cbLinkedEvent.Enabled = chkLink.Checked
        cbLinkedObject.Enabled = chkLink.Checked
    End Sub

    Private Sub ScriptEditorControl_Load(sender As Object, e As EventArgs) Handles Me.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.txtScript.Tag = 1

        cbLinkedObject.Items.AddRange(New String() {"Simulation", "Solver", "Integrator"})
        cbLinkedEvent.Items.AddRange(New String() {"Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer"})

        For Each obj In form.SimulationObjects.Values
            cbLinkedObject.Items.Add(obj.GraphicObject.Tag)
        Next

        Dim webki As String

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.WebKI.xml")
            Using t As New IO.StreamReader(filestr)
                webki = t.ReadToEnd
            End Using
        End Using

        xmlref = XDocument.Parse(webki)

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

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As ComponentModel.CancelEventArgs) Handles ContextMenuStrip1.Opening

        Dim info As NodeControlInfo = tvVariables.GetNodeControlInfoAt(tvVariables.PointToClient(MousePosition))

        If Not info.Control Is Nothing Then

            If TypeOf info.Control Is NodeControls.NodeTextBox Then

                Dim ni As BaseNodeItem = info.Node.Tag

                If ni.ItemMemberInfo IsNot Nothing Then

                    Dim url As String = ""

                    Dim node As XElement

                    If ni.ItemOwnerObject IsNot Nothing Then

                        Dim tn As String = ""

                        Select Case ni.ItemMemberInfo.MemberType
                            Case MemberTypes.Constructor
                                tn = ni.ItemMemberInfo.DeclaringType.Name & "." & ni.ItemMemberInfo.Name & " constructor"
                            Case MemberTypes.Event
                                tn = ni.ItemMemberInfo.DeclaringType.Name & "." & ni.ItemMemberInfo.Name & " event"
                            Case MemberTypes.Field
                                tn = ni.ItemMemberInfo.DeclaringType.Name & "." & ni.ItemMemberInfo.Name & " field"
                            Case MemberTypes.Method
                                tn = ni.ItemMemberInfo.DeclaringType.Name & "." & ni.ItemMemberInfo.Name & " method"
                            Case MemberTypes.Property
                                tn = ni.ItemMemberInfo.DeclaringType.Name & "." & ni.ItemMemberInfo.Name & " property"
                        End Select

                        If TypeOf ni.ItemOwnerObject Is IDictionary Or TypeOf ni.ItemOwnerObject Is IList Then

                            tn = ni.ItemObject.GetType.FullName & " class"

                        End If

                        ItemToolStripMenuItem.Text = tn

                        node = xmlref.Element("HelpKI").Elements.Where(Function(n) n.Attribute("Title").Value.ToLower.StartsWith(tn.ToLower)).FirstOrDefault

                    Else

                        Dim tn = ni.ItemObject.GetType.FullName & " class"

                        ItemToolStripMenuItem.Text = tn

                        node = xmlref.Element("HelpKI").Elements.Where(Function(n) n.Attribute("Title").Value.ToLower.StartsWith(tn.ToLower)).FirstOrDefault

                    End If

                    If node IsNot Nothing Then

                        If node.HasElements Then

                            url = "https://dwsim.org/api_help/" & node.Elements.First.Attribute("Url").Value

                        Else

                            url = "https://dwsim.org/api_help/" & node.Attribute("Url").Value

                        End If

                    End If

                    linkAPI = url
                    ViewAPIHelpTSMI.Enabled = True

                    If url <> "" Then

                        Task.Factory.StartNew(Sub()

                                                  Dim htmlpage = GetHTML(url)

                                                  If Not htmlpage Is Nothing Then

                                                      Dim spage = htmlpage.DocumentNode.Descendants("a").Where(Function(x) x.InnerText = "View Source").FirstOrDefault

                                                      If spage IsNot Nothing Then

                                                          Dim slink = spage.Attributes("href").Value.Replace("+", "%20")

                                                          form.RunCodeOnUIThread(Sub()

                                                                                     linkSC = slink
                                                                                     ViewSourceCodeTSMI.Enabled = True

                                                                                 End Sub)

                                                      End If

                                                  End If

                                              End Sub)

                    Else

                        linkSC = ""
                        ViewSourceCodeTSMI.Enabled = False

                    End If

                Else

                    ItemToolStripMenuItem.Text = ""

                    linkAPI = ""
                    linkSC = ""
                    ViewAPIHelpTSMI.Enabled = False
                    ViewSourceCodeTSMI.Enabled = False

                    e.Cancel = True

                End If


            Else

                ItemToolStripMenuItem.Text = ""

                linkAPI = ""
                linkSC = ""
                ViewAPIHelpTSMI.Enabled = False
                ViewSourceCodeTSMI.Enabled = False

                e.Cancel = True

            End If


        Else

            ItemToolStripMenuItem.Text = ""

            linkAPI = ""
            linkSC = ""
            ViewAPIHelpTSMI.Enabled = False
            ViewSourceCodeTSMI.Enabled = False

            e.Cancel = True

        End If

    End Sub

    Private Sub ViewAPIHelpTSMI_Click(sender As Object, e As EventArgs) Handles ViewAPIHelpTSMI.Click

        If linkAPI <> "" Then Process.Start(linkAPI)

    End Sub

    Private Sub ViewSourceCodeTSMI_Click(sender As Object, e As EventArgs) Handles ViewSourceCodeTSMI.Click

        If linkSC <> "" Then Process.Start(linkSC)

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
