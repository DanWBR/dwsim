'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

'Imports DWSIM.SimulationObjects
Imports System.ComponentModel
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Linq
Imports WeifenLuo.WinFormsUI.Docking
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Globalization
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports System.Threading.Tasks
Imports System.Reflection
Imports Microsoft.Win32
Imports System.Text
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.SharedClasses.Extras
Imports System.Dynamic
Imports DWSIM.SharedClasses.Flowsheet.Optimization
Imports ICSharpCode.SharpZipLib.Zip
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts

Imports CefSharp.WinForms
Imports DWSIM.Interfaces

Public Class FormMain

    Inherits Form

    Public Shared m_childcount As Integer = 1
    Public filename As String
    Public sairdevez As Boolean = False
    Public loadedCSDB As Boolean = False
    Public pathsep As Char

    Public FrmOptions As FormOptions
    Public FrmWelcome As FormWelcome
    Public FrmRec As FormRecoverFiles

    Private dropdownlist As ArrayList

    Private dlok As Boolean = False
    Public CancelClosing As Boolean = False

    Private tmpform2 As FormFlowsheet

    Public AvailableComponents As New Dictionary(Of String, Interfaces.ICompoundConstantProperties)
    Public AvailableUnitSystems As New Dictionary(Of String, SystemsOfUnits.Units)
    Public PropertyPackages As New Dictionary(Of String, PropertyPackages.PropertyPackage)
    Public FlashAlgorithms As New Dictionary(Of String, Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)
    Public Property ExternalUnitOperations As New Dictionary(Of String, Interfaces.IExternalUnitOperation)

    Public COMonitoringObjects As New Dictionary(Of String, UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)
    Public WithEvents timer1 As New Timer

    Public calculatorassembly, unitopassembly As Assembly
    Public aTypeList As New List(Of Type)

    Public SampleList As New List(Of String)
    Public FOSSEEList As New List(Of FOSSEEFlowsheet)

#Region "    Form Events"

    Public Sub InitializeChromium()
        If My.Settings.ShowWebPanel And Not DWSIM.App.IsRunningOnMono Then
            Try
                Dim settings As CefSettings = New CefSettings
                settings.IgnoreCertificateErrors = True
                settings.PersistUserPreferences = True
                settings.PersistSessionCookies = True
                settings.CachePath = Path.Combine(My.Computer.FileSystem.SpecialDirectories.CurrentUserApplicationData, "BrowserDataCache")
                CefSharp.CefSharpSettings.SubprocessExitIfParentProcessClosed = True
                CefSharp.Cef.Initialize(settings)
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub FormMain_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        If e.Data.GetDataPresent(DataFormats.FileDrop) Then
            Dim MyFiles() As String
            Dim i As Integer
            ' Assign the files to an array.
            MyFiles = e.Data.GetData(DataFormats.FileDrop)
            ' Loop through the array and add the files to the list.
            For i = 0 To MyFiles.Length - 1
                Select Case Path.GetExtension(MyFiles(i)).ToLower
                    Case ".dwxml"
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + MyFiles(i) + "..."
                        Application.DoEvents()
                        Me.LoadXML(MyFiles(i), Nothing)
                    Case ".dwxmz"
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + MyFiles(i) + "..."
                        Application.DoEvents()
                        Me.LoadAndExtractXMLZIP(MyFiles(i), Nothing)
                    Case ".dwsim"
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + MyFiles(i) + "..."
                        'Application.DoEvents()
                        'Me.LoadF(MyFiles(i))
                    Case ".dwcsd"
                        Dim NewMDIChild As New FormCompoundCreator()
                        NewMDIChild.MdiParent = Me
                        NewMDIChild.Show()
                        Dim objStreamReader As New FileStream(MyFiles(i), FileMode.Open)
                        Dim x As New BinaryFormatter()
                        NewMDIChild.mycase = x.Deserialize(objStreamReader)
                        objStreamReader.Close()
                        NewMDIChild.WriteData()
                        If Not My.Settings.MostRecentFiles.Contains(MyFiles(i)) Then
                            My.Settings.MostRecentFiles.Add(MyFiles(i))
                            Me.UpdateMRUList()
                        End If
                        NewMDIChild.Activate()
                    Case ".dwrsd"
                        Dim NewMDIChild As New FormDataRegression()
                        NewMDIChild.MdiParent = Me
                        NewMDIChild.Show()
                        Dim objStreamReader As New FileStream(MyFiles(i), FileMode.Open)
                        Dim x As New BinaryFormatter()
                        NewMDIChild.currcase = x.Deserialize(objStreamReader)
                        objStreamReader.Close()
                        NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                        If Not My.Settings.MostRecentFiles.Contains(MyFiles(i)) Then
                            My.Settings.MostRecentFiles.Add(MyFiles(i))
                            Me.UpdateMRUList()
                        End If
                        NewMDIChild.Activate()
                End Select
            Next
        End If
    End Sub

    Private Sub FormMain_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragEnter
        If e.Data.GetDataPresent(DataFormats.FileDrop) Then
            e.Effect = DragDropEffects.All
        End If
    End Sub

    Private Sub FormParent_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        If Me.MdiChildren.Length > 0 And Not Me.CancelClosing Then
            Me.CancelClosing = False
            Dim ms As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Existemsimulaesabert"), DWSIM.App.GetLocalString("Ateno"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            If ms = MsgBoxResult.No Then e.Cancel = True
        End If

        If Not e.Cancel Then

            'Check if DWSIM is running in Mono mode, then save settings to file.
            If DWSIM.App.IsRunningOnMono And GlobalSettings.Settings.OldUI Then
                Try
                    DWSIM.App.SaveSettings()
                Catch ex As UnauthorizedAccessException
                    MessageBox.Show(DWSIM.App.GetLocalString("UnauthorizedAccessError"), DWSIM.App.GetLocalString("Erroaosalvararquivo") & " dwsim.ini", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Else
                My.Application.SaveMySettingsOnExit = True
                My.Settings.Save()
            End If

            'release yeppp! resources
            Try
                If My.Settings.UseSIMDExtensions Then Yeppp.Library.Release()
            Catch ex As Exception
            End Try

            Try
                If Not DWSIM.App.IsRunningOnMono Then CefSharp.Cef.Shutdown()
            Catch ex As Exception
            End Try

        End If

    End Sub

    Private Sub MyApplication_UnhandledException(ByVal sender As Object, ByVal e As System.Threading.ThreadExceptionEventArgs)
        Try
            Dim frmEx As New FormUnhandledException
            frmEx.TextBox1.Text = e.Exception.ToString
            frmEx.ex = e.Exception
            frmEx.ShowDialog()
        Finally

        End Try
    End Sub

    Private Sub MyApplication_UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)
        Try
            Dim frmEx As New FormUnhandledException
            frmEx.TextBox1.Text = e.ExceptionObject.ToString
            frmEx.ex = e.ExceptionObject
            frmEx.ShowDialog()
        Catch ex As Exception

        End Try
    End Sub

    Public Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If GlobalSettings.Settings.OldUI Then

            calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

            aTypeList.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))
            aTypeList.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))

            My.Application.MainThreadId = Threading.Thread.CurrentThread.ManagedThreadId

            If My.Settings.BackupFolder = "" Then My.Settings.BackupFolder = My.Computer.FileSystem.SpecialDirectories.Temp & Path.DirectorySeparatorChar & "DWSIM"

            If My.Settings.BackupActivated Then
                Me.TimerBackup.Interval = My.Settings.BackupInterval * 60000
                Me.TimerBackup.Enabled = True
            End If

            Me.dropdownlist = New ArrayList
            Me.UpdateMRUList()

            'load plugins from 'Plugins' folder

            Dim pluginlist As List(Of Interfaces.IUtilityPlugin) = GetPlugins(LoadPluginAssemblies())

            For Each ip As Interfaces.IUtilityPlugin In pluginlist
                My.Application.UtilityPlugins.Add(ip.UniqueID, ip)
            Next

#If Not WINE32 Then
            'load external property packages from 'propertypackages' folder, if there is any
            Dim epplist As List(Of PropertyPackage) = GetExternalPPs(LoadExternalPPs())

            For Each pp As PropertyPackage In epplist
                PropertyPackages.Add(pp.ComponentName, pp)
            Next
#End If

            'Search and populate CAPE-OPEN Flowsheet Monitoring Object collection
            'SearchCOMOs() 'doing this only when the user hovers the mouse over the plugins toolstrip menu item

            If My.Settings.ScriptPaths Is Nothing Then My.Settings.ScriptPaths = New Collections.Specialized.StringCollection()

            Me.FrmOptions = New FormOptions
            Me.FrmOptions.Dock = DockStyle.Fill
            Me.SettingsPanel.Controls.Add(Me.FrmOptions)
            Me.ButtonClose.BringToFront()

            tsbInspector.Checked = GlobalSettings.Settings.InspectorEnabled

            Me.FrmWelcome = New FormWelcome
            Me.FrmWelcome.Owner = Me
            Me.FrmWelcome.Dock = DockStyle.Fill
            Me.WelcomePanel.Controls.Add(Me.FrmWelcome)
            Me.ButtonClose2.BringToFront()

            If My.Settings.ShowWebPanel Then

                If DWSIM.App.IsRunningOnMono Then

                    WebPanel.Visible = False

                    'Try
                    '    Dim c1, c2, c3, c4, c5 As WebBrowser
                    '    Dim a1, a2, a3, a4, a5 As String

                    '    a1 = "https://www.patreon.com/dwsim"
                    '    a2 = "https://sourceforge.net/p/dwsim/discussion/"
                    '    a3 = "https://dwsim.fossee.in/forum"
                    '    a4 = "https://www.youtube.com/channel/UCzzBQrycKoN5XbCeLV12y3Q/videos?view=0&sort=dd&flow=grid"
                    '    a5 = "https://pernaletec.shinyapps.io/dwsim/"

                    '    c1 = New WebBrowser() With {.Url = New Uri(a1), .Dock = DockStyle.Fill}
                    '    c2 = New WebBrowser() With {.Url = New Uri(a2), .Dock = DockStyle.Fill}
                    '    c3 = New WebBrowser() With {.Url = New Uri(a3), .Dock = DockStyle.Fill}
                    '    c4 = New WebBrowser() With {.Url = New Uri(a4), .Dock = DockStyle.Fill}
                    '    c5 = New WebBrowser() With {.Url = New Uri(a5), .Dock = DockStyle.Fill}

                    '    frmweb.TabPageA.Controls.Add(c1)
                    '    frmweb.TabPageB.Controls.Add(c2)
                    '    frmweb.TabPageC.Controls.Add(c3)
                    '    frmweb.TabPageD.Controls.Add(c4)
                    '    frmweb.TabPageE.Controls.Add(c5)

                    'Catch ex As Exception

                    'End Try

                Else

                    Dim frmweb As New FormWebPanel
                    frmweb.Dock = DockStyle.Fill

                    Try

                        Dim c1, c2, c3, c4, c5 As ChromiumWebBrowser
                        Dim a1, a2, a3, a4, a5 As String

                        a1 = "https://www.patreon.com/dwsim"
                        a2 = "https://sourceforge.net/p/dwsim/discussion/"
                        a3 = "https://dwsim.fossee.in/forum"
                        a4 = "https://www.youtube.com/channel/UCzzBQrycKoN5XbCeLV12y3Q/videos?view=0&sort=dd&flow=grid"
                        a5 = "https://pernaletec.shinyapps.io/dwsim/"

                        c1 = New ChromiumWebBrowser(a1) With {.Dock = DockStyle.Fill}
                        c2 = New ChromiumWebBrowser(a2) With {.Dock = DockStyle.Fill}
                        c3 = New ChromiumWebBrowser(a3) With {.Dock = DockStyle.Fill}
                        c4 = New ChromiumWebBrowser(a4) With {.Dock = DockStyle.Fill}
                        c5 = New ChromiumWebBrowser(a5) With {.Dock = DockStyle.Fill}

                        frmweb.TabPageA.Controls.Add(c1)
                        frmweb.TabPageB.Controls.Add(c2)
                        frmweb.TabPageC.Controls.Add(c3)
                        frmweb.TabPageD.Controls.Add(c4)
                        frmweb.TabPageE.Controls.Add(c5)

                        AddHandler WebPanel.VisibleChanged, Sub(sender2, e2)

                                                                If Not WebPanel.Visible Then

                                                                    Try
                                                                        c1.GetBrowser.StopLoad()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c2.GetBrowser.StopLoad()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c3.GetBrowser.StopLoad()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c4.GetBrowser.StopLoad()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c5.GetBrowser.StopLoad()
                                                                    Catch ex As Exception
                                                                    End Try

                                                                Else

                                                                    Try
                                                                        c1.GetBrowser.Reload()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c2.GetBrowser.Reload()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c3.GetBrowser.Reload()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c4.GetBrowser.Reload()
                                                                    Catch ex As Exception
                                                                    End Try
                                                                    Try
                                                                        c5.GetBrowser.Reload()
                                                                    Catch ex As Exception
                                                                    End Try

                                                                End If

                                                            End Sub

                    Catch ex As Exception

                    End Try

                    WebPanel.Controls.Add(frmweb)
                    Me.ButtonCloseWeb.BringToFront()

                End If

            Else

                WebPanel.Visible = False

            End If


        End If

        Me.Text = DWSIM.App.GetLocalString("FormParent_FormText")

        GlobalSettings.Settings.DpiScale = Me.CreateGraphics.DpiX / 96.0

    End Sub

    Sub SearchCOMOs()

        Dim keys As String() = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).GetSubKeyNames()

        For Each k2 In keys
            Dim mykey As RegistryKey = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).OpenSubKey(k2, False)
            Dim mykeys As String() = mykey.GetSubKeyNames()
            For Each s As String In mykeys
                If s = "Implemented Categories" Then
                    Dim arr As Array = mykey.OpenSubKey("Implemented Categories").GetSubKeyNames()
                    For Each s2 As String In arr
                        If s2.ToLower = "{7ba1af89-b2e4-493d-bd80-2970bf4cbe99}" Then
                            'this is a CAPE-OPEN MO
                            Dim myuo As New UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo
                            With myuo
                                .AboutInfo = mykey.OpenSubKey("CapeDescription").GetValue("About")
                                .CapeVersion = mykey.OpenSubKey("CapeDescription").GetValue("CapeVersion")
                                .Description = mykey.OpenSubKey("CapeDescription").GetValue("Description")
                                .HelpURL = mykey.OpenSubKey("CapeDescription").GetValue("HelpURL")
                                .Name = mykey.OpenSubKey("CapeDescription").GetValue("Name")
                                .VendorURL = mykey.OpenSubKey("CapeDescription").GetValue("VendorURL")
                                .Version = mykey.OpenSubKey("CapeDescription").GetValue("ComponentVersion")
                                Try
                                    .TypeName = mykey.OpenSubKey("ProgID").GetValue("")
                                Catch ex As Exception
                                End Try
                                Try
                                    .Location = mykey.OpenSubKey("InProcServer32").GetValue("")
                                Catch ex As Exception
                                    .Location = mykey.OpenSubKey("LocalServer32").GetValue("")
                                End Try
                            End With
                            Me.COMonitoringObjects.Add(myuo.TypeName, myuo)
                        End If
                    Next
                End If
            Next
            mykey.Close()
        Next

    End Sub

    Private Function LoadPluginAssemblies() As List(Of Assembly)

        Dim pluginassemblylist As List(Of Assembly) = New List(Of Assembly)

        If Directory.Exists(Path.Combine(My.Application.Info.DirectoryPath, "plugins")) Then

            Dim dinfo As New DirectoryInfo(Path.Combine(My.Application.Info.DirectoryPath, "plugins"))

            Dim files() As FileInfo = dinfo.GetFiles("*.*", SearchOption.TopDirectoryOnly)

            If Not files Is Nothing Then
                For Each fi As FileInfo In files
                    If fi.Extension.ToLower = ".exe" Or fi.Extension.ToLower = ".dll" Then
                        Try
                            pluginassemblylist.Add(Assembly.LoadFile(fi.FullName))
                        Catch ex As Exception

                        End Try
                    End If
                Next
            End If

        End If

        Return pluginassemblylist

    End Function

    Function GetPlugins(ByVal alist As List(Of Assembly)) As List(Of Interfaces.IUtilityPlugin)

        Dim availableTypes As New List(Of Type)()

        For Each currentAssembly As Assembly In alist
            Try
                availableTypes.AddRange(currentAssembly.GetTypes())
            Catch ex As ReflectionTypeLoadException
                Dim errstr As New StringBuilder()
                For Each lex As Exception In ex.LoaderExceptions
                    errstr.AppendLine(lex.ToString)
                Next
                Console.WriteLine("Error loading plugin '" & currentAssembly.FullName & "': " & errstr.ToString)
            End Try
        Next

        Dim pluginlist As List(Of Type) = availableTypes.FindAll(AddressOf isPlugin)

        Return pluginlist.ConvertAll(Of Interfaces.IUtilityPlugin)(Function(t As Type) TryCast(Activator.CreateInstance(t), Interfaces.IUtilityPlugin))

    End Function

    Function isPlugin(ByVal t As Type)
        Dim interfaceTypes As New List(Of Type)(t.GetInterfaces())
        Return (interfaceTypes.Contains(GetType(Interfaces.IUtilityPlugin)))
    End Function

    Private Function LoadExternalPPs() As List(Of Assembly)

        Dim pluginassemblylist As List(Of Assembly) = New List(Of Assembly)

        If Directory.Exists(Path.Combine(Environment.CurrentDirectory, "propertypackages")) Then

            Dim dinfo As New DirectoryInfo(Path.Combine(Environment.CurrentDirectory, "propertypackages"))

            Dim files() As FileInfo = dinfo.GetFiles("*.dll")

            If Not files Is Nothing Then
                For Each fi As FileInfo In files
                    pluginassemblylist.Add(Assembly.LoadFrom(fi.FullName))
                Next
            End If

        End If

        Return pluginassemblylist

    End Function

    Function GetExternalPPs(ByVal alist As List(Of Assembly)) As List(Of PropertyPackage)

        Dim availableTypes As New List(Of Type)()

        For Each currentAssembly As Assembly In alist
            Try
                availableTypes.AddRange(currentAssembly.GetTypes())
            Catch ex As Exception
                MessageBox.Show(ex.Message.ToCharArray, "Error loading plugin", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        Next

        Dim ppList As List(Of Type) = availableTypes.FindAll(AddressOf isPP)

        Return ppList.ConvertAll(Of PropertyPackage)(Function(t As Type) TryCast(Activator.CreateInstance(t), PropertyPackage))

    End Function


    Function isPP(ByVal t As Type)
        Return (t Is GetType(PropertyPackage))
    End Function

    Private Sub UpdateMRUList()

        'process MRU file list

        If My.Settings.MostRecentFiles.Count > 10 Then
            My.Settings.MostRecentFiles.RemoveAt(0)
        End If

        Dim j As Integer = 0
        For Each k As String In Me.dropdownlist
            Dim tsmi As ToolStripItem = Me.FileToolStripMenuItem.DropDownItems(Convert.ToInt32(k - j))
            If tsmi.DisplayStyle = ToolStripItemDisplayStyle.Text Or TypeOf tsmi Is ToolStripSeparator Then
                Me.FileToolStripMenuItem.DropDownItems.Remove(tsmi)
                j = j + 1
            End If
        Next

        Me.dropdownlist.Clear()

        Dim toremove As New ArrayList

        If Not My.Settings.MostRecentFiles Is Nothing Then
            For Each str As String In My.Settings.MostRecentFiles
                If File.Exists(str) Then
                    Dim tsmi As New ToolStripMenuItem
                    With tsmi
                        .Text = str
                        .Tag = str
                        .DisplayStyle = ToolStripItemDisplayStyle.Text
                    End With
                    Me.FileToolStripMenuItem.DropDownItems.Insert(Me.FileToolStripMenuItem.DropDownItems.Count - 1, tsmi)
                    Me.dropdownlist.Add(Me.FileToolStripMenuItem.DropDownItems.Count - 2)
                    AddHandler tsmi.Click, AddressOf Me.OpenRecent_click
                Else
                    toremove.Add(str)
                End If
            Next
            For Each s As String In toremove
                My.Settings.MostRecentFiles.Remove(s)
            Next
            If My.Settings.MostRecentFiles.Count > 0 Then
                Me.FileToolStripMenuItem.DropDownItems.Insert(Me.FileToolStripMenuItem.DropDownItems.Count - 1, New ToolStripSeparator())
                Me.dropdownlist.Add(Me.FileToolStripMenuItem.DropDownItems.Count - 2)
            End If
        Else
            My.Settings.MostRecentFiles = New System.Collections.Specialized.StringCollection
        End If

        Dim latestfolders As New List(Of String)

        For Each f As String In My.Settings.MostRecentFiles
            If File.Exists(f) And Path.GetExtension(f).ToLower <> ".dwbcs" Then
                If Not latestfolders.Contains(Path.GetDirectoryName(f)) Then latestfolders.Add(Path.GetDirectoryName(f))
            End If
        Next

        For Each s In latestfolders
            Dim tsmi As New ToolStripMenuItem With {.Text = s, .Tag = s, .DisplayStyle = ToolStripItemDisplayStyle.Text}
            Me.FileToolStripMenuItem.DropDownItems.Insert(Me.FileToolStripMenuItem.DropDownItems.Count - 1, tsmi)
            Me.dropdownlist.Add(Me.FileToolStripMenuItem.DropDownItems.Count - 2)
            AddHandler tsmi.Click, AddressOf Me.OpenRecentFolder_click
        Next

        If latestfolders.Count > 0 Then
            Me.FileToolStripMenuItem.DropDownItems.Insert(Me.FileToolStripMenuItem.DropDownItems.Count - 1, New ToolStripSeparator())
        End If

    End Sub

    Sub AddExternalUOs()

        Dim otheruos = SharedClasses.Utility.LoadAdditionalUnitOperations()

        For Each uo In otheruos
            ExternalUnitOperations.Add(uo.Description, uo)
        Next

    End Sub

    Sub AddPropPacks()

        Dim CPPP As CoolPropPropertyPackage = New CoolPropPropertyPackage()
        CPPP.ComponentName = "CoolProp"
        CPPP.ComponentDescription = DWSIM.App.GetLocalString("DescCPPP")
        PropertyPackages.Add(CPPP.ComponentName.ToString, CPPP)

        Dim CPIPP As New CoolPropIncompressiblePurePropertyPackage()
        CPIPP.ComponentName = "CoolProp (Incompressible Fluids)"
        CPIPP.ComponentDescription = "CoolProp (Incompressible Fluids)"
        PropertyPackages.Add(CPIPP.ComponentName.ToString, CPIPP)

        Dim CPIMPP As New CoolPropIncompressibleMixturePropertyPackage()
        CPIMPP.ComponentName = "CoolProp (Incompressible Mixtures)"
        CPIMPP.ComponentDescription = "CoolProp (Incompressible Mixtures)"
        PropertyPackages.Add(CPIMPP.ComponentName.ToString, CPIMPP)

        Dim SWPP As New SourWaterPropertyPackage()
        SWPP.ComponentName = DWSIM.App.GetLocalString("SourWaterPP")
        SWPP.ComponentDescription = DWSIM.App.GetLocalString("DescSourWaterPP")
        PropertyPackages.Add(SWPP.ComponentName.ToString, SWPP)

        Dim STPP As SteamTablesPropertyPackage = New SteamTablesPropertyPackage()
        STPP.ComponentName = DWSIM.App.GetLocalString("TabelasdeVaporSteamT")
        STPP.ComponentDescription = DWSIM.App.GetLocalString("DescSteamTablesPP")
        PropertyPackages.Add(STPP.ComponentName.ToString, STPP)

        Dim SEAPP As SeawaterPropertyPackage = New SeawaterPropertyPackage()
        SEAPP.ComponentName = DWSIM.App.GetLocalString("SEAPP")
        SEAPP.ComponentDescription = DWSIM.App.GetLocalString("DescSEAPP")
        PropertyPackages.Add(SEAPP.ComponentName.ToString, SEAPP)

        Dim PRPP As PengRobinsonPropertyPackage = New PengRobinsonPropertyPackage()
        PRPP.ComponentName = "Peng-Robinson (PR)"
        PRPP.ComponentDescription = DWSIM.App.GetLocalString("DescPengRobinsonPP")
        PropertyPackages.Add(PRPP.ComponentName.ToString, PRPP)

        Dim PRSV2PP As PRSV2PropertyPackage = New PRSV2PropertyPackage()
        PRSV2PP.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
        PRSV2PP.ComponentDescription = DWSIM.App.GetLocalString("DescPRSV2PP")
        PropertyPackages.Add(PRSV2PP.ComponentName.ToString, PRSV2PP)

        Dim PRSV2PPVL As PRSV2VLPropertyPackage = New PRSV2VLPropertyPackage()
        PRSV2PPVL.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
        PRSV2PPVL.ComponentDescription = DWSIM.App.GetLocalString("DescPRSV2VLPP")
        PropertyPackages.Add(PRSV2PPVL.ComponentName.ToString, PRSV2PPVL)

        Dim SRKPP As SRKPropertyPackage = New SRKPropertyPackage()
        SRKPP.ComponentName = "Soave-Redlich-Kwong (SRK)"
        SRKPP.ComponentDescription = DWSIM.App.GetLocalString("DescSoaveRedlichKwongSRK")
        PropertyPackages.Add(SRKPP.ComponentName.ToString, SRKPP)

        Dim PRLKPP As PengRobinsonLKPropertyPackage = New PengRobinsonLKPropertyPackage()
        PRLKPP.ComponentName = "Peng-Robinson / Lee-Kesler (PR/LK)"
        PRLKPP.ComponentDescription = DWSIM.App.GetLocalString("DescPRLK")

        PropertyPackages.Add(PRLKPP.ComponentName.ToString, PRLKPP)

        Dim UPP As UNIFACPropertyPackage = New UNIFACPropertyPackage()
        UPP.ComponentName = "UNIFAC"
        UPP.ComponentDescription = DWSIM.App.GetLocalString("DescUPP")

        PropertyPackages.Add(UPP.ComponentName.ToString, UPP)

        Dim ULLPP As UNIFACLLPropertyPackage = New UNIFACLLPropertyPackage()
        ULLPP.ComponentName = "UNIFAC-LL"
        ULLPP.ComponentDescription = DWSIM.App.GetLocalString("DescUPP")

        PropertyPackages.Add(ULLPP.ComponentName.ToString, ULLPP)

        Dim MUPP As MODFACPropertyPackage = New MODFACPropertyPackage()
        MUPP.ComponentName = "Modified UNIFAC (Dortmund)"
        MUPP.ComponentDescription = DWSIM.App.GetLocalString("DescMUPP")

        PropertyPackages.Add(MUPP.ComponentName.ToString, MUPP)

        Dim NUPP As NISTMFACPropertyPackage = New NISTMFACPropertyPackage()
        NUPP.ComponentName = "Modified UNIFAC (NIST)"
        NUPP.ComponentDescription = DWSIM.App.GetLocalString("DescNUPP")

        PropertyPackages.Add(NUPP.ComponentName.ToString, NUPP)

        Dim NRTLPP As NRTLPropertyPackage = New NRTLPropertyPackage()
        NRTLPP.ComponentName = "NRTL"
        NRTLPP.ComponentDescription = DWSIM.App.GetLocalString("DescNRTLPP")

        PropertyPackages.Add(NRTLPP.ComponentName.ToString, NRTLPP)

        Dim UQPP As UNIQUACPropertyPackage = New UNIQUACPropertyPackage()
        UQPP.ComponentName = "UNIQUAC"
        UQPP.ComponentDescription = DWSIM.App.GetLocalString("DescUNIQUACPP")

        PropertyPackages.Add(UQPP.ComponentName.ToString, UQPP)

        Dim CSLKPP As ChaoSeaderPropertyPackage = New ChaoSeaderPropertyPackage()
        CSLKPP.ComponentName = "Chao-Seader"
        CSLKPP.ComponentDescription = DWSIM.App.GetLocalString("DescCSLKPP")

        PropertyPackages.Add(CSLKPP.ComponentName.ToString, CSLKPP)

        Dim GSLKPP As GraysonStreedPropertyPackage = New GraysonStreedPropertyPackage()
        GSLKPP.ComponentName = "Grayson-Streed"
        GSLKPP.ComponentDescription = DWSIM.App.GetLocalString("DescGSLKPP")

        PropertyPackages.Add(GSLKPP.ComponentName.ToString, GSLKPP)

        Dim RPP As RaoultPropertyPackage = New RaoultPropertyPackage()
        RPP.ComponentName = DWSIM.App.GetLocalString("LeideRaoultGsSoluoId")
        RPP.ComponentDescription = DWSIM.App.GetLocalString("DescRPP")

        PropertyPackages.Add(RPP.ComponentName.ToString, RPP)

        Dim LKPPP As LKPPropertyPackage = New LKPPropertyPackage()
        LKPPP.ComponentName = "Lee-Kesler-Plöcker"
        LKPPP.ComponentDescription = DWSIM.App.GetLocalString("DescLKPPP")

        PropertyPackages.Add(LKPPP.ComponentName.ToString, LKPPP)

        'Dim EUQPP As ExUNIQUACPropertyPackage = New ExUNIQUACPropertyPackage()
        'EUQPP.ComponentName = "Extended UNIQUAC (Aqueous Electrolytes)"
        'EUQPP.ComponentDescription = DWSIM.App.GetLocalString("DescEUPP")

        'PropertyPackages.Add(EUQPP.ComponentName.ToString, EUQPP)

        'Dim ENQPP As New ElectrolyteNRTLPropertyPackage()
        'ENQPP.ComponentName = "Electrolyte NRTL (Aqueous Electrolytes)"
        'ENQPP.ComponentDescription = DWSIM.App.GetLocalString("DescENPP")

        'PropertyPackages.Add(ENQPP.ComponentName.ToString, ENQPP)

        Dim BOPP As BlackOilPropertyPackage = New BlackOilPropertyPackage()
        BOPP.ComponentName = "Black Oil"
        BOPP.ComponentDescription = DWSIM.App.GetLocalString("DescBOPP")

        PropertyPackages.Add(BOPP.ComponentName.ToString, BOPP)

        Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()

        For Each pp In otherpps
            PropertyPackages.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName, pp)
        Next

        'Check if DWSIM is running in Portable/Mono mode, if not then load the CAPE-OPEN Wrapper Property Package.
        If Not DWSIM.App.IsRunningOnMono Then

            Dim COPP As CAPEOPENPropertyPackage = New CAPEOPENPropertyPackage()
            COPP.ComponentName = "CAPE-OPEN"
            COPP.ComponentDescription = DWSIM.App.GetLocalString("DescCOPP")

            PropertyPackages.Add(COPP.ComponentName.ToString, COPP)

        End If

    End Sub

    Sub AddFlashAlgorithms()

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.IFlashAlgorithm") IsNot Nothing, True, False)))

        For Each item In availableTypes.OrderBy(Function(x) x.Name)
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.IFlashAlgorithm)
                If Not obj.InternalUseOnly Then FlashAlgorithms.Add(obj.Name, obj)
                If obj.Name.Contains("Gibbs") Then
                    Dim obj2 = obj.Clone
                    DirectCast(obj2, Auxiliary.FlashAlgorithms.GibbsMinimization3P).ForceTwoPhaseOnly = True
                    FlashAlgorithms.Add(obj2.Name, obj2)
                End If
                If TypeOf obj Is Auxiliary.FlashAlgorithms.NestedLoopsSLE Then
                    Dim obj2 = obj.Clone
                    DirectCast(obj2, Auxiliary.FlashAlgorithms.NestedLoopsSLE).SolidSolution = True
                    FlashAlgorithms.Add(obj2.Name, obj2)
                End If
            End If
        Next

    End Sub

    Private Sub FormParent_MdiChildActivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.MdiChildActivate

        If Me.MdiChildren.Length >= 1 Then

            Me.WebPanel.Visible = False
            Me.PainelDaWebToolStripMenuItem.Checked = False

            Me.ToolStripButton1.Enabled = True
            Me.SaveAllToolStripButton.Enabled = True
            Me.SaveToolStripButton.Enabled = True
            Me.SaveToolStripMenuItem.Enabled = True
            Me.SaveAllToolStripMenuItem.Enabled = True
            Me.SaveAsToolStripMenuItem.Enabled = True
            Me.ToolStripButton1.Enabled = True
            Me.CloseAllToolstripMenuItem.Enabled = True
            If Not Me.ActiveMdiChild Is Nothing Then
                If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                    ToolStripManager.Merge(DirectCast(ActiveMdiChild, FormFlowsheet).ToolStrip1, ToolStrip1)
                    DirectCast(ActiveMdiChild, FormFlowsheet).ToolStrip1.Visible = False
                    My.Application.ActiveSimulation = Me.ActiveMdiChild
                End If
            End If

            ToolStripManager.RevertMerge(ToolStrip1)

            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                ToolStripManager.Merge(DirectCast(ActiveMdiChild, FormFlowsheet).ToolStrip1, ToolStrip1)
            End If

        End If

    End Sub

    Private Sub FormParent_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        Dim cmdLine() As String = System.Environment.GetCommandLineArgs()

        If UBound(cmdLine) = 1 Then
            If Not cmdLine(0).StartsWith("-") And Not cmdLine(1).Contains("DWSIM.exe") Then
                Try
                    Me.filename = cmdLine(1)
                    Try
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " (" + Me.filename + ")"
                        Application.DoEvents()
                        Select Case Path.GetExtension(Me.filename).ToLower()
                            Case ".dwsim"
                                'Me.LoadF(Me.filename)
                            Case ".dwxml"
                                Me.LoadXML(Me.filename, Nothing)
                            Case ".dwxmz"
                                Me.LoadAndExtractXMLZIP(Me.filename, Nothing)
                            Case ".dwcsd"
                                Dim NewMDIChild As New FormCompoundCreator()
                                NewMDIChild.MdiParent = Me
                                NewMDIChild.Show()
                                Dim objStreamReader As New FileStream(Me.filename, FileMode.Open)
                                Dim x As New BinaryFormatter()
                                NewMDIChild.mycase = x.Deserialize(objStreamReader)
                                objStreamReader.Close()
                                NewMDIChild.WriteData()
                            Case ".dwrsd"
                                Dim NewMDIChild As New FormDataRegression()
                                NewMDIChild.MdiParent = Me
                                NewMDIChild.Show()
                                Dim objStreamReader As New FileStream(Me.filename, FileMode.Open)
                                Dim x As New BinaryFormatter()
                                NewMDIChild.currcase = x.Deserialize(objStreamReader)
                                objStreamReader.Close()
                                NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                        End Select
                    Catch ex As Exception
                        MessageBox.Show(DWSIM.App.GetLocalString("Erroaoabrirarquivo") & " " & ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        'Me.ToolStripStatusLabel1.Text = ""
                    End Try
                Catch ex As Exception
                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Else
                OpenWelcomeScreen()
            End If
        Else
            OpenWelcomeScreen()
        End If

    End Sub

    Sub CheckForUpdates()

        ' check for updates
        Task.Factory.StartNew(Function()
                                  Dim updfile = AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "version.info"
                                  Dim uinfo = "0"
                                  If (File.Exists(updfile)) Then uinfo = File.ReadAllText(updfile)
                                  GlobalSettings.Settings.CurrentRunningVersion = Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() + "." + uinfo
                                  Return SharedClasses.UpdateCheck.CheckForUpdates()
                              End Function).ContinueWith(Sub(t)
                                                             If (t.Result) Then
                                                                 Dim whatsnew = SharedClasses.UpdateCheck.GetWhatsNew()
                                                                 Me.UIThreadInvoke(Sub()
                                                                                       If MessageBox.Show(DWSIM.App.GetLocalString("UpdatedVersionAvailable") & vbCrLf & vbCrLf & whatsnew, DWSIM.App.GetLocalString("UpdateAvailable"), MessageBoxButtons.OKCancel, MessageBoxIcon.Information) = DialogResult.OK Then
                                                                                           Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Downloads#DWSIM_for_Desktop_Systems")
                                                                                       End If
                                                                                   End Sub)
                                                             End If
                                                         End Sub, TaskContinuationOptions.ExecuteSynchronously)

    End Sub

    Sub OpenWelcomeScreen()

        Me.WelcomePanel.Visible = True

        If GlobalSettings.Settings.OldUI AndAlso My.Settings.BackupFiles.Count > 0 Then
            Me.FrmRec = New FormRecoverFiles
            Me.FrmRec.ShowDialog(Me)
        End If

        UpdateFlowsheetLinks()

        CheckForUpdates()

    End Sub


    Sub UpdateFlowsheetLinks()

        For Each item In SampleList
            If File.Exists(item) Then
                Dim tsmi As New ToolStripMenuItem
                With tsmi
                    .Text = Path.GetFileNameWithoutExtension(item)
                    .Tag = item
                    .DisplayStyle = ToolStripItemDisplayStyle.Text
                End With
                Me.tsmiSamples.DropDownItems.Add(tsmi)
                AddHandler tsmi.Click, Sub()
                                           Me.UIThreadInvoke(Sub() LoadFile(item))
                                       End Sub
            End If
        Next

    End Sub

    Sub UpdateFOSSEEList()

        For Each item In FOSSEEList
            Dim tsmi As New ToolStripMenuItem
            With tsmi
                .Text = item.DisplayName
                .Tag = item.DownloadLink
                .DisplayStyle = ToolStripItemDisplayStyle.Text
            End With
            Me.tsmiFOSSEE.DropDownItems.Add(tsmi)
            AddHandler tsmi.Click, Sub()
                                       Dim sb = New StringBuilder
                                       sb.AppendLine(("Title: " + item.Title))
                                       sb.AppendLine(("Author: " + item.ProposerName))
                                       sb.AppendLine(("Institution: " + item.Institution))
                                       sb.AppendLine()
                                       sb.AppendLine("Click 'Yes' to download and open this flowsheet.")
                                       If MessageBox.Show(sb.ToString, "Open FOSSEE Flowsheet", MessageBoxButtons.YesNo, MessageBoxIcon.Information) = DialogResult.Yes Then
                                           Dim floading As New FormLoadingSimulation
                                           Dim fdlding As New FormLoadingSimulation
                                           fdlding.Label1.Text = "Downloading file..." & vbCrLf & "(" & item.Title & ")"
                                           fdlding.Show()
                                           Application.DoEvents()
                                           Task.Factory.StartNew(Function()
                                                                     Return SharedClasses.FOSSEEFlowsheets.DownloadFlowsheet(item.DownloadLink, Sub(px)
                                                                                                                                                    Me.UIThread(Sub()
                                                                                                                                                                    fdlding.Label1.Text = "Downloading file... (" & px & "%)" & vbCrLf & "(" & item.Title & ")"
                                                                                                                                                                    fdlding.ProgressBar1.Value = px
                                                                                                                                                                End Sub)
                                                                                                                                                End Sub)
                                                                 End Function).ContinueWith(Sub(tk)
                                                                                                Me.UIThread(Sub() fdlding.Close())
                                                                                                If tk.Exception IsNot Nothing Then
                                                                                                    MessageBox.Show(tk.Exception, "Error downloading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                                Else
                                                                                                    Dim xdoc = SharedClasses.FOSSEEFlowsheets.LoadFlowsheet(tk.Result)
                                                                                                    Me.UIThread(Sub()
                                                                                                                    floading.Label1.Text = DWSIM.App.GetLocalString("LoadingFile") & vbCrLf & "(" & item.Title & ")"
                                                                                                                    floading.Show()
                                                                                                                    Application.DoEvents()
                                                                                                                    Try
                                                                                                                        LoadXML2(xdoc, Sub(x)
                                                                                                                                           Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                                                                                                       End Sub)
                                                                                                                    Catch ex As Exception
                                                                                                                        MessageBox.Show(tk.Exception, "Error loading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                                                    Finally
                                                                                                                        floading.Close()
                                                                                                                    End Try
                                                                                                                End Sub)
                                                                                                End If
                                                                                            End Sub)
                                       End If
                                   End Sub
        Next


    End Sub

#End Region

#Region "    Load Databases / Property Packages / Flash Algorithms"

    Private Function GetComponents()

        'try to find chemsep xml database
        Me.LoadCSDB()

        'load DWSIM XML database
        Me.LoadDWSIMDB()

        'load CoolProp database
        Me.LoadCPDB()

        'load ChEDL database
        Me.LoadCheDLDB()

        'load Electrolyte XML database
        Me.LoadEDB()

        'load Biodiesel XML database
        Me.LoadBDDB()

        If GlobalSettings.Settings.OldUI Then

            Dim invaliddbs As New List(Of String)

            'load user databases
            For Each fpath As String In My.Settings.UserDatabases
                Try
                    Dim componentes As ConstantProperties()
                    componentes = Databases.UserDB.ReadComps(fpath)
                    If componentes.Length > 0 Then
                        If My.Settings.ReplaceComps Then
                            For Each c As ConstantProperties In componentes
                                If Not Me.AvailableComponents.ContainsKey(c.Name) Then
                                    Me.AvailableComponents.Add(c.Name, c)
                                Else
                                    Me.AvailableComponents(c.Name) = c
                                End If
                            Next
                        Else
                            For Each c As ConstantProperties In componentes
                                If Not Me.AvailableComponents.ContainsKey(c.Name) Then
                                    Me.AvailableComponents.Add(c.Name, c)
                                End If
                            Next
                        End If
                    End If
                Catch ex As Exception
                    invaliddbs.Add(fpath)
                End Try
            Next

            'remove non-existent or broken user databases from the list
            For Each str As String In invaliddbs
                My.Settings.UserDatabases.Remove(str)
            Next

        End If

        'check coolprop compat
        Dim cp As New CoolPropPropertyPackage()
        For Each c In Me.AvailableComponents.Values
            If cp.CompoundAliases.ContainsKey(c.CAS_Number) Then c.IsCOOLPROPSupported = True
        Next
        cp.Dispose()
        cp = Nothing

        Return Nothing

    End Function

    Public Sub LoadCSDB()
        Dim csdb As New Databases.ChemSep
        Dim cpa() As BaseClasses.ConstantProperties
        csdb.Load()
        cpa = csdb.Transfer()
        For Each cp As BaseClasses.ConstantProperties In cpa
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then
                Me.AvailableComponents.Add(cp.Name, cp)
            End If
        Next
        loadedCSDB = True
    End Sub

    Public Sub LoadDWSIMDB()
        Dim dwdb As New Databases.DWSIM
        Dim cpa() As BaseClasses.ConstantProperties
        dwdb.Load()
        cpa = dwdb.Transfer()
        For Each cp As BaseClasses.ConstantProperties In cpa
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then Me.AvailableComponents.Add(cp.Name, cp)
        Next
    End Sub

    Public Sub LoadBDDB()
        Dim bddb As New Databases.Biodiesel
        Dim cpa() As BaseClasses.ConstantProperties
        bddb.Load()
        cpa = bddb.Transfer()
        For Each cp As BaseClasses.ConstantProperties In cpa
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then Me.AvailableComponents.Add(cp.Name, cp)
        Next
    End Sub

    Public Sub LoadEDB()
        Dim edb As New Databases.Electrolyte
        Dim cpa() As BaseClasses.ConstantProperties
        edb.Load()
        cpa = edb.Transfer()
        For Each cp As BaseClasses.ConstantProperties In cpa
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then Me.AvailableComponents.Add(cp.Name, cp)
        Next
    End Sub

    Public Sub LoadCPDB()
        Dim cpdb As New Databases.CoolProp
        Dim cpa() As BaseClasses.ConstantProperties
        cpdb.Load()
        Try
            cpa = cpdb.Transfer()
            Dim addedcomps = AvailableComponents.Keys.Select(Function(x) x.ToLower).ToList()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not addedcomps.Contains(cp.Name.ToLower) Then
                    If AvailableComponents.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                        Me.AvailableComponents.Add(cp.Name, cp)
                        Me.AvailableComponents(cp.Name).IsCOOLPROPSupported = True
                    End If
                End If
            Next
        Catch ex As Exception
        End Try
    End Sub

    Public Sub LoadCheDLDB()

        Dim chedl As New Databases.ChEDL_Thermo
        Dim cpa() As BaseClasses.ConstantProperties
        chedl.Load()
        cpa = chedl.Transfer().ToArray()
        Dim addedcomps = AvailableComponents.Keys.Select(Function(x) x.ToLower).ToList()
        For Each cp As ConstantProperties In cpa
            If Not addedcomps.Contains(cp.Name.ToLower) AndAlso Not AvailableComponents.ContainsKey(cp.Name) Then
                If AvailableComponents.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                    AvailableComponents.Add(cp.Name, cp)
                End If
            End If
        Next

    End Sub

#End Region

#Region "    Open/Save Files"

    Function ReturnForm(ByVal str As String) As IDockContent
        Select Case str
            Case "DWSIM.LogPanel", "DWSIM.frmLog"
                Return Me.tmpform2.FormLog
            Case "DWSIM.MaterialStreamPanel", "DWSIM.frmMatList"
                Return Me.tmpform2.FormMatList
            Case "DWSIM.FlowsheetSurface", "DWSIM.frmSurface"
                Return Me.tmpform2.FormSurface
            Case "DWSIM.SpreadsheetForm"
                Return Me.tmpform2.FormSpreadsheet
            Case "DWSIM.WatchPanel", "DWSIM.frmWatch"
                Return Me.tmpform2.FormWatch
            Case "DWSIM.frmProps"
                Return Me.tmpform2.FormProps
        End Select
        Return Nothing
    End Function

    Private Function RandomString(ByVal size As Integer, ByVal lowerCase As Boolean) As String

        Dim builder As New StringBuilder()
        Dim random As New Random()
        Dim ch As Char
        Dim i As Integer
        For i = 0 To size - 1
            ch = Convert.ToChar(Convert.ToInt32((26 * random.NextDouble() + 65)))
            builder.Append(ch)
        Next
        If lowerCase Then
            Return builder.ToString().ToLower()
        End If
        Return builder.ToString()

    End Function

    Sub AddGraphicObjects(form As FormFlowsheet, data As List(Of XElement), excs As Concurrent.ConcurrentBag(Of Exception),
                          Optional ByVal pkey As String = "", Optional ByVal shift As Integer = 0, Optional ByVal reconnectinlets As Boolean = False)

        Dim objcount As Integer, searchtext As String

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("Microsoft.MSDN.Samples.GraphicObjects", "DWSIM.DrawingTools.GraphicObjects")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Ajuste", "OT_Adjust")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Especificacao", "OT_Spec")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Reciclo", "OT_Recycle")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Texto", "GO_Text")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Figura", "GO_Image")
                Dim obj As GraphicObject = Nothing
                Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                If obj Is Nothing Then
                    If xel.Element("Type").Value.Contains("OxyPlotGraphic") Then
                        obj = CType(Drawing.SkiaSharp.Extended.Shared.ReturnInstance(xel.Element("Type").Value.Replace("Shapes", "Charts")), GraphicObject)
                    Else
                        obj = CType(GraphicObject.ReturnInstance(xel.Element("Type").Value), GraphicObject)
                    End If
                End If
                If Not obj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    obj.Name = pkey & obj.Name
                    obj.X += shift
                    obj.Y += shift
                    If pkey <> "" Then
                        searchtext = obj.Tag.Split("(")(0).Trim()
                        objcount = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Select go Where go.Tag.Equals(obj.Tag)).Count
                        If objcount > 0 Then obj.Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    End If
                    If TypeOf obj Is TableGraphic Then
                        DirectCast(obj, TableGraphic).Flowsheet = form
                    ElseIf TypeOf obj Is MasterTableGraphic Then
                        DirectCast(obj, MasterTableGraphic).Flowsheet = form
                    ElseIf TypeOf obj Is SpreadsheetTableGraphic Then
                        DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = form
                    ElseIf TypeOf obj Is OxyPlotGraphic Then
                        DirectCast(obj, OxyPlotGraphic).Flowsheet = form
                    ElseIf TypeOf obj Is RigorousColumnGraphic Or TypeOf obj Is AbsorptionColumnGraphic Or TypeOf obj Is CAPEOPENGraphic Then
                        obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                        obj.PositionConnectors()
                    ElseIf TypeOf obj Is ExternalUnitOperationGraphic Then
                        Dim euo = ExternalUnitOperations.Values.Where(Function(x) x.Description = obj.Description).FirstOrDefault
                        If euo IsNot Nothing Then
                            obj.Owner = euo
                            DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = obj
                            obj.CreateConnectors(0, 0)
                            obj.Owner = Nothing
                            DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = Nothing
                        End If
                    Else
                        If obj.Name = "" Then obj.Name = obj.Tag
                        obj.CreateConnectors(0, 0)
                    End If
                    form.FormSurface.FlowsheetSurface.DrawingObjects.Add(obj)
                    form.Collections.GraphicObjectCollection.Add(obj.Name, obj)
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
            End Try
        Next

        For Each xel As XElement In data
            Try
                Dim id As String = pkey & xel.Element("Name").Value
                If id <> "" Then
                    Dim obj As GraphicObject = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If obj Is Nothing Then obj = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel.Element("Name").Value).SingleOrDefault
                    If obj IsNot Nothing Then
                        If xel.Element("InputConnectors") IsNot Nothing Then
                            Dim i As Integer = 0
                            For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    obj.InputConnectors(i).ConnectorName = pkey & xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                    obj.InputConnectors(i).Type = [Enum].Parse(obj.InputConnectors(i).Type.GetType, xel2.@ConnType)
                                    If reconnectinlets Then
                                        Dim objFrom As GraphicObject = (From go As GraphicObject In
                                                                                   form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedFromObjID).SingleOrDefault
                                        If Not objFrom Is Nothing Then
                                            If Not objFrom.OutputConnectors(xel2.@AttachedFromConnIndex).IsAttached Then
                                                form.ConnectObject(objFrom, obj, xel2.@AttachedFromConnIndex, xel2.@AttachedToConnIndex)
                                            End If
                                        End If
                                    End If
                                End If
                                i += 1
                            Next
                        End If
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
            End Try
        Next

        For Each xel As XElement In data
            Try
                Dim id As String = pkey & xel.Element("Name").Value
                If id <> "" Then
                    Dim obj As GraphicObject = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If obj IsNot Nothing Then
                        If xel.Element("OutputConnectors") IsNot Nothing Then
                            For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = pkey & xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As GraphicObject = (From go As GraphicObject In
                                                                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                        If objTo Is Nothing Then objTo = (From go As GraphicObject In
                                                                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        Dim fromidx As Integer = -1
                                        Dim cp As ConnectionPoint = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = obj.Name).SingleOrDefault
                                        If cp Is Nothing Then cp = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = xel2.@AttachedToObjID).SingleOrDefault
                                        If Not cp Is Nothing Then
                                            fromidx = cp.ConnectorName.Split("|")(1)
                                        End If
                                        If Not obj Is Nothing And Not objTo Is Nothing Then form.ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                    End If
                                End If
                            Next
                        End If
                        If xel.Element("EnergyConnector") IsNot Nothing Then
                            For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                                If xel2.@IsAttached = True Then
                                    Dim objToID = pkey & xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As GraphicObject = (From go As GraphicObject In
                                                                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                        If objTo Is Nothing Then obj = (From go As GraphicObject In
                                                                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        If Not obj Is Nothing And Not objTo Is Nothing Then form.ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
                                    End If
                                End If
                            Next
                        End If
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
            End Try
        Next


    End Sub

    Sub AddSimulationObjects(form As FormFlowsheet, objlist As Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass), excs As Concurrent.ConcurrentBag(Of Exception), Optional ByVal pkey As String = "")

        For Each obj In objlist
            Try
                obj.Name = pkey & obj.Name
                Dim id = obj.Name
                form.Collections.FlowsheetObjectCollection.Add(id, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        For Each so As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            Try
                If TryCast(so, Adjust) IsNot Nothing Then
                    Dim so2 As Adjust = so
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ManipulatedObjectData.ID) Then
                        so2.ManipulatedObject = form.Collections.FlowsheetObjectCollection(so2.ManipulatedObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
                    End If
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ControlledObjectData.ID) Then
                        so2.ControlledObject = form.Collections.FlowsheetObjectCollection(so2.ControlledObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
                    End If
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.ReferencedObjectData.ID) Then
                        so2.ReferenceObject = form.Collections.FlowsheetObjectCollection(so2.ReferencedObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
                    End If
                End If
                If TryCast(so, Spec) IsNot Nothing Then
                    Dim so2 As Spec = so
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.TargetObjectData.ID) Then
                        so2.TargetObject = form.Collections.FlowsheetObjectCollection(so2.TargetObjectData.ID)
                        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
                    End If
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.SourceObjectData.ID) Then
                        so2.SourceObject = form.Collections.FlowsheetObjectCollection(so2.SourceObjectData.ID)
                        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
                    End If
                End If
                If TryCast(so, CapeOpenUO) IsNot Nothing Then
                    DirectCast(so, CapeOpenUO).UpdateConnectors2()
                    DirectCast(so, CapeOpenUO).UpdatePortsFromConnectors()
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
            End Try
        Next

    End Sub

    Function AssemblyResolver(arg1 As Assembly, arg2 As String, arg3 As Boolean) As Type

        Return Nothing

    End Function

    Sub LoadMobileXML(ByVal path As String)

        My.Application.PushUndoRedoAction = False

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = Nothing

        Using fstr As Stream = File.OpenRead(path)
            xdoc = XDocument.Load(fstr)
        End Using

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               SharedClasses.Utility.UpdateElementForMobileXMLLoading_CrossPlatformUI(xel1)
                                           End Sub)

        Dim form As FormFlowsheet = New FormFlowsheet() With {.MobileCompatibilityMode = True}
        form.FormSpreadsheet = New FormNewSpreadsheet() With {.Flowsheet = form}
        form.FormSpreadsheet.Initialize()
        form.PanelMobileCompatMode.Visible = True

        Settings.CAPEOPENMode = False
        My.Application.ActiveSimulation = form

        Application.DoEvents()

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            form.Options.LoadData(data)
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        Me.filename = path

        form.Options.FilePath = Me.filename

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddGraphicObjects(form, data, excs)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New ConstantProperties
                obj.LoadData(xel.Elements.ToList)
                If My.Settings.IgnoreCompoundPropertiesOnLoad AndAlso AvailableComponents.ContainsKey(obj.Name) Then
                    form.Options.SelectedComponents.Add(obj.Name, AvailableComponents(obj.Name))
                Else
                    form.Options.SelectedComponents.Add(obj.Name, obj)
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Compound Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

        Dim pp As New PropertyPackages.RaoultPropertyPackage()

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("PortableDTL.DTL.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = pp.ReturnInstance(xel.Element("Type").Value)
                obj.LoadData(xel.Elements.ToList)
                Dim newID As String = Guid.NewGuid.ToString
                If form.Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                obj.Flowsheet = form
                form.Options.PropertyPackages.Add(obj.UniqueID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Property Package Information", ex))
            End Try
        Next

        My.Application.ActiveSimulation = form

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

        For Each xel In data
            Try
                Dim id As String = xel.<Name>.Value
                Dim obj As SharedClasses.UnitOperations.BaseClass = Nothing
                If xel.Element("Type").Value.Contains("MaterialStream") Then
                    obj = pp.ReturnInstance(xel.Element("Type").Value)
                Else
                    obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                End If
                Dim gobj As GraphicObject = (From go As GraphicObject In
                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                obj.GraphicObject = gobj
                gobj.Owner = obj
                obj.SetFlowsheet(form)
                If Not gobj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                            For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                phase.Compounds(c.Name).ConstantProperties = c
                            Next
                        Next
                    End If
                End If
                objlist.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        AddSimulationObjects(form, objlist, excs)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

        form.Options.ReactionSets.Clear()

        For Each xel As XElement In data
            Try
                Dim obj As New ReactionSet()
                obj.LoadData(xel.Elements.ToList)
                form.Options.ReactionSets.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Set Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New Reaction()
                obj.LoadData(xel.Elements.ToList)
                form.Options.Reactions.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Information", ex))
            End Try
        Next

        Dim sel As XElement = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis")

        If Not sel Is Nothing Then

            data = sel.Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New SensitivityAnalysisCase
                    obj.LoadData(xel.Elements.ToList)
                    form.Collections.OPT_SensAnalysisCollection.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Sensitivity Analysis Case Information", ex))
                End Try
            Next

        End If

        form.ScriptCollection = New Dictionary(Of String, Interfaces.IScript)

        form.Options.NotSelectedComponents = New Dictionary(Of String, Interfaces.ICompoundConstantProperties)

        Dim tmpc As BaseClasses.ConstantProperties
        For Each tmpc In Me.AvailableComponents.Values
            Dim newc As New BaseClasses.ConstantProperties
            newc = tmpc
            If Not form.Options.SelectedComponents.ContainsKey(tmpc.Name) Then
                form.Options.NotSelectedComponents.Add(tmpc.Name, newc)
            End If
        Next

        My.Application.ActiveSimulation = form

        m_childcount += 1

        form.m_IsLoadedFromFile = True

        ' Set DockPanel properties
        form.dckPanel.ActiveAutoHideContent = Nothing
        form.dckPanel.Parent = form

        Me.tmpform2 = form
        form.FormLog.DockPanel = Nothing
        form.FormMatList.DockPanel = Nothing
        form.FormSpreadsheet.DockPanel = Nothing
        form.FormWatch.DockPanel = Nothing
        form.FormSurface.DockPanel = Nothing

        Try
            form.FormSpreadsheet.Show(form.dckPanel)
            form.FormMatList.Show(form.dckPanel)
            form.FormSurface.Show(form.dckPanel)
            form.FormLog.Show(form.dckPanel)
            form.dckPanel.BringToFront()
            form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
        Catch ex As Exception
            excs.Add(New Exception("Error Restoring Window Layout", ex))
        End Try

        Me.Invalidate()
        Application.DoEvents()

        Dim mypath As String = path
        If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
            My.Settings.MostRecentFiles.Add(mypath)
            Me.UpdateMRUList()
        End If

        My.Application.ActiveSimulation = form

        form.MdiParent = Me
        form.Show()
        form.Activate()

        form.FrmStSim1.Init(True)

        form.FormSurface.Invalidate()

        If excs.Count > 0 Then
            form.WriteToLog("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", Color.DarkRed, MessageType.GeneralError)
            For Each ex As Exception In excs
                form.WriteToLog(ex.Message.ToString & ": " & ex.InnerException.ToString, Color.Red, MessageType.GeneralError)
            Next
        Else
            form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & Me.filename & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        'Me.ToolStripStatusLabel1.Text = ""

        My.Application.PushUndoRedoAction = True

        Application.DoEvents()

    End Sub

    Public Function LoadXML(ByVal path As String, ProgressFeedBack As Action(Of Integer), Optional ByVal simulationfilename As String = "", Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

        My.Application.PushUndoRedoAction = False

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = Nothing

        Using fstr As Stream = File.OpenRead(path)
            xdoc = XDocument.Load(fstr)
        End Using

        Try
            If My.Settings.SimulationUpgradeWarning Then
                Dim versiontext = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("BuildVersion").Value
                If versiontext.StartsWith("3") Then
                    Dim fw As New FormUpgradeWarning()
                    fw.LabelVersion.Text += versiontext & "."
                    fw.ShowDialog(Me)
                End If
            End If
        Catch ex As Exception
        End Try

        'check version

        Dim sver = New Version("1.0.0.0")

        Try
            sver = New Version(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("BuildVersion").Value)
        Catch ex As Exception
        End Try

        If sver < New Version("5.0.0.0") Then
            For Each xel1 In xdoc.Descendants
                SharedClasses.Utility.UpdateElement(xel1)
            Next
        End If

        For Each xel1 In xdoc.Descendants
            SharedClasses.Utility.UpdateElementForNewUI(xel1)
        Next

        'check saved from Classic UI

        Dim savedfromclui As Boolean = True

        Try
            savedfromclui = Boolean.Parse(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("SavedFromClassicUI").Value)
        Catch ex As Exception
        End Try

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(5)

        Dim form As FormFlowsheet = New FormFlowsheet()
        Settings.CAPEOPENMode = False
        My.Application.ActiveSimulation = form

        Application.DoEvents()

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            form.Options.LoadData(data)
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        Try
            form.Options.FlashAlgorithms.Clear()

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "FlashAlgorithms").SingleOrDefault

            If Not el Is Nothing Then
                For Each xel As XElement In el.Elements
                    Dim obj As PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm = CType(New PropertyPackages.RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.FlashAlgorithms.Add(obj)
                Next
            Else
                form.Options.FlashAlgorithms.Add(New Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops() With {.Tag = .Name})
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(15)

        If simulationfilename <> "" Then Me.filename = simulationfilename Else Me.filename = path

        form.FilePath = Me.filename
        form.Options.FilePath = Me.filename

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddGraphicObjects(form, data, excs)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(25)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New ConstantProperties
                obj.LoadData(xel.Elements.ToList)
                If My.Settings.IgnoreCompoundPropertiesOnLoad AndAlso AvailableComponents.ContainsKey(obj.Name) Then
                    form.Options.SelectedComponents.Add(obj.Name, AvailableComponents(obj.Name))
                Else
                    form.Options.SelectedComponents.Add(obj.Name, obj)
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Compound Information", ex))
            End Try
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(35)

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties").Elements.ToList

            Try

                form.ExtraProperties = New ExpandoObject

                If Not data Is Nothing Then
                    For Each xel As XElement In data
                        Try
                            Dim propname = xel.Element("Name").Value
                            Dim proptype = xel.Element("PropertyType").Value
                            Dim ptype As Type = Type.GetType(proptype)
                            Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                            DirectCast(form.ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                        Catch ex As Exception
                        End Try
                    Next
                End If

            Catch ex As Exception

                excs.Add(New Exception("Error Loading Dynamic Properties", ex))

            End Try

        End If

        Dim pp As New RaoultPropertyPackage

        data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = Nothing
                If xel.Element("Type").Value.Contains("AdvancedEOS") Then
                    Dim adveoskey As String = "PC-SAFT (with Association Support)"
                    If PropertyPackages.ContainsKey(adveoskey) Then
                        obj = PropertyPackages(adveoskey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("Advanced EOS Property Package library not found. Please download and install it in order to run this simulation.")
                    End If
                ElseIf xel.Element("Type").Value.Contains("ThermoC") Then
                    Dim thermockey As String = "ThermoC Bridge"
                    If PropertyPackages.ContainsKey(thermockey) Then
                        obj = PropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("The ThermoC bridge library was not found. Please download and install it in order to run this simulation.")
                    End If
                Else
                    Dim ppkey As String = xel.Element("ComponentName").Value
                    If ppkey = "" Then
                        obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackage)
                    Else
                        If PropertyPackages.ContainsKey(ppkey) Then
                            obj = PropertyPackages(ppkey).ReturnInstance(xel.Element("Type").Value)
                        Else
                            Throw New Exception("The " & ppkey & " Property Package library was not found. Please download and install it in order to run this simulation.")
                        End If
                    End If
                End If
                DirectCast(obj, Interfaces.ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                Dim newID As String = Guid.NewGuid.ToString
                If form.Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                obj.Flowsheet = form
                form.Options.PropertyPackages.Add(obj.UniqueID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Property Package Information", ex))
            End Try
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(65)

        My.Application.ActiveSimulation = form

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

        Dim fsuocount = (From go As GraphicObject In form.Collections.GraphicObjectCollection.Values Where go.ObjectType = ObjectType.FlowsheetUO).Count

        For Each xel In data
            Try
                Dim id As String = xel.<Name>.Value
                Dim obj As SharedClasses.UnitOperations.BaseClass = Nothing
                If xel.Element("Type").Value.Contains("Streams.MaterialStream") Then
                    obj = pp.ReturnInstance(xel.Element("Type").Value)
                Else
                    Dim uokey As String = xel.Element("ComponentDescription").Value
                    If ExternalUnitOperations.ContainsKey(uokey) Then
                        obj = ExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                    End If
                End If
                Dim gobj As GraphicObject = (From go As GraphicObject In
                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                obj.GraphicObject = gobj
                gobj.Owner = obj
                obj.SetFlowsheet(form)
                If Not gobj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                            For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                phase.Compounds(c.Name).ConstantProperties = c
                            Next
                        Next
                    ElseIf TypeOf obj Is CapeOpenUO Then
                        If DirectCast(obj, CapeOpenUO)._seluo.Name.ToLower.Contains("chemsep") Then
                            DirectCast(gobj, CAPEOPENGraphic).ChemSep = True
                            If gobj.Height = 40 And gobj.Width = 40 Then
                                gobj.Width = 144
                                gobj.Height = 180
                            End If
                        End If
                    End If
                End If
                objlist.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        AddSimulationObjects(form, objlist, excs)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(80)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

        form.Options.ReactionSets.Clear()

        For Each xel As XElement In data
            Try
                Dim obj As New ReactionSet()
                obj.LoadData(xel.Elements.ToList)
                form.Options.ReactionSets.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Set Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New Reaction()
                obj.LoadData(xel.Elements.ToList)
                form.Options.Reactions.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New OptimizationCase
                obj.LoadData(xel.Elements.ToList)
                form.Collections.OPT_OptimizationCollection.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Optimization Case Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New SensitivityAnalysisCase
                obj.LoadData(xel.Elements.ToList)
                form.Collections.OPT_SensAnalysisCollection.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Sensitivity Analysis Case Information", ex))
            End Try
        Next

        If xdoc.Element("DWSIM_Simulation_Data").Element("PetroleumAssays") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("PetroleumAssays").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Utilities.PetroleumCharacterization.Assay.Assay()
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.PetroleumAssays.Add(obj.Name, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Petroleum Assay Information", ex))
                End Try
            Next

        End If

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(85)

        If xdoc.Element("DWSIM_Simulation_Data").Element("WatchItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("WatchItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New WatchItem
                    obj.LoadData(xel.Elements.ToList)
                    form.FormWatch.items.Add(i, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Watch Item Information", ex))
                End Try
                i += 1
            Next

            form.FormWatch.PopulateList()

        End If

        form.ScriptCollection = New Dictionary(Of String, Interfaces.IScript)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New Script()
                    obj.LoadData(xel.Elements.ToList)
                    form.ScriptCollection.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Script Item Information", ex))
                End Try
                i += 1
            Next

        End If

        form.ChartCollection = New Dictionary(Of String, Interfaces.IChart)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New SharedClasses.Charts.Chart()
                    obj.LoadData(xel.Elements.ToList)
                    form.ChartCollection.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Chart Item Information", ex))
                End Try
                i += 1
            Next

        End If

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(90)

        Try
            If DWSIM.App.IsRunningOnMono Then form.FormSpreadsheet = New FormNewSpreadsheet() With {.Flowsheet = form}
            form.FormSpreadsheet.Initialize()
            If (Not (xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet")) Is Nothing) Then
                Dim rgfdataelement = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData")
                If Not (rgfdataelement) Is Nothing Then
                    Dim rgfdata As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value
                    Dim sdict As New Dictionary(Of String, String)
                    sdict = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(rgfdata)
                    form.FormSpreadsheet.Spreadsheet.RemoveWorksheet(0)
                    For Each item In sdict
                        Dim tmpfile = System.IO.Path.GetTempFileName
                        Dim sheet = form.FormSpreadsheet.Spreadsheet.NewWorksheet(item.Key)
                        Dim xmldoc = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(item.Value)
                        xmldoc.Save(tmpfile)
                        sheet.LoadRGF(tmpfile)
                        File.Delete(tmpfile)
                    Next
                    form.FormSpreadsheet.Spreadsheet.CurrentWorksheet = form.FormSpreadsheet.Spreadsheet.Worksheets(0)
                Else
                    Dim data1 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value
                    Dim data2 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value
                    If data1 <> "" Then form.FormSpreadsheet.CopyDT1FromString(data1)
                    If data2 <> "" Then form.FormSpreadsheet.CopyDT2FromString(data2)
                    form.FormSpreadsheet.CopyFromDT()
                    form.FormSpreadsheet.EvaluateAll()
                End If
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Spreadsheet Information", ex))
        End Try

        For Each obj In form.FormSurface.FlowsheetSurface.DrawingObjects
            If obj.ObjectType = ObjectType.GO_SpreadsheetTable Then
                DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = form
            End If
        Next

        form.Options.NotSelectedComponents = New Dictionary(Of String, Interfaces.ICompoundConstantProperties)

        Dim tmpc As BaseClasses.ConstantProperties
        For Each tmpc In Me.AvailableComponents.Values
            Dim newc As New BaseClasses.ConstantProperties
            newc = tmpc
            If Not form.Options.SelectedComponents.ContainsKey(tmpc.Name) Then
                form.Options.NotSelectedComponents.Add(tmpc.Name, newc)
            End If
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(100)

        If Not forcommandline Then

            My.Application.ActiveSimulation = form

            m_childcount += 1

            form.m_IsLoadedFromFile = True

            form.FormCharts.Flowsheet = form

            ' Set DockPanel properties
            form.dckPanel.ActiveAutoHideContent = Nothing
            form.dckPanel.Parent = form

            Me.tmpform2 = form
            'form.dckPanel.SuspendLayout(True)
            form.FormLog.DockPanel = Nothing
            form.FormMatList.DockPanel = Nothing
            form.FormSpreadsheet.DockPanel = Nothing
            form.FormSpreadsheet.Flowsheet = form
            form.FormWatch.DockPanel = Nothing
            form.FormSurface.DockPanel = Nothing
            form.FormProps.DockPanel = Nothing
            form.FormCharts.DockPanel = Nothing

            If Not My.Computer.Keyboard.ShiftKeyDown Then
                If savedfromclui Then
                    Dim myfile As String = My.Computer.FileSystem.GetTempFileName()
                    Try
                        Dim pnl As String = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout").Value
                        File.WriteAllText(myfile, pnl)
                        form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf Me.ReturnForm))
                    Catch ex As Exception
                    Finally
                        File.Delete(myfile)
                    End Try
                Else
                    Dim myfile As String = IO.Path.Combine(My.Application.Info.DirectoryPath, "layout.xml")
                    form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf ReturnForm))
                End If
            End If

            Try
                form.FormLog.DockPanel = form.dckPanel
                form.FormSpreadsheet.Show(form.dckPanel)
                form.FormCharts.Show(form.dckPanel)
                form.FormMatList.Show(form.dckPanel)
                form.FormSurface.Show(form.dckPanel)
                form.FormProps.Show(form.dckPanel)
                form.dckPanel.BringToFront()
                form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
            Catch ex As Exception
                excs.Add(New Exception("Error Restoring Window Layout", ex))
            End Try

            If form.FormProps.Width > form.Width / 3 Then
                form.dckPanel.DockLeftPortion = form.Width / 3
            End If

            Me.Invalidate()
            Application.DoEvents()

            Dim mypath As String = simulationfilename
            If mypath = "" Then mypath = [path]
            If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
                My.Settings.MostRecentFiles.Add(mypath)
                Me.UpdateMRUList()
            End If

            My.Application.ActiveSimulation = form

            form.MdiParent = Me
            form.Show()
            form.Activate()

            form.FrmStSim1.Init(True)

            form.FormSurface.Invalidate()

        Else

            form.CalculationQueue = New Queue(Of ICalculationArgs)

        End If

        If excs.Count > 0 Then
            form.WriteToLog("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", Color.DarkRed, MessageType.GeneralError)
            For Each ex As Exception In excs
                form.WriteToLog(ex.Message.ToString & ": " & ex.InnerException.ToString, Color.Red, MessageType.GeneralError)
            Next
        Else
            form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & Me.filename & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        'Me.ToolStripStatusLabel1.Text = ""

        My.Application.PushUndoRedoAction = True

        Application.DoEvents()

        Return form

    End Function

    Public Function LoadXML2(xdoc As XDocument, ProgressFeedBack As Action(Of Integer), Optional ByVal simulationfilename As String = "", Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

        My.Application.PushUndoRedoAction = False

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Try
            If My.Settings.SimulationUpgradeWarning Then
                Dim versiontext = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("BuildVersion").Value
                If versiontext.StartsWith("3") Then
                    Dim fw As New FormUpgradeWarning()
                    fw.LabelVersion.Text += versiontext & "."
                    fw.ShowDialog(Me)
                End If
            End If
        Catch ex As Exception
        End Try

        'check version

        Dim sver = New Version("1.0.0.0")

        Try
            sver = New Version(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("BuildVersion").Value)
        Catch ex As Exception
        End Try

        If sver < New Version("5.0.0.0") Then
            Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                                   SharedClasses.Utility.UpdateElement(xel1)
                                               End Sub)
        End If

        For Each xel1 In xdoc.Descendants
            SharedClasses.Utility.UpdateElementForNewUI(xel1)
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(5)

        Dim form As FormFlowsheet = New FormFlowsheet()
        Settings.CAPEOPENMode = False
        My.Application.ActiveSimulation = form

        Application.DoEvents()

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            form.Options.LoadData(data)
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        Try
            form.Options.FlashAlgorithms.Clear()

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "FlashAlgorithms").SingleOrDefault

            If Not el Is Nothing Then
                For Each xel As XElement In el.Elements
                    Dim obj As PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm = CType(New PropertyPackages.RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.FlashAlgorithms.Add(obj)
                Next
            Else
                form.Options.FlashAlgorithms.Add(New Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops() With {.Tag = .Name})
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(15)

        'If simulationfilename <> "" Then Me.filename = simulationfilename Else Me.filename = Path

        form.FilePath = Me.filename
        form.Options.FilePath = Me.filename

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddGraphicObjects(form, data, excs)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(25)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New ConstantProperties
                obj.LoadData(xel.Elements.ToList)
                If My.Settings.IgnoreCompoundPropertiesOnLoad AndAlso AvailableComponents.ContainsKey(obj.Name) Then
                    form.Options.SelectedComponents.Add(obj.Name, AvailableComponents(obj.Name))
                Else
                    form.Options.SelectedComponents.Add(obj.Name, obj)
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Compound Information", ex))
            End Try
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(35)

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties").Elements.ToList

            Try

                form.ExtraProperties = New ExpandoObject

                If Not data Is Nothing Then
                    For Each xel As XElement In data
                        Try
                            Dim propname = xel.Element("Name").Value
                            Dim proptype = xel.Element("PropertyType").Value
                            Dim ptype As Type = Type.GetType(proptype)
                            Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                            DirectCast(form.ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                        Catch ex As Exception
                        End Try
                    Next
                End If

            Catch ex As Exception

                excs.Add(New Exception("Error Loading Dynamic Properties", ex))

            End Try

        End If

        Dim pp As New RaoultPropertyPackage

        data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = Nothing
                If xel.Element("Type").Value.Contains("AdvancedEOS") Then
                    Dim adveoskey As String = "PC-SAFT (with Association Support)"
                    If PropertyPackages.ContainsKey(adveoskey) Then
                        obj = PropertyPackages(adveoskey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("Advanced EOS Property Package library not found. Please download and install it in order to run this simulation.")
                    End If
                ElseIf xel.Element("Type").Value.Contains("ThermoC") Then
                    Dim thermockey As String = "ThermoC Bridge"
                    If PropertyPackages.ContainsKey(thermockey) Then
                        obj = PropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("The ThermoC bridge library was not found. Please download and install it in order to run this simulation.")
                    End If
                Else
                    Dim ppkey As String = xel.Element("ComponentName").Value
                    If ppkey = "" Then
                        obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackage)
                    Else
                        If PropertyPackages.ContainsKey(ppkey) Then
                            obj = PropertyPackages(ppkey).ReturnInstance(xel.Element("Type").Value)
                        Else
                            Throw New Exception("The " & ppkey & " library was not found. Please download and install it in order to run this simulation.")
                        End If
                    End If
                End If
                DirectCast(obj, Interfaces.ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                Dim newID As String = Guid.NewGuid.ToString
                If form.Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                obj.Flowsheet = form
                form.Options.PropertyPackages.Add(obj.UniqueID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Property Package Information", ex))
            End Try
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(65)

        My.Application.ActiveSimulation = form

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

        Dim fsuocount = (From go As GraphicObject In form.Collections.GraphicObjectCollection.Values Where go.ObjectType = ObjectType.FlowsheetUO).Count

        For Each xel In data
            Try
                Dim id As String = xel.<Name>.Value
                Dim obj As SharedClasses.UnitOperations.BaseClass = Nothing
                If xel.Element("Type").Value.Contains("Streams.MaterialStream") Then
                    obj = pp.ReturnInstance(xel.Element("Type").Value)
                Else
                    Dim uokey As String = xel.Element("ComponentDescription").Value
                    If ExternalUnitOperations.ContainsKey(uokey) Then
                        obj = ExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                    End If
                End If
                Dim gobj As GraphicObject = (From go As GraphicObject In
                                    form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                obj.GraphicObject = gobj
                gobj.Owner = obj
                obj.SetFlowsheet(form)
                If Not gobj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                            For Each c As ConstantProperties In form.Options.SelectedComponents.Values
                                phase.Compounds(c.Name).ConstantProperties = c
                            Next
                        Next
                    ElseIf TypeOf obj Is CapeOpenUO Then
                        If DirectCast(obj, CapeOpenUO)._seluo.Name.ToLower.Contains("chemsep") Then
                            DirectCast(gobj, CAPEOPENGraphic).ChemSep = True
                            If gobj.Height = 40 And gobj.Width = 40 Then
                                gobj.Width = 144
                                gobj.Height = 180
                            End If
                        End If
                    End If
                End If
                objlist.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        AddSimulationObjects(form, objlist, excs)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(80)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

        form.Options.ReactionSets.Clear()

        For Each xel As XElement In data
            Try
                Dim obj As New ReactionSet()
                obj.LoadData(xel.Elements.ToList)
                form.Options.ReactionSets.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Set Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New Reaction()
                obj.LoadData(xel.Elements.ToList)
                form.Options.Reactions.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New OptimizationCase
                obj.LoadData(xel.Elements.ToList)
                form.Collections.OPT_OptimizationCollection.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Optimization Case Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New SensitivityAnalysisCase
                obj.LoadData(xel.Elements.ToList)
                form.Collections.OPT_SensAnalysisCollection.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Sensitivity Analysis Case Information", ex))
            End Try
        Next

        If xdoc.Element("DWSIM_Simulation_Data").Element("PetroleumAssays") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("PetroleumAssays").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Utilities.PetroleumCharacterization.Assay.Assay()
                    obj.LoadData(xel.Elements.ToList)
                    form.Options.PetroleumAssays.Add(obj.Name, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Petroleum Assay Information", ex))
                End Try
            Next

        End If

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(85)

        If xdoc.Element("DWSIM_Simulation_Data").Element("WatchItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("WatchItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New WatchItem
                    obj.LoadData(xel.Elements.ToList)
                    form.FormWatch.items.Add(i, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Watch Item Information", ex))
                End Try
                i += 1
            Next

            form.FormWatch.PopulateList()

        End If

        form.ScriptCollection = New Dictionary(Of String, Interfaces.IScript)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New Script()
                    obj.LoadData(xel.Elements.ToList)
                    form.ScriptCollection.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Script Item Information", ex))
                End Try
                i += 1
            Next

        End If

        form.ChartCollection = New Dictionary(Of String, Interfaces.IChart)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New SharedClasses.Charts.Chart()
                    obj.LoadData(xel.Elements.ToList)
                    form.ChartCollection.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Chart Item Information", ex))
                End Try
                i += 1
            Next

        End If

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(90)

        Try
            If DWSIM.App.IsRunningOnMono Then form.FormSpreadsheet = New FormNewSpreadsheet() With {.Flowsheet = form}
            form.FormSpreadsheet.Initialize()
            If (Not (xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet")) Is Nothing) Then
                Dim rgfdataelement = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData")
                If (Not (rgfdataelement) Is Nothing) Then
                    Dim rgfdata As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value
                    Dim sdict As New Dictionary(Of String, String)
                    sdict = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(rgfdata)
                    form.FormSpreadsheet.Spreadsheet.RemoveWorksheet(0)
                    For Each item In sdict
                        Dim tmpfile = System.IO.Path.GetTempFileName
                        Dim sheet = form.FormSpreadsheet.Spreadsheet.NewWorksheet(item.Key)
                        Dim xmldoc = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(item.Value)
                        xmldoc.Save(tmpfile)
                        sheet.LoadRGF(tmpfile)
                        File.Delete(tmpfile)
                    Next
                    form.FormSpreadsheet.Spreadsheet.CurrentWorksheet = form.FormSpreadsheet.Spreadsheet.Worksheets(0)
                Else
                    Dim data1 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value
                    Dim data2 As String = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value
                    If data1 <> "" Then form.FormSpreadsheet.CopyDT1FromString(data1)
                    If data2 <> "" Then form.FormSpreadsheet.CopyDT2FromString(data2)
                    form.FormSpreadsheet.CopyFromDT()
                    form.FormSpreadsheet.EvaluateAll()
                End If
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Spreadsheet Information", ex))
        End Try

        For Each obj In form.FormSurface.FlowsheetSurface.DrawingObjects
            If obj.ObjectType = ObjectType.GO_SpreadsheetTable Then
                DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = form
            End If
        Next

        form.Options.NotSelectedComponents = New Dictionary(Of String, Interfaces.ICompoundConstantProperties)

        Dim tmpc As BaseClasses.ConstantProperties
        For Each tmpc In Me.AvailableComponents.Values
            Dim newc As New BaseClasses.ConstantProperties
            newc = tmpc
            If Not form.Options.SelectedComponents.ContainsKey(tmpc.Name) Then
                form.Options.NotSelectedComponents.Add(tmpc.Name, newc)
            End If
        Next

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(100)

        If Not forcommandline Then

            My.Application.ActiveSimulation = form

            m_childcount += 1

            form.m_IsLoadedFromFile = True

            form.FormCharts.Flowsheet = form

            ' Set DockPanel properties
            form.dckPanel.ActiveAutoHideContent = Nothing
            form.dckPanel.Parent = form

            Me.tmpform2 = form
            'form.dckPanel.SuspendLayout(True)
            form.FormLog.DockPanel = Nothing
            form.FormMatList.DockPanel = Nothing
            form.FormSpreadsheet.DockPanel = Nothing
            form.FormSpreadsheet.Flowsheet = form
            form.FormCharts.DockPanel = Nothing
            form.FormWatch.DockPanel = Nothing
            form.FormSurface.DockPanel = Nothing

            If Not My.Computer.Keyboard.ShiftKeyDown Then
                Dim myfile As String = My.Computer.FileSystem.GetTempFileName()
                Try
                    Dim pnl As String = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout").Value
                    File.WriteAllText(myfile, pnl)
                    form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf Me.ReturnForm))
                Catch ex As Exception
                    'excs.Add(New Exception("Error Restoring Window Layout", ex))
                Finally
                    File.Delete(myfile)
                End Try
            End If

            Try
                form.FormLog.DockPanel = form.dckPanel
                form.FormSpreadsheet.Show(form.dckPanel)
                form.FormCharts.Show(form.dckPanel)
                form.FormMatList.Show(form.dckPanel)
                form.FormSurface.Show(form.dckPanel)
                form.dckPanel.BringToFront()
                form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
            Catch ex As Exception
                excs.Add(New Exception("Error Restoring Window Layout", ex))
            End Try

            Me.Invalidate()
            Application.DoEvents()

            Dim mypath As String = simulationfilename
            'If mypath = "" Then mypath = [Path]
            If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
                My.Settings.MostRecentFiles.Add(mypath)
                Me.UpdateMRUList()
            End If

            My.Application.ActiveSimulation = form

            form.MdiParent = Me
            form.Show()
            form.Activate()

        End If

        form.FrmStSim1.Init(True)

        Try
            form.FormSpreadsheet.EvaluateAll()
            form.FormSpreadsheet.EvaluateAll()
        Catch ex As Exception
            excs.Add(New Exception("Error Updating Spreadsheet Variables", ex))
        End Try

        form.FormSurface.Invalidate()

        If excs.Count > 0 Then
            form.WriteToLog("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", Color.DarkRed, MessageType.GeneralError)
            For Each ex As Exception In excs
                form.WriteToLog(ex.Message.ToString & ": " & ex.InnerException.ToString, Color.Red, MessageType.GeneralError)
            Next
        Else
            form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & Me.filename & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        'Me.ToolStripStatusLabel1.Text = ""

        My.Application.PushUndoRedoAction = True

        Application.DoEvents()

        Return form

    End Function

    Sub SaveMobileXML(ByVal path As String, ByVal form As FormFlowsheet, Optional ByVal simulationfilename As String = "")

        Dim compatmessage As String = SharedClasses.Utility.CheckSimulationForMobileCompatibility(form)

        If compatmessage <> "" Then
            Throw New NotSupportedException(compatmessage)
        End If

        If simulationfilename = "" Then simulationfilename = path

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))
        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GeneralInfo"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo")

        xel.Add(New XElement("BuildVersion", My.Application.Info.Version.ToString))
        xel.Add(New XElement("BuildDate", CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)))
        xel.Add(New XElement("OSInfo", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
        xel.Add(New XElement("SavedOn", Date.Now))


        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

        For Each go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects
            If TypeOf go Is ShapeGraphic Then DirectCast(go, ShapeGraphic).Fill = False
            Dim xdata As New XElement("GraphicObject", go.SaveData().ToArray())
            If TypeOf go Is ShapeGraphic Then DirectCast(go, ShapeGraphic).Fill = True
            If xdata.Elements.Count > 0 Then
                If go.ObjectType = ObjectType.Compressor Then xdata.Element("ObjectType").Value = xdata.Element("ObjectType").Value.Replace("Compressor", "CompressorExpander")
                If go.ObjectType = ObjectType.Expander Then xdata.Element("ObjectType").Value = xdata.Element("ObjectType").Value.Replace("Expander", "CompressorExpander")
                If go.ObjectType = ObjectType.Heater Then xdata.Element("ObjectType").Value = xdata.Element("ObjectType").Value.Replace("Heater", "HeaterCooler")
                If go.ObjectType = ObjectType.Cooler Then xdata.Element("ObjectType").Value = xdata.Element("ObjectType").Value.Replace("Cooler", "HeaterCooler")
            End If
            If Not go.IsConnector Then xel.Add(xdata)
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

        For Each so As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            If TypeOf so Is Cooler Then
                DirectCast(so, Cooler).DeltaQ = -DirectCast(so, Cooler).DeltaQ
            ElseIf TypeOf so Is Expander Then
                DirectCast(so, Expander).DeltaP = -DirectCast(so, Expander).DeltaP
            End If
            so.SetFlowsheet(form)
            xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
            If TypeOf so Is Cooler Then
                DirectCast(so, Cooler).DeltaQ = -DirectCast(so, Cooler).DeltaQ
            ElseIf TypeOf so Is Expander Then
                DirectCast(so, Expander).DeltaP = -DirectCast(so, Expander).DeltaP
            End If
        Next

        'update the flowsheet key for usage in server solution storage. 

        'if the key doesn't change, it means that the flowsheet data wasn't modified 
        'and a previous solution stored in the server may be returned instead of recalculating 
        'the entire flowsheet, saving time and resources.

        Dim hash As String = ""
        Using sha1 As System.Security.Cryptography.SHA1CryptoServiceProvider = System.Security.Cryptography.SHA1CryptoServiceProvider.Create()
            hash = BitConverter.ToString(sha1.ComputeHash(Encoding.UTF8.GetBytes(xel.ToString)))
        End Using

        form.Options.Key = hash.Replace("-", "")

        'save settings 

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Settings"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Settings")

        xel.Add(form.Options.SaveData().ToArray())

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

        For Each pp In form.Options.PropertyPackages
            Dim createdms As Boolean = False
            If pp.Value.CurrentMaterialStream Is Nothing Then
                Dim ms As New Streams.MaterialStream("", "", form, pp.Value)
                form.AddComponentsRows(ms)
                pp.Value.CurrentMaterialStream = ms
                createdms = True
            End If
            xel.Add(New XElement("PropertyPackage", {New XElement("ID", pp.Key),
                                                     pp.Value.SaveData().ToArray()}))
            If createdms Then pp.Value.CurrentMaterialStream = Nothing
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Compounds"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds")

        For Each cp As ConstantProperties In form.Options.SelectedComponents.Values
            xel.Add(New XElement("Compound", cp.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ReactionSets"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets")

        For Each pp As KeyValuePair(Of String, Interfaces.IReactionSet) In form.Options.ReactionSets
            xel.Add(New XElement("ReactionSet", DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Reactions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions")

        For Each pp As KeyValuePair(Of String, Interfaces.IReaction) In form.Options.Reactions
            xel.Add(New XElement("Reaction", {DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("OptimizationCases"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases")

        For Each pp As OptimizationCase In form.Collections.OPT_OptimizationCollection
            xel.Add(New XElement("OptimizationCase", {pp.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SensitivityAnalysis"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis")

        For Each pp As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
            xel.Add(New XElement("SensitivityAnalysisCase", {pp.SaveData().ToArray()}))
        Next

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               SharedClasses.Utility.UpdateElementForMobileXMLSaving_CrossPlatformUI(xel1)
                                           End Sub)

        xdoc.Save(path)

        Me.UIThread(New Action(Sub()
                                   Dim mypath As String = simulationfilename
                                   If mypath = "" Then mypath = [path]
                                   'process recent files list
                                   If Not My.Settings.MostRecentFiles.Contains(mypath) Then
                                       My.Settings.MostRecentFiles.Add(mypath)
                                       If Not My.Application.CommandLineArgs.Count > 1 Then Me.UpdateMRUList()
                                   End If
                                   form.Options.FilePath = Me.filename
                                   form.UpdateFormText()
                                   form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & Me.filename & DWSIM.App.GetLocalString("salvocomsucesso"), Color.Blue, MessageType.Information)
                                   'Me.ToolStripStatusLabel1.Text = ""
                               End Sub))

        Application.DoEvents()

    End Sub

    Sub SaveXML(ByVal path As String, ByVal form As FormFlowsheet, Optional ByVal simulationfilename As String = "")

        If simulationfilename = "" Then simulationfilename = path

        UIThread(Sub()
                     If (From f As DockContent In form.dckPanel.Documents Select f Where f.Name = "FormScript").Count > 0 Then
                         Dim f As FormScript = (From fs As DockContent In form.dckPanel.Documents Select fs Where fs.Name = "FormScript").First
                         f.UpdateScripts()
                     End If
                 End Sub)

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))
        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GeneralInfo"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo")

        xel.Add(New XElement("BuildVersion", My.Application.Info.Version.ToString))
        xel.Add(New XElement("BuildDate", CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)))
        xel.Add(New XElement("OSInfo", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
        xel.Add(New XElement("SavedOn", Date.Now))
        xel.Add(New XElement("SavedFromClassicUI", True))

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

        For Each so As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            so.SetFlowsheet(form)
            xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
        Next

        'update the flowsheet key for usage in server solution storage. 

        'if the key doesn't change, it means that the flowsheet data wasn't modified 
        'and a previous solution stored in the server may be returned instead of recalculating 
        'the entire flowsheet, saving time and resources.

        Dim hash As String = ""
        Using sha1 As System.Security.Cryptography.SHA1CryptoServiceProvider = System.Security.Cryptography.SHA1CryptoServiceProvider.Create()
            hash = BitConverter.ToString(sha1.ComputeHash(Encoding.UTF8.GetBytes(xel.ToString)))
        End Using

        form.Options.Key = hash.Replace("-", "")

        'save settings 

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Settings"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Settings")

        xel.Add(form.Options.SaveData().ToArray())

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicProperties"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties")

        Dim extraprops = DirectCast(form.ExtraProperties, IDictionary(Of String, Object))
        For Each item In extraprops
            Try
                xel.Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                       New XElement("PropertyType", item.Value.GetType.ToString),
                                                                       New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
            Catch ex As Exception
            End Try
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

        For Each go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects
            If Not go.IsConnector And Not go.ObjectType = ObjectType.GO_FloatingTable Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

        For Each pp In form.Options.PropertyPackages
            Dim createdms As Boolean = False
            If pp.Value.CurrentMaterialStream Is Nothing Then
                Dim ms As New Streams.MaterialStream("", "", form, pp.Value)
                form.AddComponentsRows(ms)
                pp.Value.CurrentMaterialStream = ms
                createdms = True
            End If
            xel.Add(New XElement("PropertyPackage", {New XElement("ID", pp.Key),
                                                     DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
            If createdms Then pp.Value.CurrentMaterialStream = Nothing
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Compounds"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds")

        For Each cp As ConstantProperties In form.Options.SelectedComponents.Values
            xel.Add(New XElement("Compound", cp.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ReactionSets"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets")

        For Each pp As KeyValuePair(Of String, Interfaces.IReactionSet) In form.Options.ReactionSets
            xel.Add(New XElement("ReactionSet", DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Reactions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions")

        For Each pp As KeyValuePair(Of String, Interfaces.IReaction) In form.Options.Reactions
            xel.Add(New XElement("Reaction", {DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("OptimizationCases"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases")

        For Each pp As OptimizationCase In form.Collections.OPT_OptimizationCollection
            xel.Add(New XElement("OptimizationCase", {pp.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SensitivityAnalysis"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis")

        For Each pp As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
            xel.Add(New XElement("SensitivityAnalysisCase", {pp.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PetroleumAssays"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PetroleumAssays")

        If form.Options.PetroleumAssays Is Nothing Then form.Options.PetroleumAssays = New Dictionary(Of String, Utilities.PetroleumCharacterization.Assay.Assay)

        For Each pp As KeyValuePair(Of String, Utilities.PetroleumCharacterization.Assay.Assay) In form.Options.PetroleumAssays
            xel.Add(New XElement("Assay", pp.Value.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("WatchItems"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("WatchItems")

        For Each wi As WatchItem In form.FormWatch.items.Values
            xel.Add(New XElement("WatchItem", wi.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ScriptItems"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems")

        For Each scr As Script In form.ScriptCollection.Values
            xel.Add(New XElement("ScriptItem", scr.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ChartItems"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems")

        For Each ch As SharedClasses.Charts.Chart In form.ChartCollection.Values
            xel.Add(New XElement("ChartItem", ch.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Spreadsheet"))
        xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Add(New XElement("RGFData"))
        Dim sdict As New Dictionary(Of String, String)
        For Each sheet In form.FormSpreadsheet.Spreadsheet.Worksheets
            Dim tmpfile = System.IO.Path.GetTempFileName
            sheet.SaveRGF(tmpfile)
            Dim xmldoc = New XmlDocument()
            xmldoc.Load(tmpfile)
            sdict.Add(sheet.Name, Newtonsoft.Json.JsonConvert.SerializeXmlNode(xmldoc))
            File.Delete(tmpfile)
        Next
        xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value = Newtonsoft.Json.JsonConvert.SerializeObject(sdict)
        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PanelLayout"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout")

        Dim myfile As String = My.Computer.FileSystem.GetTempFileName()
        form.dckPanel.SaveAsXml(myfile, Encoding.UTF8)
        xel.Add(File.ReadAllText(myfile).ToString)
        File.Delete(myfile)

        xdoc.Save(path)

        If IO.Path.GetExtension(simulationfilename).ToLower.Contains("dwxml") Or IO.Path.GetExtension(simulationfilename).ToLower.Contains("dwxmz") Then
            Me.UIThread(New Action(Sub()
                                       If Visible Then
                                           Dim mypath As String = simulationfilename
                                           If mypath = "" Then mypath = [path]
                                           'process recent files list
                                           If Not My.Settings.MostRecentFiles.Contains(mypath) Then
                                               My.Settings.MostRecentFiles.Add(mypath)
                                               If Not My.Application.CommandLineArgs.Count > 1 Then Me.UpdateMRUList()
                                           End If
                                           form.Options.FilePath = Me.filename
                                           form.UpdateFormText()
                                           form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & Me.filename & DWSIM.App.GetLocalString("salvocomsucesso"), Color.Blue, MessageType.Information)
                                       End If
                                   End Sub))
        End If

        If Not IO.Path.GetExtension(path).ToLower.Contains("dwbcs") Then
            form.ProcessScripts(Scripts.EventType.SimulationSaved, Scripts.ObjectType.Simulation, "")
        End If

        Application.DoEvents()

    End Sub

    Shared Function IsZipFilePasswordProtected(ByVal ZipFile As String) As Boolean
        Using fsIn As New FileStream(ZipFile, FileMode.Open, FileAccess.Read)
            Using zipInStream As New ZipInputStream(fsIn)
                Dim zEntry As ZipEntry = zipInStream.GetNextEntry()
                Return zEntry.IsCrypted
            End Using
        End Using
    End Function

    Function LoadAndExtractXMLZIP(ByVal caminho As String, ProgressFeedBack As Action(Of Integer), Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

        Dim pathtosave As String = My.Computer.FileSystem.SpecialDirectories.Temp + Path.DirectorySeparatorChar
        Dim fullname As String = ""

        Dim pwd As String = Nothing
        If IsZipFilePasswordProtected(caminho) Then
            Dim fp As New FormPassword
            If fp.ShowDialog() = Windows.Forms.DialogResult.OK Then
                pwd = fp.tbPassword.Text
            End If
        End If

        Try
            Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(caminho))
                stream.Password = pwd
                Dim entry As ZipEntry
Label_00CC:
                entry = stream.GetNextEntry()
                Do While (Not entry Is Nothing)
                    Dim fileName As String = Path.GetFileName(entry.Name)
                    If (fileName <> String.Empty) Then
                        Using stream2 As FileStream = File.Create(pathtosave + Path.GetFileName(entry.Name))
                            Dim count As Integer = 2048
                            Dim buffer As Byte() = New Byte(2048) {}
                            Do While True
                                count = stream.Read(buffer, 0, buffer.Length)
                                If (count <= 0) Then
                                    fullname = pathtosave + Path.GetFileName(entry.Name)
                                    GoTo Label_00CC
                                End If
                                stream2.Write(buffer, 0, count)
                            Loop
                        End Using
                    End If
                    entry = stream.GetNextEntry
                Loop
            End Using
            Dim fs As Interfaces.IFlowsheet
            fs = LoadXML(fullname, ProgressFeedBack, caminho, forcommandline)
            fs.FilePath = caminho
            fs.Options.FilePath = caminho
            File.Delete(fullname)
            Return fs
        Catch ex As Exception
            MessageBox.Show(ex.ToString, DWSIM.App.GetLocalString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return Nothing
        End Try

    End Function

    Sub SaveXMLZIP(ByVal zipfilename As String, ByVal form As FormFlowsheet)

        Dim xmlfile As String = Path.ChangeExtension(My.Computer.FileSystem.GetTempFileName, "xml")
        Me.SaveXML(xmlfile, form, zipfilename)

        Dim i_Files As ArrayList = New ArrayList()
        If File.Exists(xmlfile) Then i_Files.Add(xmlfile)

        Dim astrFileNames() As String = i_Files.ToArray(GetType(String))
        Dim strmZipOutputStream As ZipOutputStream

        strmZipOutputStream = New ZipOutputStream(File.Create(zipfilename))

        ' Compression Level: 0-9
        ' 0: no(Compression)
        ' 9: maximum compression
        strmZipOutputStream.SetLevel(9)

        'save with password, if set
        If form.Options.UsePassword Then strmZipOutputStream.Password = form.Options.Password

        Dim strFile As String

        For Each strFile In astrFileNames

            Dim strmFile As FileStream = File.OpenRead(strFile)
            Dim abyBuffer(strmFile.Length - 1) As Byte

            strmFile.Read(abyBuffer, 0, abyBuffer.Length)
            Dim objZipEntry As ZipEntry = New ZipEntry(Path.GetFileName(strFile))

            objZipEntry.DateTime = DateTime.Now
            objZipEntry.Size = strmFile.Length
            strmFile.Close()
            strmZipOutputStream.PutNextEntry(objZipEntry)
            strmZipOutputStream.Write(abyBuffer, 0, abyBuffer.Length)

        Next

        strmZipOutputStream.Finish()
        strmZipOutputStream.Close()

        File.Delete(xmlfile)

    End Sub

    Sub LoadFileDialog()

        If Me.OpenFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then

            LoadFile(OpenFileDialog1.FileName)

        End If

    End Sub

    Sub LoadFile(fpath As String)

        Dim floading As New FormLoadingSimulation

        floading.Label1.Text = DWSIM.App.GetLocalString("LoadingFile") & vbCrLf & "(" & fpath & ")"
        floading.Show()

        Application.DoEvents()

        Select Case Path.GetExtension(fpath).ToLower()
            Case ".dwxml"
                Dim myStream As System.IO.FileStream
                myStream = File.OpenRead(fpath)
                If Not (myStream Is Nothing) Then
                    Dim nome = myStream.Name
                    myStream.Close()
                    Me.filename = nome
                    'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + nome + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    Me.LoadXML(Me.filename, Sub(x)
                                                Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                            End Sub)
                End If
            Case ".dwxmz"
                Dim myStream As System.IO.FileStream
                myStream = File.OpenRead(fpath)
                If Not (myStream Is Nothing) Then
                    Dim nome = myStream.Name
                    myStream.Close()
                    Me.filename = nome
                    'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + nome + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    Me.LoadAndExtractXMLZIP(Me.filename, Sub(x)
                                                             Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                         End Sub)
                End If
            Case ".xml"
                Dim myStream As System.IO.FileStream
                myStream = File.OpenRead(fpath)
                If Not (myStream Is Nothing) Then
                    Dim nome = myStream.Name
                    myStream.Close()
                    Me.filename = nome
                    'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + nome + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    Me.LoadMobileXML(Me.filename)
                End If
            Case ".dwcsd"
                Application.DoEvents()
                Dim NewMDIChild As New FormCompoundCreator()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Dim objStreamReader As New FileStream(fpath, FileMode.Open, FileAccess.Read)
                Dim x As New BinaryFormatter()
                x.Binder = New VersionDeserializationBinder
                NewMDIChild.mycase = x.Deserialize(objStreamReader)
                NewMDIChild.mycase.Filename = fpath
                objStreamReader.Close()
                NewMDIChild.WriteData()
                If GlobalSettings.Settings.OldUI Then
                    If Not My.Settings.MostRecentFiles.Contains(fpath) Then
                        My.Settings.MostRecentFiles.Add(fpath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwcsd2"
                Application.DoEvents()
                Dim NewMDIChild As New FormCompoundCreator()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                NewMDIChild.mycase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of CompoundGeneratorCase)(File.ReadAllText(fpath))
                NewMDIChild.mycase.Filename = fpath
                NewMDIChild.WriteData()
                If GlobalSettings.Settings.OldUI Then
                    If Not My.Settings.MostRecentFiles.Contains(fpath) Then
                        My.Settings.MostRecentFiles.Add(fpath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwrsd"
                Application.DoEvents()
                Dim NewMDIChild As New FormDataRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Dim objStreamReader As New FileStream(fpath, FileMode.Open, FileAccess.Read)
                Dim x As New BinaryFormatter()
                x.Binder = New VersionDeserializationBinder
                NewMDIChild.currcase = x.Deserialize(objStreamReader)
                NewMDIChild.currcase.filename = fpath
                objStreamReader.Close()
                NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                If GlobalSettings.Settings.OldUI Then
                    If Not My.Settings.MostRecentFiles.Contains(fpath) Then
                        My.Settings.MostRecentFiles.Add(fpath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwrsd2"
                Application.DoEvents()
                Dim NewMDIChild As New FormDataRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                NewMDIChild.currcase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of DWSIM.Optimization.DatRegression.RegressionCase)(File.ReadAllText(fpath))
                NewMDIChild.currcase.filename = fpath
                NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                If GlobalSettings.Settings.OldUI Then
                    If Not My.Settings.MostRecentFiles.Contains(fpath) Then
                        My.Settings.MostRecentFiles.Add(fpath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwruf"
                Application.DoEvents()
                Dim NewMDIChild As New FormUNIFACRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Dim objStreamReader As New FileStream(fpath, FileMode.Open, FileAccess.Read)
                Dim x As New BinaryFormatter()
                NewMDIChild.mycase = x.Deserialize(objStreamReader)
                NewMDIChild.mycase.Filename = fpath
                objStreamReader.Close()
                NewMDIChild.LoadCase(NewMDIChild.mycase, False)
                If GlobalSettings.Settings.OldUI Then
                    If Not My.Settings.MostRecentFiles.Contains(fpath) Then
                        My.Settings.MostRecentFiles.Add(fpath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
        End Select

        floading.Close()

    End Sub

    Sub SaveBackup(sfile As String)

        If My.Settings.SaveBackupFile Then
            If File.Exists(sfile) Then
                Dim dfile = Path.GetDirectoryName(sfile) & Path.DirectorySeparatorChar & Path.GetFileNameWithoutExtension(sfile) & "_backup" & Path.GetExtension(sfile)
                File.Copy(sfile, dfile, True)
            End If
        End If

    End Sub

    Sub SaveFileDialog()

        If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
            Dim myStream As System.IO.FileStream
            Dim form2 As FormFlowsheet = Me.ActiveMdiChild
            If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
                SaveBackup(Me.SaveFileDialog1.FileName)
                myStream = Me.SaveFileDialog1.OpenFile()
                Me.filename = myStream.Name
                myStream.Close()
                If Not (myStream Is Nothing) Then
                    'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Salvandosimulao") + " (" + Me.filename + ")"
                    Application.DoEvents()
                    If Path.GetExtension(Me.filename).ToLower = ".dwxml" Then
                        Task.Factory.StartNew(Sub() SaveXML(Me.filename, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                                              'Me.ToolStripStatusLabel1.Text = ""
                                                                                                              If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                          End Sub, TaskContinuationOptions.ExecuteSynchronously)
                    ElseIf Path.GetExtension(Me.filename).ToLower = ".xml" Then
                        Task.Factory.StartNew(Sub() SaveMobileXML(Me.filename, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                                                    'Me.ToolStripStatusLabel1.Text = ""
                                                                                                                    If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                End Sub, TaskContinuationOptions.ExecuteSynchronously)
                    ElseIf Path.GetExtension(Me.filename).ToLower = ".dwxmz" Then
                        Task.Factory.StartNew(Sub() SaveXMLZIP(Me.filename, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                                                 ' Me.ToolStripStatusLabel1.Text = ""
                                                                                                                 If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                             End Sub, TaskContinuationOptions.ExecuteSynchronously)
                    Else
                        Me.bgSaveFile.RunWorkerAsync()
                    End If
                End If
            End If
        Else
            SaveFile(False)
        End If

    End Sub

    Private Sub OpenToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OpenToolStripButton.Click, OpenToolStripMenuItem.Click

        Me.LoadFileDialog()

    End Sub

#End Region

#Region "    Click Handlers"

    Private Sub RegistroCAPEOPENToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RegistroCAPEOPENToolStripMenuItem.Click, tsbRegCO.Click
        Dim f As New FormCORegistration
        f.ShowDialog(Me)
    End Sub

    Private Sub VerToolStripMenuItem_DropDownOpened(sender As Object, e As EventArgs) Handles VerToolStripMenuItem.DropDownOpened

        If Me.ActiveMdiChild IsNot Nothing Then
            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then DirectCast(Me.ActiveMdiChild, FormFlowsheet).UpdateToolstripItemVisibility()
        End If

    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles ButtonClose.Click
        Me.SettingsPanel.Visible = False
        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
    End Sub

    Private Sub LR1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)

        Dim myLink As LinkLabel = CType(sender, LinkLabel)

        If myLink.Text <> DWSIM.App.GetLocalString("vazio") Then
            Dim nome = myLink.Tag.ToString
            Me.filename = Name
            'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " (" + nome + ")"
            Application.DoEvents()
            Try
                Select Case Path.GetExtension(Me.filename).ToLower
                    Case ".dwsim"
                        'Me.LoadF(Me.filename)
                    Case ".dwcsd"
                        Dim NewMDIChild As New FormCompoundCreator()
                        NewMDIChild.MdiParent = Me
                        NewMDIChild.Show()
                        Dim objStreamReader As New FileStream(Me.filename, FileMode.Open)
                        Dim x As New BinaryFormatter()
                        NewMDIChild.mycase = x.Deserialize(objStreamReader)
                        objStreamReader.Close()
                        NewMDIChild.WriteData()
                        NewMDIChild.Activate()
                    Case ".dwrsd"
                        Dim NewMDIChild As New FormDataRegression()
                        NewMDIChild.MdiParent = Me
                        NewMDIChild.Show()
                        Dim objStreamReader As New FileStream(Me.filename, FileMode.Open)
                        Dim x As New BinaryFormatter()
                        NewMDIChild.currcase = x.Deserialize(objStreamReader)
                        objStreamReader.Close()
                        NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                        NewMDIChild.Activate()
                End Select
            Catch ex As Exception
                MessageBox.Show("Erro ao carregar arquivo: " & ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Finally
                'Me.ToolStripStatusLabel1.Text = ""
            End Try
            'Me.bgLoadFile.RunWorkerAsync()
        End If
    End Sub

    Public Sub NewToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewToolStripButton.Click, NewToolStripMenuItem.Click

        Dim newform As New FormFlowsheet()

        With newform
            .Text = "Simulation" & m_childcount
            .MdiParent = Me
            .Show()
            .MdiParent = Me
            Application.DoEvents()
        End With
        Me.ActivateMdiChild(newform)
        m_childcount += 1

    End Sub

    Private Sub CascadeToolStripMenuItem_Click(ByVal sender As Object, ByVal e As EventArgs) Handles CascadeToolStripMenuItem.Click
        Me.LayoutMdi(MdiLayout.Cascade)
        If Me.CascadeToolStripMenuItem.Checked = True Then
            Me.TileVerticalToolStripMenuItem.Checked = False
            Me.TileHorizontalToolStripMenuItem.Checked = False
        Else
            Me.TileHorizontalToolStripMenuItem.Checked = False
            Me.TileVerticalToolStripMenuItem.Checked = False
            Me.CascadeToolStripMenuItem.Checked = True
        End If
    End Sub

    Private Sub TileVerticleToolStripMenuItem_Click(ByVal sender As Object, ByVal e As EventArgs) Handles TileVerticalToolStripMenuItem.Click
        Me.LayoutMdi(MdiLayout.TileVertical)
        If Me.TileVerticalToolStripMenuItem.Checked = True Then
            Me.TileHorizontalToolStripMenuItem.Checked = False
            Me.CascadeToolStripMenuItem.Checked = False
        Else
            Me.TileHorizontalToolStripMenuItem.Checked = False
            Me.TileVerticalToolStripMenuItem.Checked = True
            Me.CascadeToolStripMenuItem.Checked = False
        End If
    End Sub

    Private Sub TileHorizontalToolStripMenuItem_Click(ByVal sender As Object, ByVal e As EventArgs) Handles TileHorizontalToolStripMenuItem.Click
        Me.LayoutMdi(MdiLayout.TileHorizontal)
        If Me.TileHorizontalToolStripMenuItem.Checked = True Then
            Me.TileVerticalToolStripMenuItem.Checked = False
            Me.CascadeToolStripMenuItem.Checked = False
        Else
            Me.TileHorizontalToolStripMenuItem.Checked = True
            Me.TileVerticalToolStripMenuItem.Checked = False
            Me.CascadeToolStripMenuItem.Checked = False
        End If
    End Sub

    Private Sub ExitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        Me.Close()
    End Sub

    Private Sub AboutToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AboutToolStripMenuItem.Click
        Dim frmAbout As New AboutBox
        frmAbout.ShowDialog(Me)
    End Sub

    Private Sub OpenRecent_click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim myLink As ToolStripMenuItem = CType(sender, ToolStripMenuItem)
        If myLink.Text <> DWSIM.App.GetLocalString("vazio") Then
            If File.Exists(myLink.Tag.ToString) Then

                Dim floading As New FormLoadingSimulation

                floading.Label1.Text = DWSIM.App.GetLocalString("LoadingFile") & vbCrLf & "(" & myLink.Tag.ToString & ")"
                floading.Show()

                Application.DoEvents()

                Dim nome = myLink.Tag.ToString
                'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " (" + nome + ")"
                Me.filename = nome
                Application.DoEvents()
                Dim objStreamReader As FileStream = Nothing
                Try
                    Select Case Path.GetExtension(nome).ToLower()
                        Case ".dwxml"
                            LoadXML(nome, Sub(x)
                                              Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                          End Sub)
                        Case ".dwxmz"
                            LoadAndExtractXMLZIP(nome, Sub(x)
                                                           Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                       End Sub)
                        Case ".dwsim"
                            ' Me.LoadF(nome)
                        Case ".xml"
                            LoadMobileXML(nome)
                        Case ".dwcsd"
                            Dim NewMDIChild As New FormCompoundCreator()
                            NewMDIChild.MdiParent = Me
                            NewMDIChild.Show()
                            objStreamReader = New FileStream(nome, FileMode.Open)
                            Dim x As New BinaryFormatter()
                            NewMDIChild.mycase = x.Deserialize(objStreamReader)
                            objStreamReader.Close()
                            NewMDIChild.mycase.Filename = nome
                            NewMDIChild.WriteData()
                        Case ".dwrsd"
                            Dim NewMDIChild As New FormDataRegression()
                            NewMDIChild.MdiParent = Me
                            NewMDIChild.Show()
                            objStreamReader = New FileStream(nome, FileMode.Open)
                            Dim x As New BinaryFormatter()
                            NewMDIChild.currcase = x.Deserialize(objStreamReader)
                            objStreamReader.Close()
                            NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                        Case ".dwruf"
                            Dim NewMDIChild As New FormUNIFACRegression()
                            NewMDIChild.MdiParent = Me
                            NewMDIChild.Show()
                            objStreamReader = New FileStream(nome, FileMode.Open)
                            Dim x As New BinaryFormatter()
                            NewMDIChild.mycase = x.Deserialize(objStreamReader)
                            NewMDIChild.mycase.Filename = nome
                            objStreamReader.Close()
                            NewMDIChild.LoadCase(NewMDIChild.mycase, False)
                    End Select
                Catch ex As Exception
                    MessageBox.Show("Erro ao carregar arquivo: " & ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    'Me.ToolStripStatusLabel1.Text = ""
                    If objStreamReader IsNot Nothing Then objStreamReader.Close()
                    floading.Close()
                End Try
            End If
        End If
    End Sub

    Private Sub OpenRecentFolder_click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim myLink As ToolStripMenuItem = CType(sender, ToolStripMenuItem)
        OpenFileDialog1.InitialDirectory = myLink.Tag
        LoadFileDialog()

    End Sub

    Private Sub SaveAllToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveAllToolStripButton.Click, SaveAllToolStripMenuItem.Click
        If Me.MdiChildren.Length > 0 Then
            Dim result As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Istoirsalvartodasass"), DWSIM.App.GetLocalString("Ateno2"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            If result = MsgBoxResult.Yes Then
                For Each form0 As Form In Me.MdiChildren
                    If TypeOf form0 Is FormFlowsheet Then
                        Dim form2 As FormFlowsheet = form0
                        If form2.Options.FilePath <> "" Then
                            'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Salvandosimulao") + " (" + Me.filename + ")"
                            SaveBackup(form2.Options.FilePath)
                            If Path.GetExtension(form2.Options.FilePath).ToLower = ".dwsim" Then
                                'Try
                                '    SaveF(form2.Options.FilePath, form2)
                                'Catch ex As Exception
                                '    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                'Finally
                                '    Me.ToolStripStatusLabel1.Text = ""
                                'End Try
                            ElseIf Path.GetExtension(form2.Options.FilePath).ToLower = ".dwxml" Then
                                Task.Factory.StartNew(Sub() SaveXML(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                     form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                 End Sub, TaskContinuationOptions.OnlyOnFaulted)
                            ElseIf Path.GetExtension(form2.Options.FilePath).ToLower = ".xml" Then
                                Task.Factory.StartNew(Sub() SaveMobileXML(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                           form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                       End Sub, TaskContinuationOptions.OnlyOnFaulted)
                            ElseIf Path.GetExtension(form2.Options.FilePath).ToLower = ".dwxmz" Then
                                Task.Factory.StartNew(Sub() SaveXMLZIP(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                        form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                    End Sub, TaskContinuationOptions.OnlyOnFaulted)
                            End If
                        Else
                            Dim myStream As System.IO.FileStream
                            If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
                                SaveBackup(Me.SaveFileDialog1.FileName)
                                myStream = Me.SaveFileDialog1.OpenFile()
                                myStream.Close()
                                If Not (myStream Is Nothing) Then
                                    ' Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Salvandosimulao") + " (" + Me.filename + ")"
                                    If Path.GetExtension(myStream.Name).ToLower = ".dwsim" Then
                                        'Try
                                        '    SaveF(myStream.Name, form2)
                                        'Catch ex As Exception
                                        '    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                        'Finally
                                        '    Me.ToolStripStatusLabel1.Text = ""
                                        'End Try
                                    ElseIf Path.GetExtension(myStream.Name).ToLower = ".dwxml" Then
                                        Task.Factory.StartNew(Sub() SaveXML(myStream.Name, form2)).ContinueWith(Sub(t)
                                                                                                                    form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                End Sub, TaskContinuationOptions.OnlyOnFaulted)
                                    ElseIf Path.GetExtension(myStream.Name).ToLower = ".xml" Then
                                        Task.Factory.StartNew(Sub() SaveMobileXML(myStream.Name, form2)).ContinueWith(Sub(t)
                                                                                                                          form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                      End Sub, TaskContinuationOptions.OnlyOnFaulted)
                                    ElseIf Path.GetExtension(myStream.Name).ToLower = ".dwxmz" Then
                                        Task.Factory.StartNew(Sub() SaveXMLZIP(myStream.Name, form2)).ContinueWith(Sub(t)
                                                                                                                       form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                   End Sub, TaskContinuationOptions.OnlyOnFaulted)
                                    End If
                                End If
                            End If
                        End If
                    ElseIf TypeOf form0 Is FormCompoundCreator Then
                        Dim filename As String = CType(Me.ActiveMdiChild, FormCompoundCreator).mycase.Filename
                        If filename = "" Then
                            If Me.SaveStudyDlg.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
                                SaveBackup(Me.SaveStudyDlg.FileName)
                                CType(Me.ActiveMdiChild, FormCompoundCreator).mycase.Filename = Me.SaveStudyDlg.FileName
                                CType(Me.ActiveMdiChild, FormCompoundCreator).StoreData()
                                Dim objStreamWriter As New FileStream(Me.SaveStudyDlg.FileName, FileMode.OpenOrCreate)
                                Dim x As New BinaryFormatter
                                x.Serialize(objStreamWriter, CType(Me.ActiveMdiChild, FormCompoundCreator).mycase)
                                objStreamWriter.Close()
                            End If
                        Else
                            SaveBackup(filename)
                            CType(Me.ActiveMdiChild, FormCompoundCreator).StoreData()
                            Dim objStreamWriter As New FileStream(filename, FileMode.OpenOrCreate)
                            Dim x As New BinaryFormatter
                            x.Serialize(objStreamWriter, CType(Me.ActiveMdiChild, FormCompoundCreator).mycase)
                            objStreamWriter.Close()
                        End If
                    ElseIf TypeOf form0 Is FormDataRegression Then
                        If Me.SaveRegStudyDlg.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
                            SaveBackup(Me.SaveRegStudyDlg.FileName)
                            Dim objStreamWriter As New FileStream(Me.SaveRegStudyDlg.FileName, FileMode.OpenOrCreate)
                            Dim x As New BinaryFormatter
                            x.Serialize(objStreamWriter, CType(form0, FormDataRegression).StoreCase())
                            objStreamWriter.Close()
                        End If
                    End If
                Next
            End If
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Noexistemsimulaesase"), DWSIM.App.GetLocalString("Informao"), MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub

    Public Sub SaveToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripButton.Click, SaveToolStripMenuItem.Click

        SaveFile(True)

    End Sub

    Public Sub SaveFile(ByVal saveasync As Boolean)

        If My.Computer.Keyboard.ShiftKeyDown Then saveasync = False

        If Not Me.ActiveMdiChild Is Nothing Then
            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                Dim form2 As FormFlowsheet = Me.ActiveMdiChild
                If form2.Options.FilePath <> "" Then
                    'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Salvandosimulao") + " (" + Me.filename + ")"
                    Application.DoEvents()
                    Me.filename = form2.Options.FilePath
                    SaveBackup(Me.filename)
                    If Path.GetExtension(Me.filename).ToLower = ".dwsim" Then
                        Try
                            'SaveF(form2.Options.FilePath, form2)
                        Catch ex As Exception
                            MessageBox.Show(DWSIM.App.GetLocalString("Erro"), ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Finally
                            'Me.ToolStripStatusLabel1.Text = ""
                        End Try
                    ElseIf Path.GetExtension(Me.filename).ToLower = ".dwxml" Then
                        If saveasync Then
                            Task.Factory.StartNew(Sub() SaveXML(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                 'Me.ToolStripStatusLabel1.Text = ""
                                                                                                                 If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                             End Sub, TaskContinuationOptions.ExecuteSynchronously)
                        Else
                            SaveXML(form2.Options.FilePath, form2)
                            'Me.ToolStripStatusLabel1.Text = ""
                        End If
                    ElseIf Path.GetExtension(Me.filename).ToLower = ".xml" Then
                        If saveasync Then
                            Task.Factory.StartNew(Sub() SaveMobileXML(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                       'Me.ToolStripStatusLabel1.Text = ""
                                                                                                                       If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                   End Sub, TaskContinuationOptions.ExecuteSynchronously)
                        Else
                            SaveMobileXML(form2.Options.FilePath, form2)
                            'Me.ToolStripStatusLabel1.Text = ""
                        End If
                    ElseIf Path.GetExtension(Me.filename).ToLower = ".dwxmz" Then
                        If saveasync Then
                            Task.Factory.StartNew(Sub() SaveXMLZIP(form2.Options.FilePath, form2)).ContinueWith(Sub(t)
                                                                                                                    'Me.ToolStripStatusLabel1.Text = ""
                                                                                                                    If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                                End Sub, TaskContinuationOptions.ExecuteSynchronously)
                        Else
                            SaveXMLZIP(form2.Options.FilePath, form2)
                            'Me.ToolStripStatusLabel1.Text = ""
                        End If
                    End If
                Else
                    Dim myStream As System.IO.FileStream
                    If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
                        SaveBackup(Me.SaveFileDialog1.FileName)
                        myStream = Me.SaveFileDialog1.OpenFile()
                        Me.filename = myStream.Name
                        myStream.Close()
                        If Not (myStream Is Nothing) Then
                            'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Salvandosimulao") + " (" + Me.filename + ")"
                            Application.DoEvents()
                            If Path.GetExtension(Me.filename).ToLower = ".dwsim" Then
                                Try
                                    'SaveF(myStream.Name, form2)
                                Catch ex As Exception
                                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                                Finally
                                    'Me.ToolStripStatusLabel1.Text = ""
                                End Try
                            ElseIf Path.GetExtension(Me.filename).ToLower = ".dwxml" Then
                                If saveasync Then
                                    Task.Factory.StartNew(Sub() SaveXML(myStream.Name, form2)).ContinueWith(Sub(t)
                                                                                                                form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                            End Sub, TaskContinuationOptions.OnlyOnFaulted)
                                Else
                                    SaveXML(myStream.Name, form2)
                                    'Me.ToolStripStatusLabel1.Text = ""
                                End If
                            ElseIf Path.GetExtension(Me.filename).ToLower = ".dwxmz" Then
                                If saveasync Then
                                    Task.Factory.StartNew(Sub() SaveXMLZIP(myStream.Name, form2)).ContinueWith(Sub(t)
                                                                                                                   form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                               End Sub, TaskContinuationOptions.OnlyOnFaulted)
                                Else
                                    SaveXMLZIP(myStream.Name, form2)
                                    'Me.ToolStripStatusLabel1.Text = ""
                                End If
                            End If
                        End If
                    End If
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormCompoundCreator Then
                Dim filename As String = CType(Me.ActiveMdiChild, FormCompoundCreator).mycase.Filename
                If filename = "" Then
                    If Me.SaveStudyDlg.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
                        SaveBackup(Me.SaveStudyDlg.FileName)
                        CType(Me.ActiveMdiChild, FormCompoundCreator).mycase.Filename = Me.SaveStudyDlg.FileName
                        CType(Me.ActiveMdiChild, FormCompoundCreator).StoreData()
                        File.WriteAllText(Me.SaveStudyDlg.FileName, Newtonsoft.Json.JsonConvert.SerializeObject(CType(Me.ActiveMdiChild, FormCompoundCreator).mycase, Newtonsoft.Json.Formatting.Indented))
                        Me.filename = Me.SaveStudyDlg.FileName
                        Me.ActiveMdiChild.Text = Me.filename
                    End If
                Else
                    filename = Path.ChangeExtension(filename, "dwcsd2")
                    SaveBackup(filename)
                    CType(Me.ActiveMdiChild, FormCompoundCreator).StoreData()
                    File.WriteAllText(filename, Newtonsoft.Json.JsonConvert.SerializeObject(CType(Me.ActiveMdiChild, FormCompoundCreator).mycase, Newtonsoft.Json.Formatting.Indented))
                    Me.filename = filename
                    Me.ActiveMdiChild.Text = filename
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormDataRegression Then
                If Me.SaveRegStudyDlg.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
                    SaveBackup(Me.SaveRegStudyDlg.FileName)
                    File.WriteAllText(Me.SaveRegStudyDlg.FileName, Newtonsoft.Json.JsonConvert.SerializeObject(CType(Me.ActiveMdiChild, FormDataRegression).StoreCase(), Newtonsoft.Json.Formatting.Indented))
                    Me.filename = Me.SaveRegStudyDlg.FileName
                    Me.ActiveMdiChild.Text = Me.filename
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormUNIFACRegression Then
                If Me.SaveUnifacIPRegrDlg.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
                    SaveBackup(Me.SaveUnifacIPRegrDlg.FileName)
                    CType(Me.ActiveMdiChild, FormUNIFACRegression).StoreData()
                    Dim objStreamWriter As New FileStream(Me.SaveUnifacIPRegrDlg.FileName, FileMode.OpenOrCreate)
                    Dim x As New BinaryFormatter
                    x.Serialize(objStreamWriter, CType(Me.ActiveMdiChild, FormUNIFACRegression).mycase)
                    objStreamWriter.Close()
                    Me.filename = Me.SaveUnifacIPRegrDlg.FileName
                    Me.ActiveMdiChild.Text = Me.filename
                End If
            End If
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Noexistemsimulaesati"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If

    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click, SaveAsToolStripMenuItem.Click
        Call Me.SaveFileDialog()
    End Sub

    Private Sub FecharTodasAsSimulacoesAbertasToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CloseAllToolstripMenuItem.Click
        If Me.MdiChildren.Length > 0 Then
            Dim form2 As Form
            For Each form2 In Me.MdiChildren
                Application.DoEvents()
                Try
                    form2.Close()
                Catch ex As Exception
                    Console.WriteLine(ex.ToString)
                End Try
                Application.DoEvents()
            Next
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Noexistemsimulaesase"), DWSIM.App.GetLocalString("Informao"), MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub

    Private Sub ManualTecnicoToolStripMenuItem_Click(sender As Object, e As EventArgs)
        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "tech_manual.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "tech_manual.pdf")
        End If
    End Sub

    Private Sub GuiaDoUsuarioToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GuiaDoUsuarioToolStripMenuItem.Click
        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "User_Guide.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "user_guide.pdf")
        End If
    End Sub

    Private Sub WikiToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles WikiToolStripMenuItem.Click
        System.Diagnostics.Process.Start("http://dwsim.inforside.com.br")
    End Sub

    Private Sub ForumToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ForumToolStripMenuItem.Click
        System.Diagnostics.Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Support")
    End Sub

    Private Sub RastreamentoDeBugsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RastreamentoDeBugsToolStripMenuItem.Click
        System.Diagnostics.Process.Start("https://sourceforge.net/p/dwsim/tickets/")
    End Sub

    Private Sub DonateToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        System.Diagnostics.Process.Start("https://gumroad.com/products/PTljX")
    End Sub

    Private Sub MostrarBarraDeFerramentasToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MostrarBarraDeFerramentasToolStripMenuItem.Click
        If Me.MostrarBarraDeFerramentasToolStripMenuItem.Checked Then
            Me.ToolStrip1.Visible = True
        Else
            Me.ToolStrip1.Visible = False
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        Me.PreferenciasDoDWSIMToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        Me.CascadeToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        Me.TileVerticleToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        Me.TileHorizontalToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ToolStripButton7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton7.Click
        Me.DonateToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ToolStripButton8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton8.Click
        Me.AboutToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub ContentsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ContentsToolStripMenuItem.Click
        'call general help
        DWSIM.App.HelpRequested("Frame.htm")
    End Sub

    Private Sub tslupd_Click(ByVal sender As Object, ByVal e As System.EventArgs)
        Dim myfile As String = My.Computer.FileSystem.SpecialDirectories.Temp & Path.DirectorySeparatorChar & "DWSIM" & Path.DirectorySeparatorChar & "dwsim.txt"
        Dim txt() As String = File.ReadAllLines(myfile)
        Dim build As Integer, bdate As Date, fname As String, dlpath As String, changelog As String = ""
        build = txt(0)
        bdate = Date.Parse(txt(1), New CultureInfo("en-US"))
        dlpath = txt(2)
        fname = txt(3)
        For i As Integer = 4 To txt.Length - 1
            changelog += txt(i) + vbCrLf
        Next
        Dim strb As New StringBuilder()
        With strb
            .AppendLine(DWSIM.App.GetLocalString("BuildNumber") & ": " & build & vbCrLf)
            .AppendLine(DWSIM.App.GetLocalString("BuildDate") & ": " & bdate.ToString(My.Application.Culture.DateTimeFormat.ShortDatePattern, My.Application.Culture) & vbCrLf)
            .AppendLine(DWSIM.App.GetLocalString("Changes") & ": " & vbCrLf & changelog & vbCrLf)
            .AppendLine(DWSIM.App.GetLocalString("DownloadQuestion"))
        End With
        Dim msgresult As MsgBoxResult = MessageBox.Show(strb.ToString, DWSIM.App.GetLocalString("NewVersionAvailable"), MessageBoxButtons.YesNo, MessageBoxIcon.Information)
        If msgresult = MsgBoxResult.Yes Then
            Process.Start(dlpath)
            'tslupd.Visible = False
        End If
    End Sub

    Private Sub NovoEstudoDoCriadorDeComponentesToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NovoEstudoDoCriadorDeComponentesToolStripMenuItem.Click
        Dim NewMDIChild As New FormCompoundCreator()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me
        'Display the new form.
        NewMDIChild.Text = "CompCreator" & m_childcount
        Me.ActivateMdiChild(NewMDIChild)
        NewMDIChild.Show()
        m_childcount += 1
    End Sub

    Private Sub NovoEstudoDeRegressaoDeDadosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NovoEstudoDeRegressaoDeDadosToolStripMenuItem.Click
        Dim NewMDIChild As New FormDataRegression()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me
        'Display the new form.
        NewMDIChild.Text = "DataRegression" & m_childcount
        Me.ActivateMdiChild(NewMDIChild)
        NewMDIChild.Show()
        m_childcount += 1
    End Sub

    Private Sub NovoRegressaoUNIFACIPs_Click(sender As Object, e As EventArgs) Handles NovoRegressaoUNIFACIPs.Click
        Dim NewMDIChild As New FormUNIFACRegression()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me
        'Display the new form.
        NewMDIChild.Text = "UNIFAC IP Regression" & m_childcount
        Me.ActivateMdiChild(NewMDIChild)
        NewMDIChild.Show()
        m_childcount += 1
    End Sub
    Private Sub DatabaseManagerToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles DatabaseManagerToolStripMenuItem.Click
        If My.Settings.UserDatabases.Count > 0 Then
            FormDBManager.DBPath = My.Settings.UserDatabases.Item(0)
            FormDBManager.ShowDialog()
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("ErrorNoUserdatabase"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If
    End Sub
    Private Sub PreferenciasDoDWSIMToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PreferenciasDoDWSIMToolStripMenuItem.Click
        Me.SettingsPanel.Visible = True
    End Sub

    Private Sub FormMain_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        'Load help - no special topic
        DWSIM.App.HelpRequested("Frame.htm")
    End Sub
#End Region

#Region "    Backup/Update"

    Private Sub TimerBackup_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles TimerBackup.Tick

        Dim folder As String = My.Settings.BackupFolder
        If Not Directory.Exists(folder) And folder <> "" Then
            Try
                Directory.CreateDirectory(folder)
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erroaocriardiretriop") & vbCrLf & DWSIM.App.GetLocalString("Verifiquesevoctemonv"),
                             DWSIM.App.GetLocalString("Cpiasdesegurana"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

        If Directory.Exists(folder) Then
            If Not Me.bgSaveBackup.IsBusy Then Me.bgSaveBackup.RunWorkerAsync()
        End If

    End Sub

    Private Sub bgSaveBackup_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles bgSaveBackup.DoWork
        If Not My.Application.CalculatorBusy Then
            Dim bw As BackgroundWorker = CType(sender, BackgroundWorker)
            Dim folder As String = My.Settings.BackupFolder
            If Not Directory.Exists(My.Settings.BackupFolder) Then Directory.CreateDirectory(My.Settings.BackupFolder)
            Dim path As String = ""
            For Each form0 As Form In Me.MdiChildren
                If TypeOf form0 Is FormFlowsheet Then
                    path = folder + IO.Path.DirectorySeparatorChar + CType(form0, FormFlowsheet).Options.BackupFileName
                    Me.SaveXMLZIP(path, form0)
                    If Not My.Settings.BackupFiles.Contains(path) Then
                        My.Settings.BackupFiles.Add(path)
                        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
                    End If
                End If
            Next
        End If
    End Sub

    Private Sub PatronToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PatronToolStripMenuItem.Click
        System.Diagnostics.Process.Start("https://patreon.com/dwsim")
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles ButtonClose2.Click
        Me.WelcomePanel.Visible = False
        Me.PainelDeBoasvindasToolStripMenuItem.Checked = False
    End Sub

    Private Sub PainelDeBoasvindasToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PainelDeBoasvindasToolStripMenuItem.Click
        If Me.PainelDeBoasvindasToolStripMenuItem.Checked Then
            Me.WelcomePanel.Visible = True
        Else
            Me.WelcomePanel.Visible = False
        End If
    End Sub

    Private Sub ButtonCloseWeb_Click(sender As Object, e As EventArgs) Handles ButtonCloseWeb.Click
        Me.WebPanel.Visible = False
        Me.PainelDaWebToolStripMenuItem.Checked = False
    End Sub

    Private Sub PainelDaWebToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PainelDaWebToolStripMenuItem.Click
        If Me.PainelDaWebToolStripMenuItem.Checked Then
            Me.WebPanel.Visible = True
        Else
            Me.WebPanel.Visible = False
        End If
    End Sub

    Private Sub bgSaveBackup_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles bgSaveBackup.RunWorkerCompleted
        If Not (e.Error Is Nothing) Then
            ' There was an error during the operation.
            Console.WriteLine("Error saving backup file: " & e.Error.Message)
        End If
    End Sub

    Private Sub tsbInspector_CheckedChanged(sender As Object, e As EventArgs) Handles tsbInspector.CheckedChanged
        GlobalSettings.Settings.InspectorEnabled = tsbInspector.Checked
        FrmOptions.chkEnableInspector.Checked = tsbInspector.Checked
    End Sub

#End Region

End Class
