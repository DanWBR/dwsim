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
Imports DWSIM.Interfaces
Imports DWSIM.Thermodynamics.AdvancedEOS
Imports DWSIM.Thermodynamics.Databases
Imports DWSIM.Simulate365.Models
Imports DWSIM.Simulate365.Services
Imports DWSIM.Simulate365.FormFactories
Imports Microsoft.VisualBasic.ApplicationServices
Imports DWSIM.ProFeatures
Imports DWSIM.SharedClassesCSharp.FilePicker.Windows

Public Class FormMain

    Inherits Form

    Public Shared m_childcount As Integer = 1
    Public loadedCSDB As Boolean = False
    Public pathsep As Char

    Public FrmOptions As FormOptions
    Public FrmWelcome As FormWelcome
    Public FrmRec As FormRecoverFiles

    Private dropdownlist As ArrayList

    Public CancelClosing As Boolean = False

    Public WithEvents timer1 As New Timer

    Public calculatorassembly, unitopassembly As Assembly
    Public aTypeList As New List(Of Type)

    Public SampleList As New List(Of String)
    Public FOSSEEList As New List(Of FOSSEEFlowsheet)

    Public Shared TranslateFunction As Func(Of String, String)

    Public Shared TranslateFormFunction As Action(Of Object)

    Public Shared Property IsPro As Boolean = False

    'Collections

    Public AvailableComponents As New Dictionary(Of String, Interfaces.ICompoundConstantProperties)

    Public AvailableUnitSystems As New Dictionary(Of String, SystemsOfUnits.Units)

    Public PropertyPackages As New Dictionary(Of String, PropertyPackages.PropertyPackage)

    Public FlashAlgorithms As New Dictionary(Of String, Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)

    Public Property ExternalUnitOperations As New Dictionary(Of String, Interfaces.IExternalUnitOperation)

    Public Property Extenders As New Dictionary(Of String, IExtenderCollection)

    Public COMonitoringObjects As New Dictionary(Of String, UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)

    Public ObjectList As New Dictionary(Of String, Interfaces.ISimulationObject)

    Public Property MostRecentFiles As Specialized.StringCollection

    Public Property AnalyticsProvider As IAnalyticsProvider

    Public Shared ExternalSolvers As New Dictionary(Of String, Interfaces.IExternalSolverIdentification)

#Region "    Form Events"

    Public Event ToolOpened(sender As Object, e As EventArgs)

    Public Event FlowsheetSavingToXML(sender As Object, e As EventArgs)

    Public Event FlowsheetSavedToXML(sender As Object, e As EventArgs)

    Public Event FlowsheetLoadingFromXML(sender As Object, e As EventArgs)

    Public Event FlowsheetLoadedFromXML(sender As Object, e As EventArgs)

    Public SavingSimulation As Func(Of IFlowsheet, Boolean)

    Public Shared WebView2Environment As Microsoft.Web.WebView2.Core.CoreWebView2Environment

    Public Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

#If LINUX = False Then
        InitializeWebView2Environment()
#End If

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            Me.MenuStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.MenuStrip1.Size = New Size(MenuStrip1.Width, 28 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()
            Me.StatusStrip1.AutoSize = False
            Me.StatusStrip1.Size = New Size(StatusStrip1.Width, 22 * Settings.DpiScale)
            Me.StatusStrip1.Invalidate()

        End Using

        unvell.ReoGrid.Editor.Common.Shared.DpiScale = Settings.DpiScale

        MostRecentFiles = My.Settings.MostRecentFiles

        ' Set default file picker
        ' SharedClassesCSharp.FilePicker.FilePickerService.GetInstance().SetFilePickerFactory(Function() New Simulate365.FormFactories.S365FilePickerForm())

        If GlobalSettings.Settings.OldUI Then

            calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

            aTypeList.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))
            aTypeList.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing And
                                                                   Not x.IsAbstract And x.GetInterface("DWSIM.Interfaces.IExternalUnitOperation") Is Nothing, True, False)))

            For Each item In aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    ObjectList.Add(obj.GetDisplayName(), obj)
                End If
            Next

            For Each item In ExternalUnitOperations.Values.OrderBy(Function(x) x.Name)
                If Not ObjectList.ContainsKey(item.Name) Then ObjectList.Add(item.Name, item)
            Next

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

            tsmiFreeProTrial.Visible = Not IsPro
            tsmiPrivateSupport.Visible = Not IsPro

#If LINUX = False Then
            If IsPro Then
                DownloadSupplementarySoftwareToolStripMenuItem.Visible = False
                StatusStrip1.Visible = False
                tsbRegCO.Visible = False
                RegistroCAPEOPENToolStripMenuItem.Enabled = False
                DashboardToolStripMenuItem.Visible = False
                tsmiProUG.Visible = False
                DatabaseManagerToolStripMenuItem.Visible = False
                ZedGraph.Variables.IsDWSIMPro = True
                ZedGraph.Variables.IsDWSIMPro = True
            End If
#End If

            'Search and populate CAPE-OPEN Flowsheet Monitoring Object collection
            'SearchCOMOs() 'doing this only when the user hovers the mouse over the plugins toolstrip menu item

            If My.Settings.ScriptPaths Is Nothing Then My.Settings.ScriptPaths = New Collections.Specialized.StringCollection()

            Me.FrmOptions = New FormOptions
            Me.FrmOptions.Dock = DockStyle.Fill
            Me.SettingsPanel.Controls.Add(Me.FrmOptions)
            Me.ButtonClose.BringToFront()

            tsbInspector.Checked = GlobalSettings.Settings.InspectorEnabled

            SetupWelcomeScreen()

        End If

    End Sub

    Private Sub InitializeWebView2Environment()

        Try
            Dim newUserFolder = System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "DWSIM", "BrowserData")
            WebView2Environment = Microsoft.Web.WebView2.Core.CoreWebView2Environment.CreateAsync(Nothing, newUserFolder, Nothing).Result
        Catch ex As Exception
            AnalyticsProvider?.RegisterError("Failed to Initialize WebView2 Environment", ex.Message, ex, Nothing)
        End Try

    End Sub

    Private Sub UpdateIcon()


#If LINUX = False Then
        If Not IsPro Then
            Icon = My.Resources.DWSIM_Icon_41
        Else
            Icon = My.Resources.Icon1282
        End If
#End If

    End Sub

    Private Sub LoadExtenders()

        ' On user details loaded
        AddHandler UserService.GetInstance().UserDetailsLoaded, AddressOf UserService_UserDetailsLoaded
        AddHandler UserService.GetInstance().AutoLoginInProgressChanged, AddressOf UserService_AutoLoginInProgress
        AddHandler UserService.GetInstance().UserLoggedOut, AddressOf UserService_UserLoggedOut
        AddHandler UserService.GetInstance().ShowLoginForm, AddressOf UserService_ShowLoginForm
        AddHandler FileManagementService.GetInstance().OnSaveFileToDashboard, AddressOf FileManagementService_SaveFileToDashboard

#If Not WINE32 Then

        'load extenders

        Dim extlist As List(Of IExtenderCollection) = GetExtenders(LoadExtenderDLLs())

        For Each extender In extlist
            Extenders.Add(extender.ID, extender)
            Try
                If extender.Level = ExtenderLevel.MainWindow Then
                    If extender.Category <> ExtenderCategory.InitializationScript Then
                        Dim newmenuitem As ToolStripMenuItem = Nothing
                        If extender.Category = ExtenderCategory.NewItem Then
                            For Each item As ToolStripMenuItem In MenuStrip1.Items
                                If item.Text = extender.DisplayText Then
                                    newmenuitem = item
                                    Exit For
                                End If
                            Next
                            If newmenuitem Is Nothing Then
                                newmenuitem = New ToolStripMenuItem()
                                newmenuitem.Text = extender.DisplayText
                                newmenuitem.DisplayStyle = ToolStripItemDisplayStyle.Text
                                If TypeOf extender Is IExtenderCollection2 Then
                                    DirectCast(extender, IExtenderCollection2).SetMenuItem(extender)
                                End If
                            End If
                        End If
                        For Each item In extender.Collection
                            item.SetMainWindow(Me)
                            Dim exttsmi As New ToolStripMenuItem
                            exttsmi.Text = item.DisplayText
                            exttsmi.Image = item.DisplayImage
                            AddHandler exttsmi.Click, Sub(s2, e2)
                                                          item.Run()
                                                      End Sub
                            If TypeOf item Is IExtender2 Then
                                DirectCast(item, IExtender2).SetMenuItem(exttsmi)
                            End If
                            Select Case extender.Category
                                Case ExtenderCategory.File
                                    If item.InsertAtPosition >= 0 Then
                                        exttsmi.MergeAction = MergeAction.Insert
                                        exttsmi.MergeIndex = item.InsertAtPosition
                                        FileTSMI.DropDownItems.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        FileTSMI.DropDownItems.Add(exttsmi)
                                    End If
                                Case ExtenderCategory.Edit
                                    If item.InsertAtPosition >= 0 Then
                                        exttsmi.MergeAction = MergeAction.Insert
                                        exttsmi.MergeIndex = item.InsertAtPosition
                                        EditTSMI.DropDownItems.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        EditTSMI.DropDownItems.Add(exttsmi)
                                    End If
                                Case ExtenderCategory.Tools
                                    If item.InsertAtPosition >= 0 Then
                                        exttsmi.MergeAction = MergeAction.Insert
                                        exttsmi.MergeIndex = item.InsertAtPosition
                                        ToolsTSMI.DropDownItems.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        ToolsTSMI.DropDownItems.Add(exttsmi)
                                    End If
                                Case ExtenderCategory.Help
                                    If item.InsertAtPosition >= 0 Then
                                        exttsmi.MergeAction = MergeAction.Insert
                                        exttsmi.MergeIndex = item.InsertAtPosition
                                        HelpTSMI.DropDownItems.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        HelpTSMI.DropDownItems.Add(exttsmi)
                                    End If
                                Case ExtenderCategory.NewItem
                                    newmenuitem?.DropDownItems.Add(exttsmi)
                            End Select
                        Next
                        If newmenuitem IsNot Nothing AndAlso Not MenuStrip1.Items.Contains(newmenuitem) Then
                            If TypeOf extender Is IExtenderCollection2 Then
                                Dim insertidx = DirectCast(extender, IExtenderCollection2).InsertAtPosition
                                newmenuitem.MergeAction = MergeAction.Insert
                                newmenuitem.MergeIndex = insertidx
                            End If
                            MenuStrip1.Items.Add(newmenuitem)
                        End If
                    Else
                        For Each item In extender.Collection
                            item.SetMainWindow(Me)
                            item.Run()
                        Next
                    End If
                End If
            Catch ex As Exception
                Logging.Logger.LogError("Extender Initialization", ex)
            End Try
        Next

#End If

    End Sub

    Private Sub FileManagementService_SaveFileToDashboard(sender As Object, e As EventArgs)
        Me.SaveFile(True, True)

    End Sub

    Private Sub UserService_UserDetailsLoaded(sender As Object, user As UserDetailsModel)
        Me.UIThread(Sub()
                        Me.LoginButton.Visible = False
                        Me.LogoutDropdown.Text = user.DisplayName
                        Me.LogoutDropdown.Visible = True
                    End Sub)
    End Sub

    Private Sub UserService_AutoLoginInProgress(sender As Object, isInProgress As Boolean)

        If (isInProgress) Then
            Me.UIThread(Sub()
                            Me.LoginButton.Visible = False
                            Me.CheckingCredentialsLabel.Visible = True
                            Me.LogoutDropdown.Visible = False
                        End Sub)
        Else
            Me.UIThread(Sub()
                            Me.LoginButton.Visible = True
                            Me.CheckingCredentialsLabel.Visible = False
                            Me.LogoutDropdown.Visible = False
                        End Sub)
        End If

    End Sub

    Private Sub UserService_UserLoggedOut(sender As Object, e As EventArgs)
        Me.UIThread(Sub()
                        Me.LogoutDropdown.Text = ""
                        Me.LogoutDropdown.Visible = False
                        Me.LoginButton.Visible = True
                    End Sub)
    End Sub

    Private Sub UserService_ShowLoginForm(sender As Object, e As EventArgs)

        Dim loginForm = New LoginForm
        loginForm.ShowDialog()

    End Sub




    Private Sub FormMain_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        If e.Data.GetDataPresent(DataFormats.FileDrop) Then
            Dim MyFiles() As String
            Dim i As Integer
            ' Assign the files to an array.
            MyFiles = e.Data.GetData(DataFormats.FileDrop)
            ' Loop through the array and add the files to the list.
            For i = 0 To MyFiles.Length - 1
                Dim handler = New SharedClassesCSharp.FilePicker.Windows.WindowsFile(MyFiles(i))
                Select Case Path.GetExtension(MyFiles(i)).ToLower
                    Case ".dwxml"
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + MyFiles(i) + "..."
                        Application.DoEvents()
                        Me.LoadXML(handler, Nothing)
                    Case ".dwxmz"
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + MyFiles(i) + "..."
                        Application.DoEvents()
                        Me.LoadAndExtractXMLZIP(handler, Nothing)
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

            Try
                If Eto.Forms.Application.Instance.Platform.IsWpf Then
                    System.Windows.Threading.Dispatcher.CurrentDispatcher.InvokeShutdown()
                End If
            Catch ex As Exception
            End Try

            'release yeppp! resources
            Try
                If My.Settings.UseSIMDExtensions Then Yeppp.Library.Release()
            Catch ex As Exception
            End Try

        End If

    End Sub

    Private Sub MyApplication_UnhandledException(ByVal sender As Object, ByVal e As System.Threading.ThreadExceptionEventArgs)
        Try
            Dim frmEx As New FormUnhandledException
            frmEx.ex = e.Exception
            frmEx.ShowDialog()
        Finally

        End Try
    End Sub

    Private Sub MyApplication_UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)
        Try
            Dim frmEx As New FormUnhandledException
            frmEx.ex = e.ExceptionObject
            frmEx.ShowDialog()
        Catch ex As Exception

        End Try
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

    Private Sub SetupWelcomeScreen()

        Dim splfile = Path.Combine(Utility.GetExtendersRootDirectory(), "WelcomeScreen.dll")

        If File.Exists(splfile) Then

            Dim types = Assembly.LoadFrom(splfile).GetExportedTypes()

            Dim tList As List(Of Type) = types.ToList().FindAll(Function(t) t.GetInterfaces().Contains(GetType(IWelcomeScreen)))

            Dim lst = tList.ConvertAll(Function(t As Type) TryCast(Activator.CreateInstance(t), IWelcomeScreen))

            lst(0).SetMainForm(Me)

            Dim ucontrol = lst(0).GetWelcomeScreen()

            ucontrol.Dock = DockStyle.Fill

            Me.WelcomePanel.Controls.Add(ucontrol)

            My.Settings.CheckForUpdates = False

        Else

            Me.FrmWelcome = New FormWelcome
            Me.FrmWelcome.Owner = Me
            Me.FrmWelcome.Dock = DockStyle.Fill
            Me.WelcomePanel.Controls.Add(Me.FrmWelcome)

            My.Settings.CheckForUpdates = True

        End If

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
                availableTypes.AddRange(currentAssembly.GetExportedTypes())
            Catch ex As ReflectionTypeLoadException
                Dim errstr As New StringBuilder()
                For Each lex As Exception In ex.LoaderExceptions
                    errstr.AppendLine(lex.ToString)
                Next
                Console.WriteLine("Error loading plugin '" & currentAssembly.FullName & "': " & errstr.ToString)
                Logging.Logger.LogError("Plugin Initialization", ex)
            End Try
        Next

        Dim pluginlist As List(Of Type) = availableTypes.FindAll(AddressOf isPlugin)

        Return pluginlist.ConvertAll(Of Interfaces.IUtilityPlugin)(Function(t As Type) TryCast(Activator.CreateInstance(t), Interfaces.IUtilityPlugin))

    End Function

    Function isPlugin(ByVal t As Type)
        Dim interfaceTypes As New List(Of Type)(t.GetInterfaces())
        Return (interfaceTypes.Contains(GetType(Interfaces.IUtilityPlugin)))
    End Function

    Private Function LoadExtenderDLLs() As List(Of Assembly)

        Dim extenderdlls As List(Of Assembly) = New List(Of Assembly)

        If Directory.Exists(Utility.GetExtendersRootDirectory()) Then

            Dim dinfo As New DirectoryInfo(Utility.GetExtendersRootDirectory())

            Dim files() As FileInfo = dinfo.GetFiles("*Extensions*.dll")

            If Not files Is Nothing Then
                For Each fi As FileInfo In files
                    extenderdlls.Add(Assembly.LoadFrom(fi.FullName))
                Next
            End If

        End If

        Return extenderdlls

    End Function

    Function GetExtenders(ByVal alist As List(Of Assembly)) As List(Of IExtenderCollection)

        Dim availableTypes As New List(Of Type)()

        For Each currentAssembly As Assembly In alist
            Try
                availableTypes.AddRange(currentAssembly.GetExportedTypes())
            Catch ex As Exception
                Logging.Logger.LogError("Extender Loading (MainForm)", ex)
            End Try
        Next

        'analytics provider

        Dim aprov As List(Of Type) = availableTypes.FindAll(Function(t) t.GetInterfaces().Contains(GetType(Interfaces.IAnalyticsProvider)))

        Dim aprovinst = aprov.ConvertAll(Of IAnalyticsProvider)(Function(t As Type) TryCast(Activator.CreateInstance(t), IAnalyticsProvider))

        If aprovinst.Count > 0 Then
            AnalyticsProvider = aprovinst(0)
        End If

        'external solvers

        Dim eslist As List(Of Type) = availableTypes.FindAll(Function(t) t.GetInterfaces().Contains(GetType(Interfaces.IExternalSolverIdentification)))

        Dim esinstances = eslist.ConvertAll(Of IExternalSolverIdentification)(Function(t As Type) TryCast(Activator.CreateInstance(t), IExternalSolverIdentification))

        For Each es In esinstances
            ExternalSolvers.Add(es.ID, es)
        Next

        'extenders

        Dim extList As List(Of Type) = availableTypes.FindAll(Function(t) t.GetInterfaces().Contains(GetType(Interfaces.IExtenderCollection)))

        Return extList.ConvertAll(Of IExtenderCollection)(Function(t As Type) TryCast(Activator.CreateInstance(t), IExtenderCollection))

    End Function

    Private Sub UpdateMRUList()

        'process MRU file list

        If My.Settings.MostRecentFiles.Count > 10 Then
            My.Settings.MostRecentFiles.RemoveAt(0)
        End If

        Dim j As Integer = 0
        For Each k As String In Me.dropdownlist
            Dim tsmi As ToolStripItem = Me.FileTSMI.DropDownItems(Convert.ToInt32(k - j))
            If tsmi.DisplayStyle = ToolStripItemDisplayStyle.Text Then
                Me.FileTSMI.DropDownItems.Remove(tsmi)
                j = j + 1
            End If
        Next

        Me.dropdownlist.Clear()

        Dim toremove As New ArrayList

        Dim tsindex = FileTSMI.DropDownItems.IndexOf(tsFileSeparator)

        If Not My.Settings.MostRecentFiles Is Nothing Then
            For Each str As String In My.Settings.MostRecentFiles
                If File.Exists(str) Then
                    Dim tsmi As New ToolStripMenuItem
                    With tsmi
                        .Text = str
                        .Tag = str
                        .DisplayStyle = ToolStripItemDisplayStyle.Text
                    End With
                    Me.FileTSMI.DropDownItems.Insert(tsindex, tsmi)
                    Me.dropdownlist.Add(Me.FileTSMI.DropDownItems.Count - 2)
                    AddHandler tsmi.Click, AddressOf Me.OpenRecent_click
                Else
                    toremove.Add(str)
                End If
            Next
            For Each s As String In toremove
                My.Settings.MostRecentFiles.Remove(s)
            Next
            If My.Settings.MostRecentFiles.Count > 0 Then
                Me.dropdownlist.Add(Me.FileTSMI.DropDownItems.Count - 2)
            End If
        Else
            My.Settings.MostRecentFiles = New System.Collections.Specialized.StringCollection
        End If

    End Sub

    Sub AddExternalUOs()

        Dim otheruos = SharedClasses.Utility.LoadAdditionalUnitOperations()

        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

        Dim euolist As List(Of Interfaces.IExternalUnitOperation) = SharedClasses.Utility.GetUnitOperations(unitopassembly)

        otheruos.AddRange(euolist)

        For Each uo In otheruos
            If Not ExternalUnitOperations.ContainsKey(uo.Description) Then
                ExternalUnitOperations.Add(uo.Description, uo)
            Else
                Console.WriteLine(String.Format("Error adding External Unit Operation '{0}'. Check the 'unitops' and 'extenders' folders for duplicate items.", uo.Description))
            End If
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

        Dim WPP As WilsonPropertyPackage = New WilsonPropertyPackage()
        WPP.ComponentName = "Wilson"
        WPP.ComponentDescription = "Wilson Activity Coefficient Model"

        PropertyPackages.Add(WPP.ComponentName.ToString, WPP)

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

        Dim RKPP As ReaktoroPropertyPackage.ReaktoroPropertyPackage = New ReaktoroPropertyPackage.ReaktoroPropertyPackage()
        PropertyPackages.Add(RKPP.ComponentName.ToString, RKPP)

        Dim ISPP As New IdealElectrolytePropertyPackage()

        PropertyPackages.Add(ISPP.ComponentName.ToString, ISPP)

        Dim BOPP As BlackOilPropertyPackage = New BlackOilPropertyPackage()
        BOPP.ComponentName = "Black Oil"
        BOPP.ComponentDescription = DWSIM.App.GetLocalString("DescBOPP")

        PropertyPackages.Add(BOPP.ComponentName.ToString, BOPP)

        Dim GERGPP As GERG2008PropertyPackage = New GERG2008PropertyPackage()

        PropertyPackages.Add(GERGPP.ComponentName.ToString, GERGPP)

        Dim PCSAFTPP As PCSAFT2PropertyPackage = New PCSAFT2PropertyPackage()

        PropertyPackages.Add(PCSAFTPP.ComponentName.ToString, PCSAFTPP)

        Dim PR78PP As PengRobinson1978PropertyPackage = New PengRobinson1978PropertyPackage()
        PR78PP.ComponentName = "Peng-Robinson 1978 (PR78)"
        PR78PP.ComponentDescription = DWSIM.App.GetLocalString("DescPengRobinson78PP")

        PropertyPackages.Add(PR78PP.ComponentName.ToString, PR78PP)

        Dim PR78Adv As PengRobinson1978AdvancedPropertyPackage = New PengRobinson1978AdvancedPropertyPackage()

        PropertyPackages.Add(PR78Adv.ComponentName.ToString, PR78Adv)

        Dim SRKAdv As SoaveRedlichKwongAdvancedPropertyPackage = New SoaveRedlichKwongAdvancedPropertyPackage()

        PropertyPackages.Add(SRKAdv.ComponentName.ToString, SRKAdv)

        If My.Settings.LoadExtensionsAndPlugins Or FormMain.IsPro Then

            Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()

            For Each pp In otherpps
                If Not PropertyPackages.ContainsKey(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName) Then
                    PropertyPackages.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName, pp)
                Else
                    Console.WriteLine(String.Format("Error adding External Property Package '{0}'. Check the 'ppacks' and 'extenders' folders for duplicate items.", pp.ComponentName))
                End If
            Next

        End If

        'Check if DWSIM is running in Portable/Mono mode, if not then load the CAPE-OPEN Wrapper Property Package.
        If Not DWSIM.App.IsRunningOnMono Then

            Dim COPP As CAPEOPENPropertyPackage = New CAPEOPENPropertyPackage()
            COPP.ComponentName = "CAPE-OPEN"
            COPP.ComponentDescription = DWSIM.App.GetLocalString("DescCOPP")

            PropertyPackages.Add(COPP.ComponentName.ToString, COPP)

        End If

    End Sub

    Private Sub FormParent_MdiChildActivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.MdiChildActivate

        If Me.MdiChildren.Length >= 1 Then

            Me.ToolStripButton1.Enabled = True
            Me.SaveToDashboardTSMI.Enabled = True
            Me.SaveToolStripButton.Enabled = True
            Me.SaveFileS365.Enabled = True
            Me.SaveToolStripMenuItem.Enabled = True
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

#If Not LINUX Then

            ToolStripManager.RevertMerge(ToolStrip1)

#End If

            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                ToolStripManager.Merge(DirectCast(ActiveMdiChild, FormFlowsheet).ToolStrip1, ToolStrip1)
            End If

            FormMain.TranslateFormFunction?.Invoke(Me)

        End If

    End Sub

    Private Sub FormParent_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        UpdateIcon()

        Dim cmdLine() As String = System.Environment.GetCommandLineArgs()

        If UBound(cmdLine) = 1 Then
            If Not cmdLine(0).StartsWith("-") And Not cmdLine(1).Contains("DWSIM.exe") Then
                Try
                    Dim filename As String
                    filename = cmdLine(1)
                    Dim handler = New SharedClassesCSharp.FilePicker.Windows.WindowsFile(filename)

                    Try
                        'Me.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " (" + Me.filename + ")"
                        Application.DoEvents()
                        Select Case Path.GetExtension(filename).ToLower()
                            Case ".dwsim"
                                'Me.LoadF(Me.filename)
                            Case ".dwxml"
                                Me.LoadXML(handler, Nothing)
                            Case ".dwxmz"
                                Me.LoadAndExtractXMLZIP(handler, Nothing)
                            Case ".dwcsd"
                                Dim NewMDIChild As New FormCompoundCreator()
                                NewMDIChild.MdiParent = Me
                                NewMDIChild.Show()
                                Dim objStreamReader As New FileStream(filename, FileMode.Open)
                                Dim x As New BinaryFormatter()
                                NewMDIChild.mycase = x.Deserialize(objStreamReader)
                                objStreamReader.Close()
                                NewMDIChild.WriteData()
                            Case ".dwrsd"
                                Dim NewMDIChild As New FormDataRegression()
                                NewMDIChild.MdiParent = Me
                                NewMDIChild.Show()
                                Dim objStreamReader As New FileStream(filename, FileMode.Open)
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

#If LINUX = False Then
        If Not FormMain.IsPro Then
            Dim currver = Assembly.GetExecutingAssembly().GetName().Version.ToString()
            If (Settings.CurrentVersion <> currver) Then
                Settings.CurrentVersion = currver
                My.Settings.CurrentVersion = currver
                Dim frmwn As New FormWhatsNew()
                frmwn.Show()
            Else
                If My.Settings.CheckForUpdates Then CheckForUpdates()
            End If
        End If
#Else
        MessageBox.Show("The Classic UI version of DWSIM is not supported on Linux. Use it at your own risk.", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
#End If


        AnalyticsProvider?.SetMainForm(Me)

        AnalyticsProvider?.Initialize()

        TranslateFormFunction?.Invoke(Me)

        If AnalyticsProvider IsNot Nothing Then
            AddHandler Me.ToolOpened,
                Sub(sender2, e2)
                    AnalyticsProvider.RegisterEvent(sender2.ToString(), "", Nothing)
                End Sub
            Task.Delay(30 * 1000).ContinueWith(
            Sub(t)
                UIThread(Sub()
                             If Not My.Settings.UserTypeSent Then tsbQuickQuestion.Visible = True
                         End Sub)
            End Sub)
        End If

    End Sub

    Sub CheckForUpdates()

        If Not IsPro Then

            ' check for updates
            Task.Factory.StartNew(Function()
                                      GlobalSettings.Settings.CurrentRunningVersion = Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." +
                                      Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() + "." +
                                      Assembly.GetExecutingAssembly().GetName().Version.Build.ToString()
                                      Return SharedClasses.UpdateCheck.CheckForUpdates()
                                  End Function).ContinueWith(Sub(t)
                                                                 If (t.Result) Then
                                                                     Dim whatsnew = SharedClasses.UpdateCheck.GetWhatsNew()
                                                                     Me.UIThreadInvoke(Sub()
                                                                                           If MessageBox.Show(DWSIM.App.GetLocalString("UpdatedVersionAvailable") & vbCrLf & vbCrLf & whatsnew, DWSIM.App.GetLocalString("UpdateAvailable"), MessageBoxButtons.OKCancel, MessageBoxIcon.Information) = DialogResult.OK Then
                                                                                               Process.Start("https://dwsim.org/downloads")
                                                                                           End If
                                                                                       End Sub)
                                                                 End If
                                                             End Sub, TaskContinuationOptions.ExecuteSynchronously)

        End If

    End Sub

    Sub OpenWelcomeScreen()

        Me.WelcomePanel.Visible = True

        If GlobalSettings.Settings.OldUI AndAlso My.Settings.BackupFiles.Count > 0 Then
            Me.FrmRec = New FormRecoverFiles
            Me.FrmRec.ShowDialog(Me)
        End If

        UpdateFlowsheetLinks()

        FormMain.TranslateFormFunction?.Invoke(Me)

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
                                           Me.UIThreadInvoke(Sub() LoadFile(New SharedClassesCSharp.FilePicker.Windows.WindowsFile(item)))
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
                                           fdlding.Text = "Downloading file..." & " (" & item.Title & ")"
                                           fdlding.Show()
                                           Application.DoEvents()
                                           Task.Factory.StartNew(Function()
                                                                     Return SharedClasses.FOSSEEFlowsheets.DownloadFlowsheet(item.DownloadLink, Sub(px)
                                                                                                                                                    Me.UIThread(Sub()
                                                                                                                                                                    fdlding.Text = "Downloading file... (" & px & "%) (" & item.Title & ")"
                                                                                                                                                                    fdlding.ProgressBar1.Value = px
                                                                                                                                                                    fdlding.Refresh()
                                                                                                                                                                End Sub)
                                                                                                                                                End Sub)
                                                                 End Function).ContinueWith(Sub(tk)
                                                                                                Me.UIThread(Sub() fdlding.Close())
                                                                                                If tk.Exception IsNot Nothing Then
                                                                                                    MessageBox.Show(tk.Exception, "Error downloading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                                Else
                                                                                                    Dim xdoc = SharedClasses.FOSSEEFlowsheets.LoadFlowsheet(tk.Result)
                                                                                                    Me.UIThread(Sub()
                                                                                                                    floading.Text = DWSIM.App.GetLocalString("Loading") + " " + item.Title
                                                                                                                    floading.Show()
                                                                                                                    Application.DoEvents()
                                                                                                                    Try
                                                                                                                        LoadXML2(xdoc, Sub(x)
                                                                                                                                           Me.Invoke(Sub()
                                                                                                                                                         floading.ProgressBar1.Value = x
                                                                                                                                                         floading.Refresh()
                                                                                                                                                     End Sub)
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
        LoadCSDB()

        'load DWSIM XML database
        LoadDWSIMDB()

        'load CoolProp database
        LoadCPDB()

        'load ChEDL database
        'LoadCheDLDB()

        'load Electrolyte XML database
        LoadEDB()

        'load Biodiesel XML database
        LoadBDDB()

        'load FoodProp compounds
        LoadFoodPropCompounds()

        'additional compounds
        LoadAdditionalCompounds()

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
        csdb.Dispose()
    End Sub

    Public Sub LoadDWSIMDB()
        Dim dwdb As New Databases.DWSIM
        Dim cpa() As BaseClasses.ConstantProperties
        dwdb.Load()
        cpa = dwdb.Transfer()
        For Each cp As BaseClasses.ConstantProperties In cpa
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then Me.AvailableComponents.Add(cp.Name, cp)
        Next
        dwdb.Dispose()
    End Sub

    Public Sub LoadAdditionalCompounds()

        Dim comps = UserDB.LoadAdditionalCompounds()

        If My.Settings.UserCompounds Is Nothing Then My.Settings.UserCompounds = New Specialized.StringCollection()

        For Each cpath In My.Settings.UserCompounds
            Try
                Dim comp As ConstantProperties = Nothing
                If cpath.StartsWith("//Simulate 365 Dashboard") Then
                    Using fileStream As Stream = FileDownloadService.GetFileBySimulatePath(cpath)
                        Using reader As New StreamReader(fileStream)
                            Dim contents = reader.ReadToEnd()
                            comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(contents)
                        End Using
                    End Using
                Else
                    comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(File.ReadAllText(cpath))
                End If
                comp.CurrentDB = "User"
                comp.OriginalDB = "User"
                comps.Add(comp)
            Catch ex As Exception
            End Try
        Next

        For Each cp As BaseClasses.ConstantProperties In comps
            If Not Me.AvailableComponents.ContainsKey(cp.Name) Then Me.AvailableComponents.Add(cp.Name, cp)
        Next

    End Sub

    Public Sub LoadFoodPropCompounds()

        Dim udb As New UserDB
        Using filestr As Stream = Assembly.GetAssembly(udb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.FoodProp.xml")
            Dim fcomps = Databases.UserDB.ReadComps(filestr)
            For Each cp As BaseClasses.ConstantProperties In fcomps
                cp.CurrentDB = "FoodProp"
                If Not AvailableComponents.ContainsKey(cp.Name) Then AvailableComponents.Add(cp.Name, cp)
            Next
        End Using

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
        cpdb.Dispose()
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
        chedl.Dispose()

    End Sub

#End Region

#Region "    Open/Save Files"

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
                    obj.Flowsheet = form
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
                                        Dim objTo As GraphicObject = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                        If objTo Is Nothing Then
                                            objTo = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        End If
                                        Dim fromidx As Integer = -1
                                        Dim cp As ConnectionPoint = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = obj.Name).SingleOrDefault
                                        If cp Is Nothing Then
                                            cp = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = xel2.@AttachedToObjID).SingleOrDefault
                                        End If
                                        If Not cp Is Nothing Then
                                            fromidx = cp.ConnectorName.Split("|")(1)
                                        End If
                                        If Not obj Is Nothing And Not objTo Is Nothing Then
                                            form.ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                        End If
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
                                        If objTo Is Nothing Then
                                            obj = (From go As GraphicObject In form.FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        End If
                                        If Not obj Is Nothing And Not objTo Is Nothing Then
                                            form.ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
                                        End If
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

    Sub LoadMobileXML(handler As IVirtualFile)

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = Nothing

        Using fstr As Stream = handler.OpenRead()
            xdoc = XDocument.Load(fstr)
        End Using

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               SharedClasses.Utility.UpdateElementForMobileXMLLoading_CrossPlatformUI(xel1)
                                           End Sub)

        Dim form As FormFlowsheet = New FormFlowsheet() With {.MobileCompatibilityMode = True}

        form.FormSpreadsheet = New FormNewSpreadsheet() With {.Flowsheet = form}
        form.FormSpreadsheet.Initialize()


        Settings.CAPEOPENMode = False
        My.Application.ActiveSimulation = form

        Application.DoEvents()

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            form.Options.LoadData(data)
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        form.Options.FilePath = handler.FullPath
        form.Options.VirtualFile = handler



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

        Dim mypath As String = handler.FullPath
        If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
            My.Settings.MostRecentFiles.Add(mypath)
            Me.UpdateMRUList()
        End If

        My.Application.ActiveSimulation = form

        form.MdiParent = Me
        form.Show()
        form.Activate()

        form.FrmStSim1.CurrentFlowsheet = form
        form.FrmStSim1.Init(True)

        form.FormSurface.Invalidate()

        If excs.Count > 0 Then
            form.WriteToLog("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", Color.DarkRed, MessageType.GeneralError)
            For Each ex As Exception In excs
                form.WriteToLog(ex.Message.ToString & ": " & ex.InnerException.ToString, Color.Red, MessageType.GeneralError)
            Next
        Else
            form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & handler.FullPath & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        Application.DoEvents()

    End Sub

    Public Function LoadJSON(handler As IVirtualFile, ProgressFeedBack As Action(Of Integer), Optional ByVal simulationfilename As String = "") As Interfaces.IFlowsheet

        RaiseEvent FlowsheetLoadingFromXML(Me, New EventArgs())

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim soptions As New Newtonsoft.Json.JsonSerializerSettings
        With soptions
            .Formatting = Newtonsoft.Json.Formatting.Indented
            .TypeNameHandling = Newtonsoft.Json.TypeNameHandling.Auto
        End With

        Dim fsx = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Global.XFlowsheet.Implementation.DefaultImplementations.Flowsheet)(handler.ReadAllText(), soptions)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(5)

        Dim form As FormFlowsheet = New FormFlowsheet()

        form.Options.VirtualFile = handler

        Application.DoEvents()

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(15)

        For Each comp In fsx.Compounds
            form.Options.SelectedComponents.Add(comp, Me.AvailableComponents(comp))
        Next

        For Each ppx In fsx.PropertyPackages
            Dim pp As PropertyPackage = Nothing
            Select Case ppx.Model
                Case Global.XFlowsheet.Interfaces.PropPackageModel.PR_EOS
                    pp = New PengRobinsonPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.PR78_EOS
                    pp = New PengRobinson1978PropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.SRK_EOS
                    pp = New SRKPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.NRTL
                    pp = New NRTLPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.UNIQUAC
                    pp = New UNIQUACPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.UNIFAC
                    pp = New UNIFACPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Mod_UNIFAC_Dortmund
                    pp = New MODFACPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Mod_UNIFAC_NIST
                    pp = New NISTMFACPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Grayson_Streed
                    pp = New GraysonStreedPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Chao_Seader
                    pp = New ChaoSeaderPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Ideal
                    pp = New RaoultPropertyPackage()
                Case Global.XFlowsheet.Interfaces.PropPackageModel.Lee_Kesler_Plocker
                    pp = New LKPPropertyPackage()
            End Select
            pp.UniqueID = ppx.ID
            pp.ComponentName = ppx.Name
            pp.ComponentDescription = ppx.Description
            pp.Tag = ppx.Name
            pp.Flowsheet = form
            form.Options.PropertyPackages.Add(pp.UniqueID, pp)
        Next

        Select Case fsx.DisplayedUnitsOfMeasure
            Case Global.XFlowsheet.Interfaces.UnitOfMeasureSet.SI
                form.Options.SelectedUnitSystem = New SystemsOfUnits.SI
            Case Global.XFlowsheet.Interfaces.UnitOfMeasureSet.SI_Engineering
                form.Options.SelectedUnitSystem = New SystemsOfUnits.SI_ENG
            Case Global.XFlowsheet.Interfaces.UnitOfMeasureSet.CGS
                form.Options.SelectedUnitSystem = New SystemsOfUnits.CGS
            Case Global.XFlowsheet.Interfaces.UnitOfMeasureSet.Imperial
                form.Options.SelectedUnitSystem = New SystemsOfUnits.English
        End Select

        For Each gobjx In fsx.PFDObjects
            Dim obj As ShapeGraphic = Nothing
            Select Case gobjx.ObjectType
                Case Global.XFlowsheet.Interfaces.ObjType.MaterialStream
                    obj = New MaterialStreamGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.EnergyStream
                    obj = New EnergyStreamGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Pump
                    obj = New PumpGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Valve
                    obj = New ValveGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Heater
                    obj = New HeaterGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Cooler
                    obj = New CoolerGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Compressor
                    obj = New CompressorGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Expander
                    obj = New TurbineGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Mixer
                    obj = New MixerGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.Splitter
                    obj = New SplitterGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.HeatExchanger
                    obj = New HeatExchangerGraphic
                Case Global.XFlowsheet.Interfaces.ObjType.SeparatorVessel
                    obj = New VesselGraphic
            End Select
            Dim ip = gobjx.Ports.Where(Function(p) p.IsInput).FirstOrDefault()
            Dim op = gobjx.Ports.Where(Function(p) p.IsOutput).FirstOrDefault()
            obj.Name = gobjx.ID
            obj.Tag = gobjx.Name
            obj.Description = gobjx.Description
            obj.SetPosition(gobjx.X, gobjx.Y)
            obj.Width = gobjx.Width
            obj.Height = gobjx.Height
            obj.CreateConnectors(0, 0)
            If ip.IsConnected Then
                obj.InputConnectors(0).ConnectorName = ip.ConnectedToObjectID + "|" + ip.ConnectedToObjectPortIndex.ToString()
            End If
            If op.IsConnected Then
                obj.OutputConnectors(0).ConnectorName = op.ConnectedToObjectID + "|" + ip.ConnectedToObjectPortIndex.ToString()
            End If
            form.FormSurface.FlowsheetSurface.DrawingObjects.Add(obj)
            form.Collections.GraphicObjectCollection.Add(obj.Name, obj)
        Next

        Dim gobjlist = form.Collections.GraphicObjectCollection.Values.ToList()

        Dim i As Integer

        For Each gobj In gobjlist
            i = 0
            For Each ip In gobj.InputConnectors
                If ip.ConnectorName.Contains("|") Then
                    Dim oname = ip.ConnectorName.Split("|")(0)
                    Dim pindex As Integer = ip.ConnectorName.Split("|")(1)
                    Dim fobj = form.Collections.GraphicObjectCollection(oname)
                    form.ConnectObjects(fobj, gobj, pindex, 0)
                End If
                i += 1
            Next
            i = 0
            For Each op In gobj.OutputConnectors
                If op.ConnectorName.Contains("|") And op.AttachedConnector IsNot Nothing Then
                    Dim oname = op.ConnectorName.Split("|")(0)
                    Dim pindex As Integer = op.ConnectorName.Split("|")(1)
                    Dim fobj = form.Collections.GraphicObjectCollection(oname)
                    form.ConnectObjects(gobj, fobj, 0, pindex)
                End If
                i += 1
            Next
        Next

        For Each sobj In fsx.SimulationObjects
            Dim gobj = form.Collections.GraphicObjectCollection(sobj.ID)
            Dim obj As BaseClass = Nothing
            Select Case sobj.ObjectType
                Case Global.XFlowsheet.Interfaces.ObjType.MaterialStream
                    obj = New Streams.MaterialStream()
                    form.AddComponentsRows(obj)
                    With DirectCast(obj, Streams.MaterialStream)
                        .SetTemperature(sobj.Parameters.Where(Function(p) p.Name = "Temperature").First().Value)
                        .SetPressure(sobj.Parameters.Where(Function(p) p.Name = "Pressure").First().Value)
                        .SetMassEnthalpy(sobj.Parameters.Where(Function(p) p.Name = "MassEnthalpy").First().Value)
                        .SetMassEntropy(sobj.Parameters.Where(Function(p) p.Name = "MassEntropy").First().Value)
                        .SetMolarFlow(sobj.Parameters.Where(Function(p) p.Name = "OverallMolarFlow").First().Value)
                        Dim comp As List(Of Double) = sobj.Parameters.Where(Function(p) p.Name = "OverallMolarComposition").First().Value
                        .SetOverallComposition(comp.ToArray())
                        .CalcOverallCompMassFractions()
                        .SetFlashSpec(sobj.Parameters.Where(Function(p) p.Name = "FlashSpec").First().Value)
                        .Phases(0).Properties.molarfraction = (sobj.Parameters.Where(Function(p) p.Name = "VaporFraction").First().Value)
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.EnergyStream
                    obj = New EnergyStream()
                    With DirectCast(obj, EnergyStream)
                        .EnergyFlow = sobj.Parameters.Where(Function(p) p.Name = "EnergyFlow").First().Value
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Cooler
                    obj = New Cooler()
                    With DirectCast(obj, Cooler)
                        .DeltaQ = sobj.Parameters.Where(Function(p) p.Name = "HeatDuty").First().Value
                        .Eficiencia = sobj.Parameters.Where(Function(p) p.Name = "Efficiency").First().Value
                        .CalcMode = Cooler.CalculationMode.HeatRemoved
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Heater
                    obj = New Heater()
                    With DirectCast(obj, Heater)
                        .DeltaQ = sobj.Parameters.Where(Function(p) p.Name = "HeatDuty").First().Value
                        .Eficiencia = sobj.Parameters.Where(Function(p) p.Name = "Efficiency").First().Value
                        .CalcMode = Heater.CalculationMode.HeatAdded
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.HeatExchanger
                    obj = New HeatExchanger
                    With DirectCast(obj, HeatExchanger)
                        .Area = sobj.Parameters.Where(Function(p) p.Name = "ExchangeArea").First().Value
                        .OverallCoefficient = sobj.Parameters.Where(Function(p) p.Name = "OverallHTC").First().Value
                        .Q = sobj.Parameters.Where(Function(p) p.Name = "HeatDuty").First().Value
                        .ThermalEfficiency = sobj.Parameters.Where(Function(p) p.Name = "Efficiency").First().Value
                        .CalculationMode = HeatExchangerCalcMode.CalcBothTemp
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Pump
                    obj = New Pump()
                    With DirectCast(obj, Pump)
                        .DeltaP = sobj.Parameters.Where(Function(p) p.Name = "PressureIncrease").First().Value
                        .Eficiencia = sobj.Parameters.Where(Function(p) p.Name = "Efficiency").First().Value
                        .CalcMode = Pump.CalculationMode.Delta_P
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Valve
                    obj = New Valve()
                    With DirectCast(obj, Valve)
                        .DeltaP = sobj.Parameters.Where(Function(p) p.Name = "PressureDecrease").First().Value
                        .CalcMode = Valve.CalculationMode.DeltaP
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Mixer
                    obj = New Mixer()
                Case Global.XFlowsheet.Interfaces.ObjType.Splitter
                    obj = New UnitOperations.UnitOperations.Splitter()
                    With DirectCast(obj, UnitOperations.UnitOperations.Splitter)
                        Dim splitfact As List(Of Double) = sobj.Parameters.Where(Function(p) p.Name = "SplitFactors").First().Value
                        .Ratios.AddRange(splitfact)
                        .OperationMode = UnitOperations.UnitOperations.Splitter.OpMode.SplitRatios
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.SeparatorVessel
                    obj = New Vessel()
                Case Global.XFlowsheet.Interfaces.ObjType.Compressor
                    obj = New Compressor()
                    With DirectCast(obj, Compressor)
                        .DeltaP = sobj.Parameters.Where(Function(p) p.Name = "PressureIncrease").First().Value
                        .AdiabaticEfficiency = sobj.Parameters.Where(Function(p) p.Name = "AdiabaticEfficiency").First().Value
                        .CalcMode = Compressor.CalculationMode.Delta_P
                    End With
                Case Global.XFlowsheet.Interfaces.ObjType.Expander
                    obj = New Expander()
                    With DirectCast(obj, Compressor)
                        .DeltaP = sobj.Parameters.Where(Function(p) p.Name = "PressureDecrease").First().Value
                        .AdiabaticEfficiency = sobj.Parameters.Where(Function(p) p.Name = "AdiabaticEfficiency").First().Value
                        .CalcMode = Compressor.CalculationMode.Delta_P
                    End With
            End Select
            obj.SetFlowsheet(form)
            If sobj.PropertyPackageID IsNot Nothing Then
                obj.PropertyPackage = form.Options.PropertyPackages(sobj.PropertyPackageID)
            End If
            obj.GraphicObject = gobj
            gobj.Owner = obj
            obj.Name = sobj.ID
            form.Collections.FlowsheetObjectCollection.Add(obj.Name, obj)
        Next

        Dim filename As String


        form.FilePath = handler.FullPath
        form.Options.FilePath = handler.FullPath

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(25)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(65)

        My.Application.ActiveSimulation = form


        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(80)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(90)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(100)

        My.Application.ActiveSimulation = form

        m_childcount += 1

        form.m_IsLoadedFromFile = True

        form.FormCharts.Flowsheet = form

        form.FormDynamics.Flowsheet = form

        form.FormFilesExplorer.Flowsheet = form

        ' Set DockPanel properties
        form.dckPanel.ActiveAutoHideContent = Nothing
        form.dckPanel.Parent = form

        'form.dckPanel.SuspendLayout(True)
        form.FormLog.DockPanel = Nothing
        form.FormMatList.DockPanel = Nothing
        form.FormSpreadsheet.DockPanel = Nothing
        form.FormSpreadsheet.Flowsheet = form
        form.FormWatch.DockPanel = Nothing
        form.FormSurface.DockPanel = Nothing
        form.FormDynamics.DockPanel = Nothing
        form.FormCharts.DockPanel = Nothing
        form.FormFilesExplorer.DockPanel = Nothing

        Try
            form.FormLog.DockPanel = form.dckPanel
            form.FormSpreadsheet?.Show(form.dckPanel)
            form.FormCharts?.Show(form.dckPanel)
            form.FormMatList?.Show(form.dckPanel)
            form.FormSurface?.Show(form.dckPanel)
            form.FormDynamics?.Show(form.dckPanel)
            form.FormFilesExplorer?.Show(form.dckPanel)
            form.dckPanel.BringToFront()
            form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
        Catch ex As Exception
            'excs.Add(New Exception("Error Restoring Window Layout", ex))
        End Try

        Me.Invalidate()
        Application.DoEvents()

        If simulationfilename <> "" Then filename = simulationfilename Else filename = handler.FullPath

        If TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
            Dim mypath As String = simulationfilename
            If mypath = "" Then mypath = handler.FullPath
            If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
                My.Settings.MostRecentFiles.Add(mypath)
                Me.UpdateMRUList()
            End If
        End If

        My.Application.ActiveSimulation = form

        form.MdiParent = Me
        form.Show()
        form.Activate()

        form.FrmStSim1.CurrentFlowsheet = form
        form.FrmStSim1.Init(True)

        form.FormSurface.Invalidate()

        If excs.Count > 0 Then
            form.WriteToLog("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", Color.DarkRed, MessageType.GeneralError)
            For Each ex As Exception In excs
                form.WriteToLog(ex.Message.ToString & ": " & ex.InnerException.ToString, Color.Red, MessageType.GeneralError)
            Next
        Else
            form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & filename & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        Application.DoEvents()

        form.ProcessScripts(Enums.Scripts.EventType.SimulationOpened, Enums.Scripts.ObjectType.Simulation, "")

        RaiseEvent FlowsheetLoadedFromXML(form, New EventArgs())

        Return form

    End Function


    Public Function LoadXML(handler As IVirtualFile, ProgressFeedBack As Action(Of Integer), Optional ByVal simulationfilename As String = "", Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

        RaiseEvent FlowsheetLoadingFromXML(Me, New EventArgs())

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = Nothing

        xdoc = XDocument.Parse(handler.ReadAllText())

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
            If xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("SavedFromClassicUI") IsNot Nothing Then
                savedfromclui = Boolean.Parse(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("SavedFromClassicUI").Value)
            End If
        Catch ex As Exception
        End Try

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(5)

        Dim form As FormFlowsheet = New FormFlowsheet()

        form.Options.VirtualFile = handler

        Settings.CAPEOPENMode = False

        My.Application.ActiveSimulation = form

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            form.Options.LoadData(data)

            If Not AvailableUnitSystems.ContainsKey(form.Options.SelectedUnitSystem1.Name) Then
                AvailableUnitSystems.Add(form.Options.SelectedUnitSystem1.Name, form.Options.SelectedUnitSystem1)
            End If

            If sver < New Version("6.3.0.0") Then
                form.Options.SkipEquilibriumCalculationOnDefinedStreams = False
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        Dim undoredoenabled = form.Options.EnabledUndoRedo

        form.Options.EnabledUndoRedo = False

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(15)

        Dim filename As String

        If simulationfilename <> "" Then filename = simulationfilename Else filename = handler.FullPath

        form.FilePath = filename
        form.Options.FilePath = filename

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddGraphicObjects(form, data, excs)

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(25)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

        For Each xel As XElement In data
            Dim obj As New ConstantProperties
            obj.Name = xel.Element("Name").Value
            If Not form.AvailableCompounds.ContainsKey(obj.Name) Then form.AvailableCompounds.Add(obj.Name, obj)
            form.Options.SelectedComponents.Add(obj.Name, obj)
        Next

        Parallel.ForEach(data, Sub(xel)
                                   Try
                                       form.Options.SelectedComponents(xel.Element("Name").Value).LoadData(xel.Elements.ToList)
                                   Catch ex As Exception
                                       excs.Add(New Exception("Error Loading Compound Information", ex))
                                   End Try
                               End Sub)

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
                            Dim assembly1 As Assembly = Nothing
                            For Each assembly In My.Application.Info.LoadedAssemblies
                                If proptype.Contains(assembly.GetName().Name) Then
                                    assembly1 = assembly
                                    Exit For
                                End If
                            Next
                            If assembly1 IsNot Nothing Then
                                Dim ptype As Type = assembly1.GetType(proptype)
                                Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                                DirectCast(form.ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                            End If
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
                If xel.Element("Type").Value.Contains("ThermoC") Then
                    Dim thermockey As String = "ThermoC Bridge"
                    If PropertyPackages.ContainsKey(thermockey) Then
                        obj = PropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
                        Throw New Exception("The ThermoC bridge library was not found. Please download and install it in order to run this simulation.")
                    End If
                Else
                    Dim ppkey As String = xel.Element("ComponentName").Value
                    If ppkey = "" Then
                        obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackage)
                    Else
                        Dim ptype = xel.Element("Type").Value
                        If ppkey.Contains("1978") And ptype.Contains("PengRobinsonPropertyPackage") Then
                            ptype = ptype.Replace("PengRobinson", "PengRobinson1978")
                        End If
                        If PropertyPackages.ContainsKey(ppkey) Then
                            obj = PropertyPackages(ppkey).ReturnInstance(ptype)
                        Else
                            form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
                            Throw New Exception("The " & ppkey & " Property Package library was not found.")
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
                    obj = New Streams.MaterialStream()
                Else
                    Dim uokey As String = xel.Element("ComponentDescription").Value
                    If ExternalUnitOperations.ContainsKey(uokey) Then
                        obj = ExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                    End If
                End If
                If obj Is Nothing Then form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
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
                    ElseIf TypeOf obj Is Input Then
                        GraphicObjectControlPanelModeEditors.SetInputDelegate(gobj, obj)
                    ElseIf TypeOf obj Is PIDController Then
                        GraphicObjectControlPanelModeEditors.SetPIDDelegate(gobj, obj)
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

        If xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions").Elements.ToList

            form.StoredSolutions.Clear()

            For Each xel As XElement In data
                Try
                    form.StoredSolutions.Add(xel.@ID, xel.Elements.ToList)
                Catch ex As Exception
                End Try
            Next

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager").Elements.ToList

            Try
                DirectCast(form.DynamicsManager, ICustomXMLSerialization).LoadData(data)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Dynamics Manager Information", ex))
            End Try

        End If

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
                    form.WatchItems.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Watch Item Information", ex))
                End Try
                i += 1
            Next

            form.FormWatch.Flowsheet = form
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

        If xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog") IsNot Nothing Then
            Try
                data = xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog").Elements.ToList
                For Each xel As XElement In data
                    form.MessagesLog.Add(xel.Value)
                Next
            Catch ex As Exception
            End Try
        End If

        form.Results = New SharedClasses.DWSIM.Flowsheet.FlowsheetResults

        If xdoc.Element("DWSIM_Simulation_Data").Element("Results") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Results").Elements.ToList

            DirectCast(form.Results, ICustomXMLSerialization).LoadData(data)

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("GHGCompositions") IsNot Nothing Then

            form.GHGEmissionCompositions = New Dictionary(Of String, IGHGComposition)()

            data = xdoc.Element("DWSIM_Simulation_Data").Element("GHGCompositions").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New GHGEmissionComposition()
                    obj.LoadData(xel.Elements.ToList)
                    form.GHGEmissionCompositions.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading GHG Composition Item Information", ex))
                End Try
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
                    rgfdata = rgfdata.Replace("Calibri", "Arial").Replace("10.25", "10")
                    Dim sdict As New Dictionary(Of String, String)
                    sdict = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(rgfdata)
                    form.FormSpreadsheet.Spreadsheet.RemoveWorksheet(0)
                    For Each item In sdict
                        Dim tmpfile = SharedClasses.Utility.GetTempFileName()
                        Dim sheet = form.FormSpreadsheet.Spreadsheet.NewWorksheet(item.Key)
                        Dim xmldoc = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(item.Value)
                        xmldoc.Save(tmpfile)
                        sheet.LoadRGF(tmpfile)
                        File.Delete(tmpfile)
                    Next
                    If (form.FormSpreadsheet.Spreadsheet.Worksheets.Count > 0) Then
                        form.FormSpreadsheet.Spreadsheet.CurrentWorksheet = form.FormSpreadsheet.Spreadsheet.Worksheets(0)
                    End If

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

            form.FormScript1.fc = form

            form.FormCharts.Flowsheet = form

            form.FormDynamics.Flowsheet = form

            form.FormIntegratorControls.Flowsheet = form

            form.FormFilesExplorer.Flowsheet = form

            form.FormObjects.Flowsheet = form

            ' Set DockPanel properties
            form.dckPanel.ActiveAutoHideContent = Nothing
            form.dckPanel.Parent = form

            'form.dckPanel.SuspendLayout(True)
            form.FormLog.DockPanel = Nothing
            form.FormMatList.DockPanel = Nothing
            form.FormSpreadsheet.DockPanel = Nothing
            form.FormSpreadsheet.Flowsheet = form
            form.FormWatch.DockPanel = Nothing
            form.FormSurface.DockPanel = Nothing
            form.FormDynamics.DockPanel = Nothing
            form.FormCharts.DockPanel = Nothing
            form.FormFilesExplorer.DockPanel = Nothing
            form.FormScript1.DockPanel = Nothing
            form.FormObjects.DockPanel = Nothing

            If Not DWSIM.App.IsRunningOnMono Then
                If Not My.Computer.Keyboard.ShiftKeyDown Then
                    If savedfromclui Then
                        Dim myfile As String = SharedClasses.Utility.GetTempFileName()
                        Try
                            Dim pnl As String = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout").Value
                            File.WriteAllText(myfile, pnl)
                            form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf form.ReturnForm))
                        Catch ex As Exception
                        Finally
                            File.Delete(myfile)
                        End Try
                    Else
                        Dim myfile As String = IO.Path.Combine(My.Application.Info.DirectoryPath, "layout.xml")
                        form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf form.ReturnForm))
                    End If
                End If
            End If

            Try
                form.FormLog.DockPanel = form.dckPanel
                form.FormLog.Hide()
                form.FormWatch.Hide()
                form.FormSpreadsheet?.Show(form.dckPanel)
                form.FormCharts?.Show(form.dckPanel)
                form.FormMatList?.Show(form.dckPanel)
                form.FormSurface?.Show(form.dckPanel)
                form.FormDynamics?.Show(form.dckPanel)
                form.FormFilesExplorer?.Show(form.dckPanel)
                form.FormScript1?.Show(form.dckPanel)
                form.FormObjects?.Show(form.dckPanel)
#If LINUX = False Then
                'form.FormIPyConsole?.Show(form.dckPanel)
#End If
                form.dckPanel.BringToFront()
                form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
            Catch ex As Exception
                'excs.Add(New Exception("Error Restoring Window Layout", ex))
            End Try

            Me.Invalidate()
            Application.DoEvents()

            If TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                Dim mypath As String = simulationfilename
                If mypath = "" Then mypath = handler.FullPath
                If Not My.Settings.MostRecentFiles.Contains(mypath) And IO.Path.GetExtension(mypath).ToLower <> ".dwbcs" Then
                    My.Settings.MostRecentFiles.Add(mypath)
                    Me.UpdateMRUList()
                End If
            End If

            My.Application.ActiveSimulation = form

            form.MdiParent = Me
            form.Show()
            form.Activate()

            form.FrmStSim1.CurrentFlowsheet = form
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
            'form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & filename & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        Application.DoEvents()

        form.ProcessScripts(Enums.Scripts.EventType.SimulationOpened, Enums.Scripts.ObjectType.Simulation, "")

        RaiseEvent FlowsheetLoadedFromXML(form, New EventArgs())

        form.Options.EnabledUndoRedo = undoredoenabled

        Return form

    End Function

    Public Function LoadXML2(xdoc As XDocument, ProgressFeedBack As Action(Of Integer), Optional ByVal simulationfilename As String = "", Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

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
            If sver < New Version("6.3.0.0") Then
                form.Options.SkipEquilibriumCalculationOnDefinedStreams = False
            End If
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        Dim undoredoenabled = form.Options.EnabledUndoRedo

        form.Options.EnabledUndoRedo = False

        If Not ProgressFeedBack Is Nothing Then ProgressFeedBack.Invoke(15)

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
                If xel.Element("Type").Value.Contains("ThermoC") Then
                    Dim thermockey As String = "ThermoC Bridge"
                    If PropertyPackages.ContainsKey(thermockey) Then
                        obj = PropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
                        Throw New Exception("The ThermoC bridge library was not found.")
                    End If
                Else
                    Dim ppkey As String = xel.Element("ComponentName").Value
                    If ppkey = "" Then
                        obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackage)
                    Else
                        Dim ptype = xel.Element("Type").Value
                        If ppkey.Contains("1978") And ptype.Contains("PengRobinsonPropertyPackage") Then
                            ptype = ptype.Replace("PengRobinson", "PengRobinson1978")
                        End If
                        If PropertyPackages.ContainsKey(ppkey) Then
                            obj = PropertyPackages(ppkey).ReturnInstance(ptype)
                        Else
                            form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
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
                If obj Is Nothing Then form.LoaderExceptions.Add(PrepareExceptionInfo(xel))
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
                    ElseIf TypeOf obj Is Input Then
                        GraphicObjectControlPanelModeEditors.SetInputDelegate(gobj, obj)
                    ElseIf TypeOf obj Is PIDController Then
                        GraphicObjectControlPanelModeEditors.SetPIDDelegate(gobj, obj)
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

        If xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions").Elements.ToList

            form.StoredSolutions.Clear()

            For Each xel As XElement In data
                Try
                    form.StoredSolutions.Add(xel.@ID, xel.Elements.ToList)
                Catch ex As Exception
                End Try
            Next

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager").Elements.ToList

            Try
                DirectCast(form.DynamicsManager, ICustomXMLSerialization).LoadData(data)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Dynamics Manager Information", ex))
            End Try

        End If

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
                    form.WatchItems.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Watch Item Information", ex))
                End Try
                i += 1
            Next

            form.FormWatch.Flowsheet = form
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

        form.Results = New SharedClasses.DWSIM.Flowsheet.FlowsheetResults

        If xdoc.Element("DWSIM_Simulation_Data").Element("Results") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Results").Elements.ToList

            DirectCast(form.Results, ICustomXMLSerialization).LoadData(data)

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("GHGCompositions") IsNot Nothing Then

            form.GHGEmissionCompositions = New Dictionary(Of String, IGHGComposition)()

            data = xdoc.Element("DWSIM_Simulation_Data").Element("GHGCompositions").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New GHGEmissionComposition()
                    obj.LoadData(xel.Elements.ToList)
                    form.GHGEmissionCompositions.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading GHG Composition Item Information", ex))
                End Try
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
                    rgfdata = rgfdata.Replace("Calibri", "Arial").Replace("10.25", "10")
                    Dim sdict As New Dictionary(Of String, String)
                    sdict = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(rgfdata)
                    form.FormSpreadsheet.Spreadsheet.RemoveWorksheet(0)
                    For Each item In sdict
                        Dim tmpfile = SharedClasses.Utility.GetTempFileName()
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

            form.FormScript1.fc = form

            form.FormCharts.Flowsheet = form

            form.FormDynamics.Flowsheet = form

            form.FormIntegratorControls.Flowsheet = form

            form.FormFilesExplorer.Flowsheet = form

            form.FormObjects.Flowsheet = form

            ' Set DockPanel properties
            form.dckPanel.ActiveAutoHideContent = Nothing
            form.dckPanel.Parent = form

            'form.dckPanel.SuspendLayout(True)
            form.FormLog.DockPanel = Nothing
            form.FormMatList.DockPanel = Nothing
            form.FormSpreadsheet.DockPanel = Nothing
            form.FormSpreadsheet.Flowsheet = form
            form.FormCharts.DockPanel = Nothing
            form.FormWatch.DockPanel = Nothing
            form.FormSurface.DockPanel = Nothing
            form.FormDynamics.DockPanel = Nothing
            form.FormFilesExplorer.DockPanel = Nothing
            form.FormScript1.DockPanel = Nothing
            form.FormObjects.DockPanel = Nothing

            If Not My.Computer.Keyboard.ShiftKeyDown Then
                Dim myfile As String = SharedClasses.Utility.GetTempFileName()
                Try
                    Dim pnl As String = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout").Value
                    File.WriteAllText(myfile, pnl)
                    form.dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf form.ReturnForm))
                Catch ex As Exception
                    'excs.Add(New Exception("Error Restoring Window Layout", ex))
                Finally
                    File.Delete(myfile)
                End Try
            End If

            Try
                form.FormLog.DockPanel = form.dckPanel
                form.FormLog.Hide()
                form.FormWatch.Hide()
                form.FormSpreadsheet.Show(form.dckPanel)
                form.FormCharts.Show(form.dckPanel)
                form.FormMatList.Show(form.dckPanel)
                form.FormSurface.Show(form.dckPanel)
                form.FormDynamics.Show(form.dckPanel)
                form.FormFilesExplorer.Show(form.dckPanel)
                'form.FormIPyConsole.Show(form.dckPanel)
                form.FormScript1.Show(form.dckPanel)
                form.FormObjects.Show(form.dckPanel)
                form.dckPanel.BringToFront()
                form.dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)
            Catch ex As Exception
                excs.Add(New Exception("Error Restoring Window Layout", ex))
            End Try

            Me.Invalidate()
            Application.DoEvents()

            My.Application.ActiveSimulation = form

            form.MdiParent = Me
            form.Show()
            form.Activate()

        End If

        form.FrmStSim1.CurrentFlowsheet = form
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
            'form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & DWSIM.App.GetLocalString("carregadocomsucesso"), Color.Blue, MessageType.Information)
        End If

        form.UpdateFormText()

        Application.DoEvents()

        form.Options.EnabledUndoRedo = undoredoenabled

        Return form

    End Function

    Public Function PrepareExceptionInfo(xel As XElement, Optional base As Exception = Nothing) As ComponentNotFoundException

        Dim ex As New ComponentNotFoundException()

        With ex
            .Base = base
            .ProductName = xel.Element("ProductName")?.Value
            .ProductAuthor = xel.Element("ProductAuthor")?.Value
            .ProductContactInfo = xel.Element("ProductContactInfo")?.Value
            .ProductDescription = xel.Element("ProductDescription")?.Value
            .ProductAssembly = xel.Element("ProductAssembly")?.Value
            .ProductPage = xel.Element("ProductPage")?.Value
            .ProductVersion = xel.Element("ProductVersion")?.Value
        End With

        If ex.ProductName Is Nothing Then
            With ex
                .Base = base
                .ProductName = xel.Element("ComponentName")?.Value
                .ProductAuthor = ""
                .ProductContactInfo = ""
                .ProductDescription = xel.Element("ComponentDescription")?.Value
                .ProductAssembly = ""
                .ProductPage = ""
                .ProductVersion = ""
            End With
        End If

        Return ex

    End Function

    Sub SaveMobileXML(handler As IVirtualFile, ByVal form As FormFlowsheet, Optional ByVal simulationfilename As String = "")

        Dim compatmessage As String = SharedClasses.Utility.CheckSimulationForMobileCompatibility(form)



        If compatmessage <> "" Then
            Throw New NotSupportedException(compatmessage)
        End If

        If simulationfilename = "" Then simulationfilename = handler.FullPath

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
                                               Try
                                                   SharedClasses.Utility.UpdateElementForMobileXMLSaving_CrossPlatformUI(xel1)
                                               Catch ex As Exception
                                               End Try
                                           End Sub)

        Using stream As New IO.MemoryStream()
            xdoc.Save(stream)
            handler.Write(stream)
        End Using



        Me.UIThread(New Action(Sub()
                                   Dim mypath As String = simulationfilename

                                   If mypath = "" Then mypath = handler.FullPath
                                   'process recent files list
                                   If Not My.Settings.MostRecentFiles.Contains(mypath) Then
                                       My.Settings.MostRecentFiles.Add(mypath)
                                       If Not My.Application.CommandLineArgs.Count > 1 Then Me.UpdateMRUList()
                                   End If
                                   form.Options.FilePath = mypath
                                   form.UpdateFormText()
                                   form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & mypath & DWSIM.App.GetLocalString("salvocomsucesso"), Color.Blue, MessageType.Information)
                                   'Me.ToolStripStatusLabel1.Text = ""
                               End Sub))

        Application.DoEvents()

    End Sub

    Sub SaveXML(handler As IVirtualFile, ByVal form As FormFlowsheet, Optional ByVal simulationfilename As String = "")

        RaiseEvent FlowsheetSavingToXML(form, New EventArgs())

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

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("StoredSolutions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions")

        For Each pp As KeyValuePair(Of String, List(Of XElement)) In form.StoredSolutions
            xel.Add(New XElement("Solution", New XAttribute("ID", pp.Key), pp.Value))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicsManager"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager")

        xel.Add(DirectCast(form.DynamicsManager, ICustomXMLSerialization).SaveData().ToArray())

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

        For Each wi As WatchItem In form.WatchItems
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
            Dim tmpfile = SharedClasses.Utility.GetTempFileName()
            sheet.SaveRGF(tmpfile)
            Dim xmldoc = New XmlDocument()
            xmldoc.Load(tmpfile)
            sdict.Add(sheet.Name, Newtonsoft.Json.JsonConvert.SerializeXmlNode(xmldoc))
            File.Delete(tmpfile)
        Next
        xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value = Newtonsoft.Json.JsonConvert.SerializeObject(sdict)

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PanelLayout"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PanelLayout")

        Dim myfile As String = SharedClasses.Utility.GetTempFileName()
        form.dckPanel.SaveAsXml(myfile, Encoding.UTF8)
        xel.Add(File.ReadAllText(myfile).ToString)
        File.Delete(myfile)

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("MessagesLog"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog")

        If form.Options.SaveFlowsheetMessagesInFile Then
            Dim inner_elements As New List(Of XElement)
            For Each item In form.MessagesLog
                inner_elements.Add(New XElement("Message", item))
            Next
            xel.Add(inner_elements)
        End If

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Results"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Results")
        xel.Add(DirectCast(form.Results, ICustomXMLSerialization).SaveData().ToArray())

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GHGCompositions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GHGCompositions")

        For Each ghgcomp In form.GHGEmissionCompositions.Values
            xel.Add(New XElement("GHGComposition", DirectCast(ghgcomp, ICustomXMLSerialization).SaveData().ToArray()))
        Next

        Using stream As New IO.MemoryStream()
            xdoc.Save(stream)
            handler.Write(stream)
        End Using

        If simulationfilename = "" Then simulationfilename = handler.FullPath
        Dim fileExtension As String = IO.Path.GetExtension(simulationfilename).ToLower

        If (fileExtension.Contains("dwxml") Or fileExtension.Contains("dwxmz")) Then
            If Visible Then
                Dim mypath As String = simulationfilename
                If mypath = "" Then mypath = handler.FullPath
                form.UIThread(Sub()
                                  'process recent files list
                                  If Not My.Settings.MostRecentFiles.Contains(mypath) Then
                                      My.Settings.MostRecentFiles.Add(mypath)
                                      If Not My.Application.CommandLineArgs.Count > 1 Then Me.UpdateMRUList()
                                  End If
                                  form.Options.FilePath = mypath
                                  form.UpdateFormText()
                              End Sub)
                form.WriteToLog(DWSIM.App.GetLocalString("Arquivo") & mypath & DWSIM.App.GetLocalString("salvocomsucesso"), Color.Blue, MessageType.Information)
            End If
        End If

        If Not IO.Path.GetExtension(handler.FullPath).ToLower.Contains("dwbcs") Then
            form.ProcessScripts(Scripts.EventType.SimulationSaved, Scripts.ObjectType.Simulation, "")
        End If

        RaiseEvent FlowsheetSavedToXML(form, New EventArgs())

    End Sub

    Shared Function IsZipFilePasswordProtected(ByVal ZipFile As Stream) As Boolean
        Using zipInStream As New ZipInputStream(ZipFile)
            Dim zEntry As ZipEntry = zipInStream.GetNextEntry()
            Return zEntry.IsCrypted
        End Using
    End Function

    Function LoadAndExtractXMLZIP(handler As IVirtualFile, ProgressFeedBack As Action(Of Integer), Optional ByVal forcommandline As Boolean = False) As Interfaces.IFlowsheet

        Dim pathtosave As String = Path.Combine(My.Computer.FileSystem.SpecialDirectories.Temp, Guid.NewGuid().ToString())

        Directory.CreateDirectory(pathtosave)

        Dim fullname As String = ""

        Dim pwd As String = Nothing
        Using fstream = handler.OpenRead()
            If IsZipFilePasswordProtected(fstream) Then
                Dim fp As New FormPassword
                If fp.ShowDialog() = Windows.Forms.DialogResult.OK Then
                    pwd = fp.tbPassword.Text
                End If
            End If
        End Using

        Dim dbfile As String = ""

        Try
            Using fstream = handler.OpenRead()
                Using stream As ZipInputStream = New ZipInputStream(fstream)
                    stream.Password = pwd
                    Dim entry As ZipEntry
Label_00CC:
                    entry = stream.GetNextEntry()
                    Do While (Not entry Is Nothing)
                        Dim fileName As String = Path.GetFileName(entry.Name)
                        If (fileName <> String.Empty) Then
                            Using stream2 As FileStream = File.Create(Path.Combine(pathtosave, Path.GetFileName(entry.Name)))
                                Dim count As Integer = 2048
                                Dim buffer As Byte() = New Byte(2048) {}
                                Do While True
                                    count = stream.Read(buffer, 0, buffer.Length)
                                    If (count <= 0) Then
                                        Dim extension = Path.GetExtension(entry.Name).ToLower()
                                        If extension = ".xml" Then
                                            fullname = Path.Combine(pathtosave, Path.GetFileName(entry.Name))
                                        ElseIf extension = ".db" Then
                                            dbfile = Path.Combine(pathtosave, Path.GetFileName(entry.Name))
                                        End If
                                        GoTo Label_00CC
                                    End If
                                    stream2.Write(buffer, 0, count)
                                Loop
                            End Using
                        End If
                        entry = stream.GetNextEntry
                    Loop
                End Using
            End Using
            Dim fs As Interfaces.IFlowsheet
            fs = LoadXML(New SharedClassesCSharp.FilePicker.Windows.WindowsFile(fullname), ProgressFeedBack, handler.FullPath, forcommandline)
            fs.FlowsheetOptions.VirtualFile = handler
            fs.FilePath = handler.FullPath
            fs.Options.FilePath = handler.FullPath
            DirectCast(fs, FormFlowsheet).UpdateFormText()
            If File.Exists(dbfile) Then
                Try
                    fs.FileDatabaseProvider.LoadDatabase(dbfile)
                    DirectCast(fs, FormFlowsheet).FormFilesExplorer.ListFiles()
                Catch ex As Exception
                Finally
                    File.Delete(dbfile)
                End Try
            End If
            File.Delete(fullname)
            Return fs
        Catch ex As Exception
            MessageBox.Show(ex.ToString, DWSIM.App.GetLocalString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return Nothing
        End Try

        Try
            Directory.Delete(pathtosave, True)
        Catch ex As Exception
        End Try

    End Function

    Sub SaveJSON(handler As IVirtualFile, ByVal form As FormFlowsheet)

        File.WriteAllText(handler.FullPath, XFlowsheet.Exporter.Export(form))

    End Sub

    Sub SaveXMLZIP(handler As IVirtualFile, ByVal form As FormFlowsheet)

        Dim xmlfile As String = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), "xml")

        Me.SaveXML(New SharedClassesCSharp.FilePicker.Windows.WindowsFile(xmlfile), form, handler.FullPath)

        Dim i_Files As ArrayList = New ArrayList()
        If File.Exists(xmlfile) Then i_Files.Add(xmlfile)

        Dim dbfile As String = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), "db")

        form.FileDatabaseProvider.ExportDatabase(dbfile)

        If File.Exists(dbfile) Then i_Files.Add(dbfile)

        Dim astrFileNames() As String = i_Files.ToArray(GetType(String))

        Using stream As New MemoryStream()

            Using strmZipOutputStream = New ZipOutputStream(stream)

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

                stream.Position = 0
                handler.Write(stream)

            End Using

        End Using

        Try
            If Path.GetExtension(handler.FullPath).ToLower() <> ".dwbcs" Then
                form.Options.FilePath = handler.FullPath
            End If
        Catch ex As Exception
        End Try

        form.UpdateFormText()

        File.Delete(xmlfile)

        File.Delete(dbfile)

    End Sub

    Sub LoadFileDialog(Optional dashboardpicker As Boolean = False)

        Dim filePickerForm As IFilePicker

        If dashboardpicker Then
            filePickerForm = New Simulate365.FormFactories.S365FilePickerForm()
        Else
            filePickerForm = SharedClassesCSharp.FilePicker.FilePickerService.GetInstance().GetFilePicker()
        End If

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
            {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("All Supported Files", New String() {"*.dwxmz", "*.dwxml", "*.xml", "*.pfdx", "*.dwcsd", "*.dwcsd2", "*.dwrsd", "*.dwrsd2", "*.dwruf"}),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Simulation File", New String() {"*.dwxmz", "*.dwxml", "*.xml", "*.pfdx"}),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Compound Creator Study", New String() {"*.dwcsd", "*.dwcsd2"}),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Data Regression Study", New String() {"*.dwrsd", "*.dwrsd2"}),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("UNIFAC Parameter Regression Study", "*.dwruf")})

        If openedFile IsNot Nothing Then

            LoadFile(openedFile)

        End If

    End Sub

    Sub LoadFile(handler As IVirtualFile)

        Me.WelcomePanel.Visible = False
        PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim fpath = handler.FullPath

        Dim floading As New FormLoadingSimulation

        floading.Text = DWSIM.App.GetLocalString("Loading") + " '" + Path.GetFileNameWithoutExtension(handler.FullPath) + "'..."
        floading.Show()

        Application.DoEvents()

        Select Case handler.GetExtension().ToLower()
            Case ".pfdx"
                Me.LoadJSON(handler, Sub(x)
                                         Me.Invoke(Sub()
                                                       floading.ProgressBar1.Value = x
                                                       floading.Refresh()
                                                   End Sub)
                                     End Sub, "")
            Case ".dwxml"
                Me.LoadXML(handler, Sub(x)
                                        Me.Invoke(Sub()
                                                      floading.ProgressBar1.Value = x
                                                      floading.Refresh()
                                                  End Sub)
                                    End Sub, "", False)
            Case ".dwxmz"
                Me.LoadAndExtractXMLZIP(handler, Sub(x)
                                                     Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                 End Sub, False)
            Case ".xml"
                Me.LoadMobileXML(handler)
            Case ".dwcsd"
                Application.DoEvents()
                Dim NewMDIChild As New FormCompoundCreator()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Dim objStreamReader As New FileStream(fpath, FileMode.Open, FileAccess.Read)
                Dim x As New BinaryFormatter()
                x.Binder = New VersionDeserializationBinder
                NewMDIChild.mycase = x.Deserialize(objStreamReader)
                NewMDIChild.mycase.Filename = handler.FullPath
                objStreamReader.Close()
                NewMDIChild.WriteData()
                If GlobalSettings.Settings.OldUI And TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                    If Not My.Settings.MostRecentFiles.Contains(handler.FullPath) Then
                        My.Settings.MostRecentFiles.Add(handler.FullPath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwcsd2"
                Application.DoEvents()
                Dim NewMDIChild As New FormCompoundCreator()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                NewMDIChild.mycase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of CompoundGeneratorCase)(handler.ReadAllText())
                NewMDIChild.mycase.Filename = handler.FullPath
                NewMDIChild.WriteData()
                If GlobalSettings.Settings.OldUI And TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                    If Not My.Settings.MostRecentFiles.Contains(handler.FullPath) Then
                        My.Settings.MostRecentFiles.Add(handler.FullPath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwrsd"
                Application.DoEvents()
                Dim NewMDIChild As New FormDataRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Using objStreamReader = handler.OpenRead()
                    Dim x As New BinaryFormatter()
                    x.Binder = New VersionDeserializationBinder
                    NewMDIChild.currcase = x.Deserialize(objStreamReader)
                End Using
                NewMDIChild.currcase.filename = handler.FullPath
                NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                If GlobalSettings.Settings.OldUI And TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                    If Not My.Settings.MostRecentFiles.Contains(handler.FullPath) Then
                        My.Settings.MostRecentFiles.Add(handler.FullPath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwrsd2"
                Application.DoEvents()
                Dim NewMDIChild As New FormDataRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                NewMDIChild.currcase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of DWSIM.Optimization.DatRegression.RegressionCase)(handler.ReadAllText())
                NewMDIChild.currcase.filename = fpath
                NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                If GlobalSettings.Settings.OldUI And TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                    If Not My.Settings.MostRecentFiles.Contains(handler.FullPath) Then
                        My.Settings.MostRecentFiles.Add(handler.FullPath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
            Case ".dwruf"
                Application.DoEvents()
                Dim NewMDIChild As New FormUNIFACRegression()
                NewMDIChild.MdiParent = Me
                NewMDIChild.Show()
                Using objStreamReader = handler.OpenRead()
                    Dim x As New BinaryFormatter()
                    x.Binder = New VersionDeserializationBinder
                    NewMDIChild.mycase = x.Deserialize(objStreamReader)
                    NewMDIChild.mycase.Filename = handler.FullPath
                End Using
                NewMDIChild.LoadCase(NewMDIChild.mycase, False)
                If GlobalSettings.Settings.OldUI And TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                    If Not My.Settings.MostRecentFiles.Contains(handler.FullPath) Then
                        My.Settings.MostRecentFiles.Add(handler.FullPath)
                        Me.UpdateMRUList()
                    End If
                End If
                NewMDIChild.Activate()
        End Select

        floading.Close()

    End Sub

    Sub SaveBackup(handler As IVirtualFile)

        If My.Settings.SaveBackupFile Then
            If TypeOf handler Is SharedClassesCSharp.FilePicker.Windows.WindowsFile Then
                If File.Exists(handler.FullPath) Then
                    Try
                        Dim dfile = Path.GetDirectoryName(handler.FullPath) & Path.DirectorySeparatorChar & Path.GetFileNameWithoutExtension(handler.FullPath) & "_backup" & Path.GetExtension(handler.FullPath)
                        File.Copy(handler.FullPath, dfile, True)
                    Catch ex As Exception
                    End Try
                End If
            End If
        End If

    End Sub

    Sub SaveFileDialog(Optional dashboardpicker As Boolean = False)

        If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then

            Dim form2 As FormFlowsheet = Me.ActiveMdiChild

            Dim filename = form2.Options.FilePath

            Dim filePickerForm As IFilePicker

            If dashboardpicker Then
                filePickerForm = New Simulate365.FormFactories.S365FilePickerForm()
                Try
                    Dim fname = Path.GetFileNameWithoutExtension(form2.Options.FilePath)
                    filePickerForm.SuggestedFilename = fname
                    If form2.Options.VirtualFile IsNot Nothing Then
                        filePickerForm.SuggestedDirectory = form2.Options.VirtualFile.ParentUniqueIdentifier
                    End If

                Catch ex As Exception
                End Try
            Else
                filePickerForm = SharedClassesCSharp.FilePicker.FilePickerService.GetInstance().GetFilePicker()
                Try
                    Dim fname = Path.GetFileNameWithoutExtension(form2.Options.FilePath)
                    Dim fpath = Path.GetDirectoryName(form2.Options.FilePath)
                    filePickerForm.SuggestedFilename = fname
                    filePickerForm.SuggestedDirectory = fpath
                    If TypeOf filePickerForm Is Simulate365.FormFactories.S365FilePickerForm Then
                        filePickerForm.SuggestedDirectory = form2.Options.VirtualFile.ParentUniqueIdentifier
                    End If
                Catch ex As Exception
                End Try
            End If

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
            {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Compressed XML Simulation File", "*.dwxmz"),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("XML Simulation File", "*.dwxml"),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Interchangeable PFD Simulation File", "*.pfdx"),
            New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Mobile XML Simulation File", "*.xml")})

            If handler IsNot Nothing Then

                If SavingSimulation IsNot Nothing Then
                    If SavingSimulation.Invoke(form2) = False Then Exit Sub
                End If
                SaveBackup(handler)
                'Application.DoEvents()
                Console.WriteLine(handler.GetExtension().ToLower())
                If handler.GetExtension().ToLower() = ".dwxml" Then
                    TaskHelper.Run(Sub() SaveXML(handler, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                               'Me.ToolStripStatusLabel1.Text = ""
                                                                                               If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                           End Sub, TaskContinuationOptions.ExecuteSynchronously)

                ElseIf handler.GetExtension().ToLower() = ".xml" Then
                    TaskHelper.Run(Sub() SaveMobileXML(handler, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                                     'Me.ToolStripStatusLabel1.Text = ""
                                                                                                     If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                 End Sub, TaskContinuationOptions.ExecuteSynchronously)
                ElseIf handler.GetExtension().ToLower() = ".dwxmz" Then
                    TaskHelper.Run(Sub() SaveXMLZIP(handler, Me.ActiveMdiChild)).ContinueWith(Sub(t)
                                                                                                  ' Me.ToolStripStatusLabel1.Text = ""
                                                                                                  If Not t.Exception Is Nothing Then form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                              End Sub, TaskContinuationOptions.ExecuteSynchronously)

                ElseIf handler.GetExtension().ToLower() = ".pfdx" Then
                    SaveJSON(handler, Me.ActiveMdiChild)
                Else
                    Me.bgSaveFile.RunWorkerAsync()
                End If
                If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                    DirectCast(ActiveMdiChild, FormFlowsheet).FlowsheetOptions.VirtualFile = handler
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

    Private Sub VerToolStripMenuItem_DropDownOpened(sender As Object, e As EventArgs) Handles EditTSMI.DropDownOpened

        If Me.ActiveMdiChild IsNot Nothing Then
            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                DirectCast(Me.ActiveMdiChild, FormFlowsheet).UpdateToolstripItemVisibility()
            End If
        End If

    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles ButtonClose.Click
        Me.SettingsPanel.Visible = False
        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
    End Sub

    Public Sub NewToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NewToolStripButton.Click, NewToolStripMenuItem.Click

        Me.WelcomePanel.Visible = False
        PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim newform As New FormFlowsheet()

        RaiseEvent ToolOpened("New Flowsheet", New EventArgs())

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

                Dim handler = New SharedClassesCSharp.FilePicker.Windows.WindowsFile(myLink.Tag.ToString)

                Me.WelcomePanel.Visible = False
                PainelDeBoasvindasToolStripMenuItem.Checked = False

                Dim floading As New FormLoadingSimulation

                floading.Text = DWSIM.App.GetLocalString("Loading") + " '" + Path.GetFileNameWithoutExtension(myLink.Tag.ToString()) + "'..."
                floading.Show()

                Application.DoEvents()

                Dim nome = myLink.Tag.ToString
                Application.DoEvents()
                Dim objStreamReader As FileStream = Nothing
                Try
                    Select Case Path.GetExtension(nome).ToLower()
                        Case ".dwxml"
                            LoadXML(handler, Sub(x)
                                                 Me.Invoke(Sub()
                                                               floading.ProgressBar1.Value = x
                                                               floading.Refresh()
                                                           End Sub)
                                             End Sub)
                        Case ".dwxmz"
                            LoadAndExtractXMLZIP(handler, Sub(x)
                                                              Me.Invoke(Sub()
                                                                            floading.ProgressBar1.Value = x
                                                                            floading.Refresh()
                                                                        End Sub)
                                                          End Sub)
                        Case ".dwsim"
                            ' Me.LoadF(nome)
                        Case ".xml"
                            LoadMobileXML(handler)
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
        LoadFileDialog()

    End Sub

    Public Sub SaveToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripButton.Click, SaveToolStripMenuItem.Click

        SaveFile(True)

    End Sub

    'Make sure that IVirtualFile is not WindowsFile if shouldSaveToDashboard, because Save causes it to save it to file system
    Function IsCorrectVirtualFile(ByVal shouldSaveToDashboard As Boolean, ByVal file As IVirtualFile) As Boolean
        If shouldSaveToDashboard = True Then
            If TypeOf file Is WindowsFile Then
                Return False
            End If
        End If
        Return True
    End Function

    Public Function SaveFile(ByVal saveasync As Boolean, Optional saveToDashboard As Boolean = False) As String

        If My.Computer.Keyboard.ShiftKeyDown Then saveasync = False

        Dim filePickerForm As IFilePicker = SharedClassesCSharp.FilePicker.FilePickerService.GetInstance().GetFilePicker()

        If saveToDashboard Then
            filePickerForm = New Simulate365.FormFactories.S365FilePickerForm()
        End If

        Dim filename As String

        saveToDashboard = saveToDashboard Or TypeOf filePickerForm Is S365FilePickerForm

        If Not Me.ActiveMdiChild Is Nothing Then
            If TypeOf Me.ActiveMdiChild Is FormFlowsheet Then
                Dim form2 As FormFlowsheet = Me.ActiveMdiChild
                ' SavingSimulation event
                If SavingSimulation IsNot Nothing Then
                    If SavingSimulation.Invoke(form2) = False Then Return ""
                End If

                ' save window file to existing location
                If File.Exists(form2.Options.FilePath) And saveToDashboard = False Then
                    Dim handler = New SharedClassesCSharp.FilePicker.Windows.WindowsFile(form2.Options.FilePath)
                    ' If file exists, save to same location
                    'Application.DoEvents()
                    filename = form2.Options.FilePath
                    SaveBackup(handler)
                    If Path.GetExtension(filename).ToLower = ".dwxml" Then
                        SaveXML(handler, form2)
                    ElseIf Path.GetExtension(filename).ToLower = ".xml" Then
                        If saveasync Then
                            TaskHelper.Run(Sub() SaveMobileXML(handler, form2)).ContinueWith(Sub(t)
                                                                                                 'Me.ToolStripStatusLabel1.Text = ""
                                                                                                 If Not t.Exception Is Nothing Then
                                                                                                     form2.WriteToLog(DWSIM.App.GetLocalString("Erroaosalvararquivo") & t.Exception.ToString, Color.Red, MessageType.GeneralError)
                                                                                                     Console.WriteLine(t.Exception.ToString())
                                                                                                 End If
                                                                                             End Sub, TaskContinuationOptions.ExecuteSynchronously)
                        Else
                            SaveMobileXML(handler, form2)
                        End If
                    ElseIf Path.GetExtension(filename).ToLower = ".dwxmz" Then
                        SaveXMLZIP(handler, form2)
                    ElseIf Path.GetExtension(filename).ToLower = ".pfdx" Then
                        SaveJSON(handler, form2)
                    End If
                Else ' If file doesn't exist, open file picker
                    Try
                        Dim fname = Path.GetFileNameWithoutExtension(form2.Options.FilePath)
                        Dim fpath = Path.GetDirectoryName(form2.Options.FilePath)
                        filePickerForm.SuggestedFilename = fname
                        filePickerForm.SuggestedDirectory = fpath
                    Catch ex As Exception
                    End Try
                    Dim handler As IVirtualFile = Nothing
                    If (form2.Options.VirtualFile IsNot Nothing And IsCorrectVirtualFile(saveToDashboard, form2.Options.VirtualFile)) Then
                        handler = form2.Options.VirtualFile
                    Else
                        handler = filePickerForm.ShowSaveDialog(
                                  New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
                                    {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Simulation File", New String() {"*.dwxmz", "*.dwxml", "*.xml", ".pfdx"})
                                  })
                        form2.Options.VirtualFile = handler
                    End If
                    If handler IsNot Nothing Then
                        SaveBackup(handler)
                        'Application.DoEvents()
                        Console.WriteLine(handler.GetExtension().ToLower())
                        If handler.GetExtension().ToLower() = ".dwxml" Then
                            SaveXML(handler, Me.ActiveMdiChild)
                        ElseIf handler.GetExtension().ToLower() = ".xml" Then
                            SaveMobileXML(handler, Me.ActiveMdiChild)
                        ElseIf handler.GetExtension().ToLower() = ".dwxmz" Then
                            SaveXMLZIP(handler, Me.ActiveMdiChild)
                        ElseIf handler.GetExtension().ToLower() = ".pfdx" Then
                            SaveJSON(handler, Me.ActiveMdiChild)
                        End If
                    End If
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormCompoundCreator Then
                Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                        New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
                        {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Compound Creator Study File", "*.dwcsd2")})
                If handler IsNot Nothing Then
                    Using stream As New IO.MemoryStream()
                        Using writer As New IO.StreamWriter(stream) With {.AutoFlush = True}
                            SaveBackup(handler)
                            CType(Me.ActiveMdiChild, FormCompoundCreator).mycase.Filename = handler.FullPath
                            CType(Me.ActiveMdiChild, FormCompoundCreator).StoreData()
                            Dim text = Newtonsoft.Json.JsonConvert.SerializeObject(CType(Me.ActiveMdiChild, FormCompoundCreator).mycase, Newtonsoft.Json.Formatting.Indented)
                            writer.Write(text)
                            handler.Write(stream)
                            Me.ActiveMdiChild.Text = handler.FullPath
                        End Using
                    End Using
                    Return handler.FullPath
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormDataRegression Then
                Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                        New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
                        {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("Regression Study File", "*.dwrsd2")})
                If handler IsNot Nothing Then
                    Using stream As New IO.MemoryStream()
                        Using writer As New IO.StreamWriter(stream) With {.AutoFlush = True}
                            SaveBackup(handler)
                            Dim text = Newtonsoft.Json.JsonConvert.SerializeObject(CType(Me.ActiveMdiChild, FormDataRegression).StoreCase(), Newtonsoft.Json.Formatting.Indented)
                            writer.Write(text)
                            handler.Write(stream)
                            Me.ActiveMdiChild.Text = handler.FullPath
                        End Using
                    End Using
                    Return handler.FullPath
                End If
            ElseIf TypeOf Me.ActiveMdiChild Is FormUNIFACRegression Then
                Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                        New List(Of SharedClassesCSharp.FilePicker.FilePickerAllowedType) From
                        {New SharedClassesCSharp.FilePicker.FilePickerAllowedType("UNIFAC Regression Study File", "*.dwruf")})
                If handler IsNot Nothing Then
                    Using stream As New IO.MemoryStream()
                        SaveBackup(handler)
                        CType(Me.ActiveMdiChild, FormUNIFACRegression).StoreData()
                        Dim x As New BinaryFormatter
                        x.Serialize(stream, CType(Me.ActiveMdiChild, FormUNIFACRegression).mycase)
                        handler.Write(stream)
                        Me.ActiveMdiChild.Text = handler.FullPath
                    End Using
                    Return handler.FullPath
                End If
            End If
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Noexistemsimulaesati"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If
        Return ""

    End Function

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

    Private Sub WikiToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles WikiToolStripMenuItem.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://dwsim.org", "DWSIM")
        Else
            System.Diagnostics.Process.Start("https://dwsim.org")
        End If
    End Sub

    Private Sub ForumToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ForumToolStripMenuItem.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://dwsim.org/wiki/index.php?title=Support", "DWSIM Support (Open-Source)")
        Else
            System.Diagnostics.Process.Start("https://dwsim.org/wiki/index.php?title=Support")
        End If
    End Sub

    Private Sub RastreamentoDeBugsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RastreamentoDeBugsToolStripMenuItem.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://github.com/DanWBR/dwsim/issues", "DWSIM Issues (Open-Source)")
        Else
            System.Diagnostics.Process.Start("https://github.com/DanWBR/dwsim/issues")
        End If
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

    Private Sub ToolStripButton7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        System.Diagnostics.Process.Start("https://www.paypal.com/cgi-bin/webscr?item_name=Donation+to+DWSIM+-+Open+Source+Process+Simulator&cmd=_donations&business=danielwag%40gmail.com&lc=US")
    End Sub

    Private Sub ToolStripButton8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Me.AboutToolStripMenuItem_Click(sender, e)
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

        Me.WelcomePanel.Visible = False
        PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim NewMDIChild As New FormCompoundCreator()

        RaiseEvent ToolOpened("New Compound Creator", New EventArgs())

        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me
        'Display the new form.
        NewMDIChild.Text = "CompCreator" & m_childcount
        Me.ActivateMdiChild(NewMDIChild)
        NewMDIChild.Show()
        m_childcount += 1

    End Sub

    Private Sub NovoEstudoDeRegressaoDeDadosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NovoEstudoDeRegressaoDeDadosToolStripMenuItem.Click

        Me.WelcomePanel.Visible = False
        PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim NewMDIChild As New FormDataRegression()

        RaiseEvent ToolOpened("New Regression Study", New EventArgs())

        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me
        'Display the new form.
        NewMDIChild.Text = "DataRegression" & m_childcount
        Me.ActivateMdiChild(NewMDIChild)
        NewMDIChild.Show()
        m_childcount += 1
    End Sub

    Private Sub NovoRegressaoUNIFACIPs_Click(sender As Object, e As EventArgs) Handles NovoRegressaoUNIFACIPs.Click

        Me.WelcomePanel.Visible = False
        PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim NewMDIChild As New FormUNIFACRegression()

        RaiseEvent ToolOpened("New UNIFAC IP Regression", New EventArgs())

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

        RaiseEvent ToolOpened("View General Settings", New EventArgs())

        SettingsPanel.Width = 500 * Settings.DpiScale
        SettingsPanel.Visible = True

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
                    Me.SaveXMLZIP(New SharedClassesCSharp.FilePicker.Windows.WindowsFile(path), form0)
                    If Not My.Settings.BackupFiles.Contains(path) Then
                        My.Settings.BackupFiles.Add(path)
                        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
                    End If
                End If
            Next
        End If
    End Sub

    Private Sub PainelDeBoasvindasToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PainelDeBoasvindasToolStripMenuItem.Click
        If Me.PainelDeBoasvindasToolStripMenuItem.Checked Then
            Me.WelcomePanel.Visible = True
        Else
            Me.WelcomePanel.Visible = False
        End If
    End Sub

    Private Sub bgSaveBackup_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles bgSaveBackup.RunWorkerCompleted
        If Not (e.Error Is Nothing) Then
            ' There was an error during the operation.
            Console.WriteLine("Error saving backup file: " & e.Error.Message)
        End If
    End Sub

    Private Sub NNUOToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NNUOToolStripMenuItem.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=Neural_Network_Unit_Operation")
    End Sub

    Private Sub PNUOToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PNUOToolStripMenuItem.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=Pipe_Network_Unit_Operation")
    End Sub

    Private Sub CapitalCostToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CapitalCostToolStripMenuItem.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=Capital_Cost_Estimator")
    End Sub

    Private Sub OPCPluginToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OPCPluginToolStripMenuItem.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=OPC_Client_Plugin")
    End Sub

    Private Sub DTLToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DTLToolStripMenuItem.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=DTL")
    End Sub

    Private Sub PsycrometrySimulationTemplateToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PsycrometrySimulationTemplateToolStripMenuItem.Click
        Process.Start("https://github.com/Spogis/Psychrometry")
    End Sub

    Private Sub LoginToolStripButton_Click(sender As Object, e As EventArgs) Handles LoginButton.Click
        Dim loginForm As LoginForm = New LoginForm
        loginForm.ShowDialog()
    End Sub

    Private Sub LogoutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LogoutToolStripMenuItem.Click
        UserService.Logout()
    End Sub

    Private Sub LoggedInS365Button_Click(sender As Object, e As EventArgs) Handles LoggedInS365Button.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://simulate365.com", "Simulate 365")
        Else
            Process.Start("https://simulate365.com")
        End If
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles tsmiFreeProTrial.Click
        Dim userService As UserService = UserService.GetInstance()
        Dim isLoggedIn As Boolean = userService._IsLoggedIn()
        If isLoggedIn Then
            ProFeatures.Functions.DisplayTransitionForm(Me.AnalyticsProvider, Nothing, "Access DWSIM Pro Now")
        Else
            Dim loginForm = New LoginForm()
            loginForm.ShowDialog()
        End If

    End Sub

    Private Sub AbrirDoDashboardToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AbrirDoDashboardToolStripMenuItem.Click
        LoadFileDialog(True)
    End Sub

    Private Sub SaveToDashboardTSMI_Click(sender As Object, e As EventArgs) Handles SaveToDashboardTSMI.Click
        SaveFileDialog(True)
    End Sub

    Private Sub OpenFileS365_Click(sender As Object, e As EventArgs) Handles OpenFileS365.Click
        LoadFileDialog(True)
    End Sub

    Private Sub SaveFileS365_Click(sender As Object, e As EventArgs) Handles SaveFileS365.Click
        SaveFileDialog(True)
    End Sub

    Private Sub DashboardToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DashboardToolStripMenuItem.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://dashboard.simulate365.com", "Simulate 365 Dashboard")
        Else
            Process.Start("https://dashboard.simulate365.com")
        End If
    End Sub

    Private Sub DIscordChannelToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DIscordChannelToolStripMenuItem.Click
        If IsPro Then
            Dim fb As New FormBrowser()
            fb.Show()
            fb.DisplayURL("https://discord.com/channels/974049809176608818/974049809176608821", "DWSIM Discord Server (Open-Source)")
        Else
            Process.Start("https://discord.com/channels/974049809176608818/974049809176608821")
        End If
    End Sub

    Private Sub ToolStripDropDownButton2_Click(sender As Object, e As EventArgs) Handles tsbdonate1.Click
        Process.Start("https://www.buymeacoffee.com/dwsim")
    End Sub

    Private Sub ToolStripDropDownButton1_Click(sender As Object, e As EventArgs) Handles tsbdonate2.Click
        Process.Start("https://www.patreon.com/dwsim")
    End Sub

    Private Sub ToolStripButton9_Click(sender As Object, e As EventArgs)

        LoadFileDialog()

    End Sub

    Private Sub WhatsNewToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles WhatsNewToolStripMenuItem.Click
        Dim frm As New FormWhatsNew()
        frm.Show()
    End Sub

    Private Sub tsmiPrivateSupport_Click(sender As Object, e As EventArgs) Handles tsmiPrivateSupport.Click
        Process.Start("https://simulate365.com/private-support/")
    End Sub

    Private Sub ToolStripDropDownButton1_Click_1(sender As Object, e As EventArgs) Handles tsbQuickQuestion.Click

        Dim fq As New FormOccupancyQuestion()
        fq.ShowDialog(Me)

    End Sub

    Private Sub UsersGuideToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles UsersGuideToolStripMenuItem.Click

        RaiseEvent ToolOpened("View User Guide", New EventArgs())

        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "User_Guide.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            If IsPro Then
                Dim fb As New FormBrowser()
                fb.Show()
                fb.DisplayURL(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "user_guide.pdf")
            Else
                Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "user_guide.pdf")
            End If
        End If

    End Sub

    Private Sub tsmiProUG_Click(sender As Object, e As EventArgs) Handles tsmiProUG.Click

        RaiseEvent ToolOpened("View DWSIM Pro User Guide", New EventArgs())

        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "Pro_User_Guide.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "Pro_User_Guide.pdf")
        End If

    End Sub

    Private Sub tsmiNewCompoundWizard_Click(sender As Object, e As EventArgs) Handles tsmiNewCompoundWizard.Click

        Dim wform As New UI.Desktop.Editors.CompoundCreatorWizard(Nothing)
        wform.SetupAndDisplayPage(1)

    End Sub

    Private Sub ToolStripDropDownButton1_Click_2(sender As Object, e As EventArgs) Handles ToolStripDropDownButton1.Click

        Process.Start("https://www.patreon.com/dwsim/shop")

    End Sub

    Private Sub tsbInspector_CheckedChanged(sender As Object, e As EventArgs) Handles tsbInspector.CheckedChanged
        GlobalSettings.Settings.InspectorEnabled = tsbInspector.Checked
        FrmOptions.chkEnableInspector.Checked = tsbInspector.Checked
    End Sub

#End Region

End Class
