Imports System.IO
Imports Nini.Config
Imports System.Globalization
Imports System.Reflection
Imports System.Linq
Imports Cudafy

'    Shared Functions
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

Namespace DWSIM

    <System.Serializable()> Public Class App

        Public Shared Function InitializeSettings() As List(Of Exception)

            'set language
            GlobalSettings.Settings.CultureInfo = My.Settings.CultureInfo
            GlobalSettings.Settings.CurrentCulture = My.Settings.CultureInfo

            'set CUDA params
            CudafyModes.Compiler = eGPUCompiler.All
            CudafyModes.Target = My.Settings.CudafyTarget

            'set global settings
            GlobalSettings.Settings.SolverMode = My.Settings.SolverMode
            GlobalSettings.Settings.SolverTimeoutSeconds = My.Settings.SolverTimeoutSeconds
            GlobalSettings.Settings.SolverBreakOnException = My.Settings.SolverBreakOnException
            GlobalSettings.Settings.TaskScheduler = My.Settings.TaskScheduler
            GlobalSettings.Settings.MaxThreadMultiplier = My.Settings.MaxThreadMultiplier
            GlobalSettings.Settings.MaxDegreeOfParallelism = My.Settings.MaxDegreeOfParallelism
            GlobalSettings.Settings.EnableParallelProcessing = My.Settings.EnableParallelProcessing
            GlobalSettings.Settings.EnableGPUProcessing = My.Settings.EnableGPUProcessing
            GlobalSettings.Settings.CudafyTarget = My.Settings.CudafyTarget
            GlobalSettings.Settings.CudafyDeviceID = My.Settings.CudafyDeviceID
            GlobalSettings.Settings.UseSIMDExtensions = My.Settings.UseSIMDExtensions
            GlobalSettings.Settings.DebugLevel = My.Settings.DebugLevel
            GlobalSettings.Settings.DefaultEditFormLocation = My.Settings.DefaultEditorLocation

            GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents = My.Settings.HideSolidPhase_CO

            GlobalSettings.Settings.OctavePath = My.Settings.OctavePath
            GlobalSettings.Settings.OctaveFileTempDir = My.Settings.OctaveTempPath
            GlobalSettings.Settings.OctaveTimeoutInMinutes = My.Settings.OctaveProcessTimeout

            GlobalSettings.Settings.CalculatorActivated = True

            GlobalSettings.Settings.UserDatabases.Clear()
            If My.Settings.UserDatabases Is Nothing Then My.Settings.UserDatabases = New Specialized.StringCollection
            For Each item In My.Settings.UserDatabases
                GlobalSettings.Settings.UserDatabases.Add(item)
            Next

            GlobalSettings.Settings.UserInteractionsDatabases.Clear()
            If My.Settings.UserInteractionsDatabases Is Nothing Then My.Settings.UserInteractionsDatabases = New Specialized.StringCollection
            For Each item In My.Settings.UserInteractionsDatabases
                GlobalSettings.Settings.UserInteractionsDatabases.Add(item)
            Next

            Try
                If GlobalSettings.Settings.EnableGPUProcessing Then Calculator.InitComputeDevice()
            Catch ex As Exception
                MessageBox.Show("GPU initialization failed: " & ex.Message)
            End Try

            Return Thermodynamics.NativeLibraries.Files.InitLibraries()

        End Function

        Public Shared Sub WriteToConsole(text As String, minlevel As Integer)

            If My.Settings.RedirectOutput Then
                My.Application.OpenForms(0).UIThread(Sub()
                                                         If My.Settings.DebugLevel >= minlevel Then
                                                             If Not My.Application.ActiveSimulation Is Nothing Then
                                                                 If Not My.Application.ActiveSimulation.FormOutput Is Nothing Then
                                                                     My.Application.ActiveSimulation.FormOutput.TextBox1.AppendText(text & vbCrLf)
                                                                 End If
                                                             End If
                                                         End If
                                                     End Sub)
            Else
                If My.Settings.DebugLevel >= minlevel Then Console.WriteLine(text)
            End If

        End Sub

        Public Shared Sub CheckParallelPInvoke()

            If My.Settings.EnableParallelProcessing Then Throw New InvalidOperationException(DWSIM.App.GetLocalString("ParallelPInvokeError"))

        End Sub

        Public Shared Sub HelpRequested(topic As String)
            Dim pathsep = System.IO.Path.DirectorySeparatorChar
            Dim filename As String = My.Application.Info.DirectoryPath & pathsep & "help" & pathsep & topic
            System.Diagnostics.Process.Start(filename)
        End Sub

        Public Shared Function GetLocalTipString(ByVal id As String) As String

            If My.Application._HelpManager Is Nothing Then

                'loads the current language
                My.Application._CultureInfo = New Globalization.CultureInfo(My.Settings.CultureInfo)
                My.Application.ChangeUICulture(My.Settings.CultureInfo)

                'loads the resource manager
                My.Application._HelpManager = New System.Resources.ResourceManager("DWSIM.Tips", System.Reflection.Assembly.GetExecutingAssembly())

            End If

            If id <> "" Then
                Dim retstr As String
                retstr = My.Application._HelpManager.GetString(id, My.Application._CultureInfo)
                If retstr Is Nothing Then Return id Else Return retstr
            Else
                Return ""
            End If
        End Function

        Public Shared Function GetLocalString(ByVal id As String) As String

            If My.Application._ResourceManager Is Nothing Then

                Dim cultureinfo As String = "en"
                Try
                    cultureinfo = My.Settings.CultureInfo
                Catch ex As System.Xml.XmlException
                    Dim filename As String = ex.SourceUri
                    Console.WriteLine(ex.Message & " [" & filename & "]")
                    If File.Exists(filename) Then File.Delete(filename)
                End Try

                'loads the current language
                My.Application._CultureInfo = New Globalization.CultureInfo(cultureinfo)
                My.Application.ChangeUICulture(cultureinfo)

                'loads the resource manager
                My.Application._ResourceManager = New System.Resources.ResourceManager("DWSIM.DWSIM", System.Reflection.Assembly.GetExecutingAssembly())

            End If

            If id <> "" Then
                Dim retstr As String
                retstr = My.Application._ResourceManager.GetString(id, My.Application._CultureInfo)
                If retstr Is Nothing Then Return id Else Return retstr
            Else
                Return ""
            End If
        End Function

        Public Shared Function GetPropertyName(ByVal PropID As String, Optional ByRef fp As FormMain = Nothing) As String

            If My.Application._ResourceManager Is Nothing Then

                'loads the current language
                My.Application._CultureInfo = New Globalization.CultureInfo(My.Settings.CultureInfo)
                My.Application.ChangeUICulture(My.Settings.CultureInfo)

                'loads the resource manager
                My.Application._ResourceManager = New System.Resources.ResourceManager("DWSIM.DWSIM", System.Reflection.Assembly.GetExecutingAssembly())

            End If

            'loads the property name manager
            If My.Application._PropertyNameManager Is Nothing Then

                My.Application._PropertyNameManager = New System.Resources.ResourceManager("DWSIM.Properties", System.Reflection.Assembly.GetExecutingAssembly())

            End If

            Dim retstr As String
            If Not PropID Is Nothing Then
                Dim prop As String = PropID.Split("/")(0)
                Dim sname As String = ""
                If PropID.Split("/").Length = 2 Then
                    sname = PropID.Split("/")(1)
                    retstr = My.Application._PropertyNameManager.GetString(prop, My.Application._CultureInfo) + " / " + sname
                    If retstr Is Nothing Then Return PropID Else Return retstr
                Else
                    retstr = My.Application._PropertyNameManager.GetString(prop, My.Application._CultureInfo)
                    If retstr Is Nothing Then Return PropID Else Return retstr
                End If
            Else
                retstr = ""
            End If

            Return Nothing

        End Function

        Public Shared Function GetComponentType(ByRef comp As BaseClasses.ConstantProperties) As String
            If comp.IsHYPO Then
                Return GetLocalString("CompHypo")
            ElseIf comp.IsPF Then
                Return GetLocalString("CompPseudo")
            ElseIf comp.IsBlackOil Then
                Return "Black Oil"
            Else
                Return GetLocalString("CompNormal")
            End If
        End Function

        Public Shared Function IsMainThread() As Boolean
            Return System.Threading.Thread.CurrentThread.ManagedThreadId = My.Application.MainThreadId
        End Function

        Public Shared Function IsRunningOnMono() As Boolean
            Return Not Type.GetType("Mono.Runtime") Is Nothing
        End Function

        Public Enum Platform
            Windows
            Linux
            Mac
        End Enum

        Public Shared Function RunningPlatform() As Platform
            Select Case Environment.OSVersion.Platform
                Case PlatformID.Unix
                    ' Well, there are chances MacOSX is reported as Unix instead of MacOSX.
                    ' Instead of platform check, we'll do a feature checks (Mac specific root folders)
                    If Directory.Exists("/Applications") And Directory.Exists("/System") And Directory.Exists("/Users") And Directory.Exists("/Volumes") Then
                        Return Platform.Mac
                    Else
                        Return Platform.Linux
                    End If
                Case PlatformID.MacOSX
                    Return Platform.Mac
                Case Else
                    Return Platform.Windows
            End Select
        End Function

        Shared Sub LoadSettings(Optional ByVal configfile As String = "")

            If configfile = "" Then configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
            If Not File.Exists(configfile) Then File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile)

            Dim source As New IniConfigSource(configfile)
            Dim col() As String

            My.Settings.MostRecentFiles = New Collections.Specialized.StringCollection()

            With source
                col = .Configs("RecentFiles").GetValues()
                For Each Str As String In col
                    My.Settings.MostRecentFiles.Add(Str)
                Next
            End With

            My.Settings.ScriptPaths = New Collections.Specialized.StringCollection()

            With source
                col = .Configs("ScriptPaths").GetValues()
                For Each Str As String In col
                    My.Settings.ScriptPaths.Add(Str)
                Next
            End With

            My.Settings.UserDatabases = New Collections.Specialized.StringCollection()

            With source
                col = .Configs("UserDatabases").GetValues()
                For Each Str As String In col
                    My.Settings.UserDatabases.Add(Str)
                Next
            End With

            My.Settings.UserInteractionsDatabases = New Collections.Specialized.StringCollection()

            With source
                col = .Configs("UserInteractionsDatabases").GetValues()
                For Each Str As String In col
                    My.Settings.UserInteractionsDatabases.Add(Str)
                Next
            End With

            My.Settings.BackupFiles = New Collections.Specialized.StringCollection()

            With source
                col = .Configs("BackupFiles").GetValues()
                For Each Str As String In col
                    My.Settings.BackupFiles.Add(Str)
                Next
            End With

            My.Settings.BackupActivated = source.Configs("Backup").GetBoolean("BackupActivated", True)
            My.Settings.BackupFolder = source.Configs("Backup").Get("BackupFolder", My.Computer.FileSystem.SpecialDirectories.Temp + Path.DirectorySeparatorChar + "DWSIM")
            My.Settings.BackupInterval = source.Configs("Backup").GetInt("BackupInterval", 5)

            My.Settings.CultureInfo = source.Configs("Localization").Get("CultureInfo", "en-US")

            My.Settings.ChemSepDatabasePath = source.Configs("Databases").Get("ChemSepDBPath", "")
            My.Settings.ReplaceComps = source.Configs("Databases").GetBoolean("ReplaceComps", True)

            My.Settings.UserUnits = source.Configs("Misc").Get("UserUnits", "")
            My.Settings.ShowTips = source.Configs("Misc").GetBoolean("ShowTips", True)
            My.Settings.RedirectOutput = source.Configs("Misc").GetBoolean("RedirectConsoleOutput", False)

            My.Settings.EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", False)
            My.Settings.MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
            My.Settings.EnableGPUProcessing = source.Configs("Misc").GetBoolean("EnableGPUProcessing", False)
            My.Settings.SelectedGPU = source.Configs("Misc").Get("SelectedGPU", "")
            My.Settings.CudafyTarget = source.Configs("Misc").GetInt("CudafyTarget", 1)
            My.Settings.CudafyDeviceID = source.Configs("Misc").GetInt("CudafyDeviceID", 0)

            My.Settings.DebugLevel = source.Configs("Misc").GetInt("DebugLevel", 0)
            My.Settings.SolverMode = source.Configs("Misc").GetInt("SolverMode", 0)
            My.Settings.ServiceBusConnectionString = source.Configs("Misc").Get("ServiceBusConnectionString", "")
            My.Settings.ServerIPAddress = source.Configs("Misc").Get("ServerIPAddress", "")
            My.Settings.ServerPort = source.Configs("Misc").Get("ServerPort", "")
            My.Settings.SolverTimeoutSeconds = source.Configs("Misc").GetInt("SolverTimeoutSeconds", 300)

            My.Settings.SaveBackupFile = source.Configs("Misc").GetBoolean("SaveBackupFile", True)
            My.Settings.MaxThreadMultiplier = source.Configs("Misc").GetInt("MaxThreadMultiplier", 8)
            My.Settings.TaskScheduler = source.Configs("Misc").GetInt("TaskScheduler", 0)
            My.Settings.UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

            My.Settings.CloseFormsOnDeselecting = source.Configs("Misc").GetBoolean("CloseFormsOnDeselecting", True)

            My.Settings.AutomaticUpdates = source.Configs("Misc").GetBoolean("AutoUpdate", True)

            My.Settings.DefaultEditorLocation = source.Configs("Misc").GetInt("DefaultEditorLocation", 8)
            My.Settings.EnableMultipleObjectEditors = source.Configs("Misc").GetBoolean("EnableMultipleObjectEditors", True)
            My.Settings.SimulationUpgradeWarning = source.Configs("Misc").GetBoolean("SimulationUpgradeWarning", True)

            My.Settings.HideSolidPhase_CO = source.Configs("Misc").GetBoolean("HideSolidPhase_COInterface", False)
            My.Settings.IgnoreCompoundPropertiesOnLoad = source.Configs("Misc").GetBoolean("IgnoreCompoundConstantPropertyDatainXMLFile", False)

        End Sub

        Shared Sub SaveSettings(Optional ByVal configfile As String = "")

            If configfile = "" Then
                configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
                File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile, True)
            Else
                File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "excelcompat.ini", configfile, True)
            End If


            Dim source As New IniConfigSource(configfile)

            For Each Str As String In My.Settings.MostRecentFiles
                source.Configs("RecentFiles").Set(My.Settings.MostRecentFiles.IndexOf(Str), Str)
            Next

            For Each Str As String In My.Settings.ScriptPaths
                source.Configs("ScriptPaths").Set(My.Settings.ScriptPaths.IndexOf(Str), Str)
            Next

            For Each Str As String In My.Settings.UserDatabases
                source.Configs("UserDatabases").Set(My.Settings.UserDatabases.IndexOf(Str), Str)
            Next

            For Each Str As String In My.Settings.UserInteractionsDatabases
                source.Configs("UserInteractionsDatabases").Set(My.Settings.UserInteractionsDatabases.IndexOf(Str), Str)
            Next

            For Each Str As String In My.Settings.BackupFiles
                source.Configs("BackupFiles").Set(My.Settings.BackupFiles.IndexOf(Str), Str)
            Next

            source.Configs("Backup").Set("BackupActivated", My.Settings.BackupActivated)
            source.Configs("Backup").Set("BackupFolder", My.Settings.BackupFolder)
            source.Configs("Backup").Set("BackupInterval", My.Settings.BackupInterval)

            source.Configs("Localization").Set("CultureInfo", My.Settings.CultureInfo)

            source.Configs("Databases").Set("ChemSepDBPath", My.Settings.ChemSepDatabasePath)
            source.Configs("Databases").Set("ReplaceComps", My.Settings.ReplaceComps)

            source.Configs("Misc").Set("UserUnits", My.Settings.UserUnits)
            source.Configs("Misc").Set("ShowTips", My.Settings.ShowTips)
            source.Configs("Misc").Set("RedirectConsoleOutput", My.Settings.RedirectOutput)

            source.Configs("Misc").Set("EnableParallelProcessing", My.Settings.EnableParallelProcessing)
            source.Configs("Misc").Set("MaxDegreeOfParallelism", My.Settings.MaxDegreeOfParallelism)
            source.Configs("Misc").Set("EnableGPUProcessing", My.Settings.EnableGPUProcessing)
            source.Configs("Misc").Set("SelectedGPU", My.Settings.SelectedGPU)
            source.Configs("Misc").Set("CudafyTarget", My.Settings.CudafyTarget)
            source.Configs("Misc").Set("CudafyDeviceID", My.Settings.CudafyDeviceID)

            source.Configs("Misc").Set("DebugLevel", My.Settings.DebugLevel)
            source.Configs("Misc").Set("SolverMode", My.Settings.SolverMode)
            source.Configs("Misc").Set("ServiceBusConnectionString", My.Settings.ServiceBusConnectionString)
            source.Configs("Misc").Set("ServerIPAddress", My.Settings.ServerIPAddress)
            source.Configs("Misc").Set("ServerPort", My.Settings.ServerPort)
            source.Configs("Misc").Set("SolverTimeoutSeconds", My.Settings.SolverTimeoutSeconds)
            source.Configs("Misc").Set("SaveBackupFile", My.Settings.SaveBackupFile)
            source.Configs("Misc").Set("MaxThreadMultiplier", My.Settings.MaxThreadMultiplier)
            source.Configs("Misc").Set("TaskScheduler", My.Settings.TaskScheduler)
            source.Configs("Misc").Set("UseSIMDExtensions", My.Settings.UseSIMDExtensions)
            source.Configs("Misc").Set("CloseFormsOnDeselecting", My.Settings.CloseFormsOnDeselecting)
            source.Configs("Misc").Set("AutoUpdate", My.Settings.AutomaticUpdates)

            source.Configs("Misc").Set("DefaultEditorLocation", My.Settings.DefaultEditorLocation)
            source.Configs("Misc").Set("EnableMultipleObjectEditors", My.Settings.EnableMultipleObjectEditors)
            source.Configs("Misc").Set("SimulationUpgradeWarning", My.Settings.SimulationUpgradeWarning)

            source.Configs("Misc").Set("HideSolidPhase_COInterface", My.Settings.HideSolidPhase_CO)
            source.Configs("Misc").Set("IgnoreCompoundConstantPropertyDatainXMLFile", My.Settings.IgnoreCompoundPropertiesOnLoad)

            source.Save(configfile)

        End Sub

    End Class

End Namespace
