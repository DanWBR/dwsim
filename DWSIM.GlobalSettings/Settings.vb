Imports Cudafy
Imports System.Threading
Imports Nini.Config
Imports System.IO

Public Class Settings

    Public Enum Platform
        Windows
        Linux
        Mac
    End Enum

    Public Enum WindowsPlatformRenderer
        WinForms = 0
        WinForms_Direct2D = 1
        WPF = 2
        Gtk2 = 3
    End Enum

    Public Enum LinuxPlatformRenderer
        Gtk2 = 0
        WinForms = 1
    End Enum

    Public Enum MacOSPlatformRenderer
        MonoMac = 0
        Gtk2 = 1
        WinForms = 2
    End Enum

    Public Enum SkiaCanvasRenderer
        CPU = 0
        OpenGL = 1
    End Enum

    Public Shared Property WindowsRenderer As WindowsPlatformRenderer = WindowsPlatformRenderer.WinForms

    Public Shared Property LinuxRenderer As LinuxPlatformRenderer = LinuxPlatformRenderer.Gtk2

    Public Shared Property MacOSRenderer As MacOSPlatformRenderer = MacOSPlatformRenderer.MonoMac

    Public Shared Property FlowsheetRenderer As SkiaCanvasRenderer = SkiaCanvasRenderer.CPU

    Public Shared Property AppTaskScheduler As TaskScheduler = Tasks.TaskScheduler.Default

    Public Shared Property gpu As Cudafy.Host.GPGPU
    Public Shared Property gpumod As CudafyModule
    Public Shared Property prevlang As Integer = 0 '0 = CUDA, 1 = OpenCL

    Private Shared _tcks As CancellationTokenSource
    Public Shared Property TaskCancellationTokenSource As CancellationTokenSource
        Get
            If _tcks Is Nothing Then _tcks = New CancellationTokenSource
            Return _tcks
        End Get
        Set(value As CancellationTokenSource)
            _tcks = value
        End Set
    End Property
    Public Shared Property CAPEOPENMode As Boolean = False
    Public Shared Property ExcelMode As Boolean = False
    Public Shared Property MaxDegreeOfParallelism As Integer = -1
    Public Shared Property UseSIMDExtensions As Boolean = False
    Public Shared Property EnableParallelProcessing As Boolean = True
    Public Shared Property EnableGPUProcessing As Boolean = False
    Public Shared Property CudafyTarget As Integer = 0
    Public Shared Property CudafyDeviceID As Integer = 0
    Public Shared Property DebugLevel As Integer = 0
    Public Shared Property MaxThreadMultiplier As Integer = 8
    Public Shared Property TaskScheduler As Integer = 0
    Public Shared Property SolverTimeoutSeconds As Integer = 300
    Public Shared Property SolverMode As Integer = 0
    Public Shared Property ServiceBusConnectionString As String = ""
    Public Shared Property CalculatorStopRequested As Boolean
    Public Shared Property CalculatorActivated As Boolean
    Public Shared Property CalculatorBusy As Boolean
    Public Shared Property ServerIPAddress As String = ""
    Public Shared Property ServerPort As Integer = 0
    Public Shared Property CurrentCulture As String = "en"

    Public Shared DefaultEditFormLocation As Integer = 8

    Public Shared SolverBreakOnException As Boolean = False
    Public Shared Property SelectedGPU As String = ""
    Public Shared Property CultureInfo As String = "en"
    Public Shared Property InitializedCOPPM As Boolean = False
    Public Shared Property ExcelErrorHandlingMode As Integer = 0
    Public Shared Property ExcelFlashSettings As String = ""

    Public Shared Property UserInteractionsDatabases As New List(Of String)
    Public Shared Property UserDatabases As New List(Of String)

    Public Shared Property HideSolidPhaseFromCAPEOPENComponents As Boolean = False

    Public Shared Property DrawingAntiAlias As Boolean = True

    Public Shared Property AutomationMode As Boolean = False

    Public Shared Property OctavePath As String = ""

    Public Shared Property OctaveTimeoutInMinutes As Double = 5

    Public Shared Property CurrentEnvironment As Integer = 0

    Public Shared Property CurrentPlatform As String = "Windows"

    Public Shared Property PythonPath As String = ""

    Public Shared Property PythonTimeoutInMinutes As Double = 1

    Public Shared Property PythonInitialized As Boolean = False

    Public Shared Property EnableBackupCopies As Boolean = True

    Public Shared Property SaveExistingFile As Boolean = True

    Public Shared Property BackupInterval As Integer = 5

    Public Shared Property UserUnits As String = "{ }"

    Public Shared Property MostRecentFiles As New List(Of String)

    Public Shared Property ResultsReportFontSize As Integer = 10

    Public Shared Property OldUI As Boolean = True


    Public Shared AutomaticUpdates As Boolean = True


    Public Shared CurrentVersion As String = ""


    Public Shared CurrentRunningVersion As String = ""

    Public Shared Property CalculationRequestID As String = ""

    Public Shared Property InspectorEnabled As Boolean = True

    Public Shared Property ClearInspectorHistoryOnNewCalculationRequest As Boolean = True

    Public Shared Property EditorFontSize As Integer = -1

    Public Shared Property EditorTextBoxFixedSize As Boolean = True

    Public Shared Property EditOnSelect As Boolean = True

    Public Shared Property CallSolverOnEditorPropertyChanged As Boolean = True

    Public Shared Property DpiScale As Double = 1.0

    Public Shared Property DarkMode As Boolean = False

    Public Shared Property UIScalingFactor As Double = 1.0

    Public Shared Property ObjectEditor As Integer = 0

    Public Shared Property CrossPlatformUIItemSpacing As Integer = 5

    Public Shared Property EnableCustomTouchBar As Boolean = True

    Shared Sub LoadExcelSettings(Optional ByVal configfile As String = "")

        Dim configfiledir = GetConfigFileDir()

        If Not Directory.Exists(configfiledir) Then Directory.CreateDirectory(configfiledir)

        If configfile = "" Then configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"

        If Not File.Exists(configfile) Then File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile)

        Dim doc As New Nini.Ini.IniDocument(configfile, Nini.Ini.IniFileType.WindowsStyle)
        Dim source As New IniConfigSource(doc)
        Dim col As String()

        UserDatabases.Clear()

        With source
            col = .Configs("UserDatabases").GetValues()
            For Each str As String In col
                UserDatabases.Add(str)
            Next
        End With

        UserInteractionsDatabases.Clear()

        With source
            col = .Configs("UserInteractionsDatabases").GetValues()
            For Each str As String In col
                UserInteractionsDatabases.Add(str)
            Next
        End With

        EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", False)
        MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
        UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

        EnableGPUProcessing = source.Configs("Misc").GetBoolean("EnableGPUProcessing", False)
        SelectedGPU = source.Configs("Misc").GetString("SelectedGPU", "")
        CudafyTarget = source.Configs("Misc").GetInt("CudafyTarget", 0)
        CudafyDeviceID = source.Configs("Misc").GetInt("CudafyDeviceID", 0)

        If source.Configs("ExcelAddIn") Is Nothing Then source.AddConfig("ExcelAddIn")

        ExcelErrorHandlingMode = source.Configs("ExcelAddIn").GetInt("ExcelErrorHandlingMode", 0)
        ExcelFlashSettings = source.Configs("ExcelAddIn").GetString("ExcelFlashSettings", "")

        If source.Configs("OSInfo") Is Nothing Then source.AddConfig("OSInfo")

        CurrentPlatform = source.Configs("OSInfo").GetString("Platform")
        CurrentEnvironment = source.Configs("OSInfo").GetInt("Environment", 0)

        If source.Configs("OctaveBridge") Is Nothing Then source.AddConfig("OctaveBridge")

        OctavePath = source.Configs("OctaveBridge").GetString("OctavePath", "")
        OctaveTimeoutInMinutes = source.Configs("OctaveBridge").GetFloat("OctaveProcessTimeout", 15)

    End Sub

    Shared Sub SaveExcelSettings(Optional ByVal configfile As String = "")

        If configfile = "" Then
            configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
            File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile, True)
        Else
            File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "excelcompat.ini", configfile, True)
        End If

        Dim source As New IniConfigSource(configfile)

        source.Configs("Misc").Set("EnableParallelProcessing", Settings.EnableParallelProcessing)
        source.Configs("Misc").Set("MaxDegreeOfParallelism", Settings.MaxDegreeOfParallelism)
        source.Configs("Misc").Set("EnableGPUProcessing", Settings.EnableGPUProcessing)
        source.Configs("Misc").Set("SelectedGPU", Settings.SelectedGPU)
        source.Configs("Misc").Set("CudafyTarget", Settings.CudafyTarget)
        source.Configs("Misc").Set("CudafyDeviceID", Settings.CudafyDeviceID)
        source.Configs("Misc").Set("UseSIMDExtensions", Settings.UseSIMDExtensions)

        If source.Configs("ExcelAddIn") Is Nothing Then source.AddConfig("ExcelAddIn")

        source.Configs("ExcelAddIn").Set("ExcelErrorHandlingMode", Settings.ExcelErrorHandlingMode)
        source.Configs("ExcelAddIn").Set("ExcelFlashSettings", Settings.ExcelFlashSettings)

        If source.Configs("OSInfo") Is Nothing Then source.AddConfig("OSInfo")

        source.Configs("OSInfo").Set("Platform", GetPlatform)
        source.Configs("OSInfo").Set("Environment", GetEnvironment)

        If source.Configs("OctaveBridge") Is Nothing Then source.AddConfig("OctaveBridge")

        source.Configs("OctaveBridge").Set("OctavePath", OctavePath)
        source.Configs("OctaveBridge").Set("OctaveProcessTimeout", OctaveTimeoutInMinutes)

        Try
            For Each spath In UserDatabases
                source.Configs("UserDatabases").Set(IO.Path.GetFileNameWithoutExtension(spath), spath)
            Next
        Catch ex As Exception
        End Try

        Try
            For Each spath In UserInteractionsDatabases
                source.Configs("UserInteractionsDatabases").Set(IO.Path.GetFileNameWithoutExtension(spath), spath)
            Next
        Catch ex As Exception
        End Try

        source.Save(configfile)

    End Sub

    Public Shared Function GetEnvironment() As Integer

        If Environment.Is64BitProcess Then
            Return 64
        Else
            Return 32
        End If

    End Function

    Public Shared Function GetPlatform() As String

        If RunningPlatform() = Platform.Windows Then
            Return "Windows"
        ElseIf RunningPlatform() = Platform.Linux Then
            Return "Linux"
        ElseIf RunningPlatform() = Platform.Mac Then
            Return "Mac"
        Else
            Return "None"
        End If

    End Function

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

    Public Shared Function IsRunningOnMono() As Boolean
        Return Not Type.GetType("Mono.Runtime") Is Nothing
    End Function

    Shared Function GetConfigFileDir() As String
        Dim configfiledir As String = ""
        If Settings.RunningPlatform = Platform.Mac Then
            configfiledir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), "Documents", "DWSIM Application Data") & Path.DirectorySeparatorChar
        Else
            configfiledir = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar
        End If
        Return configfiledir
    End Function

    Shared Sub LoadSettings(Optional ByVal configfile As String = "")

        Dim configfiledir = GetConfigFileDir()

        If Not Directory.Exists(configfiledir) Then Directory.CreateDirectory(configfiledir)

        If configfile = "" Then configfile = configfiledir & "dwsim.ini" Else configfile = configfiledir & configfile

        If Not File.Exists(configfile) Then File.WriteAllText(configfile, "")

        Dim source As New IniConfigSource(configfile)
        Dim col() As String

        MostRecentFiles = New List(Of String)

        If source.Configs("RecentFiles") Is Nothing Then source.AddConfig("RecentFiles")

        With source
            col = .Configs("RecentFiles").GetValues()
            For Each Str As String In col
                MostRecentFiles.Add(Str)
            Next
        End With

        UserDatabases = New List(Of String)

        If source.Configs("UserDatabases") Is Nothing Then source.AddConfig("UserDatabases")

        With source
            col = .Configs("UserDatabases").GetValues()
            For Each Str As String In col
                UserDatabases.Add(Str)
            Next
        End With

        UserInteractionsDatabases = New List(Of String)

        If source.Configs("UserInteractionsDatabases") Is Nothing Then source.AddConfig("UserInteractionsDatabases")

        With source
            col = .Configs("UserInteractionsDatabases").GetValues()
            For Each Str As String In col
                UserInteractionsDatabases.Add(Str)
            Next
        End With

        If source.Configs("Backup") Is Nothing Then source.AddConfig("Backup")

        EnableBackupCopies = source.Configs("Backup").GetBoolean("EnableBackupCopies", True)
        BackupInterval = source.Configs("Backup").GetInt("BackupInterval", 3)

        If source.Configs("Localization") Is Nothing Then source.AddConfig("Localization")

        CultureInfo = source.Configs("Localization").Get("CultureInfo", "en")

        'ChemSepDatabasePath = source.Configs("Databases").Get("ChemSepDBPath", "")
        'ReplaceComps = source.Configs("Databases").GetBoolean("ReplaceComps", True)

        If source.Configs("UserUnits") Is Nothing Then source.AddConfig("UserUnits")

        UserUnits = source.Configs("UserUnits").Get("UserUnits", "{ }")
        'ShowTips = source.Configs("Misc").GetBoolean("ShowTips", True)
        'RedirectOutput = source.Configs("Misc").GetBoolean("RedirectConsoleOutput", False)

        If source.Configs("Misc") Is Nothing Then source.AddConfig("Misc")

        EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", True)
        MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
        EnableGPUProcessing = source.Configs("Misc").GetBoolean("EnableGPUProcessing", False)
        SelectedGPU = source.Configs("Misc").Get("SelectedGPU", "")
        CudafyTarget = source.Configs("Misc").GetInt("CudafyTarget", 1)
        CudafyDeviceID = source.Configs("Misc").GetInt("CudafyDeviceID", 0)

        DebugLevel = source.Configs("Misc").GetInt("DebugLevel", 0)
        SolverMode = source.Configs("Misc").GetInt("SolverMode", 1)
        ServiceBusConnectionString = source.Configs("Misc").Get("ServiceBusConnectionString", "")
        ServerIPAddress = source.Configs("Misc").Get("ServerIPAddress", "")
        ServerPort = source.Configs("Misc").Get("ServerPort", 0)
        SolverTimeoutSeconds = source.Configs("Misc").GetInt("SolverTimeoutSeconds", 300)

        SaveExistingFile = source.Configs("Misc").GetBoolean("SaveBackupFile", True)
        MaxThreadMultiplier = source.Configs("Misc").GetInt("MaxThreadMultiplier", 8)
        TaskScheduler = source.Configs("Misc").GetInt("TaskScheduler", 0)
        UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

        CurrentVersion = source.Configs("Misc").Get("CurrentVersion", "")

        InspectorEnabled = source.Configs("Misc").GetBoolean("InspectorEnabled", False)
        ClearInspectorHistoryOnNewCalculationRequest = source.Configs("Misc").GetBoolean("ClearInspectorHistoryOnNewCalculationRequest", True)

        EditorFontSize = source.Configs("Misc").GetInt("EditorFontSize", -1)

        EditorTextBoxFixedSize = source.Configs("Misc").GetBoolean("EditorTextBoxFixedSize", True)

        EditOnSelect = source.Configs("Misc").GetBoolean("EditOnSelect", True)

        CallSolverOnEditorPropertyChanged = source.Configs("Misc").GetBoolean("CallSolverOnEditorPropertyChanged", True)

        UIScalingFactor = source.Configs("Misc").GetDouble("UIScalingFactor", 1.0)

        ObjectEditor = source.Configs("Misc").GetInt("ObjectEditor", 0)

        EnableCustomTouchBar = source.Configs("Misc").GetBoolean("EnableCustomTouchBar", True)

        'CloseFormsOnDeselecting = source.Configs("Misc").GetBoolean("CloseFormsOnDeselecting", True)

        'autom = source.Configs("Misc").GetBoolean("AutoUpdate", True)

        'DefaultEditorLocation = source.Configs("Misc").GetInt("DefaultEditorLocation", 8)
        'EnableMultipleObjectEditors = source.Configs("Misc").GetBoolean("EnableMultipleObjectEditors", True)
        'SimulationUpgradeWarning = source.Configs("Misc").GetBoolean("SimulationUpgradeWarning", True)

        HideSolidPhaseFromCAPEOPENComponents = source.Configs("Misc").GetBoolean("HideSolidPhase_COInterface", False)
        'IgnoreCompoundPropertiesOnLoad = source.Configs("Misc").GetBoolean("IgnoreCompoundConstantPropertyDatainXMLFile", False)

        If source.Configs("OctaveBridge") Is Nothing Then source.AddConfig("OctaveBridge")

        OctavePath = source.Configs("OctaveBridge").GetString("OctavePath", "")
        OctaveTimeoutInMinutes = source.Configs("OctaveBridge").GetFloat("OctaveProcessTimeout", 15)

        If source.Configs("PythonBridge") Is Nothing Then source.AddConfig("PythonBridge")

        PythonPath = source.Configs("PythonBridge").GetString("PythonPath", "")
        PythonTimeoutInMinutes = source.Configs("PythonBridge").GetFloat("PythonProcessTimeout", 1)

        If source.Configs("OSInfo") Is Nothing Then source.AddConfig("OSInfo")

        CurrentPlatform = source.Configs("OSInfo").GetString("Platform", GetPlatform())
        CurrentEnvironment = source.Configs("OSInfo").GetInt("Environment", GetEnvironment())

        If source.Configs("UserUnits") Is Nothing Then source.AddConfig("UserUnits")

        UserUnits = source.Configs("UserUnits").GetString("UserUnits", "{ }")

        If source.Configs("PlatformRenderers") Is Nothing Then source.AddConfig("PlatformRenderers")

        WindowsRenderer = [Enum].Parse(WindowsRenderer.GetType(), source.Configs("PlatformRenderers").GetString("Windows", "WPF"))
        LinuxRenderer = [Enum].Parse(LinuxRenderer.GetType(), source.Configs("PlatformRenderers").GetString("Linux", "Gtk2"))
        MacOSRenderer = [Enum].Parse(MacOSRenderer.GetType(), source.Configs("PlatformRenderers").GetString("Mac", "MonoMac"))
        FlowsheetRenderer = [Enum].Parse(FlowsheetRenderer.GetType(), source.Configs("PlatformRenderers").GetString("FlowsheetRenderer", "CPU"))

    End Sub

    Shared Sub SaveSettings(Optional ByVal configfile As String = "")

        Dim configfiledir = GetConfigFileDir()

        If Not Directory.Exists(configfiledir) Then Directory.CreateDirectory(configfiledir)

        If configfile = "" Then configfile = configfiledir & "dwsim.ini" Else configfile = configfiledir & configfile

        If Not File.Exists(configfile) Then File.WriteAllText(configfile, "")

        Dim source As New IniConfigSource(configfile)

        If source.Configs("RecentFiles") Is Nothing Then source.AddConfig("RecentFiles")

        For Each Str As String In MostRecentFiles
            source.Configs("RecentFiles").Set(MostRecentFiles.IndexOf(Str), Str)
        Next

        If source.Configs("UserDatabases") Is Nothing Then source.AddConfig("UserDatabases")

        For Each Str As String In UserDatabases
            source.Configs("UserDatabases").Set(UserDatabases.IndexOf(Str), Str)
        Next

        If source.Configs("UserInteractionsDatabases") Is Nothing Then source.AddConfig("UserInteractionsDatabases")

        For Each Str As String In UserInteractionsDatabases
            source.Configs("UserInteractionsDatabases").Set(UserInteractionsDatabases.IndexOf(Str), Str)
        Next

        If source.Configs("Backup") Is Nothing Then source.AddConfig("Backup")

        source.Configs("Backup").Set("EnableBackupCopies", EnableBackupCopies)
        source.Configs("Backup").Set("BackupInterval", BackupInterval)

        If source.Configs("Localization") Is Nothing Then source.AddConfig("Localization")

        source.Configs("Localization").Set("CultureInfo", CultureInfo)

        'source.Configs("Databases").Set("ChemSepDBPath", ChemSepDatabasePath)
        'source.Configs("Databases").Set("ReplaceComps", ReplaceComps)

        'source.Configs("Misc").Set("ShowTips", ShowTips)
        'source.Configs("Misc").Set("RedirectConsoleOutput", RedirectOutput)

        If source.Configs("Misc") Is Nothing Then source.AddConfig("Misc")

        source.Configs("Misc").Set("Misc", CultureInfo)

        source.Configs("Misc").Set("EnableParallelProcessing", EnableParallelProcessing)
        source.Configs("Misc").Set("MaxDegreeOfParallelism", MaxDegreeOfParallelism)
        source.Configs("Misc").Set("EnableGPUProcessing", EnableGPUProcessing)
        source.Configs("Misc").Set("SelectedGPU", SelectedGPU)
        source.Configs("Misc").Set("CudafyTarget", CudafyTarget)
        source.Configs("Misc").Set("CudafyDeviceID", CudafyDeviceID)

        source.Configs("Misc").Set("DebugLevel", DebugLevel)
        source.Configs("Misc").Set("SolverMode", SolverMode)
        source.Configs("Misc").Set("ServiceBusConnectionString", ServiceBusConnectionString)
        source.Configs("Misc").Set("ServerIPAddress", ServerIPAddress)
        source.Configs("Misc").Set("ServerPort", ServerPort)
        source.Configs("Misc").Set("SolverTimeoutSeconds", SolverTimeoutSeconds)
        source.Configs("Misc").Set("SaveBackupFile", SaveExistingFile)
        source.Configs("Misc").Set("MaxThreadMultiplier", MaxThreadMultiplier)
        source.Configs("Misc").Set("TaskScheduler", TaskScheduler)
        source.Configs("Misc").Set("UseSIMDExtensions", UseSIMDExtensions)

        source.Configs("Misc").Set("CurrentVersion", CurrentVersion)

        source.Configs("Misc").Set("InspectorEnabled", InspectorEnabled)
        source.Configs("Misc").Set("ClearInspectorHistoryOnNewCalculationRequest", ClearInspectorHistoryOnNewCalculationRequest)

        source.Configs("Misc").Set("EditorFontSize", EditorFontSize)
        source.Configs("Misc").Set("EditorTextBoxFixedSize", EditorTextBoxFixedSize)
        source.Configs("Misc").Set("EditOnSelect", EditOnSelect)
        source.Configs("Misc").Set("CallSolverOnEditorPropertyChanged", CallSolverOnEditorPropertyChanged)

        source.Configs("Misc").Set("UIScalingFactor", UIScalingFactor)

        source.Configs("Misc").Set("ObjectEditor", ObjectEditor)

        source.Configs("Misc").Set("EnableCustomTouchBar", EnableCustomTouchBar)

        'source.Configs("Misc").Set("CloseFormsOnDeselecting", CloseFormsOnDeselecting)
        'source.Configs("Misc").Set("AutoUpdate", AutomaticUpdates)

        'source.Configs("Misc").Set("DefaultEditorLocation", DefaultEditorLocation)
        'source.Configs("Misc").Set("EnableMultipleObjectEditors", EnableMultipleObjectEditors)
        'source.Configs("Misc").Set("SimulationUpgradeWarning", SimulationUpgradeWarning)

        'source.Configs("Misc").Set("HideSolidPhase_COInterface", HideSolidPhase_CO)
        'source.Configs("Misc").Set("IgnoreCompoundConstantPropertyDatainXMLFile", IgnoreCompoundPropertiesOnLoad)

        If source.Configs("OctaveBridge") Is Nothing Then source.AddConfig("OctaveBridge")

        source.Configs("OctaveBridge").Set("OctavePath", OctavePath)
        source.Configs("OctaveBridge").Set("OctaveProcessTimeout", OctaveTimeoutInMinutes)

        If source.Configs("PythonBridge") Is Nothing Then source.AddConfig("PythonBridge")

        source.Configs("PythonBridge").Set("PythonPath", PythonPath)
        source.Configs("PythonBridge").Set("PythonProcessTimeout", PythonTimeoutInMinutes)

        If source.Configs("OSInfo") Is Nothing Then source.AddConfig("OSInfo")

        source.Configs("OSInfo").Set("Platform", CurrentPlatform)
        source.Configs("OSInfo").Set("Environment", CurrentEnvironment)

        If source.Configs("UserUnits") Is Nothing Then source.AddConfig("UserUnits")

        source.Configs("UserUnits").Set("UserUnits", UserUnits)

        If source.Configs("PlatformRenderers") Is Nothing Then source.AddConfig("PlatformRenderers")

        source.Configs("PlatformRenderers").Set("Windows", WindowsRenderer)
        source.Configs("PlatformRenderers").Set("Linux", LinuxRenderer)
        source.Configs("PlatformRenderers").Set("Mac", MacOSRenderer)
        source.Configs("PlatformRenderers").Set("FlowsheetRenderer", FlowsheetRenderer)

        source.Save()

    End Sub

End Class


