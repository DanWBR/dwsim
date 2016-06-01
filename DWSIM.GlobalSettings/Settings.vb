Imports Cudafy
Imports System.Threading
Imports Nini.Config
Imports System.IO

Public Class Settings

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
    Public Shared Property CAPEOPENMode As Boolean
    Public Shared Property MaxDegreeOfParallelism As Integer = -1
    Public Shared Property UseSIMDExtensions As Boolean = True
    Public Shared Property EnableParallelProcessing As Boolean = True
    Public Shared Property EnableGPUProcessing As Boolean = False
    Public Shared Property CudafyTarget As Integer = 0
    Public Shared Property CudafyDeviceID As Integer = 0
    Public Shared Property DebugLevel As Integer = 0
    Public Shared Property MaxThreadMultiplier As Integer = 8
    Public Shared Property TaskScheduler As Integer = 0
    Public Shared Property SolverTimeoutSeconds As Integer = 300
    Public Shared Property SolverMode As Integer = 0
    Public Shared Property SimultaneousAdjustEnabled As Boolean
    Public Shared Property ServiceBusConnectionString As String
    Public Shared Property CalculatorStopRequested As Boolean
    Public Shared Property CalculatorActivated As Boolean
    Public Shared Property CalculatorBusy As Boolean
    Public Shared Property ServerIPAddress As String
    Public Shared Property ServerPort As Integer
    Public Shared Property CurrentCulture As String = "en"

    Public Shared DefaultEditFormLocation As Integer = 8
    Public Shared SolverBreakOnException As Boolean = False

    Shared Property SelectedGPU As String

    Shared Property CultureInfo As String

    Shared Sub LoadSettings(Optional ByVal configfile As String = "")

        If configfile = "" Then configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
        If Not File.Exists(configfile) Then File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile)

        Dim source As New IniConfigSource(configfile)
        'Dim col() As String

        'Settings.MostRecentFiles = New Collections.Specialized.StringCollection()

        'With source
        '    col = .Configs("RecentFiles").GetValues()
        '    For Each Str As String In col
        '        Settings.MostRecentFiles.Add(Str)
        '    Next
        'End With

        'Settings.ScriptPaths = New Collections.Specialized.StringCollection()

        'With source
        '    col = .Configs("ScriptPaths").GetValues()
        '    For Each Str As String In col
        '        Settings.ScriptPaths.Add(Str)
        '    Next
        'End With

        'Settings.UserDatabases = New Collections.Specialized.StringCollection()

        'With source
        '    col = .Configs("UserDatabases").GetValues()
        '    For Each Str As String In col
        '        Settings.UserDatabases.Add(Str)
        '    Next
        'End With

        'Settings.UserInteractionsDatabases = New Collections.Specialized.StringCollection()

        'With source
        '    col = .Configs("UserInteractionsDatabases").GetValues()
        '    For Each Str As String In col
        '        Settings.UserInteractionsDatabases.Add(Str)
        '    Next
        'End With

        'Settings.BackupFiles = New Collections.Specialized.StringCollection()

        'With source
        '    col = .Configs("BackupFiles").GetValues()
        '    For Each Str As String In col
        '        Settings.BackupFiles.Add(Str)
        '    Next
        'End With

        'Settings.BackupActivated = source.Configs("Backup").GetBoolean("BackupActivated", True)
        'Settings.BackupFolder = source.Configs("Backup").Get("BackupFolder", My.Computer.FileSystem.SpecialDirectories.Temp + Path.DirectorySeparatorChar + "DWSIM")
        'Settings.BackupInterval = source.Configs("Backup").GetInt("BackupInterval", 5)

        'Settings.CultureInfo = source.Configs("Localization").Get("CultureInfo", "en-US")

        'Settings.ChemSepDatabasePath = source.Configs("Databases").Get("ChemSepDBPath", "")
        'Settings.ReplaceComps = source.Configs("Databases").GetBoolean("ReplaceComps", True)

        'Settings.UserUnits = source.Configs("Misc").Get("UserUnits", "")
        'Settings.ShowTips = source.Configs("Misc").GetBoolean("ShowTips", True)
        'Settings.RedirectOutput = source.Configs("Misc").GetBoolean("RedirectConsoleOutput", False)

        Settings.EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", False)
        Settings.MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
        Settings.EnableGPUProcessing = source.Configs("Misc").GetBoolean("EnableGPUProcessing", False)
        'Settings.SelectedGPU = source.Configs("Misc").Get("SelectedGPU", "")
        Settings.CudafyTarget = source.Configs("Misc").GetInt("CudafyTarget", 1)
        Settings.CudafyDeviceID = source.Configs("Misc").GetInt("CudafyDeviceID", 0)

        Settings.DebugLevel = source.Configs("Misc").GetInt("DebugLevel", 0)
        Settings.SolverMode = source.Configs("Misc").GetInt("SolverMode", 0)
        Settings.ServiceBusConnectionString = source.Configs("Misc").Get("ServiceBusConnectionString", "")
        Settings.ServerIPAddress = source.Configs("Misc").Get("ServerIPAddress", "")
        Settings.ServerPort = source.Configs("Misc").Get("ServerPort", "")
        Settings.SolverTimeoutSeconds = source.Configs("Misc").GetInt("SolverTimeoutSeconds", 300)

        'Settings.SaveBackupFile = source.Configs("Misc").GetBoolean("SaveBackupFile", True)
        Settings.MaxThreadMultiplier = source.Configs("Misc").GetInt("MaxThreadMultiplier", 8)
        Settings.TaskScheduler = source.Configs("Misc").GetInt("TaskScheduler", 0)
        Settings.UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

        'Settings.CloseFormsOnDeselecting = source.Configs("Misc").GetBoolean("CloseFormsOnDeselecting", True)

    End Sub

End Class


