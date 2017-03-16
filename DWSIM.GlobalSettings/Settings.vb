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
    Public Shared Property ServerPort As Integer
    Public Shared Property CurrentCulture As String = "en"
    Public Shared DefaultEditFormLocation As Integer = 8
    Public Shared SolverBreakOnException As Boolean = False
    Public Shared Property SelectedGPU As String
    Public Shared Property CultureInfo As String = "en"
    Public Shared Property InitializedCOPPM As Boolean = False
    Public Shared Property ExcelErrorHandlingMode As Integer = 0
    Public Shared Property ExcelFlashSettings As String = ""

    Public Shared Property UserInteractionsDatabases As New List(Of String)
    Public Shared Property UserDatabases As New List(Of String)

    Public Shared Property HideSolidPhaseFromCAPEOPENComponents As Boolean = False

    Public Shared Property DrawingAntiAlias As Boolean = True

    Public Shared Property AutomationMode As Boolean

    Public Shared Property OctavePath As String = ""

    Public Shared Property OctaveTimeoutInMinutes As Double = 5

    Public Shared Property CurrentEnvironment As Integer = 0

    Public Shared Property CurrentPlatform As String = ""

    Public Shared Property PythonPath As String = ""
    Public Shared Property PythonTimeoutInMinutes As Double = 1

    Shared Sub LoadExcelSettings(Optional ByVal configfile As String = "")

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

        Settings.EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", False)
        Settings.MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
        Settings.UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

        Settings.EnableGPUProcessing = source.Configs("Misc").GetBoolean("EnableGPUProcessing", False)
        Settings.SelectedGPU = source.Configs("Misc").GetString("SelectedGPU", "")
        Settings.CudafyTarget = source.Configs("Misc").GetInt("CudafyTarget", 0)
        Settings.CudafyDeviceID = source.Configs("Misc").GetInt("CudafyDeviceID", 0)

        If source.Configs("ExcelAddIn") Is Nothing Then source.AddConfig("ExcelAddIn")

        Settings.ExcelErrorHandlingMode = source.Configs("ExcelAddIn").GetInt("ExcelErrorHandlingMode", 0)
        Settings.ExcelFlashSettings = source.Configs("ExcelAddIn").GetString("ExcelFlashSettings", "")

        If source.Configs("OSInfo") Is Nothing Then source.AddConfig("OSInfo")

        Settings.CurrentPlatform = source.Configs("OSInfo").GetString("Platform")
        Settings.CurrentEnvironment = source.Configs("OSInfo").GetInt("Environment", 0)

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

End Class


