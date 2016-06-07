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
    Public Shared Property CAPEOPENMode As Boolean = False
    Public Shared Property ExcelMode As Boolean = False
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
    Public Shared Property SelectedGPU As String
    Public Shared Property CultureInfo As String
    Public Shared Property InitializedCOPPM As Boolean = False

    Shared Sub LoadSettings(Optional ByVal configfile As String = "")

        If configfile = "" Then configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
        If Not File.Exists(configfile) Then File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile)

        Dim source As New IniConfigSource(configfile)
        Settings.EnableParallelProcessing = source.Configs("Misc").GetBoolean("EnableParallelProcessing", False)
        Settings.MaxDegreeOfParallelism = source.Configs("Misc").GetInt("MaxDegreeOfParallelism", -1)
        
        Settings.MaxThreadMultiplier = source.Configs("Misc").GetInt("MaxThreadMultiplier", 8)
        Settings.TaskScheduler = source.Configs("Misc").GetInt("TaskScheduler", 0)
        Settings.UseSIMDExtensions = source.Configs("Misc").GetBoolean("UseSIMDExtensions", True)

    End Sub

End Class


