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

    Shared Sub LoadExcelSettings(Optional ByVal configfile As String = "")

        If configfile = "" Then configfile = My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "dwsim.ini"
        If Not File.Exists(configfile) Then File.Copy(My.Application.Info.DirectoryPath + Path.DirectorySeparatorChar + "default.ini", configfile)

        Dim doc As New Nini.Ini.IniDocument(configfile, Nini.Ini.IniFileType.WindowsStyle)
        Dim source As New IniConfigSource(doc)

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

        source.Save(configfile)

    End Sub

End Class


