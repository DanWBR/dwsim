Imports Cudafy
Imports System.Threading

Public Class Settings

    Public Shared Property AppTaskScheduler As TaskScheduler = Tasks.TaskScheduler.Default
    Public Shared Property gpu As Cudafy.Host.GPGPU
    Public Shared Property gpumod As CudafyModule
    Public Shared Property prevlang As Integer = 0 '0 = CUDA, 1 = OpenCL
    Public Shared Property TaskCancellationTokenSource As New CancellationTokenSource
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
    Public Shared Property StorePreviousSolutions As Boolean = False
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

End Class


