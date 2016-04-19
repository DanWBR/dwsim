Imports Cudafy
Imports System.Threading

Public Class Settings
    Public Shared Property AppTaskScheduler As TaskScheduler = Tasks.TaskScheduler.Default
    Public Shared Property gpu As Cudafy.Host.GPGPU
    Public Shared Property gpumod As CudafyModule
    Public Shared Property prevlang As Integer = 0 '0 = CUDA, 1 = OpenCL
    Public Shared Property TaskCancellationTokenSource As CancellationTokenSource
    Public Shared Property CAPEOPENMode As Boolean
    Public Shared Property MaxDegreeOfParallelism As Integer = -1
    Public Shared Property UseSIMDExtensions As Boolean = True
    Public Shared Property EnableParallelProcessing As Boolean = True
    Public Shared Property EnableGPUProcessing As Boolean = True
    Public Shared Property CudafyTarget As Integer = 0
    Public Shared Property CudafyDeviceID As Integer = 0
    Public Shared Property DebugLevel As Integer = 0
    Shared Property MaxThreadMultiplier As Integer = 8
    Shared Property TaskScheduler As Integer = 0
    Shared Property SolverTimeoutSeconds As Integer = 300
    Shared Property StorePreviousSolutions As Boolean = False
    Shared Property SolverMode As Integer = 0

    Shared Property SimultaneousAdjustEnabled As Boolean

    Shared Property ServiceBusConnectionString As String

    Shared Property CalculatorStopRequested As Boolean

    Shared Property ServerIPAddress As String

    Shared Property ServerPort As Integer

End Class


