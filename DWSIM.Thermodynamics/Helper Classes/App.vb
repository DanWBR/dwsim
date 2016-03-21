Imports System.IO
Imports Cudafy.Translator
Imports Cudafy
Imports Cudafy.Host

Public Class App

    Public Shared Property Flowsheet As Interfaces.IFlowsheet
    Public Shared Property AppTaskScheduler As TaskScheduler
    Public Shared Property gpu As Cudafy.Host.GPGPU
    Public Shared Property gpumod As CudafyModule
    Public Shared Property prevlang As Integer = 0 '0 = CUDA, 1 = OpenCL

    Sub New()

    End Sub

    Shared Property TaskCancellationTokenSource As Object

    Shared Property IsRunningParallelTasks As Boolean

    Shared Property CAPEOPENMode As Boolean

    Shared Sub WriteToConsole(text As String, level As Integer)

    End Sub

    Shared Function GetLocalString(text As String) As String
        Return text
    End Function

    Public Shared Sub CheckParallelPInvoke()

        If My.Settings.EnableParallelProcessing Then Throw New InvalidOperationException(GetLocalString("ParallelPInvokeError"))

    End Sub

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

    Shared Sub InitComputeDevice()

        If gpu Is Nothing Then

            'set target language

            Select Case My.Settings.CudafyTarget
                Case 0, 1
                    CudafyTranslator.Language = eLanguage.Cuda
                Case 2
                    CudafyTranslator.Language = eLanguage.OpenCL
            End Select

            'get the gpu instance

            Dim gputype As eGPUType = My.Settings.CudafyTarget

            gpu = CudafyHost.GetDevice(gputype, My.Settings.CudafyDeviceID)

            'cudafy all classes that contain a gpu function

            If gpumod Is Nothing Then
                Select Case My.Settings.CudafyTarget
                    Case 0, 1
                        gpumod = CudafyModule.TryDeserialize("cudacode.cdfy")
                    Case 2
                        'OpenCL code is device-specific and must be compiled on each initialization
                End Select
                If gpumod Is Nothing OrElse Not gpumod.TryVerifyChecksums() Then
                    Select Case My.Settings.CudafyTarget
                        Case 0
                            gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.Auxiliary.SRK))
                            gpumod.Serialize("emulator.cdfy")
                        Case 1
                            Dim cp As New Cudafy.CompileProperties()
                            With cp
                                .Architecture = eArchitecture.sm_20
                                .CompileMode = eCudafyCompileMode.Default
                                .Platform = ePlatform.All
                                .WorkingDirectory = My.Computer.FileSystem.SpecialDirectories.Temp
                                'CUDA SDK v6.5 path
                                .CompilerPath = "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v6.5\bin\nvcc.exe"
                                .IncludeDirectoryPath = "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v6.5\include"
                            End With
                            gpumod = CudafyTranslator.Cudafy(cp, GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.Auxiliary.SRK))
                            gpumod.Serialize("cudacode.cdfy")
                        Case 2
                            gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.Auxiliary.SRK))
                    End Select
                End If
            End If

            'load cudafy module

            If Not gpu.IsModuleLoaded(gpumod.Name) Then gpu.LoadModule(gpumod)

        End If

    End Sub

    Shared Function GetComponentName(Name As String) As Object
        Throw New NotImplementedException
    End Function

End Class
