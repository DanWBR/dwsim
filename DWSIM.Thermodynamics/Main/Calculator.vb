Imports System.IO
Imports Cudafy.Translator
Imports Cudafy
Imports Cudafy.Host
Imports System.Threading
Imports DWSIM.GlobalSettings

Public Class Calculator

    Public Shared _ResourceManager As System.Resources.ResourceManager

    Public Shared ExcelLogForm As LogForm

    Shared Sub WriteToConsole(text As String, level As Integer)
        If level <= Settings.DebugLevel Then Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] " + text)
    End Sub

    Public Shared Function GetLocalString(ByVal text As String) As String

        If _ResourceManager Is Nothing Then

            Dim cultureinfo As String = If(Settings.ExcelMode, "en", GlobalSettings.Settings.CultureInfo)

            My.Application.ChangeUICulture(cultureinfo)

            'loads the resource manager
            _ResourceManager = New System.Resources.ResourceManager("DWSIM.Thermodynamics.Strings", System.Reflection.Assembly.GetExecutingAssembly())

        End If

        If text <> "" Then

            Dim cultureinfo As String = If(Settings.ExcelMode, "en", GlobalSettings.Settings.CultureInfo)
            If My.Application.UICulture.Name <> cultureinfo Then
                My.Application.ChangeUICulture(cultureinfo)
            End If

            Dim retstr As String = _ResourceManager.GetString(text, My.Application.UICulture)
            If retstr Is Nothing Then Return text Else Return retstr

        Else

            Return ""

        End If

    End Function
    Public Shared Sub CheckParallelPInvoke()

        If Settings.EnableParallelProcessing Then
            GlobalSettings.Settings.EnableParallelProcessing = False
            Dim ex As New InvalidOperationException(GetLocalString("ParallelPInvokeError"))
            ex.Data.Add("DetailedDescription", "This calculation will use a native (C++/FORTRAN) library which doesn't support multithreading, that is, cannot do multiple calculations at once.")
            ex.Data.Add("UserAction", "Go to the Global Settings Panel, disable the CPU Parallel Acceleration and try again.")
            Throw ex
        End If

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

        If Settings.gpu Is Nothing Then

            'set target language

            Select Case Settings.CudafyTarget
                Case 0, 1
                    CudafyTranslator.Language = eLanguage.Cuda
                Case 2
                    CudafyTranslator.Language = eLanguage.OpenCL
            End Select

            'get the gpu instance

            Dim gputype As eGPUType = Settings.CudafyTarget

            Settings.gpu = CudafyHost.GetDevice(gputype, Settings.CudafyDeviceID)

            'cudafy all classes that contain a gpu function

            If Settings.gpumod Is Nothing Then
                Select Case Settings.CudafyTarget
                    Case 0, 1
                        Dim errmsg As String = ""
                        Settings.gpumod = CudafyModule.TryDeserialize("cudacode.cdfy", errmsg)
                        If errmsg <> "" Then Throw New CudafyException(errmsg)
                    Case 2
                        'OpenCL code is device-specific and must be compiled on each initialization
                End Select
                If Settings.gpumod Is Nothing Then
                    Select Case Settings.CudafyTarget
                        Case 0
                            Settings.gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker),
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.ThermoPlugs.SRK))
                            Settings.gpumod.Serialize("emulator.cdfy")
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
                            Settings.gpumod = CudafyTranslator.Cudafy(cp, GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker),
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.ThermoPlugs.SRK))
                            Settings.gpumod.Serialize("cudacode.cdfy")
                        Case 2
                            'Settings.gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker),
                            '            GetType(PropertyPackages.ThermoPlugs.PR),
                            '            GetType(PropertyPackages.ThermoPlugs.SRK))
                            Settings.gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.ThermoPlugs.SRK))
                    End Select
                End If
            End If

            'load cudafy module

            If Not Settings.gpu.IsModuleLoaded(Settings.gpumod.Name) Then Settings.gpu.LoadModule(Settings.gpumod)

        End If

    End Sub

End Class
