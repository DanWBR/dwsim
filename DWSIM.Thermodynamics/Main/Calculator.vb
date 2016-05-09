Imports System.IO
Imports Cudafy.Translator
Imports Cudafy
Imports Cudafy.Host
Imports System.Threading
Imports DWSIM.GlobalSettings

Public Class Calculator

    Shared Sub WriteToConsole(text As String, level As Integer)
        'If level > DebugLevel Then Console.WriteLine(text)
    End Sub

    Shared Function GetLocalString(text As String) As String
        Return text
    End Function

    Public Shared Sub CheckParallelPInvoke()

        If Settings.EnableParallelProcessing Then Throw New InvalidOperationException(GetLocalString("ParallelPInvokeError"))

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
                        Settings.gpumod = CudafyModule.TryDeserialize("cudacode.cdfy")
                    Case 2
                        'OpenCL code is device-specific and must be compiled on each initialization
                End Select
                If Settings.gpumod Is Nothing OrElse Not Settings.gpumod.TryVerifyChecksums() Then
                    Select Case Settings.CudafyTarget
                        Case 0
                            Settings.gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
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
                            Settings.gpumod = CudafyTranslator.Cudafy(cp, GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.ThermoPlugs.SRK))
                            Settings.gpumod.Serialize("cudacode.cdfy")
                        Case 2
                            Settings.gpumod = CudafyTranslator.Cudafy(GetType(PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                        GetType(PropertyPackages.ThermoPlugs.PR),
                                        GetType(PropertyPackages.ThermoPlugs.SRK))
                    End Select
                End If
            End If

            'load cudafy module

            If Not Settings.gpu.IsModuleLoaded(Settings.gpumod.Name) Then Settings.gpu.LoadModule(Settings.gpumod)

        End If

    End Sub

    Shared Function InitLibraries() As List(Of Exception)

        Dim plat As String, envir As Integer

        If Environment.Is64BitProcess Then
            envir = 64
        Else
            envir = 32
        End If

        If RunningPlatform() = Platform.Windows Then
            plat = "Windows"
        ElseIf RunningPlatform() = Platform.Linux Then
            plat = "Linux"
        Else
            plat = "None"
        End If

        Dim copyfiles As Boolean = False

        If My.Settings.CurrentEnvironment <> envir Then
            copyfiles = True
            My.Settings.CurrentEnvironment = envir
        End If

        If My.Settings.CurrentPlatform <> plat Then
            copyfiles = True
            My.Settings.CurrentPlatform = plat
        End If

        Dim exlist As New List(Of Exception)

        If copyfiles And plat <> "None" Then

            Dim dlist, alist As New List(Of String)

            If plat = "Windows" Then

                dlist.Add("CoolProp.dll")
                dlist.Add("fprops_ascend.dll")
                dlist.Add("Ipopt39.dll")
                dlist.Add("IpOptFSS39.dll")
                dlist.Add("lpsolve55.dll")
                dlist.Add("PC_SAFT_PROP.dll")
                If envir = 32 Then dlist.Add("PetAz.dll")

                alist.Add("W" + envir.ToString + "_" + "CoolProp.dll")
                alist.Add("W" + envir.ToString + "_" + "fprops_ascend.dll")
                alist.Add("W" + envir.ToString + "_" + "Ipopt39.dll")
                alist.Add("W" + envir.ToString + "_" + "IpOptFSS39.dll")
                alist.Add("W" + envir.ToString + "_" + "lpsolve55.dll")
                alist.Add("W" + envir.ToString + "_" + "PC_SAFT_PROP.dll")
                If envir = 32 Then alist.Add("W" + envir.ToString + "_" + "PetAz.dll")

            ElseIf plat = "Linux" Then

                dlist.Add("Cureos.Numerics.dll.config")
                dlist.Add("DWSIM.exe.config")
                dlist.Add("libfprops_ascend.so")
                dlist.Add("liblpsolve55.so")
                dlist.Add("libPC_SAFT_PROP.so")
                If envir = 32 Then
                    dlist.Add("libipopt_mono_dwsim_ubuntu_11.10_32.tar.gz")
                Else
                    dlist.Add("libipopt_mono_dwsim_ubuntu_15.10_64.tar.gz")
                    dlist.Add("libCoolProp.so")
                End If

                alist.Add("L_" + "Cureos.Numerics.dll.config")
                alist.Add("L_" + "DWSIM.exe.config")
                alist.Add("L" + envir.ToString + "_" + "libfprops_ascend.so")
                alist.Add("L" + envir.ToString + "_" + "liblpsolve55.so")
                alist.Add("L" + envir.ToString + "_" + "libPC_SAFT_PROP.so")
                If envir = 32 Then
                    alist.Add("L" + envir.ToString + "_" + "libipopt_mono_dwsim_ubuntu_11.10_32.tar.gz")
                Else
                    alist.Add("L" + envir.ToString + "_" + "libipopt_mono_dwsim_ubuntu_15.10_64.tar.gz")
                    alist.Add("L" + envir.ToString + "_" + "libCoolProp.so")
                End If

            End If

            'copy files

            For i As Integer = 0 To dlist.Count - 1

                ' Get the embedded resource stream that holds the Internal DLL in this assembly.
                ' The name looks funny because it must be the default namespace of this project
                ' (MyAssembly.) plus the name of the Properties subdirectory where the
                ' embedded resource resides (Properties.) plus the name of the file.
                Using stm As Stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics." + alist(i))
                    ' Copy the assembly to the temporary file
                    Try
                        Using outFile As Stream = File.Create(Path.Combine(Environment.CurrentDirectory, dlist(i)))
                            Const sz As Integer = 4096
                            Dim buf As Byte() = New Byte(sz - 1) {}
                            While True
                                Dim nRead As Integer = stm.Read(buf, 0, sz)
                                If nRead < 1 Then
                                    Exit While
                                End If
                                outFile.Write(buf, 0, nRead)
                            End While
                        End Using
                    Catch ex As Exception
                        Console.WriteLine(dlist(i) & ": " & ex.Message.ToString)
                        exlist.Add(ex)
                    End Try
                End Using

            Next

        End If

        Return exlist

    End Function

    Shared Function RemoveLibraries() As List(Of Exception)

        Dim plat As String, envir As Integer

        If Environment.Is64BitProcess Then
            envir = 64
        Else
            envir = 32
        End If

        If RunningPlatform() = Platform.Windows Then
            plat = "Windows"
        ElseIf RunningPlatform() = Platform.Linux Then
            plat = "Linux"
        Else
            plat = "None"
        End If

        Dim deletefiles As Boolean = False

        If My.Settings.CurrentEnvironment = envir And My.Settings.CurrentPlatform = plat Then deletefiles = True

        Dim exlist As New List(Of Exception)

        If deletefiles And plat <> "None" Then

            Dim dlist As New List(Of String)

            If plat = "Windows" Then

                dlist.Add("CoolProp.dll")
                dlist.Add("fprops_ascend.dll")
                dlist.Add("Ipopt39.dll")
                dlist.Add("IpOptFSS39.dll")
                dlist.Add("lpsolve55.dll")
                dlist.Add("PC_SAFT_PROP.dll")
                If envir = 32 Then dlist.Add("PetAz.dll")

            ElseIf plat = "Linux" Then

                dlist.Add("Cureos.Numerics.dll.config")
                dlist.Add("DWSIM.exe.config")
                dlist.Add("libfprops_ascend.so")
                dlist.Add("liblpsolve55.so")
                dlist.Add("libPC_SAFT_PROP.so")
                If envir = 32 Then
                    dlist.Add("libipopt_mono_dwsim_ubuntu_11.10_32.tar.gz")
                Else
                    dlist.Add("libipopt_mono_dwsim_ubuntu_15.10_64.tar.gz")
                    dlist.Add("libCoolProp.so")
                End If

            End If

            'delete files

            For i As Integer = 0 To dlist.Count - 1

                ' Copy the assembly to the temporary file

                Try
                    File.Delete(Path.Combine(Environment.CurrentDirectory, dlist(i)))
                Catch ex As Exception
                    Console.WriteLine(dlist(i) & ": " & ex.Message.ToString)
                    exlist.Add(ex)
                End Try

            Next

        End If

        Return exlist

    End Function

End Class
