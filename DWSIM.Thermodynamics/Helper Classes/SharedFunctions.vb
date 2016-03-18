Imports System.IO
Imports Nini.Config
Imports System.Globalization
Imports System.Reflection
Imports System.Linq
Imports Cudafy.Translator
Imports Cudafy
Imports Cudafy.Host

'    Shared Functions
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Namespace DWSIM

    <System.Serializable()> Public Class App

        Public Shared Sub CheckParallelPInvoke()

            If My.Settings.EnableParallelProcessing Then Throw New InvalidOperationException(App.GetLocalString("ParallelPInvokeError"))

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

            If My.Application.gpu Is Nothing Then

                'set target language

                Select Case My.Settings.CudafyTarget
                    Case 0, 1
                        CudafyTranslator.Language = eLanguage.Cuda
                    Case 2
                        CudafyTranslator.Language = eLanguage.OpenCL
                End Select

                'get the gpu instance

                Dim gputype As eGPUType = My.Settings.CudafyTarget

                My.Application.gpu = CudafyHost.GetDevice(gputype, My.Settings.CudafyDeviceID)

                'cudafy all classes that contain a gpu function

                If My.Application.gpumod Is Nothing Then
                    Select Case My.Settings.CudafyTarget
                        Case 0, 1
                            My.Application.gpumod = CudafyModule.TryDeserialize("cudacode.cdfy")
                        Case 2
                            'OpenCL code is device-specific and must be compiled on each initialization
                    End Select
                    If My.Application.gpumod Is Nothing OrElse Not My.Application.gpumod.TryVerifyChecksums() Then
                        Select Case My.Settings.CudafyTarget
                            Case 0
                                My.Application.gpumod = CudafyTranslator.Cudafy(GetType(DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.PR),
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.SRK))
                                My.Application.gpumod.Serialize("emulator.cdfy")
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
                                My.Application.gpumod = CudafyTranslator.Cudafy(cp, GetType(DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.PR),
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.SRK))
                                My.Application.gpumod.Serialize("cudacode.cdfy")
                            Case 2
                                My.Application.gpumod = CudafyTranslator.Cudafy(GetType(DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LeeKeslerPlocker), _
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.PR),
                                            GetType(DWSIM.SimulationObjects.PropertyPackages.ThermoPlugs.SRK))
                        End Select
                    End If
                End If

                'load cudafy module

                If Not My.Application.gpu.IsModuleLoaded(My.Application.gpumod.Name) Then My.Application.gpu.LoadModule(My.Application.gpumod)

            End If

        End Sub

    End Class

End Namespace
