Imports System.IO
Imports DWSIM.GlobalSettings.Settings

Public Class Files

    Shared Function InitLibraries() As List(Of Exception)

        Dim plat As String, envir As Integer

        plat = GetPlatform()
        envir = GetEnvironment()

        Dim copyfiles As Boolean = False

        If GlobalSettings.Settings.CurrentEnvironment <> envir Then
            copyfiles = True
            GlobalSettings.Settings.CurrentEnvironment = envir
        End If

        If GlobalSettings.Settings.CurrentPlatform <> plat Then
            copyfiles = True
            GlobalSettings.Settings.CurrentPlatform = plat
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
                dlist.Add("PetAz.dll")

                alist.Add("W" + envir.ToString + "_" + "CoolProp.dll")
                alist.Add("W" + envir.ToString + "_" + "fprops_ascend.dll")
                alist.Add("W" + envir.ToString + "_" + "Ipopt39.dll")
                alist.Add("W" + envir.ToString + "_" + "IpOptFSS39.dll")
                alist.Add("W" + envir.ToString + "_" + "lpsolve55.dll")
                alist.Add("W" + envir.ToString + "_" + "PC_SAFT_PROP.dll")
                alist.Add("W" + envir.ToString + "_" + "PetAz.dll")

            ElseIf plat = "Linux" Then

                dlist.Add("Cureos.Numerics.dll.config")
                dlist.Add("DWSIM.exe.config")
                dlist.Add("libfprops_ascend.so")
                dlist.Add("liblpsolve55.so")
                dlist.Add("libPC_SAFT_PROP.so")
                dlist.Add("libPetAz.so")
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
                alist.Add("L" + envir.ToString + "_" + "libPetAz.so")
                If envir = 32 Then
                    alist.Add("L" + envir.ToString + "_" + "libipopt_mono_dwsim_ubuntu_11.10_32.tar.gz")
                Else
                    alist.Add("L" + envir.ToString + "_" + "libipopt_mono_dwsim_ubuntu_15.10_64.tar.gz")
                    alist.Add("L" + envir.ToString + "_" + "libCoolProp.so")
                End If

            End If

            'copy files

            For i As Integer = 0 To dlist.Count - 1

                Using stm As Stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics.NativeLibraries." + alist(i))
                    ' Copy the assembly to the temporary file
                    Try
                        Dim fname As String = Path.Combine(Environment.CurrentDirectory, dlist(i))
                        Using outFile As Stream = File.Create(fname, 4 * 1024, FileOptions.None, New System.Security.AccessControl.FileSecurity(fname, Security.AccessControl.AccessControlSections.Owner))
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

        plat = GetPlatform()
        envir = GetEnvironment()

        Dim deletefiles As Boolean = False

        If GlobalSettings.Settings.CurrentEnvironment = envir And GlobalSettings.Settings.CurrentPlatform = plat Then deletefiles = False

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
                dlist.Add("PetAz.dll")

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

                ' Delete the assemblies

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
