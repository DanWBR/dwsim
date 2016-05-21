Imports System.IO
Imports System.Linq
Imports System.Security.Cryptography
Imports System.Reflection

Module Module1

    Sub Main()

        Dim sep As Char = Path.DirectorySeparatorChar

        If File.Exists(My.Application.Info.DirectoryPath & sep & "update.run") Then

            Console.WriteLine()
            Console.WriteLine("DWSIM Updater Application")
            Console.WriteLine(My.Application.Info.Copyright)
            Dim dt As DateTime = CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)
            Console.WriteLine("Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor & _
            ", Build " & My.Application.Info.Version.Build & " (" & Format(dt, "dd/MM/yyyy HH:mm") & ")")
            If Type.GetType("Mono.Runtime") Is Nothing Then
                Console.WriteLine("Microsoft .NET Framework Runtime Version " & System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
            Else
                Dim displayName As MethodInfo = Type.GetType("Mono.Runtime").GetMethod("GetDisplayName", BindingFlags.NonPublic Or BindingFlags.[Static])
                If displayName IsNot Nothing Then
                    Console.WriteLine("Mono " + displayName.Invoke(Nothing, Nothing) + " / " + System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
                Else
                    Console.WriteLine(System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString())
                End If
            End If
            Console.WriteLine()

            Dim udir As String = My.Application.Info.DirectoryPath & sep & "update"
            Dim extrc As String = ""
            Dim ufiles As New Dictionary(Of String, String)
            Dim ustring = "0"

            If RunningPlatform() = Platform.Linux Then
                extrc = My.Application.Info.DirectoryPath & sep & "7zip" & sep & "linux" & sep & "7za"
            Else
                extrc = My.Application.Info.DirectoryPath & sep & "7zip" & sep & "windows" & sep & "7za.exe"
            End If

            If Directory.Exists(udir) Then
                If File.Exists(udir & sep & "filelist.txt") Then
                    For Each line In File.ReadAllLines(udir & sep & "filelist.txt")
                        ufiles.Add(udir & sep & line.Split(vbTab)(0), line.Split(vbTab)(1))
                    Next
                End If
            End If

            Try

                Dim toextract As New List(Of String)

                For Each item In ufiles
                    If File.Exists(item.Key) Then
                        Using _md5 = MD5.Create()
                            Using stream = File.OpenRead(item.Key)
                                Dim md5s = BitConverter.ToString(_md5.ComputeHash(stream)).Replace("-", "").ToLower()
                                If md5s = item.Value Then toextract.Add(item.Key)
                            End Using
                        End Using
                    Else
                        Throw New FileNotFoundException("Update file not found.", item.Key)
                    End If
                Next

                For Each efile In toextract
                    Dim p As New Process
                    With p.StartInfo
                        .FileName = extrc
                        .Arguments = "x " & Chr(34) & efile & Chr(34) & " -aoa -bb1 -r -o" & Chr(34) & My.Application.Info.DirectoryPath & Chr(34)
                        .WorkingDirectory = My.Application.Info.DirectoryPath
                        .UseShellExecute = False
                    End With
                    Console.WriteLine()
                    Console.WriteLine("Processing update file " & Path.GetFileName(efile) & "...")
                    Console.WriteLine()
                    p.Start()
                    p.WaitForExit()
                    ustring = Path.GetFileNameWithoutExtension(efile).Split("_")(1).Trim("u")
                Next

                If ustring <> "0" Then
                    File.WriteAllText(My.Application.Info.DirectoryPath & sep & "version.info", ustring)
                Else
                    Throw New ArgumentNullException("No update files were processed.")
                End If

                Console.WriteLine()

                If RunningPlatform() = Platform.Windows Then
                    Console.WriteLine("DWSIM was updated successfully. Press any key to close the updater and launch DWSIM.")
                    Console.ReadKey(True)
                Else
                    Console.WriteLine("DWSIM was updated successfully. Now closing the updater and launching DWSIM.")
                End If

                If RunningPlatform() = Platform.Linux Then
                    Dim startInfo = New ProcessStartInfo("mono", My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "DWSIM.exe")
                    startInfo.UseShellExecute = True
                    Process.Start(startInfo)
                Else
                    Process.Start(My.Application.Info.DirectoryPath & sep & "DWSIM.exe")
                End If

            Catch ex As Exception

                Console.WriteLine()
                Console.WriteLine("Update failed: " & ex.Message.ToString)
                Console.WriteLine()
                If RunningPlatform() = Platform.Windows Then
                    Console.WriteLine("DWSIM was not updated successfully. Press any key to close the updater.")
                    Console.ReadKey(True)
                Else
                    Console.WriteLine("DWSIM was not updated successfully. Closing the updater...")
                End If

            Finally

                For Each f In Directory.GetFiles(udir)
                    Try
                        File.Delete(f)
                    Catch ex2 As Exception
                    End Try
                Next

                Try
                    Directory.Delete(udir)
                Catch ex As Exception
                End Try

                File.Delete(My.Application.Info.DirectoryPath & sep & "update.run")

                Environment.Exit(0)

            End Try

        End If

    End Sub

    Public Enum Platform
        Windows
        Linux
        Mac
    End Enum

    Public Function RunningPlatform() As Platform
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

End Module
