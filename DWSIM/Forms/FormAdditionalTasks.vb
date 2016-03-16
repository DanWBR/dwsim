Imports System.IO

Public Class FormAdditionalTasks

    Private Sub FormAdditionalTasks_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        lblOSPlatform.Text = My.Computer.Info.OSFullName & " / " & My.Computer.Info.OSVersion & " / " & My.Computer.Info.OSPlatform

        If Environment.Is64BitOperatingSystem Then
            lblOSEnvironment.Text = "64-bit"
        Else
            lblOSEnvironment.Text = "32-bit"
        End If

        If Environment.Is64BitProcess Then
            lblOSEnvironment.Text += " / 64-bit"
        Else
            lblOSEnvironment.Text += " / 32-bit"
        End If

        If DWSIM.App.RunningPlatform = DWSIM.App.Platform.Linux Then
            cbtask2.Enabled = True
            cbtask2.Checked = True
        End If

    End Sub

    Sub Task1()

        'copy static DLLs according to the executing platform (32 or 64-bit Linux/Windows)

        Dim dlls As String() = {}
        If Environment.Is64BitProcess Then
            'copy 64-bit DLLs
            If DWSIM.App.RunningPlatform = DWSIM.App.Platform.Windows Then
                dlls = Directory.GetFiles(My.Application.Info.DirectoryPath & "\windows\win64\", "*")
            ElseIf DWSIM.App.RunningPlatform = DWSIM.App.Platform.Linux Then
                dlls = Directory.GetFiles(My.Application.Info.DirectoryPath & "/linux/linux64/", "*.so")
            End If
        Else
            'copy 32-bit DLLs
            If DWSIM.App.RunningPlatform = DWSIM.App.Platform.Windows Then
                dlls = Directory.GetFiles(My.Application.Info.DirectoryPath & "\windows\win32\", "*")
            ElseIf DWSIM.App.RunningPlatform = DWSIM.App.Platform.Linux Then
                dlls = Directory.GetFiles(My.Application.Info.DirectoryPath & "/linux/linux32/", "*.so")
            End If
        End If

        For Each dll In dlls
            Try
                File.Copy(dll, My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & Path.GetFileName(dll), True)
            Catch ex As Exception
            End Try
        Next

    End Sub

    Sub Task2()

        Dim info = New ProcessStartInfo()
        info.FileName = "sudo"
        info.UseShellExecute = False
        info.CreateNoWindow = False

        If Environment.Is64BitProcess Then
            info.Arguments = "tar -C /usr/lib -zxvf linux/linux64/libipopt_mono_dwsim_ubuntu_15.10_64.tar.gz"
        Else
            info.Arguments = "tar -C /usr/lib -zxvf linux/linux32/libipopt_mono_dwsim_ubuntu_11.10_32.tar.gz"
        End If

        Dim p = Process.Start(info)
        p.WaitForExit()

    End Sub

    Sub Task3()

        If DWSIM.App.RunningPlatform = DWSIM.App.Platform.Windows Then
            File.Copy(My.Application.Info.DirectoryPath & "\windows\DWSIM.exe.config", My.Application.Info.DirectoryPath & "\DWSIM.exe.config", True)
        ElseIf DWSIM.App.RunningPlatform = DWSIM.App.Platform.Linux Then
            File.Copy(My.Application.Info.DirectoryPath & "/linux/DWSIM.exe.config", My.Application.Info.DirectoryPath & "/DWSIM.exe.config", True)
            File.Copy(My.Application.Info.DirectoryPath & "/linux/Cureos.Numerics.dll.config", My.Application.Info.DirectoryPath & "/Cureos.Numerics.dll.config", True)
        End If

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        If cbtask1.Checked Then
            Try
                Task1()
            Catch ex As Exception
                MessageBox.Show("Error running Task 1: " & ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
        If cbtask2.Checked Then
            Try
                Task2()
            Catch ex As Exception
                MessageBox.Show("Error running Task 2: " & ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
        If cbtask3.Checked Then
            Try
                Task3()
            Catch ex As Exception
                MessageBox.Show("Error running Task 3: " & ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

        If MessageBox.Show(DWSIM.App.GetLocalString("necessrioreiniciaroD"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.Yes Then
            If DWSIM.App.IsRunningOnMono Then
                DWSIM.App.SaveSettings()
                Application.Exit()
            Else
                Application.Restart()
            End If
        Else
            Me.Close()
        End If

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Me.Close()

    End Sub

End Class