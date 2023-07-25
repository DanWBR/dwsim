Imports Cudafy
Imports Cudafy.Host
Imports Yeppp

Public Class AboutForm

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Me.Close()
    End Sub

    Private Sub AboutForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Application.EnableVisualStyles()

        Me.Text += " (" & My.Application.Info.DirectoryPath & "\DWSIM.xll)"

        Version.Text = "Excel Add-In Version " & My.Application.Info.Version.ToString

        Copyright.Text = My.Application.Info.Copyright

        LblOSInfo.Text = My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"
        LblCLRInfo.Text = "Microsoft .NET Framework, Runtime Version " & System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion.ToString()
    
        Lblcpuinfo.Text = "Retrieving CPU info..."

        Threading.Tasks.Task.Factory.StartNew(Function()
                                                  Dim scrh As New System.Management.ManagementObjectSearcher("select * from Win32_Processor")
                                                  Dim text1 As String = System.Environment.GetEnvironmentVariable("PROCESSOR_IDENTIFIER")
                                                  For Each qinfo In scrh.Get()
                                                      text1 += " / " & qinfo.Properties("Name").Value.ToString
                                                  Next
                                                  text1 += " (" & Yeppp.Library.GetProcessABI().Description & ")"
                                                  Return text1
                                              End Function).ContinueWith(Sub(t)
                                                                             Lblcpuinfo.Text = t.Result
                                                                         End Sub, Threading.Tasks.TaskScheduler.FromCurrentSynchronizationContext)


        Lblcpusimd.Text = "Querying CPU SIMD capabilities..."

        Threading.Tasks.Task.Factory.StartNew(Function()
                                                  Dim text1 As String = ""
                                                  For Each item In Library.GetCpuArchitecture.CpuSimdFeatures
                                                      text1 += item.ToString & " "
                                                  Next
                                                  Return text1
                                              End Function).ContinueWith(Sub(t)
                                                                             Lblcpusimd.Text = t.Result
                                                                         End Sub, Threading.Tasks.TaskScheduler.FromCurrentSynchronizationContext)

    End Sub

End Class