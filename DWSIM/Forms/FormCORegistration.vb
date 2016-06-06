Imports System.IO
Imports System.Reflection
Imports System.Linq

Public Class FormCORegistration

    Dim calculatorassembly As Assembly
    Dim unitopassembly As Assembly

    Dim systemregfile As String = My.Application.Info.DirectoryPath & "\registry\CO_register_admin.reg"
    Dim systemunregfile As String = My.Application.Info.DirectoryPath & "\registry\CO_unregister_admin.reg"
    Dim userregfile As String = My.Application.Info.DirectoryPath & "\registry\CO_register_user.reg"
    Dim userunregfile As String = My.Application.Info.DirectoryPath & "\registry\CO_unregister_user.reg"

    Private Sub FormCORegistration_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

    End Sub

    Private Sub btnRegisterSystem_Click(sender As Object, e As EventArgs) Handles btnRegisterSystem.Click

        UpdateRegFile(systemregfile)

        Dim p As New Process
        p.StartInfo.FileName = "regedit.exe"
        p.StartInfo.Arguments = Chr(34) & systemregfile & Chr(34)
        If System.Environment.OSVersion.Version.Major >= 6 Then p.StartInfo.Verb = "runas"
        p.Start()
        p.WaitForExit()

    End Sub

    Private Sub btnUnregisterSystem_Click(sender As Object, e As EventArgs) Handles btnUnregisterSystem.Click

        Dim p As New Process
        p.StartInfo.FileName = "regedit.exe"
        p.StartInfo.Arguments = Chr(34) & systemunregfile & Chr(34)
        If System.Environment.OSVersion.Version.Major >= 6 Then p.StartInfo.Verb = "runas"
        p.Start()
        p.WaitForExit()

    End Sub

    Private Sub btnRegisterUser_Click(sender As Object, e As EventArgs) Handles btnRegisterUser.Click

        UpdateRegFile(userregfile)

        Dim p As Process = Process.Start("regedit.exe", Chr(34) & userregfile & Chr(34))
        p.WaitForExit()

    End Sub

    Private Sub btnUnregisterUser_Click(sender As Object, e As EventArgs) Handles btnUnregisterUser.Click

        Dim p As Process = Process.Start("regedit.exe", Chr(34) & userunregfile & Chr(34))
        p.WaitForExit()

    End Sub

    Sub UpdateRegFile(filepath As String)

        Dim regtext = File.ReadAllText(filepath)

        regtext = regtext.Replace("%DTLVER%", calculatorassembly.GetName.Version.ToString)
        regtext = regtext.Replace("%DTLASSEMBLY%", calculatorassembly.FullName)
        regtext = regtext.Replace("%DTLPATH%", calculatorassembly.CodeBase.Replace("\", "/"))
        regtext = regtext.Replace("%UOVER%", unitopassembly.GetName.Version.ToString)
        regtext = regtext.Replace("%UOASSEMBLY%", unitopassembly.FullName)
        regtext = regtext.Replace("%UOPATH%", unitopassembly.CodeBase.Replace("\", "/"))

        File.WriteAllText(filepath, regtext)

    End Sub

End Class