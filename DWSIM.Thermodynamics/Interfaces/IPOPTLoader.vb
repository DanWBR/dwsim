Imports System.IO
Imports System.Runtime.InteropServices

Public Class IPOPTLoader

    ''' <summary>
    ''' DLL Name
    ''' </summary>
    Private Const DLL1Name As String = "IpOpt39.dll"
    Private Const DLL2Name As String = "IpOptFSS39.dll"

    ''' <summary>
    ''' Preload the DLL
    ''' </summary>
    Public Shared Sub Load()

        Dim libraryFile As String = Path.Combine(Path.GetDirectoryName(GetType(IPOPTLoader).Assembly.Location), DLL1Name)

        ' Load the library
        Dim res = IPOPTLoader.LoadLibrary(libraryFile)

        If (res = IntPtr.Zero) Then
            Console.WriteLine("Failed to load IpOpt39.dll.")
        Else
            Console.WriteLine("Loaded IpOpt39.dll (" + res.ToString() + ")")
        End If

        Dim libraryFile2 As String = Path.Combine(Path.GetDirectoryName(GetType(IPOPTLoader).Assembly.Location), DLL2Name)

        ' Load the library
        Dim res2 = IPOPTLoader.LoadLibrary(libraryFile)

        If (res2 = IntPtr.Zero) Then
            Console.WriteLine("Failed to load IpOptFSS39.dll.")
        Else
            Console.WriteLine("Loaded IpOptFSS39.dll (" + res2.ToString() + ")")
        End If

    End Sub

    Private Declare Function LoadLibrary Lib "kernel32.dll" Alias "LoadLibraryA" (ByVal lpFileName As String) As IntPtr

End Class
