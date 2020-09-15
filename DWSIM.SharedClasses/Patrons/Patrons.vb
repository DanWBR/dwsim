Public Class Patrons

    Public Shared Function GetList() As String

        Dim plist As String
        Using filestr As IO.Stream = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.SharedClasses.activepatrons.txt")
            Using t As New IO.StreamReader(filestr)
                plist = t.ReadToEnd()
            End Using
        End Using

        Return plist.Replace(vbCrLf, ", ").TrimEnd(" ").TrimEnd(",")

    End Function

End Class
