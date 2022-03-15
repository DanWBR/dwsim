Public Interface IVirtualFile

    ''' <summary>
    ''' Contains only FILE NAME, without FULL PATH.
    ''' </summary>
    ReadOnly Property Filename As String

    ''' <summary>
    ''' Returns path to a file. This isn't windows file path in all cases. For Simulate 365 it's just path from Dashboard root.
    ''' </summary>
    ''' <returns></returns>
    ReadOnly Property FullPath As String

    Function GetExtension() As String

    Function ReadAllText() As String

    Function OpenRead() As System.IO.Stream

    Sub Write(stream As System.IO.Stream)
    Sub Write(localFile As String)

    Sub Delete()

    Function Exists() As Boolean

End Interface
