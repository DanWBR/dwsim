Public Interface IVirtualFile
    ReadOnly Property Filename As String

    Function ReadAllText() As String

    Sub Write(stream As System.IO.Stream)
    Sub Write(localFile As String)

End Interface
