Public Interface IFileDatabaseProvider

    ReadOnly Property IsDatabaseLoaded As Boolean

    Sub PutFile(filepath As String)

    Sub PutFile(stream As IO.Stream, filename As String)

    Function GetFileStream(filename As String) As IO.MemoryStream

    Function GetFileAsImage(filename As String) As Drawing.Image

    Function GetFileAsText(filename As String) As String

    Sub ExportFile(filename As String, exportpath As String)

    Sub ExportFile(filename As String, stream As IO.MemoryStream)

    Sub DeleteFile(filename As String)

    Function GetFiles() As List(Of String)

    Function CheckIfExists(filename As String) As Boolean

    Sub LoadDatabase(dbpath As String)

    Sub ExportDatabase(filepath As String)

    Sub ReleaseDatabase()

    Sub CreateDatabase()

    Function GetSizeinKB() As Integer

End Interface
