Public Interface IUndoRedoAction

    Property ID As String
    Property Name As String
    Property AType As Enums.UndoRedoActionType
    Property ObjID As String
    Property ObjID2 As String
    Property OldValue As Object
    Property NewValue As Object
    Property Tag As Object
    Property PropertyName As String

End Interface
