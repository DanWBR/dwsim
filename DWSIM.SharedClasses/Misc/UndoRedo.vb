
<System.Serializable()> Public Class UndoRedoAction

    Implements Interfaces.IUndoRedoAction

    Public Property AType As Enums.UndoRedoActionType Implements Interfaces.IUndoRedoAction.AType

    Public Property ID As String = "" Implements Interfaces.IUndoRedoAction.ID

    Public Property Name As String = "" Implements Interfaces.IUndoRedoAction.Name

    Public Property NewValue As Object = Nothing Implements Interfaces.IUndoRedoAction.NewValue

    Public Property ObjID As String = "" Implements Interfaces.IUndoRedoAction.ObjID

    Public Property ObjID2 As String = "" Implements Interfaces.IUndoRedoAction.ObjID2

    Public Property OldValue As Object = Nothing Implements Interfaces.IUndoRedoAction.OldValue

    Public Property PropertyName As String = "" Implements Interfaces.IUndoRedoAction.PropertyName

    Public Property Tag As Object = Nothing Implements Interfaces.IUndoRedoAction.Tag

End Class

