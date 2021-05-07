Public Class TaskHelper

    Public Shared Function Run(action As Action) As Task

        Return Task.Run(action)

    End Function

    Public Shared Function Run(action As Action, ct As Threading.CancellationToken) As Task

        Return Task.Run(action, ct)

    End Function

End Class
