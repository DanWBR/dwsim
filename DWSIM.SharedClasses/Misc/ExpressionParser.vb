Public Class ExpressionParser

    Public Shared ExpContext As Ciloci.Flee.ExpressionContext

    Public Shared Sub InitializeExpressionParser()

        ExpContext = New Ciloci.Flee.ExpressionContext

        ExpContext.Imports.AddType(GetType(System.Math))
        ExpContext.Variables.Clear()
        ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

        ParserInitialized = True

    End Sub

    Public Shared Property ParserInitialized As Boolean = False

End Class
