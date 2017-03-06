Module Module1

    Sub Main()

        Dim data = ChemeoParser.GetCompoundData(ChemeoParser.GetCompoundCASNos("Formic acid", True)(0)(0))

    End Sub

End Module
