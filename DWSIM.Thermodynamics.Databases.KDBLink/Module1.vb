Module Module1

    Sub Main()

        'KDBParser.GetCompoundData(1)

        Dim id1 = KDBParser.GetCompoundIDs("ethanol", True)
        Dim id2 = KDBParser.GetCompoundIDs("water", True)

        Dim sets = KDBParser.GetBinaryVLESetIDs(id1(0)(0), id2(0)(0))

        Dim vledata = KDBParser.GetVLEData(784)

    End Sub

End Module
