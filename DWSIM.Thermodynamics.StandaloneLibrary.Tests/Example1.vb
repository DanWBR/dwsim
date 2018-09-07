Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages

Module Example1

    Sub Main()

        Console.WriteLine("DTL Property and Equilibrium calculation example with Water and Ethanol")
        Console.WriteLine(vbCrLf)

        Dim dtlc As New DWSIM.Thermodynamics.CalculatorInterface.Calculator

        dtlc.Initialize()

        Dim proppacks As String() = dtlc.GetPropPackList()

        Dim nrtl As String = proppacks(8)

        Dim prpp As PropertyPackage = dtlc.GetPropPackInstance(nrtl)

        Dim compprops As String()

        Dim T, P As Double, pval As String

        T = 355 'K
        P = 101325 'Pa

        compprops = dtlc.GetCompoundConstPropList()

        Console.WriteLine("Ethanol constant properties:" & vbCrLf)
        For Each prop As String In compprops
            pval = dtlc.GetCompoundConstProp("Ethanol", prop)
            Console.WriteLine(prop.PadRight(40) & vbTab & pval)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        compprops = dtlc.GetCompoundPDepPropList()

        Console.Write(vbCrLf)
        Console.WriteLine("Ethanol pressure-dependent properties at P = " & P.ToString("#") & " Pa:" & vbCrLf)
        For Each prop As String In compprops
            pval = dtlc.GetCompoundPDepProp("Ethanol", prop, P)
            Console.WriteLine(prop.PadRight(40) & vbTab & pval)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        compprops = dtlc.GetCompoundTDepPropList()

        Console.Write(vbCrLf)
        Console.WriteLine("Ethanol temperature-dependent properties at T = " & T.ToString("###.##") & " K:" & vbCrLf)
        For Each prop As String In compprops
            pval = dtlc.GetCompoundTDepProp("Ethanol", prop, T)
            Console.WriteLine(prop.PadRight(40) & vbTab & pval)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        Console.Write(vbCrLf)
        Console.WriteLine("Water/Ethanol Interaction Parameters for NRTL model:")
        Console.Write(vbCrLf)

        'uncheck this if you have a CUDA or OpenCL device to use
        'dtlc.EnableGPUProcessing()
        'dtlc.InitComputeDevice(Cudafy.eLanguage.Cuda, 0)

        Dim ip As InteractionParameter = dtlc.GetInteractionParameterSet("NRTL", "Water", "Ethanol")
        Console.WriteLine("A12 = " & ip.Parameters("A12").ToString & " cal/mol")
        Console.WriteLine("A21 = " & ip.Parameters("A21").ToString & " cal/mol")
        Console.WriteLine("alpha = " & ip.Parameters("alpha").ToString)

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        Console.Write(vbCrLf)
        Console.WriteLine("PT Flash of an equimolar mixture of Water and Ethanol at T = " & T.ToString("###.##") & " K and P = " & P.ToString("#") & " Pa:" & vbCrLf)
        Console.WriteLine("Using NRTL model for equilibrim calculations.")
        Console.Write(vbCrLf)
        Dim result2 As Object(,) = dtlc.PTFlash(prpp, 0, P, T, New String() {"Water", "Ethanol"}, New Double() {0.5, 0.5})

        Console.Write(vbCrLf)
        Console.WriteLine("Flash calculation results:")
        Console.Write(vbCrLf)

        Dim i, j As Integer, line As String
        For i = 0 To result2.GetLength(0) - 1
            Select Case i
                Case 0
                    line = "Phase Name".PadRight(30) & vbTab
                Case 1
                    line = "Phase Mole Fraction in Mixture".PadRight(30) & vbTab
                Case 2
                    line = "Water Mole Fraction in Phase".PadRight(30) & vbTab
                Case 3
                    line = "Ethanol Mole Fraction in Phase".PadRight(30) & vbTab
                Case Else
                    line = ""
            End Select
            For j = 0 To result2.GetLength(1) - 1
                If Double.TryParse(result2(i, j).ToString, New Double) Then
                    line += Double.Parse(result2(i, j).ToString).ToString("0.0000").PadRight(10)
                Else
                    line += result2(i, j).ToString.PadRight(10)
                End If
            Next
            Console.WriteLine(line)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        Console.Write(vbCrLf)
        Console.WriteLine("Vapor Phase Mixture Properties at T = " & T.ToString("###.##") & " K and P = " & P.ToString("#") & " Pa:" & vbCrLf)

        Dim compphaseprops As String() = dtlc.GetPropList()
        Dim values As Object()
        For Each prop As String In compphaseprops
            values = dtlc.CalcProp(prpp, prop, "Mole", "Vapor", New String() {"Water", "Ethanol"}, T, P, New Double() {Double.Parse(result2(2, 0).ToString), Double.Parse(result2(3, 0).ToString)})
            line = ""
            For i = 0 To values.Length - 1
                line += Double.Parse(values(i).ToString).ToString("N6") & vbTab
            Next
            Console.WriteLine(prop.PadRight(30) & vbTab & line)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey(True)

        Console.Write(vbCrLf)
        Console.WriteLine("Liquid Phase Mixture Properties at T = " & T.ToString("###.##") & " K and P = " & P.ToString("#") & " Pa:" & vbCrLf)

        For Each prop As String In compphaseprops
            values = dtlc.CalcProp(prpp, prop, "Mole", "Liquid", New String() {"Water", "Ethanol"}, T, P, New Double() {Double.Parse(result2(2, 1).ToString), Double.Parse(result2(3, 1).ToString)})
            line = ""
            For i = 0 To values.Length - 1
                line += Double.Parse(values(i).ToString).ToString("N6") & vbTab
            Next
            Console.WriteLine(prop.PadRight(30) & vbTab & line)
        Next

        Console.Write(vbCrLf)
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey(True)

    End Sub

End Module
