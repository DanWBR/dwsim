Module Module1

    Sub Main()

        'create automation manager
        Dim interf As New DWSIM.Automation.Automation

        Dim sim As Interfaces.IFlowsheet

        'load Cavett's Problem simulation file
        sim = interf.LoadFlowsheet("samples" & IO.Path.DirectorySeparatorChar & "Cavett's Problem.dwxml")

        'set a listener to catch solver messages
        'sim.SetMessageListener(Sub(msg As String)
        '                           Console.WriteLine(msg)
        '                       End Sub)

        'use CAPE-OPEN interfaces to manipulate objects
        Dim feed, vap_out, liq_out As CapeOpen.ICapeThermoMaterialObject

        feed = sim.GetFlowsheetSimulationObject1("2")
        vap_out = sim.GetFlowsheetSimulationObject1("8")
        liq_out = sim.GetFlowsheetSimulationObject1("18")

        'mass flow rate values in kg/s
        Dim flows(3) As Double

        flows(0) = 170.0#
        flows(1) = 180.0#
        flows(2) = 190.0#
        flows(3) = 200.0#

        'vapor and liquid flows
        Dim vflow, lflow As Double

        For i = 0 To flows.Length - 1
            'set feed mass flow
            feed.SetProp("totalflow", "overall", Nothing, "", "mass", New Double() {flows(i)})
            'calculate the flowsheet (run the simulation)
            Console.WriteLine("Running simulation with F = " & flows(i) & " kg/s, please wait...")
            interf.CalculateFlowsheet(sim, Nothing)
            'check for errors during the last run
            If sim.Solved = False Then
                Console.WriteLine("Error solving flowsheet: " & sim.ErrorMessage)
            End If
            'get vapor outlet mass flow value
            vflow = vap_out.GetProp("totalflow", "overall", Nothing, "", "mass")(0)
            'get liquid outlet mass flow value
            lflow = liq_out.GetProp("totalflow", "overall", Nothing, "", "mass")(0)
            'display results
            Console.WriteLine("Simulation run #" & (i + 1) & " results:" & vbCrLf & "Feed: " & flows(i) & ", Vapor: " & vflow & ", Liquid: " & lflow & " kg/s" & vbCrLf & "Mass balance error: " & (flows(i) - vflow - lflow) & " kg/s")
        Next

        Console.WriteLine("Finished OK! Press any key to close.")
        Console.ReadKey()

    End Sub

End Module
