Public Interface IFlowsheetSolver

    Function SolveFlowsheet(fs As IFlowsheet) As List(Of Exception)

End Interface
