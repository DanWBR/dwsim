Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps

Public Interface IExternalColumnInitialEstimatesProvider

    Function GetInitialEstimates(column As Column) As ColumnSolverInputData

End Interface

Public Interface IExternalColumnSolver

    Function SolveColumn(column As Column, initialestimates As ColumnSolverInputData) As ColumnSolverOutputData

End Interface
