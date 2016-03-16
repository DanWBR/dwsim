Public Class FormConfigBase

    Inherits System.Windows.Forms.Form

    Public _pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage
    Public _form As FormFlowsheet
    Public _comps As Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)

End Class