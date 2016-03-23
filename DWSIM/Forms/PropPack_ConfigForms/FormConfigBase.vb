Public Class FormConfigBase

    Inherits System.Windows.Forms.Form

    Public _pp As PropertyPackages.PropertyPackage
    Public _form As FormFlowsheet
    Public _comps As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)

End Class