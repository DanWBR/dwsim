Public Class FormConfigPropertyPackageBase

    Inherits System.Windows.Forms.Form

    Public _pp As PropertyPackages.PropertyPackage
    Public _form As Interfaces.IFlowsheet
    Public _comps As Dictionary(Of String, BaseClasses.ConstantProperties)

End Class