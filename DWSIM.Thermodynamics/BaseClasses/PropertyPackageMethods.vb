Imports DWSIM.Interfaces

<System.Serializable>
Public Class PropertyPackageMethods

    Implements Interfaces.IPropertyPackageMethods

    Public Property Vapor_Fugacity As String = "" Implements IPropertyPackageMethods.Vapor_Fugacity

    Public Property Vapor_Enthalpy_Entropy_CpCv As String = "" Implements IPropertyPackageMethods.Vapor_Enthalpy_Entropy_CpCv

    Public Property Vapor_Thermal_Conductivity As String = "Experimental / Ely-Hanley" Implements IPropertyPackageMethods.Vapor_Thermal_Conductivity

    Public Property Vapor_Viscosity As String = "Experimental / Lucas / Jossi-Stiel-Thodos" Implements IPropertyPackageMethods.Vapor_Viscosity

    Public Property Vapor_Density As String = "" Implements IPropertyPackageMethods.Vapor_Density

    Public Property Liquid_Fugacity As String = "" Implements IPropertyPackageMethods.Liquid_Fugacity

    Public Property Liquid_Enthalpy_Entropy_CpCv As String = "" Implements IPropertyPackageMethods.Liquid_Enthalpy_Entropy_CpCv

    Public Property Liquid_ThermalConductivity As String = "Experimental / Latini" Implements IPropertyPackageMethods.Liquid_ThermalConductivity

    Public Property Liquid_Viscosity As String = "Experimental / Letsou-Stiel" Implements IPropertyPackageMethods.Liquid_Viscosity

    Public Property Liquid_Density As String = "Experimental / Rackett / COSTALD" Implements IPropertyPackageMethods.Liquid_Density

    Public Property SurfaceTension As String = "Experimental / Brock-Bird" Implements IPropertyPackageMethods.SurfaceTension

    Public Property Solid_Density As String = "Experimental Data / User-Defined" Implements IPropertyPackageMethods.Solid_Density

    Public Property Solid_Enthalpy_Entropy_CpCv As String = "Experimental Solid Cp / From Liquid Phase Enthalpy + Enthalpy of Fusion" Implements IPropertyPackageMethods.Solid_Enthalpy_Entropy_CpCv

End Class
