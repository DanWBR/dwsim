Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.ExtensionMethods

Namespace DWSIM.Thermodynamics.AdvancedEOS

    <System.Serializable> Public Class GERG2008PropertyPackage

        Inherits PropertyPackage

        Dim pr As New PropertyPackages.Auxiliary.PengRobinson

        Private _gerg_compounds As New List(Of String)({"Methane",
                                                      "Nitrogen",
                                                      "Carbon dioxide",
                                                      "Ethane",
                                                      "Propane",
                                                      "Isobutane",
                                                      "N-butane",
                                                      "Isopentane",
                                                      "N-pentane",
                                                      "N-hexane",
                                                      "N-heptane",
                                                      "N-octane",
                                                      "N-nonane",
                                                      "N-decane",
                                                      "Hydrogen",
                                                      "Oxygen",
                                                      "Carbon monoxide",
                                                      "Water",
                                                      "Hydrogen sulfide",
                                                      "Helium",
                                                      "Argon"
                                                    })
        Public ReadOnly Property GERGcompounds()
            Get
                Return _gerg_compounds
            End Get
        End Property

        Public Sub New()

            ComponentName = "GERG-2008"
            ComponentDescription = "The Groupe Européen de Recherches Gazières (GERG) 2008 multi-parameter equation of state (EOS) is considered the reference model for the prediction of natural gas mixture properties."

            IsConfigurable = True
        End Sub

        Public Overrides Sub RunPostMaterialStreamSetRoutine()
            If CurrentMaterialStream IsNot Nothing Then
                Dim compounds = CurrentMaterialStream.Phases(0).Compounds.Keys
                For Each comp As String In compounds
                    If Not Me.GERGcompounds.Contains(comp) And CurrentMaterialStream.Phases(0).Compounds(comp).MoleFraction > 0 Then
                        Flowsheet?.ShowMessage(comp + " not part of GERG 2008, compound ignored. ChemSep compounds preferred.", IFlowsheet.MessageType.Warning)
                    End If
                Next
            End If
        End Sub

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New GERG2008PropertyPackage()

        End Function

        Public Overrides Sub DisplayEditingForm()

            MyBase.DisplayEditingForm()

        End Sub

        Private Function GetPRD(Vx() As Double, T As Double, P As Double, tipo As String)

            Return -P / (pr.Z_PR(T, P, Vx, RET_VKij, RET_VTC, RET_VPC, RET_VW, tipo) * 8.314 * T) / 1000

        End Function

        Private Function GetVz(Vz0 As Double()) As Double()

            'The compositions in the x() array use the following order and must be sent as mole fractions:
            '    1 - Methane
            '    2 - Nitrogen
            '    3 - Carbon dioxide
            '    4 - Ethane
            '    5 - Propane
            '    6 - Isobutane
            '    7 - n-Butane
            '    8 - Isopentane
            '    9 - n-Pentane
            '   10 - n-Hexane
            '   11 - n-Heptane
            '   12 - n-Octane
            '   13 - n-Nonane
            '   14 - n-Decane
            '   15 - Hydrogen
            '   16 - Oxygen
            '   17 - Carbon monoxide
            '   18 - Water
            '   19 - Hydrogen sulfide
            '   20 - Helium
            '   21 - Argon
            Dim vector(22) As Double
            Dim compounds = RET_VNAMES().ToList
            Dim i As Integer

            For i = 0 To Me.GERGcompounds.Count - 1
                If compounds.Contains(Me.GERGcompounds(i)) Then vector(i + 1) = Vz0(compounds.IndexOf(Me.GERGcompounds(i)))
            Next

            Return vector.NormalizeY

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Sub DW_CalcProp([property] As String, phase As Phase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = "", pstate As State

            Dim T, P, MW As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    state = "V"
                    pstate = PropertyPackages.State.Vapor
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    state = "L"
                    pstate = PropertyPackages.State.Liquid
                Case Phase.Solid
                    state = "S"
                    pstate = PropertyPackages.State.Solid
            End Select

            Select Case phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case pstate
                Case PropertyPackages.State.Vapor
                    D = GetPRD(RET_VMOL(phase), T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(RET_VMOL(phase)), D, ierr, herr)
                Case Else
                    D = GetPRD(RET_VMOL(phase), T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(RET_VMOL(phase)), D, ierr, herr)
            End Select

            If ierr > 0 And RET_VMOL(phase).Sum > 0.0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(RET_VMOL(phase)), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            MW = Me.AUX_MMM(phase)

            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

            Select Case [property].ToLower
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
                Case "compressibilityfactor"
                    result = Z
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Cp / MW
                Case "heatcapacitycv"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Cv / MW
                Case "enthalpy", "enthalpynf"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = H / MW
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = S / MW
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    If state = "L" Then
                        result = Me.AUX_LIQVISCm(T, P)
                    Else
                        result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If state = "L" Then
                        result = LIQDENS(T, P, RET_VMOL(phase))
                    Else
                        result = Me.AUX_VAPDENS(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(Phase As Phase)

            Dim result As Double

            Dim dwpl As Phase, pstate As State

            Dim T, P, MW As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                    dwpl = PropertyPackages.Phase.Mixture
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                    dwpl = PropertyPackages.Phase.Vapor
                    pstate = State.Vapor
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                    dwpl = PropertyPackages.Phase.Liquid1
                    pstate = State.Liquid
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                    dwpl = PropertyPackages.Phase.Liquid2
                    pstate = State.Liquid
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                    dwpl = PropertyPackages.Phase.Liquid3
                    pstate = State.Liquid
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                    dwpl = PropertyPackages.Phase.Liquid
                    pstate = State.Liquid
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                    dwpl = PropertyPackages.Phase.Aqueous
                    pstate = State.Liquid
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
                    dwpl = PropertyPackages.Phase.Solid
                    pstate = State.Solid
            End Select

            If phaseID > 0 Then
                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                If Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault > 0 Then
                    result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case pstate
                Case PropertyPackages.State.Vapor
                    D = GetPRD(RET_VMOL(Phase), T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(RET_VMOL(Phase)), D, ierr, herr)
                Case Else
                    D = GetPRD(RET_VMOL(Phase), T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(RET_VMOL(Phase)), D, ierr, herr)
            End Select

            If ierr > 0 And RET_VMOL(Phase).Sum > 0.0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(RET_VMOL(Phase)), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                MW = Me.AUX_MMM(Phase)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

                result = LIQDENS(T, P, RET_VMOL(dwpl))

                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = Z

                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Cp / MW
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Cv / MW

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                result = Me.AUX_CONDTL(T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                result = Me.AUX_LIQVISCm(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                MW = Me.AUX_MMM(Phase)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

                result = Me.AUX_VAPDENS(T, P)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = Z

                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Cp / MW
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Cv / MW

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density, MW)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 7 Then

                result = Me.AUX_SOLIDDENS
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next

                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 0.0# 'result

                result = Me.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = 0.0# 'result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = 1.0E+20
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = 1.0E+20

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()

            Else

                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                If Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault > 0 And overallmolarflow > 0 Then
                    result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

        End Sub

        Public Overrides Sub DW_CalcCompPartialVolume(phase As Phase, T As Double, P As Double)

            Dim pi As Integer = 0
            Select Case phase
                Case Phase.Liquid
                Case Phase.Aqueous
                    pi = 6
                Case Phase.Liquid1
                    pi = 3
                Case Phase.Liquid2
                    pi = 4
                Case Phase.Liquid3
                    pi = 5
                Case Phase.Vapor
                    Dim vapdens = AUX_VAPDENS(T, P)
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / vapdens
                    Next
            End Select
            If pi <> 0 Then
                For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(pi).Compounds.Values
                    subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                Next
            End If

        End Sub

        Public Overrides Function DW_CalcEnthalpy(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim Hid As Double = 0 'Me.RET_Hid(298.15, T, Vx)

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case st
                Case State.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case State.Liquid, State.Solid
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            If ierr > 0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            If st = State.Solid Then
                Return H / AUX_MMM(Vx) + Hid - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            Else
                Return H / AUX_MMM(Vx) + Hid
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case st
                Case State.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case State.Liquid, State.Solid
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            If ierr > 0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            If st = State.Solid Then
                Return H / AUX_MMM(Vx) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            Else
                Return H / AUX_MMM(Vx)
            End If

        End Function

        Public Overrides Function DW_CalcEntropy(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim Sid As Double = 0 'Me.RET_Sid(298.15, T, P, Vx)

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case st
                Case State.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case State.Liquid, State.Solid
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            If ierr > 0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            If st = State.Solid Then
                Return S / AUX_MMM(Vx) + Sid - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            Else
                Return S / AUX_MMM(Vx) + Sid
            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case st
                Case State.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case State.Liquid, State.Solid
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            If ierr > 0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            If st = State.Solid Then
                Return S / AUX_MMM(Vx) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            Else
                Return S / AUX_MMM(Vx)
            End If

        End Function

        Public Overrides Function DW_CalcFugCoeff(Vx As Array, T As Double, P As Double, st As State) As Double()

            If DirectCast(Vx, Double()).Sum = 0.0 Then Return RET_UnitaryVector()

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim R As Double = 8.314472

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim V, Z, Z0, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A As Double

            Dim f1, f2, Ar1, Ar2, Ar As Double
            Dim lnfugcoeff(Vx.Length - 1), fugcoeff(Vx.Length - 1), x1(Vx.Length - 1), x2(Vx.Length - 1), a0(2) As Double

            Select Case st
                Case State.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case Else
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            If ierr > 0 Then Throw New Exception("GERG error: unable to calculate density.")

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

            gerg.SetupGERG()

            gerg.Alpha0GERG(T, D, GetVz(Vx), a0)

            Ar = A / (R * T) - a0(0) 'dimensionless

            Z0 = Z

            V = 1.0 / D

            Dim epsilon As Double = 0.00001

            Dim Vn As Double() = Vx.Clone

            Vn = Vn.MultiplyConstY(1.0)

            For j = 0 To Vx.Length - 1

                For k = 0 To Vx.Length - 1
                    x1(k) = Vn(k)
                    x2(k) = Vn(k)
                Next

                x1(j) = Vn(j) - epsilon
                If x1(j) < 0 Then x1(j) = Vn(j) + 0.5 * epsilon
                x2(j) = Vn(j) + epsilon

                gerg.SetupGERG()

                gerg.PropertiesGERG(T, x1.Sum / V, GetVz(x1.NormalizeY), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

                gerg.SetupGERG()

                gerg.Alpha0GERG(T, x1.Sum / V, GetVz(x1.NormalizeY), a0)

                Ar1 = A / (R * T) - a0(0) 'dimensionless

                f1 = Ar1 * x1.Sum

                gerg.SetupGERG()

                gerg.PropertiesGERG(T, x2.Sum / V, GetVz(x2.NormalizeY), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

                gerg.SetupGERG()

                gerg.Alpha0GERG(T, x2.Sum / V, GetVz(x2.NormalizeY), a0)

                Ar2 = A / (R * T) - a0(0) 'dimensionless

                f2 = Ar2 * x2.Sum

                lnfugcoeff(j) = (f2 - f1) / (x2(j) - x1(j)) - Math.Log(Z0)

                fugcoeff(j) = Math.Exp(lnfugcoeff(j))

            Next

            gerg = Nothing

            Return fugcoeff

        End Function

        Public Overrides Function DW_CalcFugCoeff(Vz() As Double, T As Double, V As Double) As Double()

            If DirectCast(Vz, Double()).Sum = 0.0 Then Return RET_UnitaryVector()

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim R As Double = 8.314472

            Dim D As Double, herr As String = ""
            Dim Z, Z0, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A As Double

            Dim f1, f2, Ar1, Ar2, Ar As Double
            Dim lnfugcoeff(Vz.Length - 1), fugcoeff(Vz.Length - 1), x1(Vz.Length - 1), x2(Vz.Length - 1), a0(2) As Double

            D = 1.0 / V / 1000.0

            Dim V2 = 1000.0 * V

            Dim P = DW_CalcP(Vz, T, V)

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vz), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

            gerg.SetupGERG()

            gerg.Alpha0GERG(T, D, GetVz(Vz), a0)

            Ar = A / (R * T) - a0(0) 'dimensionless

            Z0 = Z

            Dim epsilon As Double = 0.00001

            Dim Vn As Double() = Vz.Clone

            Vn = Vn.MultiplyConstY(1.0)

            For j = 0 To Vz.Length - 1

                For k = 0 To Vz.Length - 1
                    x1(k) = Vn(k)
                    x2(k) = Vn(k)
                Next

                x1(j) = Vn(j) - epsilon
                If x1(j) < 0 Then x1(j) = Vn(j) + 0.5 * epsilon
                x2(j) = Vn(j) + epsilon

                gerg.SetupGERG()

                gerg.PropertiesGERG(T, x1.Sum / V2, GetVz(x1.NormalizeY), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

                gerg.SetupGERG()

                gerg.Alpha0GERG(T, x1.Sum / V2, GetVz(x1.NormalizeY), a0)

                Ar1 = A / (R * T) - a0(0) 'dimensionless

                f1 = Ar1 * x1.Sum

                gerg.SetupGERG()

                gerg.PropertiesGERG(T, x2.Sum / V2, GetVz(x2.NormalizeY), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa, A)

                gerg.SetupGERG()

                gerg.Alpha0GERG(T, x2.Sum / V2, GetVz(x2.NormalizeY), a0)

                Ar2 = A / (R * T) - a0(0) 'dimensionless

                f2 = Ar2 * x2.Sum

                lnfugcoeff(j) = (f2 - f1) / (x2(j) - x1(j)) - Math.Log(Z0)

                fugcoeff(j) = Math.Exp(lnfugcoeff(j))

            Next

            gerg = Nothing

            Return fugcoeff

        End Function

        Public Overrides Function DW_CalcP(Vz() As Double, T As Double, V As Double) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D = 1.0 / V / 1000.0

            Dim P, Z As Double

            gerg.PressureGERG(T, D, GetVz(Vz), P, Z)

            gerg = Nothing

            Return P * 1000.0

        End Function

        Public Overrides Function SupportsComponent(comp As ICompoundConstantProperties) As Boolean

            Return True

        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(Phase1 As Phase, T As Double, P As Double, Optional Pvp As Double = 0) As Double

            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQDENS(T)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPDENS(T, P)
            Else
                Return Me.CurrentMaterialStream.Phases(1).Properties.volumetric_flow.GetValueOrDefault * Me.AUX_LIQDENS(T) / Me.CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault + Me.CurrentMaterialStream.Phases(2).Properties.volumetric_flow.GetValueOrDefault * Me.AUX_VAPDENS(T, P) / Me.CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault
            End If

        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T, P)
            Else
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            End If

        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            Return Me.AUX_SURFTM(T)

        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(T As Double, P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Liquid), T, P, State.Liquid)
            HV = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, State.Vapor)
            HM = Me.CurrentMaterialStream.Phases(1).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV

            Dim ent_massica = HM
            Dim flow = Me.CurrentMaterialStream.Phases(0).Properties.massflow
            Return ent_massica * flow

        End Function

        Public Overrides Function DW_CalcCp_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case Phase1
                Case PhaseName.Vapor
                    D = GetPRD(RET_VMOL(Phase1), T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(RET_VMOL(Phase1)), D, ierr, herr)
                Case PhaseName.Liquid
                    D = GetPRD(RET_VMOL(Phase1), T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(RET_VMOL(Phase1)), D, ierr, herr)
            End Select

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(RET_VMOL(Phase1)), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            Return Cp / AUX_MMM(Phase1)

        End Function

        Public Overrides Function DW_CalcCv_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case Phase1
                Case PhaseName.Vapor
                    D = GetPRD(RET_VMOL(Phase1), T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(RET_VMOL(Phase1)), D, ierr, herr)
                Case PhaseName.Liquid
                    D = GetPRD(RET_VMOL(Phase1), T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(RET_VMOL(Phase1)), D, ierr, herr)
            End Select

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(RET_VMOL(Phase1)), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            Return Cv / AUX_MMM(Phase1)

        End Function

        Public Overrides Function DW_CalcK_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            If Phase1 = Phase.Liquid Then
                Return Me.AUX_CONDTL(T)
            Else
                Return Me.AUX_CONDTG(T, P)
            End If

        End Function

        Public Overrides Function DW_CalcMM_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            Return Me.AUX_MMM(Phase1)

        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(T As Double) As Double

            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Phase.Liquid), Me.RET_VPC(Phase.Liquid), Me.RET_VW(Phase.Liquid))

        End Function

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim gerg As New GERGBase

            gerg.SetupGERG()

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Select Case state
                Case PhaseName.Vapor
                    D = GetPRD(Vx, T, P, "V")
                    gerg.DensityGERG(0, T, P / 1000, GetVz(Vx), D, ierr, herr)
                Case PhaseName.Liquid
                    D = GetPRD(Vx, T, P, "L")
                    gerg.DensityGERG(2, T, P / 1000, GetVz(Vx), D, ierr, herr)
            End Select

            gerg.SetupGERG()

            gerg.PropertiesGERG(T, D, GetVz(Vx), P / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)

            gerg = Nothing

            Return Z

        End Function

        Public Overrides Function AUX_VAPDENS(T As Double, P As Double) As Double

            Dim val As Double

            val = AUX_Z(RET_VMOL(Phase.Vapor), T, P, PhaseName.Vapor)

            val = (8.314 * val * T / P)
            val = 1 / val * Me.AUX_MMM(Phase.Vapor) / 1000

            Return val

        End Function

        Public Function LIQDENS(T As Double, P As Double, Vx() As Double) As Double

            Dim val As Double

            val = AUX_Z(Vx, T, P, PhaseName.Liquid)

            val = (8.314 * val * T / P)
            val = 1 / val * Me.AUX_MMM(Vx) / 1000

            Return val

        End Function

        Public Overrides Function CalcSpeedOfSound(p As IPhase) As Double

            Dim gerg As New GERGBase

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Dim Temperature, Pressure As Double
            Temperature = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Pressure = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            gerg.SetupGERG()

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    D = GetPRD(RET_VMOL(Phase.Vapor), Temperature, Pressure, "V")
                    gerg.DensityGERG(0, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Vapor)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Vapor)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    D = GetPRD(RET_VMOL(Phase.Liquid1), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid1)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid1)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Liquid2"
                    D = GetPRD(RET_VMOL(Phase.Liquid2), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid2)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid2)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Liquid3"
                    D = GetPRD(RET_VMOL(Phase.Liquid3), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid3)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid3)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Aqueous"
                    D = GetPRD(RET_VMOL(Phase.Aqueous), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Aqueous)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Aqueous)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Solid"
                    Return 0.0#
            End Select

            gerg = Nothing

            Return W

        End Function

        Public Overrides Function CalcJouleThomsonCoefficient(p As IPhase) As Double

            Dim gerg As New GERGBase

            Dim D As Double, ierr As Integer, herr As String = ""
            Dim Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa As Double

            Dim Temperature, Pressure As Double
            Temperature = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Pressure = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            gerg.SetupGERG()

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    D = GetPRD(RET_VMOL(Phase.Vapor), Temperature, Pressure, "V")
                    gerg.DensityGERG(0, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Vapor)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Vapor)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    D = GetPRD(RET_VMOL(Phase.Liquid1), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid1)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid1)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Liquid2"
                    D = GetPRD(RET_VMOL(Phase.Liquid2), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid2)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid2)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Liquid3"
                    D = GetPRD(RET_VMOL(Phase.Liquid3), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Liquid3)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Liquid3)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Aqueous"
                    D = GetPRD(RET_VMOL(Phase.Aqueous), Temperature, Pressure, "L")
                    gerg.DensityGERG(2, Temperature, Pressure / 1000, GetVz(RET_VMOL(Phase.Aqueous)), D, ierr, herr)
                    gerg.PropertiesGERG(Temperature, D, GetVz(RET_VMOL(Phase.Aqueous)), Pressure / 1000, Z, dPdD, d2PdD2, d2PdTD, dPdT, U, H, S, Cv, Cp, W, G, JT, Kappa)
                Case "Solid"
                    Return 0.0#
            End Select

            gerg = Nothing

            Return JT / 1000

        End Function

    End Class

End Namespace
