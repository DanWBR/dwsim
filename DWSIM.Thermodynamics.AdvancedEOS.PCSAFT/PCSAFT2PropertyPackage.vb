Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.ExtensionMethods
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports System.IO
Imports FileHelpers
Imports System.Windows.Forms

Namespace DWSIM.Thermodynamics.AdvancedEOS

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class PCSParam

        Public compound As String = ""
        Public casno As String = ""
        Public mw As Double = 0.0#
        Public m As Double = 0.0#
        Public sigma As Double = 0.0#
        Public epsilon As Double = 0.0#
        <FieldNullValue(0.0#)> Public kAiBi As Double = 0.0#
        <FieldNullValue(0.0#)> Public epsilon2 As Double = 0.0#
        <FieldHidden()> Public associationparams As String = ""

    End Class

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> Public Class PCSIP

        Implements ICloneable

        Public compound1 As String = ""
        Public casno1 As String = ""
        Public compound2 As String = ""
        Public casno2 As String = ""
        Public kij As Double = 0.0#

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New PCSIP
            With newclass
                .compound1 = Me.compound1
                .compound2 = Me.compound2
                .casno1 = Me.casno1
                .casno2 = Me.casno2
                .kij = Me.kij
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable> Public Class PCSAFT2PropertyPackage

        Inherits PropertyPackage

        Dim pr As New PengRobinson
        Dim lk As New LeeKesler

        Public Property CompoundParameters As Dictionary(Of String, PCSParam) = New Dictionary(Of String, PCSParam)

        Public Property InteractionParameters As Dictionary(Of String, Dictionary(Of String, PCSIP)) = New Dictionary(Of String, Dictionary(Of String, PCSIP))

        Public Property UseLeeKeslerEnthalpy As Boolean = True

        Public Property UseLeeKeslerCpCv As Boolean = True

        Public Overrides ReadOnly Property DisplayDescription As String
            Get
                Return ComponentDescription
            End Get
        End Property

        Public Sub New()

            ComponentName = "PC-SAFT (with Association Support) (.NET Code)"
            ComponentDescription = "The Perturbed Chain SAFT model is a state-of-the-art, engineering-like equation of state. It is designed for modelling mixtures of all types of substances: gases, solvents and polymers."

            IsConfigurable = True

            ReadParameters()

            With PropertyMethodsInfo
                .Vapor_Fugacity = "PC-SAFT EOS"
                .Vapor_Enthalpy_Entropy_CpCv = "PC-SAFT EOS"
                .Vapor_Density = "PC-SAFT EOS"
                .Liquid_Fugacity = "PC-SAFT EOS"
                .Liquid_Enthalpy_Entropy_CpCv = "PC-SAFT EOS"
            End With

        End Sub

        Protected Sub ReadParameters()

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar
            Dim pcsaftdatac() As PCSParam = Nothing
            Dim fh1 As FileHelperEngine(Of PCSParam) = New FileHelperEngine(Of PCSParam)

            Dim res = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceNames
            Dim filestr As Stream = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("pcsaft.dat")

            Using t As StreamReader = New StreamReader(filestr)
                pcsaftdatac = fh1.ReadStream(t)
                For Each pcsaftdata As PCSParam In pcsaftdatac
                    pcsaftdata.associationparams = ("2" & Environment.NewLine & "[0 " _
                        & (pcsaftdata.kAiBi & ("; " _
                        & (pcsaftdata.kAiBi & (" 0]" & Environment.NewLine & "[0 " _
                        & (pcsaftdata.epsilon2 & ("; " _
                        & (pcsaftdata.epsilon2 & " 0]"))))))))
                    If Not CompoundParameters.ContainsKey(pcsaftdata.casno) Then
                        CompoundParameters.Add(pcsaftdata.casno, pcsaftdata)
                    End If
                Next
            End Using

            fh1 = Nothing

            Dim pripc() As PCSIP = Nothing

            Dim fh2 As FileHelperEngine(Of PCSIP) = New FileHelperEngine(Of PCSIP)

            filestr = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("pcsaft_ip.dat")
            Using t As StreamReader = New StreamReader(filestr)
                pripc = fh2.ReadStream(t)
                For Each ip As PCSIP In pripc
                    If InteractionParameters.ContainsKey(ip.casno1) Then
                        If InteractionParameters(ip.casno1).ContainsKey(ip.casno2) Then

                        Else
                            InteractionParameters(ip.casno1).Add(ip.casno2, CType(ip.Clone, PCSIP))
                        End If

                    Else
                        InteractionParameters.Add(ip.casno1, New Dictionary(Of String, PCSIP))
                        InteractionParameters(ip.casno1).Add(ip.casno2, CType(ip.Clone, PCSIP))
                    End If

                Next
            End Using
            For Each ip As PCSIP In pripc
                If InteractionParameters.ContainsKey(ip.casno1) Then
                    If InteractionParameters(ip.casno1).ContainsKey(ip.casno2) Then

                    Else
                        InteractionParameters(ip.casno1).Add(ip.casno2, CType(ip.Clone, PCSIP))
                    End If

                Else
                    InteractionParameters.Add(ip.casno1, New Dictionary(Of String, PCSIP))
                    InteractionParameters(ip.casno1).Add(ip.casno2, CType(ip.Clone, PCSIP))
                End If

            Next

            pripc = Nothing
            fh2 = Nothing

        End Sub

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New PCSAFT2PropertyPackage()

        End Function

        Public Overrides Sub DisplayEditingForm()

            Dim f As New FormConfig() With {.PP = Me}

            f.ShowDialog()

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfig() With {.PP = Me}

        End Function

        Private Function GetPRZ(Vx() As Double, T As Double, P As Double, tipo As String)

            Return pr.Z_PR(T, P, Vx, RET_VKij, RET_VTC, RET_VPC, RET_VW, tipo)

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

            MW = Me.AUX_MMM(phase)

            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

            Dim pcs As New PCSAFT2(Me, RET_VMOL(phase))

            Select Case [property].ToLower
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
                Case "compressibilityfactor"
                    result = AUX_Z(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = DW_CalcCp_ISOL(phase, T, P)
                Case "heatcapacitycv"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = DW_CalcCv_ISOL(phase, T, P)
                Case "enthalpy", "enthalpynf"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
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

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase))

                Dim Zest = GetPRZ(RET_VMOL(Phase), T, P, "L")

                MW = Me.AUX_MMM(Phase)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

                result = LIQDENS(T, P, RET_VMOL(dwpl))

                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = DW_CalcCp_ISOL(dwpl, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = DW_CalcCv_ISOL(dwpl, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = pcs.CalcZ(T, P, "liq", Zest)

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

                Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase))

                Dim Zest = GetPRZ(RET_VMOL(Phase), T, P, "V")

                MW = Me.AUX_MMM(Phase)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = MW

                result = Me.AUX_VAPDENS(T, P)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = DW_CalcCp_ISOL(dwpl, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = DW_CalcCv_ISOL(dwpl, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = pcs.CalcZ(T, P, "gas", Zest)

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

            If UseLeeKeslerEnthalpy Then
                Dim H As Double
                If st = State.Liquid Then
                    H = lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                ElseIf st = State.Vapor Then
                    H = lk.H_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                ElseIf st = State.Solid Then
                    H = lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx)) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                End If
                Return H
            Else
                Dim Hid = Me.RET_Hid(298.15, T, Vx)
                Return DW_CalcEnthalpyDeparture(Vx, T, P, st) + Hid
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim pcs As New PCSAFT2(Me, Vx)

            Dim H = pcs.CalcHr(T, P, If(st = State.Liquid, "liq", "gas"), GetPRZ(Vx, T, P, If(st = State.Liquid, "L", "V"))) / AUX_MMM(Vx)

            If st = State.Solid Then
                Return H - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            Else
                Return H
            End If

        End Function

        Public Overrides Function DW_CalcEntropy(Vx As Array, T As Double, P As Double, st As State) As Double

            If UseLeeKeslerEnthalpy Then
                Dim S As Double
                If st = State.Liquid Then
                    S = lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                ElseIf st = State.Vapor Then
                    S = lk.S_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                ElseIf st = State.Solid Then
                    S = lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx)) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                End If
                Return S
            Else
                Dim Sid As Double = Me.RET_Sid(298.15, T, P, Vx)
                Return DW_CalcEntropyDeparture(Vx, T, P, st) + Sid
            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(Vx As Array, T As Double, P As Double, st As State) As Double

            Dim pcs As New PCSAFT2(Me, Vx)

            Dim Zest = GetPRZ(Vx, T, P, If(st = State.Liquid, "L", "V"))

            Dim S = pcs.CalcSr(T, P, If(st = State.Liquid, "liq", "gas"), Zest) / AUX_MMM(Vx)

            If st = State.Solid Then
                Return S - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            Else
                Return S
            End If

        End Function

        Public Overrides Function DW_CalcFugCoeff(Vx As Array, T As Double, P As Double, st As State) As Double()

            If DirectCast(Vx, Double()).Sum = 0.0 Then Return RET_UnitaryVector()

            Dim pcs As New PCSAFT2(Me, Vx)

            Dim Zest = GetPRZ(Vx, T, P, If(st = State.Liquid, "L", "V"))

            Return pcs.CalcFugCoeff(T, P, If(st = State.Liquid, "liq", "gas"), Zest)

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

            If UseLeeKeslerCpCv Then
                Select Case Phase1
                    Case Phase.Vapor
                        Return lk.CpCvR_LK("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC, RET_VPC, RET_VCP(T), RET_VMM, RET_VW, RET_VZRa)(1)
                    Case Else
                        Return lk.CpCvR_LK("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC, RET_VPC, RET_VCP(T), RET_VMM, RET_VW, RET_VZRa)(1)
                End Select
            Else
                Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase1))
                Select Case Phase1
                    Case Phase.Vapor
                        Dim Zest = GetPRZ(RET_VMOL(Phase1), T, P, "V")
                        Dim Cp = pcs.CalcCp(T, P, "gas", Zest, Function(x) RET_Hid(298.15, x, RET_VMOL(Phase1)))
                        Return Cp
                    Case Else
                        Dim Zest = GetPRZ(RET_VMOL(Phase1), T, P, "L")
                        Dim Cp = pcs.CalcCp(T, P, "liq", Zest, Function(x) RET_Hid(298.15, x, RET_VMOL(Phase1)))
                        Return Cp
                End Select
            End If

        End Function

        Public Overrides Function DW_CalcCv_ISOL(Phase1 As Phase, T As Double, P As Double) As Double

            If UseLeeKeslerCpCv Then
                Select Case Phase1
                    Case Phase.Vapor
                        Return lk.CpCvR_LK("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC, RET_VPC, RET_VCP(T), RET_VMM, RET_VW, RET_VZRa)(2)
                    Case Else
                        Return lk.CpCvR_LK("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC, RET_VPC, RET_VCP(T), RET_VMM, RET_VW, RET_VZRa)(2)
                End Select
            Else
                Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase1))
                Select Case Phase1
                    Case Phase.Vapor
                        Dim Zest = GetPRZ(RET_VMOL(Phase1), T, P, "V")
                        Dim Cv = pcs.CalcCv(T, P, "gas", Zest, Function(x, y) RET_Sid(298.15, x, y, RET_VMOL(Phase1)))
                        Return Cv
                    Case Else
                        Dim Zest = GetPRZ(RET_VMOL(Phase1), T, P, "L")
                        Dim Cv = pcs.CalcCv(T, P, "liq", Zest, Function(x, y) RET_Sid(298.15, x, y, RET_VMOL(Phase1)))
                        Return Cv
                End Select
            End If

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

            Dim pcs As New PCSAFT2(Me, Vx)

            Dim Zest = GetPRZ(Vx, T, P, If(state = PhaseName.Vapor, "V", "L"))

            Return pcs.CalcZ(T, P, If(state = PhaseName.Vapor, "gas", "liq"), Zest)

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

        Public Overrides Function CalcIsothermalCompressibility(p As IPhase) As Double

            Dim Z, P0, P1, T, Z1 As Double

            If Not p.Properties.molarfraction.HasValue Then Return 0.0

            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            Z = p.Properties.compressibilityFactor.GetValueOrDefault

            P1 = P0 + 100

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    Z1 = AUX_Z(RET_VMOL(Phase.Vapor), T, P1, PhaseName.Vapor)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid1), T, P1, PhaseName.Liquid)
                Case "Liquid2"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid2), T, P1, PhaseName.Liquid)
                Case "Liquid3"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid3), T, P1, PhaseName.Liquid)
                Case "Aqueous"
                    Z1 = AUX_Z(RET_VMOL(Phase.Aqueous), T, P1, PhaseName.Liquid)
                Case "Solid"
                    Return 0.0#
            End Select

            Dim K As Double = 1 / P0 - 1 / Z * (Z1 - Z) / 100

            If Double.IsNaN(K) Or Double.IsInfinity(K) Then K = 0.0#

            Return K

        End Function

        Public Overrides Function CalcSpeedOfSound(p As IPhase) As Double

            Dim K, rho As Double

            K = 1 / CalcIsothermalCompressibility(p)

            rho = p.Properties.density.GetValueOrDefault

            Return (K / rho) ^ 0.5

        End Function

        Public Overrides Function CalcJouleThomsonCoefficient(p As IPhase) As Double

            Return MyBase.CalcJouleThomsonCoefficient(p)

            'Dim Temperature, Pressure As Double
            '    Temperature = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            '    Pressure = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

            '    Select Case p.Name
            '        Case "Mixture"
            '            Return 0.0#
            '        Case "Vapor"
            '            If RET_VMOL(Phase.Vapor).Sum = 0.0 Then Return 0.0
            '            Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase.Vapor))
            '            Dim Zest = GetPRZ(RET_VMOL(Phase.Vapor), Temperature, Pressure, "V")
            '            Return pcs.CalcJT(Temperature, Pressure, "gas", Zest, p.Properties.heatCapacityCp.GetValueOrDefault).JT
            '        Case "OverallLiquid"
            '            Return 0.0#
            '        Case "Liquid1"
            '            If RET_VMOL(Phase.Liquid1).Sum = 0.0 Then Return 0.0
            '            Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase.Liquid1))
            '            Dim Zest = GetPRZ(RET_VMOL(Phase.Liquid1), Temperature, Pressure, "L")
            '            Return pcs.CalcJT(Temperature, Pressure, "liq", Zest, p.Properties.heatCapacityCp.GetValueOrDefault).JT
            '        Case "Liquid2"
            '            If RET_VMOL(Phase.Liquid2).Sum = 0.0 Then Return 0.0
            '            Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase.Liquid2))
            '            Dim Zest = GetPRZ(RET_VMOL(Phase.Liquid2), Temperature, Pressure, "L")
            '            Return pcs.CalcJT(Temperature, Pressure, "liq", Zest, p.Properties.heatCapacityCp.GetValueOrDefault).JT
            '        Case "Liquid3"
            '            If RET_VMOL(Phase.Liquid3).Sum = 0.0 Then Return 0.0
            '            Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase.Liquid3))
            '            Dim Zest = GetPRZ(RET_VMOL(Phase.Liquid3), Temperature, Pressure, "L")
            '            Return pcs.CalcJT(Temperature, Pressure, "liq", Zest, p.Properties.heatCapacityCp.GetValueOrDefault).JT
            '        Case "Aqueous"
            '            If RET_VMOL(Phase.Aqueous).Sum = 0.0 Then Return 0.0
            '            Dim pcs As New PCSAFT2(Me, RET_VMOL(Phase.Aqueous))
            '            Dim Zest = GetPRZ(RET_VMOL(Phase.Aqueous), Temperature, Pressure, "L")
            '            Return pcs.CalcJT(Temperature, Pressure, "liq", Zest, p.Properties.heatCapacityCp.GetValueOrDefault).JT
            '        Case "Solid"
            '            Return 0.0#
            '    End Select
            '    Return 0.0#

        End Function

        Public Overrides Function SaveData() As List(Of System.Xml.Linq.XElement)

            Dim data = MyBase.SaveData

            data.Add(New XElement("UseLeeKeslerEnthalpy", UseLeeKeslerEnthalpy))
            data.Add(New XElement("UseLeeKeslerCpCv", UseLeeKeslerCpCv))

            Dim casnos = New List(Of String)
            If (Not (Me.CurrentMaterialStream) Is Nothing) Then
                casnos = Me.CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.CAS_Number).ToList
            End If

            Dim ci As System.Globalization.CultureInfo = System.Globalization.CultureInfo.InvariantCulture
            data.Add(New XElement("InteractionParameters"))
            For Each kvp As KeyValuePair(Of String, Dictionary(Of String, PCSIP)) In InteractionParameters
                For Each kvp2 As KeyValuePair(Of String, PCSIP) In kvp.Value
                    If (Not (Me.CurrentMaterialStream) Is Nothing) Then
                        If (casnos.Contains(kvp.Key) And casnos.Contains(kvp2.Key)) Then
                            data((data.Count - 1)).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp2.Value.compound1), New XAttribute("Compound2", kvp2.Value.compound2), New XAttribute("CAS1", kvp.Key), New XAttribute("CAS2", kvp2.Key), New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                        End If
                    End If
                Next
            Next

            data.Add(New XElement("CompoundParameters"))
            For Each kvp As KeyValuePair(Of String, PCSParam) In CompoundParameters
                If (Not (Me.CurrentMaterialStream) Is Nothing) Then
                    If casnos.Contains(kvp.Key) Then
                        data((data.Count - 1)).Add(New XElement("CompoundParameterSet", New XAttribute("Compound", kvp.Value.compound), New XAttribute("CAS_ID", kvp.Value.casno), New XAttribute("MW", kvp.Value.mw.ToString(ci)), New XAttribute("m", kvp.Value.m.ToString(ci)), New XAttribute("sigma", kvp.Value.sigma.ToString(ci)), New XAttribute("epsilon_k", kvp.Value.epsilon.ToString(ci)), New XAttribute("assocparam", kvp.Value.associationparams.Replace(System.Environment.NewLine, "|"))))
                    End If
                End If
            Next

            Return data

        End Function

        Public Overrides Function LoadData(ByVal data As List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim ci As System.Globalization.CultureInfo = System.Globalization.CultureInfo.InvariantCulture

            Try
                UseLeeKeslerEnthalpy = (From el As XElement In data Select el Where el.Name = "UseLeeKeslerEnthalpy").FirstOrDefault.Value
            Catch ex As Exception
            End Try

            Try
                UseLeeKeslerCpCv = (From el As XElement In data Select el Where el.Name = "UseLeeKeslerCpCv").FirstOrDefault.Value
            Catch ex As Exception
            End Try

            For Each xel As XElement In (From xel2 In data Where xel2.Name = "InteractionParameters" Select xel2).SingleOrDefault().Elements().ToList()

                Dim ip As PCSIP = New PCSIP()
                With ip
                    .compound1 = xel.Attribute("Compound1").Value
                    .compound2 = xel.Attribute("Compound2").Value
                    .casno1 = xel.Attribute("CAS1").Value
                    .casno2 = xel.Attribute("CAS2").Value
                    .kij = Double.Parse(xel.Attribute("Value").Value, ci)
                End With

                Dim dic As Dictionary(Of String, PCSIP) = New Dictionary(Of String, PCSIP)
                dic.Add(xel.Attribute("CAS1").Value, ip)

                If Not Me.InteractionParameters.ContainsKey(xel.Attribute("CAS1").Value) Then
                    Me.InteractionParameters.Add(xel.Attribute("CAS1").Value, dic)
                ElseIf Not Me.InteractionParameters(xel.Attribute("CAS1").Value).ContainsKey(xel.Attribute("CAS2").Value) Then
                    Me.InteractionParameters(xel.Attribute("CAS1").Value).Add(xel.Attribute("CAS2").Value, ip)
                Else
                    Me.InteractionParameters(xel.Attribute("CAS1").Value)(xel.Attribute("CAS2").Value) = ip
                End If

            Next

            For Each xel As XElement In (From xel2 In data Where xel2.Name = "CompoundParameters" Select xel2).SingleOrDefault().Elements().ToList()

                Dim param As PCSParam = New PCSParam()
                With param
                    .compound = xel.Attribute("Compound").Value
                    .casno = xel.Attribute("CAS_ID").Value
                    .mw = Double.Parse(xel.Attribute("MW").Value, ci)
                    .m = Double.Parse(xel.Attribute("m").Value, ci)
                    .sigma = Double.Parse(xel.Attribute("sigma").Value, ci)
                    .epsilon = Double.Parse(xel.Attribute("epsilon_k").Value, ci)
                    .associationparams = xel.Attribute("assocparam").Value.Replace("|", System.Environment.NewLine)
                End With

                If Not Me.CompoundParameters.ContainsKey(xel.Attribute("CAS_ID").Value) Then
                    Me.CompoundParameters.Add(xel.Attribute("CAS_ID").Value, param)
                Else
                    Me.CompoundParameters(xel.Attribute("CAS_ID").Value) = param
                End If

            Next

            Return True

        End Function

    End Class

End Namespace