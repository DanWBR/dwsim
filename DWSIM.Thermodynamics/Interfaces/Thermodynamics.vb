'    DWSIM/DTL Interface  Methods
'    Copyright 2012-2014 Daniel Wagner O. de Medeiros
'
'    This file is part of DTL.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DTL.  If not, see <http://www.gnu.org/licenses/>.
'
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.SharedClasses.UnitOperations
Imports DWSIM.Thermodynamics.BaseClasses
Imports CAPEOPEN110
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms

Namespace CalculatorInterface

    <Serializable()> <ComClass(Calculator.ClassId, Calculator.InterfaceId, Calculator.EventsId)>
    Public Class Calculator

        Public Const ClassId As String = "5F2B671E-FA61-401e-8D14-71FB5B328F9B"
        Public Const InterfaceId As String = "0EA44EDE-AD65-435c-B8CC-0D1146BD182B"
        Public Const EventsId As String = "0817BD3F-5278-4e49-A7FB-92416A8A7E4E"

        Private _availablecomps As Dictionary(Of String, BaseClasses.ConstantProperties)

        Private _rpp As RaoultPropertyPackage

        Sub New()

        End Sub

        ''' <summary>
        ''' Initializes the calculator and loads the compound databases into memory.
        ''' </summary>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(1)> Sub Initialize()

            'load databases
            _availablecomps = New Dictionary(Of String, BaseClasses.ConstantProperties)

            LoadCSDB()
            LoadDWSIMDB()
            LoadBDDB()
            LoadEDB()
            LoadCheDLDB()

            _rpp = New RaoultPropertyPackage(True)
            TransferCompounds(_rpp)

            GlobalSettings.Settings.InspectorEnabled = False

        End Sub

        ''' <summary>
        ''' Initializes the calculator and loads the compound databases into memory, including the specified user databases.
        ''' </summary>
        ''' <param name="userdbs">A string array containing the full path of DWSIM-generated user databases to load.</param>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(2)> Sub Initialize(ByVal userdbs() As String)

            Initialize()

            For Each db As String In userdbs
                LoadUserDB(db)
            Next

        End Sub

        Public Sub TransferCompounds(ByVal pp As PropertyPackage)

            pp._availablecomps = _availablecomps

        End Sub

        Public ReadOnly Property AvailableCompounds As Dictionary(Of String, BaseClasses.ConstantProperties)
            Get
                Return _availablecomps
            End Get
        End Property

        ''' <summary>
        ''' Enables CPU parallel processing for some tasks.
        ''' </summary>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(3)> Sub EnableParallelProcessing()

            GlobalSettings.Settings.EnableParallelProcessing = True

        End Sub

        ''' <summary>
        ''' Disables CPU parallel processing.
        ''' </summary>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(4)> Sub DisableParallelProcessing()

            GlobalSettings.Settings.EnableParallelProcessing = False

        End Sub

        ''' <summary>
        ''' Sets the debug level, which controls the amount of information which is written to the screen.
        ''' </summary>
        ''' <param name="level">Debug level: 0 = none, 1 = low, 2 = medium, 3 = high</param>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(50)> Public Sub SetDebugLevel(level As Integer)

            GlobalSettings.Settings.DebugLevel = level

        End Sub

        ''' <summary>
        ''' Activates CPU-accelerated SIMD vector operations.
        ''' </summary>
        ''' <remarks>This uses the routines implemented in the Yeppp! (http://www.yeppp.info) library, which must be in the same directory as DTL's.</remarks>
        <Runtime.InteropServices.DispId(51)> Public Sub EnableSIMDExtensions()
            GlobalSettings.Settings.UseSIMDExtensions = True
        End Sub

        ''' <summary>
        ''' Deactivates CPU-accelerated SIMD vector operations.
        ''' </summary>
        ''' <remarks>This uses the routines implemented in the Yeppp! (http://www.yeppp.info) library, which must be in the same directory as DTL's.</remarks>
        <Runtime.InteropServices.DispId(52)> Public Sub DisableSIMDExtensions()
            GlobalSettings.Settings.UseSIMDExtensions = False
        End Sub

        Private Sub LoadUserDB(ByVal path As String)
            Dim cpa() As ConstantProperties
            cpa = Databases.UserDB.ReadComps(path)
            For Each cp As ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Private Sub LoadCSDB()
            Dim csdb As New Databases.ChemSep
            Dim cpa() As ConstantProperties
            'Try
            csdb.Load()
            cpa = csdb.Transfer()
            For Each cp As ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Private Sub LoadDWSIMDB()
            Dim dwdb As New Databases.DWSIM
            Dim cpa() As ConstantProperties
            dwdb.Load()
            cpa = dwdb.Transfer()
            For Each cp As ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadCheDLDB()

            Dim chedl As New Databases.ChEDL_Thermo
            Dim cpa() As BaseClasses.ConstantProperties
            chedl.Load()
            cpa = chedl.Transfer().ToArray()
            Dim addedcomps = _availablecomps.Keys.Select(Function(x) x.ToLower).ToList()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not addedcomps.Contains(cp.Name.ToLower) AndAlso Not _availablecomps.ContainsKey(cp.Name) Then
                    If _availablecomps.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                        _availablecomps.Add(cp.Name, cp)
                    End If
                End If
            Next

        End Sub

        Private Sub LoadBDDB()
            Dim bddb As New Databases.Biodiesel
            Dim cpa() As BaseClasses.ConstantProperties
            bddb.Load()
            cpa = bddb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Private Sub LoadEDB()
            Dim edb As New Databases.Electrolyte
            Dim cpa() As BaseClasses.ConstantProperties
            edb.Load()
            cpa = edb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Private Sub LoadCPDB()
            Dim cpdb As New Databases.CoolProp
            Dim cpa() As BaseClasses.ConstantProperties
            cpdb.Load()
            Try
                cpa = cpdb.Transfer()
                For Each cp As BaseClasses.ConstantProperties In cpa
                    If _availablecomps.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                        _availablecomps.Add(cp.Name, cp)
                        _availablecomps(cp.Name).IsCOOLPROPSupported = True
                    End If
                Next
            Catch ex As Exception
            End Try
        End Sub

        Friend Sub SetIP(ByVal proppack As String, ByRef pp As PropertyPackage, ByVal compounds As Object, ByVal ip1 As Object, ByVal ip2 As Object, ByVal ip3 As Object, ByVal ip4 As Object)

            Dim i, j As Integer

            Select Case proppack
                Case "Peng-Robinson (PR)"
                    With CType(pp, PengRobinsonPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Peng-Robinson 1978 (PR78)"
                    With CType(pp, PengRobinson1978PropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
                    With CType(pp, PRSV2PropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1.ToLower, New Dictionary(Of String, Auxiliary.PRSV2_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1.ToLower).Add(c2.ToLower, New Auxiliary.PRSV2_IPData())
                                    With .Item(c1.ToLower).Item(c2.ToLower)
                                        .kij = ip1(i, j)
                                        .kji = ip2(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                    With CType(pp, PRSV2VLPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1.ToLower, New Dictionary(Of String, Auxiliary.PRSV2_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1.ToLower).Add(c2.ToLower, New Auxiliary.PRSV2_IPData())
                                    With .Item(c1.ToLower).Item(c2.ToLower)
                                        .kij = ip1(i, j)
                                        .kji = ip2(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Soave-Redlich-Kwong (SRK)"
                    With CType(pp, SRKPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Peng-Robinson / Lee-Kesler (PR/LK)"
                    With CType(pp, PengRobinsonLKPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "UNIFAC"
                    With CType(pp, UNIFACPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "UNIFAC-LL"
                    With CType(pp, UNIFACLLPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "NRTL"
                    With CType(pp, NRTLPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                    With CType(pp, NRTLPropertyPackage).m_uni.InteractionParameters
                        If Not ip2 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.NRTL_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.NRTL_IPData())
                                    With .Item(c1).Item(c2)
                                        .A12 = ip2(i, j)
                                        .A21 = ip3(i, j)
                                        .alpha12 = ip4(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "UNIQUAC"
                    With CType(pp, UNIQUACPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                    With CType(pp, UNIQUACPropertyPackage).m_uni.InteractionParameters
                        If Not ip2 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.UNIQUAC_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.UNIQUAC_IPData())
                                    With .Item(c1).Item(c2)
                                        .A12 = ip2(i, j)
                                        .A21 = ip3(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Modified UNIFAC (Dortmund)"
                    With CType(pp, MODFACPropertyPackage).m_pr.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.PR_IPData())
                                    With .Item(c1).Item(c2)
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Lee-Kesler-Plöcker"
                    With CType(pp, LKPPropertyPackage).m_lk.InteractionParameters
                        If Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                .Add(c1, New Dictionary(Of String, Auxiliary.LKP_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    .Item(c1).Add(c2, New Auxiliary.LKP_IPData())
                                    With .Item(c1).Item(c2)
                                        .ID1 = c1
                                        .ID2 = c2
                                        .kij = ip1(i, j)
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Chao-Seader"
                Case "Grayson-Streed"
                Case "IAPWS-IF97 Steam Tables"
                Case "Raoult's Law"
            End Select

        End Sub

        ''' <summary>
        ''' Returns a single constant property value for a compound.
        ''' </summary>
        ''' <param name="compound">Compound name.</param>
        ''' <param name="prop">Property identifier.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(10)> Public Function GetCompoundConstProp(
            ByVal compound As String,
            ByVal prop As String) As String

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                phase.Compounds.Add(compound, New Compound(compound, ""))
                phase.Compounds(compound).ConstantProperties = pp._availablecomps(compound)
            Next

            Dim tmpcomp As ConstantProperties = pp._availablecomps(compound)
            pp._selectedcomps.Add(compound, tmpcomp)

            ms._pp = pp
            pp.SetMaterial(ms)

            Dim results As Object = Nothing

            results = ms.GetCompoundConstant(New Object() {prop}, New Object() {compound})

            Return results(0)

            ms.Dispose()
            ms = Nothing

        End Function

        ''' <summary>
        ''' Returns a single temperature-dependent property value for a compound.
        ''' </summary>
        ''' <param name="compound">Compound name.</param>
        ''' <param name="prop">Property identifier.</param>
        ''' <param name="temperature">Temperature in K.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(11)> Public Function GetCompoundTDepProp(
            ByVal compound As String,
            ByVal prop As String,
            ByVal temperature As Double) As String

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                phase.Compounds.Add(compound, New Compound(compound, ""))
                phase.Compounds(compound).ConstantProperties = pp._availablecomps(compound)
            Next

            Dim tmpcomp As ConstantProperties = pp._availablecomps(compound)
            pp._selectedcomps.Add(compound, tmpcomp)

            ms._pp = pp
            pp.SetMaterial(ms)

            Dim results As Object = Nothing

            ms.GetTDependentProperty(New Object() {prop}, temperature, New Object() {compound}, results)

            Return results(0)

            ms.Dispose()
            ms = Nothing

        End Function

        ''' <summary>
        ''' Returns a single pressure-dependent property value for a compound.
        ''' </summary>
        ''' <param name="compound">Compound name.</param>
        ''' <param name="prop">Property identifier.</param>
        ''' <param name="pressure">Pressure in Pa.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(12)> Public Function GetCompoundPDepProp(
            ByVal compound As String,
            ByVal prop As String,
            ByVal pressure As Double) As String

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                phase.Compounds.Add(compound, New Compound(compound, ""))
                phase.Compounds(compound).ConstantProperties = pp._availablecomps(compound)
            Next

            Dim tmpcomp As ConstantProperties = pp._availablecomps(compound)
            pp._selectedcomps.Add(compound, tmpcomp)

            ms._pp = pp
            pp.SetMaterial(ms)

            Dim results As Object = Nothing

            ms.GetPDependentProperty(New Object() {prop}, pressure, New Object() {compound}, results)

            Return results(0)

            ms.Dispose()
            ms = Nothing

        End Function

        ''' <summary>
        ''' Returns a list of the available single compound constant properties.
        ''' </summary>
        ''' <returns>A list of the available single compound properties</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(13)> Public Function GetCompoundConstPropList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim props As New ArrayList

            props.AddRange(pp.GetConstPropList())

            Dim values As Object() = props.ToArray

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Returns a list of the available single compound temperature-dependent properties.
        ''' </summary>
        ''' <returns>A list of the available single compound properties</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(14)> Public Function GetCompoundTDepPropList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim props As New ArrayList

            props.AddRange(pp.GetTDependentPropList())

            Dim values As Object() = props.ToArray

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Returns a list of the available single compound pressure-dependent properties.
        ''' </summary>
        ''' <returns>A list of the available single compound properties</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(15)> Public Function GetCompoundPDepPropList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim props As New ArrayList

            props.AddRange(pp.GetPDependentPropList())

            Dim values As Object() = props.ToArray

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Calculates properties using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="prop">The property to calculate.</param>
        ''' <param name="basis">The returning basis of the properties: Mole, Mass or UNDEFINED.</param>
        ''' <param name="phaselabel">The name of the phase to calculate properties from.</param>
        ''' <param name="compounds">The list of compounds to include.</param>
        ''' <param name="temperature">Temperature in K.</param>
        ''' <param name="pressure">Pressure in Pa.</param>
        ''' <param name="molefractions">*Normalized* mole fractions of the compounds in the mixture.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(16)> Public Function CalcProp(
            ByVal proppack As String,
            ByVal prop As String,
            ByVal basis As String,
            ByVal phaselabel As String,
            ByVal compounds As String(),
            ByVal temperature As Double,
            ByVal pressure As Double,
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object()

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage = ppm.GetPropertyPackage(proppack)

            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                pp._selectedcomps.Add(c, tmpcomp)
            Next

            Dim dwp As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel Then dwp = pi.DWPhaseID
            Next

            ms.SetPhaseComposition(molefractions, dwp)
            ms.Phases(0).Properties.temperature = temperature
            ms.Phases(0).Properties.pressure = pressure
            ms.Phases(0).Properties.molarfraction = 1.0

            ms._pp = pp
            pp.SetMaterial(ms)

            If prop.ToLower <> "molecularweight" Then
                pp.CalcSinglePhaseProp(New Object() {prop}, phaselabel)
            End If

            Dim results As Double() = Nothing
            Dim allres As New ArrayList
            Dim i As Integer

            results = Nothing
            If prop.ToLower <> "molecularweight" Then
                ms.GetSinglePhaseProp(prop, phaselabel, basis, results)
            Else
                results = New Double() {pp.AUX_MMM(dwp)}
            End If
            For i = 0 To results.Length - 1
                allres.Add(results(i))
            Next

            pp.Dispose()
            pp = Nothing

            ms.Dispose()
            ms = Nothing

            Dim values As Object() = allres.ToArray()

            Dim results2(values.Length - 1) As Object

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function
        ''' <summary>
        ''' Calculates properties using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The Property Package instance to use.</param>
        ''' <param name="prop">The property to calculate.</param>
        ''' <param name="basis">The returning basis of the properties: Mole, Mass or UNDEFINED.</param>
        ''' <param name="phaselabel">The name of the phase to calculate properties from.</param>
        ''' <param name="compounds">The list of compounds to include.</param>
        ''' <param name="temperature">Temperature in K.</param>
        ''' <param name="pressure">Pressure in Pa.</param>
        ''' <param name="molefractions">*Normalized* mole fractions of the compounds in the mixture.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(17)> Public Function CalcProp(
            ByVal proppack As PropertyPackage,
            ByVal prop As String,
            ByVal basis As String,
            ByVal phaselabel As String,
            ByVal compounds As String(),
            ByVal temperature As Double,
            ByVal pressure As Double,
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object()

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            Dim dwp As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel Then dwp = pi.DWPhaseID
            Next

            ms.SetPhaseComposition(molefractions, dwp)
            ms.CalcPhaseMassComposition(dwp)
            ms.Phases(0).Properties.temperature = temperature
            ms.Phases(0).Properties.pressure = pressure

            ms._pp = pp
            pp.SetMaterial(ms)

            If prop.ToLower <> "molecularweight" Then
                pp.CalcSinglePhaseProp(New Object() {prop}, phaselabel)
            End If

            Dim results As Double() = Nothing
            Dim allres As New ArrayList
            Dim i As Integer

            results = Nothing
            If prop.ToLower <> "molecularweight" Then
                ms.GetSinglePhaseProp(prop, phaselabel, basis, results)
            Else
                results = New Double() {pp.AUX_MMM(dwp)}
            End If
            For i = 0 To results.Length - 1
                allres.Add(results(i))
            Next

            ms.Dispose()
            ms = Nothing

            Dim values As Object() = allres.ToArray()

            Dim results2(values.Length - 1) As Object

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function
        ''' <summary>
        ''' Calculates two phase properties (K-values, Ln(K-values) or Surface Tension) using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="prop">The property to calculate.</param>
        ''' <param name="basis">The returning basis of the properties: Mole, Mass or UNDEFINED.</param>
        ''' <param name="phaselabel1">The name of the first phase.</param>
        ''' <param name="phaselabel2">The name of the second phase.</param>
        ''' <param name="compounds">The list of compounds to include.</param>
        ''' <param name="temperature">Temperature in K.</param>
        ''' <param name="pressure">Pressure in Pa.</param>
        ''' <param name="molefractions1">*Normalized* mole fractions of the compounds in the first phase.</param>
        ''' <param name="molefractions2">*Normalized* mole fractions of the compounds in the second phase.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(18)> Public Function CalcTwoPhaseProp(
            ByVal proppack As String,
            ByVal prop As String,
            ByVal basis As String,
            ByVal phaselabel1 As String,
            ByVal phaselabel2 As String,
            ByVal compounds As String(),
            ByVal temperature As Double,
            ByVal pressure As Double,
            ByVal molefractions1 As Double(),
            ByVal molefractions2 As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object()

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage = ppm.GetPropertyPackage(proppack)

            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                pp._selectedcomps.Add(c, tmpcomp)
            Next

            Dim dwp1 As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel1 Then dwp1 = pi.DWPhaseID
            Next

            Dim dwp2 As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel2 Then dwp2 = pi.DWPhaseID
            Next

            ms.SetPhaseComposition(molefractions1, dwp1)
            ms.SetPhaseComposition(molefractions2, dwp2)
            ms.Phases(0).Properties.temperature = temperature
            ms.Phases(0).Properties.pressure = pressure

            ms._pp = pp
            pp.SetMaterial(ms)

            pp.CalcTwoPhaseProp(New Object() {prop}, New Object() {phaselabel1, phaselabel2})

            Dim results As Double() = Nothing
            Dim allres As New ArrayList
            Dim i As Integer

            results = Nothing
            ms.GetTwoPhaseProp(prop, New Object() {phaselabel1, phaselabel2}, basis, results)
            For i = 0 To results.Length - 1
                allres.Add(results(i))
            Next

            pp.Dispose()
            pp = Nothing

            ms.Dispose()
            ms = Nothing

            Dim values As Object() = allres.ToArray()

            Dim results2(values.Length - 1) As Object

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function
        ''' <summary>
        ''' Calculates two phase properties (K-values, Ln(K-values) or Surface Tension) using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The Property Package instance to use.</param>
        ''' <param name="prop">The property to calculate.</param>
        ''' <param name="basis">The returning basis of the properties: Mole, Mass or UNDEFINED.</param>
        ''' <param name="phaselabel1">The name of the first phase.</param>
        ''' <param name="phaselabel2">The name of the second phase.</param>
        ''' <param name="compounds">The list of compounds to include.</param>
        ''' <param name="temperature">Temperature in K.</param>
        ''' <param name="pressure">Pressure in Pa.</param>
        ''' <param name="molefractions1">*Normalized* mole fractions of the compounds in the first phase.</param>
        ''' <param name="molefractions2">*Normalized* mole fractions of the compounds in the second phase.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(19)> Public Function CalcTwoPhaseProp(
            ByVal proppack As PropertyPackage,
            ByVal prop As String,
            ByVal basis As String,
            ByVal phaselabel1 As String,
            ByVal phaselabel2 As String,
            ByVal compounds As String(),
            ByVal temperature As Double,
            ByVal pressure As Double,
            ByVal molefractions1 As Double(),
            ByVal molefractions2 As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object()

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage
            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            Dim dwp1 As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel1 Then dwp1 = pi.DWPhaseID
            Next

            Dim dwp2 As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
            For Each pi As PhaseInfo In pp.PhaseMappings.Values
                If pi.PhaseLabel = phaselabel2 Then dwp2 = pi.DWPhaseID
            Next

            ms.SetPhaseComposition(molefractions1, dwp1)
            ms.SetPhaseComposition(molefractions2, dwp2)
            ms.Phases(0).Properties.temperature = temperature
            ms.Phases(0).Properties.pressure = pressure

            ms._pp = pp
            pp.SetMaterial(ms)

            pp.CalcTwoPhaseProp(New Object() {prop}, New Object() {phaselabel1, phaselabel2})

            Dim results As Double() = Nothing
            Dim allres As New ArrayList
            Dim i As Integer

            results = Nothing
            ms.GetTwoPhaseProp(prop, New Object() {phaselabel1, phaselabel2}, basis, results)
            For i = 0 To results.Length - 1
                allres.Add(results(i))
            Next

            pp.Dispose()
            pp = Nothing

            ms.Dispose()
            ms = Nothing

            Dim values As Object() = allres.ToArray()

            Dim results2(values.Length - 1) As Object

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function
        ''' <summary>
        ''' Returns a list of the available Property Packages.
        ''' </summary>
        ''' <returns>A list of the available Property Packages</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(20)> Public Function GetPropPackList() As String()

            Dim ppm As New CAPEOPENManager()

            Dim values As Object() = ppm.GetPropertyPackageList()

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

            ppm.Dispose()
            ppm = Nothing

        End Function

        ''' <summary>
        ''' Returns a Property Package instance that can be reused on multiple function calls.
        ''' </summary>
        ''' <returns>A Property Package instance.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(21)> Public Function GetPropPackInstance(ByVal proppackname As String) As PropertyPackage

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage = ppm.GetPropertyPackage(proppackname)

            TransferCompounds(pp)

            ppm.Dispose()
            ppm = Nothing

            Return pp

        End Function

        ''' <summary>
        ''' Returns a list of the available single-phase properties.
        ''' </summary>
        ''' <returns>A list of the available single-phase properties</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(22)> Public Function GetPropList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim values As Object() = pp.GetSinglePhasePropList()

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Returns a list of the available two-phase properties.
        ''' </summary>
        ''' <returns>A list of the available two-phase properties</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(23)> Public Function GetTwoPhasePropList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim values As Object() = pp.GetTwoPhasePropList()

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Returns a list of the available phases.
        ''' </summary>
        ''' <returns>A list of the available phases</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(24)> Public Function GetPhaseList() As String()

            Dim pp = _rpp
            pp._selectedcomps.Clear()

            Dim values As Object() = pp.GetPhaseList()

            Dim results2(values.Length - 1) As String
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i) = values(i)
            Next

            Return results2

        End Function

        ''' <summary>
        ''' Returns a list of the available compounds.
        ''' </summary>
        ''' <returns>A list of the available compounds</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(25)> Public Function GetCompoundList() As String()

            Try

                Dim comps As New ArrayList

                For Each c As ConstantProperties In _availablecomps.Values
                    comps.Add(c.Name)
                Next

                Dim values As Object() = comps.ToArray

                Dim results2(values.Length - 1) As String
                Dim i As Integer

                For i = 0 To values.Length - 1
                    results2(i) = values(i)
                Next

                Return results2

            Catch ex As Exception

                Return New Object() {ex.ToString}

            End Try

        End Function

        ''' <summary>
        ''' Calculates a PT Flash using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="T">Temperature in K.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(26)> Public Function PTFlash(
            ByVal proppack As String,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal T As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = ppm.GetPropertyPackage(proppack)
            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.temperature = T
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            If TypeOf proppack Is String Then
                pp.Dispose()
                pp = Nothing
            End If

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PH Flash using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="H">Mixture Mass Enthalpy in kJ/kg.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(27)> Public Function PHFlash(
            ByVal proppack As String,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal H As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = ppm.GetPropertyPackage(proppack)
            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.enthalpy = H
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            If TypeOf proppack Is String Then
                pp.Dispose()
                pp = Nothing
            End If

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PH Flash using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="S">Mixture Mass Entropy in kJ/[kg.K].</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(28)> Public Function PSFlash(
            ByVal proppack As String,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal S As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = ppm.GetPropertyPackage(proppack)
            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.entropy = S
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            If TypeOf proppack Is String Then
                pp.Dispose()
                pp = Nothing
            End If

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PVF Flash using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="VF">Mixture Mole Vapor Fraction.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(29)> Public Function PVFFlash(
            ByVal proppack As String,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal VF As Double,
            ByVal compounds As Object(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = ppm.GetPropertyPackage(proppack)
            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(2).Properties.molarfraction = VF
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            If TypeOf proppack Is String Then
                pp.Dispose()
                pp = Nothing
            End If

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a TVF Flash using the selected Property Package.
        ''' </summary>
        ''' <param name="proppack">The name of the Property Package to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="T">Temperature in K.</param>
        ''' <param name="VF">Mixture Mole Vapor Fraction.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        '''<param name="InitialPressureEstimate">Initial estimate for the pressure, in Pa.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(30)> Public Function TVFFlash(
            ByVal proppack As String,
            ByVal flashalg As Integer,
            ByVal T As Double,
            ByVal VF As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialPressureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = ppm.GetPropertyPackage(proppack)
            TransferCompounds(pp)

            SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(2).Properties.molarfraction = VF
            ms.Phases(0).Properties.temperature = T

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.pressure = InitialPressureEstimate

            pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.pressure.GetValueOrDefault

            If TypeOf proppack Is String Then
                pp.Dispose()
                pp = Nothing
            End If

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function

        ''' <summary>
        ''' Calculates a PT Flash using the referenced Property Package.
        ''' </summary>
        ''' <param name="proppack">A reference to the Property Package object to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="T">Temperature in K.</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(31)> Public Function PTFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal T As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.temperature = T
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PH Flash using the referenced Property Package.
        ''' </summary>
        ''' <param name="proppack">A reference to the Property Package object to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="H">Mixture Mass Enthalpy in kJ/kg.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(32)> Public Function PHFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal H As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.enthalpy = H
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PS Flash using the referenced Property Package.
        ''' </summary>
        ''' <param name="proppack">A reference to the Property Package object to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="S">Mixture Mass Entropyin kJ/[kg.K].</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(33)> Public Function PSFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal S As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(0).Properties.entropy = S
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a PVF Flash using the referenced Property Package.
        ''' </summary>
        ''' <param name="proppack">A reference to the Property Package object to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="P">Pressure in Pa.</param>
        ''' <param name="VF">Mixture Mole Vapor Fraction.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <param name="InitialTemperatureEstimate">Initial estimate for the temperature, in K.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(34)> Public Function PVFFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal VF As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialTemperatureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(2).Properties.molarfraction = VF
            ms.Phases(0).Properties.pressure = P

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.temperature = InitialTemperatureEstimate

            pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.temperature.GetValueOrDefault

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Calculates a TVF Flash using the referenced Property Package.
        ''' </summary>
        ''' <param name="proppack">A reference to the Property Package object to use.</param>
        ''' <param name="flashalg">[DEPRECATED] Flash Algorithm (2 = Global Def., 0 = NL VLE, 1 = IO VLE, 3 = IO VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = NL VLLE, 7 = NL SLE, 8 = NL Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)</param>
        ''' <param name="T">Temperature in K.</param>
        ''' <param name="VF">Mixture Mole Vapor Fraction.</param>
        ''' <param name="compounds">Compound names.</param>
        ''' <param name="molefractions">Compound mole fractions.</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        '''<param name="InitialPressureEstimate">Initial estimate for the pressure, in Pa.</param>
        ''' <returns>A matrix containing phase fractions and compound distribution in mole fractions.</returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(35)> Public Function TVFFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal T As Double,
            ByVal VF As Double,
            ByVal compounds As String(),
            ByVal molefractions As Double(),
            Optional ByVal ip1 As Object = Nothing,
            Optional ByVal ip2 As Object = Nothing,
            Optional ByVal ip3 As Object = Nothing,
            Optional ByVal ip4 As Object = Nothing,
            Optional ByVal InitialPressureEstimate As Double = 0.0#) As Object(,)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            ppm.Dispose()
            ppm = Nothing

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)
            ms.Phases(2).Properties.molarfraction = VF
            ms.Phases(0).Properties.temperature = T

            ms._pp = pp
            pp.SetMaterial(ms)

            ms.Phases(0).Properties.pressure = InitialPressureEstimate

            pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

            Dim labels As String() = Nothing
            Dim statuses As eCapePhaseStatus() = Nothing

            ms.GetPresentPhases(labels, statuses)

            Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

            Dim res As Object = Nothing

            Dim i, j As Integer
            i = 0
            For Each l As String In labels
                If statuses(i) = eCapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    fractions(0, i) = labels(i)
                    ms.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                    fractions(1, i) = res(0)
                    ms.GetSinglePhaseProp("fraction", labels(i), "Mole", res)
                    For j = 0 To compounds.Length - 1
                        fractions(2 + j, i) = res(j)
                    Next
                End If
                i += 1
            Next

            fractions(compounds.Length + 2, 0) = ms.Phases(0).Properties.pressure.GetValueOrDefault

            ms.Dispose()
            ms = Nothing

            Return fractions

        End Function
        ''' <summary>
        ''' Returns a list of the thermodynamic models.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(36)> Public Function GetModelList() As ArrayList

            Dim modellist As New ArrayList

            modellist.Add("Peng-Robinson")
            modellist.Add("Peng-Robinson (1978)")
            modellist.Add("Peng-Robinson-Stryjek-Vera 2 (Van Laar)")
            modellist.Add("Peng-Robinson-Stryjek-Vera 2 (Margules)")
            modellist.Add("Soave-Redlich-Kwong")
            modellist.Add("Lee-Kesler-Plöcker")
            modellist.Add("PC-SAFT")
            modellist.Add("NRTL")
            modellist.Add("UNIQUAC")

            Return modellist

        End Function

        ''' <summary>
        ''' "Returns the interaction parameters stored in DWSIM's database for a given binary/model combination.
        ''' </summary>
        ''' <param name="Model">Thermodynamic Model (use 'GetModelList' to get a list of available models).</param>
        ''' <param name="Compound1">The name of the first compound.</param>
        ''' <param name="Compound2">The name of the second compound.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(37)> Public Function GetInteractionParameterSet(ByVal Model As String, ByVal Compound1 As String, ByVal Compound2 As String) As InteractionParameter

            Dim pars As New Dictionary(Of String, Object)

            Select Case Model
                Case "Peng-Robinson"
                    Dim pp As New PengRobinsonPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson 1978"
                    Dim pp As New PengRobinson1978PropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Van Laar)"
                    Dim pp As New PRSV2VLPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij},
                                                                           {"kji", pp.m_pr.InteractionParameters(Compound1)(Compound2).kji}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij},
                                                                                   {"kji", pp.m_pr.InteractionParameters(Compound2)(Compound1).kji}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kji},
                                                                           {"kji", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Margules)"
                    Dim pp As New PRSV2PropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij},
                                                                           {"kji", pp.m_pr.InteractionParameters(Compound1)(Compound2).kji}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij},
                                                                                   {"kji", pp.m_pr.InteractionParameters(Compound2)(Compound1).kji}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kji},
                                                                           {"kji", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Soave-Redlich-Kwong"
                    Dim pp As New SRKPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Lee-Kesler-Plöcker"
                    Dim pp As New LKPPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound1)(Compound2).kij}}
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"kij", pp.m_pr.InteractionParameters(Compound2)(Compound1).kij}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "NRTL"
                    Dim pp As New NRTLPropertyPackage(True)
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound1)(Compound2).A12},
                                                                           {"A21", pp.m_uni.InteractionParameters(Compound1)(Compound2).A21},
                                                                           {"alpha", pp.m_uni.InteractionParameters(Compound1)(Compound2).alpha12}}
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound2)(Compound1).A12},
                                                                                   {"A21", pp.m_uni.InteractionParameters(Compound2)(Compound1).A21},
                                                                                   {"alpha", pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound2)(Compound1).A21},
                                                        {"A21", pp.m_uni.InteractionParameters(Compound2)(Compound1).A12},
                                                        {"alpha", pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "UNIQUAC"
                    Dim pp As New UNIQUACPropertyPackage(True)
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound1)(Compound2).A12},
                                                                           {"A21", pp.m_uni.InteractionParameters(Compound1)(Compound2).A21}}
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound2)(Compound1).A12},
                                                                                   {"A21", pp.m_uni.InteractionParameters(Compound2)(Compound1).A21}}
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            pars = New Dictionary(Of String, Object) From {{"A12", pp.m_uni.InteractionParameters(Compound2)(Compound1).A21},
                                                                          {"A21", pp.m_uni.InteractionParameters(Compound2)(Compound1).A12}}
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
            End Select

            Return New InteractionParameter() With {.Comp1 = Compound1, .Comp2 = Compound2, .Model = Model, .Parameters = pars}

        End Function

        Public Enum FlashCalculationType
            PressureTemperature = 0
            PressureEnthalpy = 1
            PressureEntropy = 2
            TemperatureEnthalpy = 3
            TemperatureEntropy = 4
            PressureVaporFraction = 5
            TemperatureVaporFraction = 6
            PressureSolidFraction = 7
            TemperatureSolidFraction = 8
        End Enum

        ''' <summary>
        ''' Calculates Phase Equilibria for a given mixture at specified conditions.
        ''' </summary>
        ''' <param name="flashtype">The type of the flash algorithm to calculate</param>
        ''' <param name="flashalg">[DEPRECATED] The flash algorithm to use (set to -1 to use the current instance)</param>
        ''' <param name="val1">Value of the first flash state specification (P in Pa, T in K, H in kJ/kg, S in kJ/[kg.K], VAP/SF in mole fraction from 0 to 1)</param>
        ''' <param name="val2">Value of the second flash state specification (P in Pa, T in K, H in kJ/kg, S in kJ/[kg.K], VAP/SF in mole fraction from 0 to 1)</param>
        ''' <param name="pp">Property Package instance</param>
        ''' <param name="compounds">Compound names</param>
        ''' <param name="molefractions">Vector of mixture mole fractions</param>
        ''' <param name="initialKval">Vector containing initial estimates for the K-values (set to 'Nothing' (VB) or 'null' (C#) if none).</param>
        ''' <param name="initialestimate">Initial estimate for Temperature (K) or Pressure (Pa), whichever will be calculated</param>
        ''' <param name="ip1">Interaction Parameters Set #1.</param>
        ''' <param name="ip2">Interaction Parameters Set #2.</param>
        ''' <param name="ip3">Interaction Parameters Set #3.</param>
        ''' <param name="ip4">Interaction Parameters Set #4.</param>
        ''' <returns>A FlashCalculationResult instance with the results of the calculations</returns>
        ''' <remarks>This function must be used instead of the older type-specific flash functions.
        ''' Check if the 'ResultException' property of the result object is nothing/null before proceeding.</remarks>
        <Runtime.InteropServices.DispId(39)> Public Function CalcEquilibrium(flashtype As FlashCalculationType,
                                                                                           flashalg As Integer,
                                                                                           val1 As Double, val2 As Double,
                                                                                           pp As PropertyPackage,
                                                                                           compounds As String(),
                                                                                           molefractions As Double(),
                                                                                           initialKval As Double(),
                                                                                           initialestimate As Double,
                                                                                           Optional ByVal ip1 As Object = Nothing,
                                                                                           Optional ByVal ip2 As Object = Nothing,
                                                                                           Optional ByVal ip3 As Object = Nothing,
                                                                                           Optional ByVal ip4 As Object = Nothing) As FlashCalculationResult

            TransferCompounds(pp)

            SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4)

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)

            ms._pp = pp
            pp.SetMaterial(ms)

            Dim results As New FlashCalculationResult

            Select Case flashtype
                Case FlashCalculationType.PressureTemperature
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.PressureEnthalpy
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.H, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.PressureEntropy
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.S, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.PressureVaporFraction
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.VAP, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.TemperatureVaporFraction
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.T, FlashSpec.VAP, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.TemperatureEnthalpy
                    results.ResultException = New NotImplementedException("Flash type not yet supported.")
                Case FlashCalculationType.TemperatureEntropy
                    results.ResultException = New NotImplementedException("Flash type not yet supported.")
                Case FlashCalculationType.PressureSolidFraction
                    results = pp.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.SF, val1, val2, pp, molefractions, initialKval, initialestimate)
                Case FlashCalculationType.TemperatureSolidFraction
                    results.ResultException = New NotImplementedException("Flash type not yet supported.")
                Case Else
                    results.ResultException = New NotImplementedException("Flash type not yet supported.")
            End Select

            Return results

        End Function

        ''' <summary>
        ''' Setups a Material Stream with the specified compounds and associated it with the property package.
        ''' </summary>
        ''' <param name="proppack">Property Package instance</param>
        ''' <param name="compounds">Compounds to add</param>
        ''' <param name="molefractions">Compound mole fractions</param>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(42)> Public Sub SetupPropertyPackage(
            ByVal proppack As PropertyPackage,
            ByVal compounds As String(),
            ByVal molefractions As Double())

            Dim pp As PropertyPackage

            pp = proppack
            TransferCompounds(pp)

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
            Next

            ms.SetOverallComposition(molefractions)

            ms._pp = pp
            pp.SetMaterial(ms)

        End Sub

        ''' <summary>
        ''' Setups a Material Stream with the specified compounds.
        ''' </summary>
        ''' <param name="compounds">Compounds to add</param>
        ''' <param name="molefractions">Compound mole fractions</param>
        ''' <remarks></remarks>
        <Runtime.InteropServices.DispId(41)> Public Function CreateMaterialStream(
            ByVal compounds As String(),
            ByVal molefractions As Double()) As Streams.MaterialStream

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = _availablecomps(c)
                Next
            Next

            ms.SetOverallComposition(molefractions)

            Return ms

        End Function

        ''' <summary>
        ''' Links/Associates a Material Stream with a Property Package and vice-versa.
        ''' </summary>
        ''' <param name="stream">Material Stream instance</param>
        ''' <param name="pp">Property Package instance</param>
        Public Sub Link(stream As MaterialStream, pp As PropertyPackage)
            stream.PropertyPackage = pp
            pp.CurrentMaterialStream = stream
        End Sub

    End Class

End Namespace
