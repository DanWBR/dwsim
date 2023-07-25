'    DWSIM Excel Interface Shared Methods
'    Copyright 2011-2015 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
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
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

'    Using ExcelDNA library
'    Copyright (C) 2005-2011 Govert van Drimmelen

Imports ExcelDna.Integration
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.ExtensionMethods
Imports System.Reflection
Imports System.IO

Namespace ExcelAddIn

    Public Class ExcelIntegration

#Region "Information Procedures"

        <ExcelFunction(Description:="Returns a list of compound constants and their values from the corresponding loaded database.", HelpTopic:="ExcelAddInHelp.chm!1")>
        Public Shared Function GetCompoundConstants(<ExcelArgument("Compound name.")> ByVal compound As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {compound})

            Try

                Settings.ExcelMode = True

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim pp As New RaoultPropertyPackage(True)

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    phase.Compounds.Add(compound, New BaseClasses.Compound(compound, ""))
                    phase.Compounds(compound).ConstantProperties = pp._availablecomps(compound)
                Next

                Dim tmpcomp As ConstantProperties = pp._availablecomps(compound)

                Dim props As FieldInfo() = tmpcomp.GetType.GetFields()

                Dim results(props.Length - 1, 1) As Object

                Dim i = 0
                For Each prop As FieldInfo In props
                    results(i, 0) = prop.Name
                    results(i, 1) = tmpcomp.GetType.GetField(prop.Name).GetValue(tmpcomp)
                    i += 1
                Next

                pp.Dispose()
                pp = Nothing

                ms.Dispose()
                ms = Nothing

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results)

                Return results

            Catch ex As Exception

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Returns a single property value for a compound. For a constant property, set T = 0 and P = 0. For a T-dep property, set P = 0. For a P-dep property, set T = 0.", HelpTopic:="ExcelAddInHelp.chm!2")>
        Public Shared Function GetCompoundProp(
            <ExcelArgument("Compound name.")> ByVal compound As String,
            <ExcelArgument("Property identifier.")> ByVal prop As String,
            <ExcelArgument("Temperature in K, if needed. Set as zero for a constant or P-dep property.")> ByVal temperature As Double,
            <ExcelArgument("Pressure in Pa, if needed. Set as zero for a constant or T-dep property.")> ByVal pressure As Double) As Object

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {compound, prop, temperature, pressure})

            Settings.ExcelMode = True

            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

            Try

                Dim pp As New RaoultPropertyPackage(True)

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    phase.Compounds.Add(compound, New BaseClasses.Compound(compound, ""))
                    phase.Compounds(compound).ConstantProperties = pp._availablecomps(compound)
                Next

                Dim tmpcomp As ConstantProperties = pp._availablecomps(compound)
                pp._selectedcomps.Add(compound, tmpcomp)
                pp._availablecomps.Remove(compound)

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                Dim results As Object = Nothing

                If pressure <> 0.0# And temperature = 0.0# Then
                    ms.GetPDependentProperty(New Object() {prop}, pressure, New Object() {compound}, results)
                ElseIf pressure = 0.0# And temperature <> 0.0# Then
                    ms.GetTDependentProperty(New Object() {prop}, temperature, New Object() {compound}, results)
                Else
                    results = ms.GetCompoundConstant(New Object() {prop}, New Object() {compound})
                End If

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results(0))

                Return results(0)

                pp.Dispose()
                pp = Nothing

                ms.Dispose()
                ms = Nothing

            Catch ex As Exception

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return ex.Message
                    Case 1
                        Return ex.ToString
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return "Error"
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Returns a list of the available single compound properties.", HelpTopic:="ExcelAddInHelp.chm!3")>
        Public Shared Function GetCompoundPropList() As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {})

            Settings.ExcelMode = True

            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

            Dim pp As New RaoultPropertyPackage(True)

            Dim props As New ArrayList

            props.AddRange(pp.GetConstPropList())
            props.AddRange(pp.GetPDependentPropList())
            props.AddRange(pp.GetTDependentPropList())

            pp.Dispose()
            pp = Nothing

            Dim values As Object() = props.ToArray

            Dim results2(values.Length - 1, 0) As Object
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i, 0) = values(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

            Return results2

        End Function

        <ExcelFunction(Description:="Returns a list of the available Property Packages.", HelpTopic:="ExcelAddInHelp.chm!4")>
        Public Shared Function GetPropPackList() As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {})

            Settings.ExcelMode = True

            Dim ppm As New CAPEOPENManager()

            Dim values As Object() = ppm.GetPropertyPackageList()

            Dim results2(values.Length - 1, 0) As Object
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i, 0) = values(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

            Return results2

            ppm.Dispose()
            ppm = Nothing

        End Function

        <ExcelFunction(Description:="Returns a list of the thermodynamic models.", HelpTopic:="ExcelAddInHelp.chm!5")>
        Public Shared Function GetModelList() As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {})

            Dim modellist As New ArrayList

            modellist.Add("Peng-Robinson")
            modellist.Add("Peng-Robinson-Stryjek-Vera 2 (Van Laar)")
            modellist.Add("Peng-Robinson-Stryjek-Vera 2 (Margules)")
            modellist.Add("Soave-Redlich-Kwong")
            modellist.Add("Lee-Kesler-Plöcker")
            modellist.Add("PC-SAFT")
            modellist.Add("NRTL")
            modellist.Add("UNIQUAC")

            Dim list(modellist.Count - 1, 0) As Object

            For i As Integer = 0 To modellist.Count - 1
                list(i, 0) = modellist(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), list)

            Return list

        End Function

        <ExcelFunction(Description:="Returns the interaction parameters stored in DWSIM's database for a given binary/model combination.", HelpTopic:="ExcelAddInHelp.chm!6")>
        Public Shared Function GetInteractionParameterSet(
            <ExcelArgument("Thermodynamic Model (use 'GetModelList' to get a list of available models).")> ByVal Model As String,
            <ExcelArgument("The name of the first compound.")> ByVal Compound1 As String,
            <ExcelArgument("The name of the second compound.")> ByVal Compound2 As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {Model, Compound1, Compound2})

            Settings.ExcelMode = True

            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

            Dim ipdata(1, 8) As Object

            ipdata(0, 0) = "ID1"
            ipdata(0, 1) = "ID2"
            ipdata(0, 2) = "kij/A12"
            ipdata(0, 3) = "kji/A21"
            ipdata(0, 4) = "B12"
            ipdata(0, 5) = "B21"
            ipdata(0, 6) = "C12"
            ipdata(0, 7) = "C21"
            ipdata(0, 8) = "alpha12"

            ipdata(1, 0) = Compound1
            ipdata(1, 1) = Compound2

            Select Case Model
                Case "Peng-Robinson"
                    Dim pp As New PengRobinsonPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Van Laar)"
                    Dim pp As New PRSV2VLPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kji
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                                    ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Margules)"
                    Dim pp As New PRSV2PropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kji
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                                    ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Soave-Redlich-Kwong"
                    Dim pp As New SRKPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Lee-Kesler-Plöcker"
                    Dim pp As New LKPPropertyPackage(True)
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "NRTL"
                    Dim pp As New NRTLPropertyPackage(True)
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A12
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A21
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B12
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B21
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C12
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C21
                            ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound1)(Compound2).alpha12
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                                    ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                                    ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                                    ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                                    ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                                    ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                                    ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                            ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "UNIQUAC"
                    Dim pp As New UNIQUACPropertyPackage(True)
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A12
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A21
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B12
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B21
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C12
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C21
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                                    ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                                    ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                                    ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                                    ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                                    ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
            End Select

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), ipdata)

            Return ipdata

        End Function

        Public Shared Function GetInteractionParameterSet(ByVal proppack As PropertyPackage, Model As String,
            ByVal Compound1 As String, ByVal Compound2 As String) As Object(,)

            Dim ipdata(1, 8) As Object

            ipdata(0, 0) = "ID1"
            ipdata(0, 1) = "ID2"
            ipdata(0, 2) = "kij/A12"
            ipdata(0, 3) = "kji/A21"
            ipdata(0, 4) = "B12"
            ipdata(0, 5) = "B21"
            ipdata(0, 6) = "C12"
            ipdata(0, 7) = "C21"
            ipdata(0, 8) = "alpha12"

            ipdata(1, 0) = Compound1
            ipdata(1, 1) = Compound2

            Select Case Model
                Case "Peng-Robinson"
                    Dim pp As PengRobinsonPropertyPackage = proppack
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Van Laar)"
                    Dim pp As PRSV2VLPropertyPackage = proppack
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kji
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                                    ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Peng-Robinson-Stryjek-Vera 2 (Margules)"
                    Dim pp As PRSV2PropertyPackage = proppack
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kji
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                                    ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kji
                            ipdata(1, 3) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Soave-Redlich-Kwong"
                    Dim pp As SRKPropertyPackage = proppack
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "Lee-Kesler-Plöcker"
                    Dim pp As LKPPropertyPackage = proppack
                    If pp.m_pr.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_pr.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound1)(Compound2).kij
                        Else
                            If pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                                End If
                            End If
                        End If
                    ElseIf pp.m_pr.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_pr.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_pr.InteractionParameters(Compound2)(Compound1).kij
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "NRTL"
                    Dim pp As NRTLPropertyPackage = proppack
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A12
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A21
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B12
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B21
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C12
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C21
                            ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound1)(Compound2).alpha12
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                                    ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                                    ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                                    ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                                    ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                                    ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                                    ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                            ipdata(1, 8) = pp.m_uni.InteractionParameters(Compound2)(Compound1).alpha12
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
                Case "UNIQUAC"
                    Dim pp As UNIQUACPropertyPackage = proppack
                    If pp.m_uni.InteractionParameters.ContainsKey(Compound1) Then
                        If pp.m_uni.InteractionParameters(Compound1).ContainsKey(Compound2) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A12
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound1)(Compound2).A21
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B12
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound1)(Compound2).B21
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C12
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound1)(Compound2).C21
                        Else
                            If pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                                If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                                    ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                                    ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                                    ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                                    ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                                    ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                                    ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                                End If
                            End If
                        End If
                    ElseIf pp.m_uni.InteractionParameters.ContainsKey(Compound2) Then
                        If pp.m_uni.InteractionParameters(Compound2).ContainsKey(Compound1) Then
                            ipdata(1, 2) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A21
                            ipdata(1, 3) = pp.m_uni.InteractionParameters(Compound2)(Compound1).A12
                            ipdata(1, 4) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B21
                            ipdata(1, 5) = pp.m_uni.InteractionParameters(Compound2)(Compound1).B12
                            ipdata(1, 6) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C21
                            ipdata(1, 7) = pp.m_uni.InteractionParameters(Compound2)(Compound1).C12
                        End If
                    End If
                    pp.Dispose()
                    pp = Nothing
            End Select

            Return ipdata

        End Function

        <ExcelFunction(Description:="Returns the interaction parameters stored in user databases for a given binary/model combination.", HelpTopic:="ExcelAddInHelp.chm!7")>
        Public Shared Function GetUserInteractionParameterSet(
            <ExcelArgument("Thermodynamic Model (use 'GetModelList' to get a list of available models).")> ByVal Model As String,
            <ExcelArgument("The name of the first compound.")> ByVal Compound1 As String,
            <ExcelArgument("The name of the second compound.")> ByVal Compound2 As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {Model, Compound1, Compound2})

            Settings.ExcelMode = True

            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

            Dim iplist As New List(Of BaseClasses.InteractionParameter) '= UserIPDB.GetStoredIPsets(Compound1, Compound2, Model)

            Dim ipdata(iplist.Count, 9) As Object

            ipdata(0, 0) = "ID1"
            ipdata(0, 1) = "ID2"
            ipdata(0, 2) = "kij/A12"
            ipdata(0, 3) = "kji/A21"
            ipdata(0, 4) = "B12"
            ipdata(0, 5) = "B21"
            ipdata(0, 6) = "C12"
            ipdata(0, 7) = "C21"
            ipdata(0, 8) = "alpha12"
            ipdata(0, 9) = "RegressionFile"

            Dim i As Integer = 1
            For Each ip As InteractionParameter In iplist
                ipdata(i, 0) = Compound1
                ipdata(i, 1) = Compound2
                For Each p As KeyValuePair(Of String, Object) In ip.Parameters
                    Select Case p.Key
                        Case "kij"
                            ipdata(i, 2) = p.Value
                        Case "kji"
                            ipdata(i, 3) = p.Value
                        Case "A12"
                            ipdata(i, 2) = p.Value
                        Case "A21"
                            ipdata(i, 3) = p.Value
                        Case "B12"
                            ipdata(i, 4) = p.Value
                        Case "B21"
                            ipdata(i, 5) = p.Value
                        Case "C12"
                            ipdata(i, 6) = p.Value
                        Case "C21"
                            ipdata(i, 7) = p.Value
                        Case "alpha12"
                            ipdata(i, 8) = p.Value
                    End Select
                Next
                ipdata(i, 9) = ip.RegressionFile
                i += 1
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), ipdata)

            Return ipdata

        End Function

        <ExcelFunction(Description:="Returns a list of the available properties.", HelpTopic:="ExcelAddInHelp.chm!8")>
        Public Shared Function GetPropList(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack})

            Settings.ExcelMode = True

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

            ppm.Dispose()
            ppm = Nothing

            Dim values As Object() = pp.GetSinglePhasePropList()

            Dim results2(values.Length - 1, 0) As Object
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i, 0) = values(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

            Return results2

            pp.Dispose()
            pp = Nothing

        End Function

        <ExcelFunction(Description:="Returns a list of the available phases.", HelpTopic:="ExcelAddInHelp.chm!9")>
        Public Shared Function GetPhaseList(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack})

            Settings.ExcelMode = True

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

            ppm.Dispose()
            ppm = Nothing

            Dim values As Object() = pp.GetPhaseList()

            Dim results2(values.Length - 1, 0) As Object
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i, 0) = values(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

            Return results2

            pp.Dispose()
            pp = Nothing

        End Function

        <ExcelFunction(Description:="Returns a list of the available compounds.", HelpTopic:="ExcelAddInHelp.chm!10")>
        Public Shared Function GetCompoundList(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack})

            Settings.ExcelMode = True

            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

            Dim ppm As New CAPEOPENManager()

            Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

            ppm.Dispose()
            ppm = Nothing

            Dim comps As New ArrayList

            For Each c As ConstantProperties In pp._availablecomps.Values
                comps.Add(c.Name)
            Next

            pp.Dispose()
            pp = Nothing

            Dim values As Object() = comps.ToArray

            Dim results2(values.Length - 1, 0) As Object
            Dim i As Integer

            For i = 0 To values.Length - 1
                results2(i, 0) = values(i)
            Next

            WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

            Return results2

        End Function

#End Region

#Region "Property Calculation Functions"

        <ExcelFunction(Description:="Calculates properties using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!11")>
        Public Shared Function CalcProp(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("The property to calculate.")> ByVal prop As String,
        <ExcelArgument("The returning basis of the properties: Mole, Mass or UNDEFINED.")> ByVal basis As String,
        <ExcelArgument("The name of the phase to calculate properties from.")> ByVal phaselabel As String,
        <ExcelArgument("The list of compounds to include.")> ByVal compounds As Object(),
        <ExcelArgument("Temperature in K.")> ByVal temperature As Double,
        <ExcelArgument("Pressure in Pa.")> ByVal pressure As Double,
        <ExcelArgument("*Normalized* mole fractions of the compounds in the mixture.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, prop, basis, phaselabel, compounds, temperature, pressure, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    pp._selectedcomps.Add(c, tmpcomp)
                    pp._availablecomps.Remove(c)
                Next

                Dim dwp As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
                For Each pi As PropertyPackages.PhaseInfo In pp.PhaseMappings.Values
                    If pi.PhaseLabel = phaselabel Then dwp = pi.DWPhaseID
                Next

                ms.SetPhaseComposition(molefractions, dwp)
                ms.CalcPhaseMassComposition(dwp)
                ms.Phases(0).Properties.temperature = temperature
                ms.Phases(0).Properties.pressure = pressure

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                If prop.ToLower <> "molecularweight" Then
                    pp.CalcSinglePhaseProp(New Object() {prop}, phaselabel)
                End If

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
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

                Dim results2(values.Length - 1, 0) As Object

                For i = 0 To values.Length - 1
                    results2(i, 0) = values(i)
                Next

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

                Return results2

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try


        End Function

        <ExcelFunction(Description:="Calculates properties using the selected Property Package with custom data.", HelpTopic:="ExcelAddInHelp.chm!11")>
        Public Shared Function CalcProp2(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("The property to calculate.")> ByVal prop As String,
        <ExcelArgument("The returning basis of the properties: Mole, Mass or UNDEFINED.")> ByVal basis As String,
        <ExcelArgument("The name of the phase to calculate properties from.")> ByVal phaselabel As String,
        <ExcelArgument("The list of compounds to include.")> ByVal compounds As Object(),
        <ExcelArgument("Temperature in K.")> ByVal temperature As Double,
        <ExcelArgument("Pressure in Pa.")> ByVal pressure As Double,
        <ExcelArgument("*Normalized* mole fractions of the compounds in the mixture.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, prop, basis, phaselabel, compounds, temperature, pressure, molefractions, ppdata})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    pp._selectedcomps.Add(c, tmpcomp)
                    pp._availablecomps.Remove(c)
                Next

                Dim dwp As PropertyPackages.Phase = PropertyPackages.Phase.Mixture
                For Each pi As PropertyPackages.PhaseInfo In pp.PhaseMappings.Values
                    If pi.PhaseLabel = phaselabel Then dwp = pi.DWPhaseID
                Next

                ms.SetPhaseComposition(molefractions, dwp)
                ms.CalcPhaseMassComposition(dwp)
                ms.Phases(0).Properties.temperature = temperature
                ms.Phases(0).Properties.pressure = pressure

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                If prop.ToLower <> "molecularweight" Then
                    pp.CalcSinglePhaseProp(New Object() {prop}, phaselabel)
                End If

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
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

                Dim results2(values.Length - 1, 0) As Object

                For i = 0 To values.Length - 1
                    results2(i, 0) = values(i)
                Next

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), results2)

                Return results2

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try


        End Function

        <ExcelFunction(Description:="Returns the default units for a given property.", HelpTopic:="ExcelAddInHelp.chm!11")>
        Public Shared Function GetPropUnits(
        <ExcelArgument("The property to get units for.")> ByVal prop As String,
        <ExcelArgument("The returning basis of the propertiy: Mole, Mass or UNDEFINED.")> ByVal basis As String) As String

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {prop, basis})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ms As New Streams.MaterialStream("", "")

                Dim units = ms.GetSinglePhasePropDefaultUnits(prop, basis)

                ms.Dispose()
                ms = Nothing

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), units)

                Return units

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return ex.Message
                    Case 1
                        Return ex.ToString
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return "Ërror"
                End Select

            End Try


        End Function


#End Region

#Region "Flash Calculation Routines, v1"

        <ExcelFunction(Description:="Calculates a Pressure / Temperature Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!12")>
        Public Shared Function PTFlash(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, flashalg, P, T, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.temperature = T
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Case 10
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE
                    Case 11
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimizationMulti
                End Select

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Enthalpy Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!13")>
        Public Shared Function PHFlash(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
         <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Enthalpy in kJ/kg.")> ByVal H As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            Return PHFlash2(proppack, flashalg, P, H, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, 0.0#)

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Entropy Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!14")>
        Public Shared Function PSFlash(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Entropy in kJ/[kg.K].")> ByVal S As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            Return PSFlash2(proppack, flashalg, P, S, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, 0.0#)

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Vapor Fraction Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!15")>
        Public Shared Function PVFFlash(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            Return PVFFlash2(proppack, flashalg, P, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, 0.0#)

        End Function

        <ExcelFunction(Description:="Calculates a Temperature / Vapor Fraction Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!16")>
        Public Shared Function TVFFlash(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            Return TVFFlash2(proppack, flashalg, T, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, 0.0#)

        End Function

#End Region

#Region "Flash Calculation Routines, v2 (accept an initial estimate)"

        <ExcelFunction(Description:="Calculates a Pressure / Enthalpy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!17")>
        Public Shared Function PHFlash2(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Enthalpy in kJ/kg.")> ByVal H As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, flashalg, P, H, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.enthalpy = H
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Case 10
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE
                    Case 11
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimizationMulti
                End Select

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Entropy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!18")>
        Public Shared Function PSFlash2(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Entropy in kJ/[kg.K].")> ByVal S As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, flashalg, P, S, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.entropy = S
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Case 10
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE
                    Case 11
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimizationMulti
                End Select

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!19")>
        Public Shared Function PVFFlash2(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, flashalg, P, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Case 10
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE
                    Case 11
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimizationMulti
                End Select

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Temperature / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the pressure search.", HelpTopic:="ExcelAddInHelp.chm!20")>
        Public Shared Function TVFFlash2(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("[DEPRECATED] Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE, 10 = Nested Loops SVLLE, 11 = Gibbs Minimization SVLLE)")> ByVal flashalg As Integer,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for pressure search, in Pa.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, flashalg, T, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.temperature = T

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Case 10
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE
                    Case 11
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimizationMulti
                End Select

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.pressure = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

#End Region

#Region "Flash Calculation Routines, v3 (accept custom property package data)"

        <ExcelFunction(Description:="Calculates a Pressure / Temperature Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!12")>
        Public Shared Function PTFlash3(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, T, compounds, molefractions, ppdata})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.temperature = T
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Enthalpy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!17")>
        Public Shared Function PHFlash3(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Enthalpy in kJ/kg.")> ByVal H As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, H, compounds, molefractions, ppdata, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.enthalpy = H
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Entropy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!18")>
        Public Shared Function PSFlash3(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Entropy in kJ/[kg.K].")> ByVal S As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, S, compounds, molefractions, ppdata, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.entropy = S
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!19")>
        Public Shared Function PVFFlash3(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, VF, compounds, molefractions, ppdata, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Temperature / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the pressure search.", HelpTopic:="ExcelAddInHelp.chm!20")>
        Public Shared Function TVFFlash3(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Serialized Property Package data.")> ByVal ppdata As String,
        <ExcelArgument("Initial estimate for pressure search, in Pa.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, T, VF, compounds, molefractions, ppdata, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.temperature = T

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.LoadData(Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of XElement))(ppdata))

                ms.Phases(0).Properties.pressure = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

#End Region

#Region "Flash Calculation Routines, v4 (automatic flash)"

        <ExcelFunction(Description:="Calculates a Pressure / Temperature Flash using the selected Property Package.", HelpTopic:="ExcelAddInHelp.chm!12")>
        Public Shared Function PTFlash4(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, T, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.temperature = T
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Enthalpy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!17")>
        Public Shared Function PHFlash4(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Enthalpy in kJ/kg.")> ByVal H As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, H, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.enthalpy = H
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Entropy Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!18")>
        Public Shared Function PSFlash4(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mass Entropy in kJ/[kg.K].")> ByVal S As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, S, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.entropy = S
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Pressure / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the temperature search.", HelpTopic:="ExcelAddInHelp.chm!19")>
        Public Shared Function PVFFlash4(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Pressure in Pa.")> ByVal P As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for temperature search, in K.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, P, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.temperature = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

        <ExcelFunction(Description:="Calculates a Temperature / Vapor Fraction Flash using the selected Property Package. Accepts an initial estimate for the pressure search.", HelpTopic:="ExcelAddInHelp.chm!20")>
        Public Shared Function TVFFlash4(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("Temperature in K.")> ByVal T As Double,
        <ExcelArgument("Mixture Mole Vapor Fraction.")> ByVal VF As Double,
        <ExcelArgument("Compound names.")> ByVal compounds As Object(),
        <ExcelArgument("Compound mole fractions.")> ByVal molefractions As Double(),
        <ExcelArgument("Interaction Parameters Set #1.")> ByVal ip1 As Object,
        <ExcelArgument("Interaction Parameters Set #2.")> ByVal ip2 As Object,
        <ExcelArgument("Interaction Parameters Set #3.")> ByVal ip3 As Object,
        <ExcelArgument("Interaction Parameters Set #4.")> ByVal ip4 As Object,
        <ExcelArgument("Interaction Parameters Set #5.")> ByVal ip5 As Object,
        <ExcelArgument("Interaction Parameters Set #6.")> ByVal ip6 As Object,
        <ExcelArgument("Interaction Parameters Set #7.")> ByVal ip7 As Object,
        <ExcelArgument("Interaction Parameters Set #8.")> ByVal ip8 As Object,
        <ExcelArgument("Initial estimate for pressure search, in Pa.")> ByVal InitialEstimate As Double) As Object(,)

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, T, VF, compounds, molefractions, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, InitialEstimate})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = ppm.GetPropertyPackage(proppack)
                SetIP(proppack, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.temperature = T

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSVLLE

                If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
                    Try
                        pp.FlashAlgorithm.FlashSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
                    Catch ex As Exception
                    End Try
                End If

                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2
                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next
                pp.FlashAlgorithm.FlashSettings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps.ToArrayString

                ms.Phases(0).Properties.pressure = InitialEstimate

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), fractions)

                Return fractions

            Catch ex As Exception

                WriteErrorMessage(Reflection.MethodBase.GetCurrentMethod(), ex)

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try

        End Function

#End Region

#Region "Helper Procedures"

        Public Shared Sub SetIP(ByVal proppack As String, ByVal pp As PropertyPackage, ByVal compounds As Object, ByVal ip1 As Object, ByVal ip2 As Object,
                               ByVal ip3 As Object, ByVal ip4 As Object, ByVal ip5 As Object, ByVal ip6 As Object,
                               ByVal ip7 As Object, ByVal ip8 As Object)

            Dim i, j As Integer

            Select Case proppack
                Case "Peng-Robinson (PR)"
                    With CType(pp, PengRobinsonPropertyPackage).m_pr.InteractionParameters
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1.ToLower) Then .Add(c1.ToLower, New Dictionary(Of String, PRSV2_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1.ToLower).ContainsKey(c2.ToLower) Then .Item(c1.ToLower).Add(c2.ToLower, New PRSV2_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1.ToLower) Then .Add(c1.ToLower, New Dictionary(Of String, PRSV2_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1.ToLower).ContainsKey(c2.ToLower) Then .Item(c1.ToLower).Add(c2.ToLower, New PRSV2_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip2 Is ExcelMissing And Not ip2 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, NRTL_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New NRTL_IPData())
                                    With .Item(c1).Item(c2)
                                        .A12 = ip2(i, j)
                                        .A21 = ip3(i, j)
                                        .alpha12 = ip4(i, j)
                                        If Not TypeOf ip5 Is ExcelMissing And Not ip5 Is Nothing Then
                                            .B12 = ip5(i, j)
                                            .B21 = ip6(i, j)
                                            .C12 = ip7(i, j)
                                            .C21 = ip8(i, j)
                                        End If
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "UNIQUAC"
                    With CType(pp, UNIQUACPropertyPackage).m_pr.InteractionParameters
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip2 Is ExcelMissing And Not ip2 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, UNIQUAC_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New UNIQUAC_IPData())
                                    With .Item(c1).Item(c2)
                                        .A12 = ip2(i, j)
                                        .A21 = ip3(i, j)
                                        If Not TypeOf ip5 Is ExcelMissing And Not ip5 Is Nothing Then
                                            .B12 = ip4(i, j)
                                            .B21 = ip5(i, j)
                                            .C12 = ip6(i, j)
                                            .C21 = ip7(i, j)
                                        End If
                                    End With
                                    j += 1
                                Next
                                i += 1
                            Next
                        End If
                    End With
                Case "Modified UNIFAC (Dortmund)"
                    With CType(pp, MODFACPropertyPackage).m_pr.InteractionParameters
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, PR_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New PR_IPData())
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
                        If Not TypeOf ip1 Is ExcelMissing And Not ip1 Is Nothing Then
                            .Clear()
                            i = 0
                            For Each c1 As String In compounds
                                If Not .ContainsKey(c1) Then .Add(c1, New Dictionary(Of String, LKP_IPData))
                                j = 0
                                For Each c2 As String In compounds
                                    If Not .Item(c1).ContainsKey(c2) Then .Item(c1).Add(c2, New LKP_IPData())
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

        Public Shared Sub AddCompounds(ByVal proppack As PropertyPackage, ByVal compounds As Object())

            Dim ms As New Streams.MaterialStream("", "")

            For Each phase As BaseClasses.Phase In ms.Phases.Values
                For Each c As String In compounds
                    phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                    phase.Compounds(c).ConstantProperties = proppack._availablecomps(c)
                Next
            Next

            For Each c As String In compounds
                Dim tmpcomp As ConstantProperties = proppack._availablecomps(c)
                If Not proppack._selectedcomps.ContainsKey(c) Then proppack._selectedcomps.Add(c, tmpcomp)
            Next

            ms._pp = proppack
            proppack.SetMaterial(ms)

        End Sub

        <ExcelFunction(Description:="Returns base Property Package data, which can be modified and used as an input for other functions.")>
        Public Shared Function GetBasePropertyPackageData(
        <ExcelArgument("The name of the Property Package to use.")> ByVal proppack As String,
        <ExcelArgument("The list of compounds to include.")> ByVal compounds As Object()) As Object

            WriteMethodInfo(Reflection.MethodBase.GetCurrentMethod(), {proppack, compounds})

            Settings.ExcelMode = True

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage = ppm.GetPropertyPackage(proppack)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    pp._selectedcomps.Add(c, tmpcomp)
                    pp._availablecomps.Remove(c)
                Next

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                Dim data = Newtonsoft.Json.JsonConvert.SerializeObject(pp.SaveData(), Newtonsoft.Json.Formatting.Indented)

                pp.Dispose()
                pp = Nothing

                ms.Dispose()
                ms = Nothing

                WriteMethodFinishedMessage(Reflection.MethodBase.GetCurrentMethod(), data)

                Return data

            Catch ex As Exception

                Select Case GlobalSettings.Settings.ExcelErrorHandlingMode
                    Case 0
                        Return New Object(,) {{ex.Message}, {""}}
                    Case 1
                        Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}
                    Case Else
                        Application.EnableVisualStyles()
                        My.Application.ChangeCulture("en")
                        My.Application.ChangeUICulture("en")
                        Dim frmEx As New FormUnhandledException
                        frmEx.TextBox1.Text = ex.ToString
                        frmEx.ex = ex
                        frmEx.ShowDialog()
                        Return New Object(,) {{"Error"}, {""}}
                End Select

            End Try


        End Function

        Shared Sub WriteMethodInfo(method As Reflection.MethodBase, argvalues As Object())

            Console.WriteLine()

            Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] Running Excel function '" + method.Name + "' with arguments:")

            Dim args = method.GetParameters
            Dim i As Integer = 0
            For Each arg In args
                If argvalues(i) IsNot Nothing Then
                    If TypeOf argvalues(i) Is ExcelDna.Integration.ExcelMissing Then
                        Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] " + arg.Name + " (" + argvalues(i).GetType.ToString + "): NO VALUE")
                    ElseIf argvalues(i).GetType.IsArray Then
                        Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] " + arg.Name + " (" + argvalues(i).GetType.ToString + "): " + DirectCast(argvalues(i), Array).ToArrayString)
                    Else
                        Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] " + arg.Name + " (" + argvalues(i).GetType.ToString + "): " + argvalues(i).ToString)
                    End If
                End If
                i += 1
            Next

            Console.WriteLine()

        End Sub

        Shared Sub WriteMethodFinishedMessage(method As Reflection.MethodBase, result As Object)

            Console.WriteLine()

            If result.GetType.IsArray Then
                Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] Excel function '" + method.Name + "' ran successfully. Return value: " + DirectCast(result, Array).ToArrayString)
            Else
                Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] Excel function '" + method.Name + "' ran successfully. Return value: " + result.ToString)
            End If

            Console.WriteLine()

        End Sub

        Shared Sub WriteErrorMessage(method As Reflection.MethodBase, ex As Exception)

            Console.WriteLine()
            Console.WriteLine("[Thread ID: " + Threading.Thread.CurrentThread.ManagedThreadId.ToString + "][" + Date.Now.ToString + "] Excel function '" + method.Name + "' finished with errors: " + ex.ToString)
            Console.WriteLine()

        End Sub

#End Region

#Region "Fast Functions"

        Public Shared Function PTFlash(
               ByVal proppack As PropertyPackage,
               ByVal flashalg As Integer,
               ByVal P As Double,
               ByVal T As Double,
               ByVal compounds As Object(),
               ByVal molefractions As Double(),
               ByVal ip1 As Object,
               ByVal ip2 As Object,
               ByVal ip3 As Object,
               ByVal ip4 As Object,
                ByVal ip5 As Object,
                ByVal ip6 As Object,
                ByVal ip7 As Object,
                ByVal ip8 As Object) As Object(,)

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = proppack
                SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.temperature = T
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                End Select


                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TP", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 1, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

            Catch ex As Exception

                Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}

            End Try

        End Function

        Public Shared Function PHFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal H As Double,
            ByVal compounds As Object(),
            ByVal molefractions As Double(),
            ByVal ip1 As Object,
            ByVal ip2 As Object,
            ByVal ip3 As Object,
            ByVal ip4 As Object,
            ByVal ip5 As Object,
            ByVal ip6 As Object,
            ByVal ip7 As Object,
            ByVal ip8 As Object) As Object(,)

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = proppack
                SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.enthalpy = H
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                End Select

                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PH", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

            Catch ex As Exception

                Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}

            End Try

        End Function

        Public Shared Function PSFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal S As Double,
            ByVal compounds As Object(),
            ByVal molefractions As Double(),
            ByVal ip1 As Object,
            ByVal ip2 As Object,
            ByVal ip3 As Object,
            ByVal ip4 As Object,
            ByVal ip5 As Object,
            ByVal ip6 As Object,
            ByVal ip7 As Object,
            ByVal ip8 As Object) As Object(,)

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = proppack
                SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(0).Properties.entropy = S
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                End Select

                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PS", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

            Catch ex As Exception

                Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}

            End Try

        End Function

        Public Shared Function PVFFlash(
            ByVal proppack As PropertyPackage,
            ByVal flashalg As Integer,
            ByVal P As Double,
            ByVal VF As Double,
            ByVal compounds As Object(),
            ByVal molefractions As Double(),
            ByVal ip1 As Object,
            ByVal ip2 As Object,
            ByVal ip3 As Object,
            ByVal ip4 As Object,
            ByVal ip5 As Object,
            ByVal ip6 As Object,
            ByVal ip7 As Object,
            ByVal ip8 As Object) As Object(,)

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = proppack
                SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.pressure = P

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                End Select

                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "PVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

            Catch ex As Exception

                Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}

            End Try

        End Function

        Public Shared Function TVFFlash(
                ByVal proppack As PropertyPackage,
                ByVal flashalg As Integer,
                ByVal T As Double,
                ByVal VF As Double,
                ByVal compounds As Object(),
                ByVal molefractions As Double(),
                ByVal ip1 As Object,
                ByVal ip2 As Object,
                ByVal ip3 As Object,
                ByVal ip4 As Object,
                ByVal ip5 As Object,
                ByVal ip6 As Object,
                ByVal ip7 As Object,
                ByVal ip8 As Object) As Object(,)

            Try

                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)

                Dim ppm As New CAPEOPENManager()

                Dim pp As PropertyPackages.PropertyPackage

                pp = proppack
                SetIP(pp.ComponentName, pp, compounds, ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8)

                ppm.Dispose()
                ppm = Nothing

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase As BaseClasses.Phase In ms.Phases.Values
                    For Each c As String In compounds
                        phase.Compounds.Add(c, New BaseClasses.Compound(c, ""))
                        phase.Compounds(c).ConstantProperties = pp._availablecomps(c)
                    Next
                Next

                For Each c As String In compounds
                    Dim tmpcomp As ConstantProperties = pp._availablecomps(c)
                    If Not pp._selectedcomps.ContainsKey(c) Then pp._selectedcomps.Add(c, tmpcomp)
                    'pp._availablecomps.Remove(c)
                Next

                ms.SetOverallComposition(molefractions)
                ms.Phases(2).Properties.molarfraction = VF
                ms.Phases(0).Properties.temperature = T

                ms.SetPropertyPackageInstance(pp)
                pp.SetMaterial(ms)

                'Flash Algorithm (0 or 2 = Nested Loops VLE, 1 = Inside-Out VLE, 3 = Inside-Out VLLE, 4 = Gibbs VLE, 
                '5 = Gibbs VLLE, 6 = Nested-Loops VLLE, 7 = Nested-Loops SLE, 8 = Nested-Loops Immisc., 9 = Simple LLE
                Select Case flashalg
                    Case 0, 2
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                    Case 1
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonBrittInsideOut
                    Case 3
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.BostonFournierInsideOut3P
                    Case 4
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = True}
                    Case 5
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.GibbsMinimization3P With {.ForceTwoPhaseOnly = False}
                    Case 6
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops3PV3
                    Case 7
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Case 8
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsImmiscible
                    Case 9
                        pp.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                End Select

                Dim comps(compounds.Length - 1) As String
                Dim k As Integer
                For Each c As String In compounds
                    comps(k) = c
                    k += 1
                Next

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Calculator.InitComputeDevice()
                    Settings.gpu.EnableMultithreading()
                End If

                pp.CalcEquilibrium(ms, "TVF", "UNDEFINED")

                If GlobalSettings.Settings.EnableGPUProcessing Then
                    Settings.gpu.DisableMultithreading()
                    Settings.gpu.FreeAll()
                End If

                Dim labels As String() = Nothing
                Dim statuses As CapeOpen.CapePhaseStatus() = Nothing

                ms.GetPresentPhases(labels, statuses)

                Dim fractions(compounds.Length + 2, labels.Length - 1) As Object

                Dim res As Object = Nothing

                Dim i, j As Integer
                i = 0
                For Each l As String In labels
                    If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
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

            Catch ex As Exception

                Return New Object(,) {{ex.GetType.ToString}, {ex.ToString}}

            End Try

        End Function

#End Region

    End Class

End Namespace
