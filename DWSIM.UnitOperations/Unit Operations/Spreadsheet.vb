'    Excel/Spreadsheet Unit Calculation Routines 
'    Copyright 2014 Gregor Reichert, 2015 Daniel Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports GS = GemBox.Spreadsheet
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations.Auxiliary

    <System.Serializable()> Public Class ExcelParameter
        Public Name As String = ""
        Public Value As Double = 0.0#
        Public Unit As String = ""
        Public Annotation As String = ""
    End Class

End Namespace

Namespace UnitOperations

    <System.Serializable()> Public Class ExcelUO

        Inherits UnitOpBaseClass

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_SpreadsheetUO

        Protected m_DQ As Nullable(Of Double)
        Protected m_FileName As String = ""
        Protected m_InputParams As New Dictionary(Of String, ExcelParameter)
        Protected m_OutputParams As New Dictionary(Of String, ExcelParameter)

        Public Property InputParams() As Dictionary(Of String, ExcelParameter)
            Get
                Return m_InputParams
            End Get
            Set(value As Dictionary(Of String, ExcelParameter))
                m_InputParams = value
            End Set
        End Property
        Public Property OutputParams() As Dictionary(Of String, ExcelParameter)
            Get
                Return m_OutputParams
            End Get
            Set(value As Dictionary(Of String, ExcelParameter))
                m_OutputParams = value
            End Set
        End Property

        Public Property Filename() As String
            Get
                Return m_FileName
            End Get
            Set(ByVal value As String)
                m_FileName = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Private Function ExctractFilepath(ByVal S As String) As String
            Dim P1, P2 As Integer
            P1 = InStr(1, S, "(") + 1
            P2 = InStrRev(S, "\") + 1

            Return Mid(S, P1, P2 - P1)
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim k, ci, co As Integer

            Dim excelType As Type = Nothing

            If Not Calculator.IsRunningOnMono Then excelType = Type.GetTypeFromProgID("Excel.Application")

            If Not Calculator.IsRunningOnMono And Not excelType Is Nothing Then

                Dim excelProxy As Object = Activator.CreateInstance(excelType)

                Using xcl As New Excel.Application(Nothing, excelProxy)

                    For Each CurrAddin As Excel.AddIn In xcl.AddIns
                        If CurrAddin.Installed Then
                            CurrAddin.Installed = False
                            CurrAddin.Installed = True
                        End If
                    Next

                    'xcl.Visible = True 'uncomment for debugging

                    Dim mybook As Excel.Workbook
                    Dim AppPath = Application.StartupPath

                    'Load Excel definition file
                    If My.Computer.FileSystem.FileExists(Filename) Then
                        mybook = xcl.Workbooks.Open(Filename)
                    Else
                        xcl.Quit()
                        'xcl.Dispose()
                        Throw New Exception("Definition file '" & Filename & "' :" & FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"))
                    End If

                    xcl.Calculation = XlCalculation.xlCalculationManual

                    Dim mysheetIn As Excel.Worksheet = mybook.Sheets("Input")
                    Dim mysheetOut As Excel.Worksheet = mybook.Sheets("Output")
                    '=====================================================================================================

                    If Not Me.GraphicObject.InputConnectors(4).IsAttached Then 'Check if Energy stream existing
                        mybook.Close(saveChanges:=False)
                        xcl.Quit()
                        'xcl.Dispose()
                        'CalculateFlowsheet(FlowSheet, objargs, Nothing)
                        Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow1"))
                    End If

                    'check if at least one input and output connection is available
                    For k = 0 To 3
                        If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                        If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                    Next
                    If ci = 0 Or co = 0 Then
                        mybook.Close(saveChanges:=False)
                        xcl.Quit()
                        'xcl.Dispose()
                        'CalculateFlowsheet(FlowSheet, objargs, Nothing)
                        Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                    End If

                    Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double
                    Dim es As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
                    Dim ParName As String
                    Dim i As Integer

                    '======= write data to Excel ==============================================================
                    mysheetIn.Range("B5:E8").Value = "" 'delete Name, T, P, H of streams
                    mysheetIn.Range("A12:E150").Value = "" 'delete molar flows of streams

                    '======= write stream names to Excel =========
                    For k = 0 To 3
                        If GraphicObject.InputConnectors(k).IsAttached Then
                            mysheetIn.Cells(5, 2 + k).Value = Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Tag
                        Else
                            mysheetIn.Cells(5, 2 + k).Value = ""
                        End If
                        If GraphicObject.OutputConnectors(k).IsAttached Then
                            mysheetOut.Cells(5, 2 + k).Value = Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Tag
                        Else
                            mysheetOut.Cells(5, 2 + k).Value = ""
                        End If
                    Next

                    '======== write input parameters ============
                    k = 0
                    For Each EP As ExcelParameter In InputParams.Values
                        mysheetIn.Cells(5 + k, 8).Formula = EP.Value
                        k += 1
                    Next

                    '======== Input streams to unit =======
                    Dim S As MaterialStream
                    For k = 0 To 3
                        If Me.GraphicObject.InputConnectors(k).IsAttached Then
                            S = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                            Me.PropertyPackage.CurrentMaterialStream = S
                            Ti = S.Phases(0).Properties.temperature.GetValueOrDefault.ToString
                            Pi = S.Phases(0).Properties.pressure.GetValueOrDefault.ToString
                            Hi = S.Phases(0).Properties.enthalpy.GetValueOrDefault.ToString
                            Wi = S.Phases(0).Properties.massflow.GetValueOrDefault.ToString
                            Hin += Hi * Wi
                            Win += Wi

                            '======= transfer data to Excel ===========================================================
                            mysheetIn.Cells(6, 2 + k).Value = Ti
                            mysheetIn.Cells(7, 2 + k).Value = Pi
                            mysheetIn.Cells(8, 2 + k).Value = Hi

                            Dim dy As Integer = 0
                            For Each comp As BaseClasses.Compound In S.Phases(0).Compounds.Values
                                mysheetIn.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetOut.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetIn.Cells(12 + dy, 2 + k).Value = comp.MolarFlow
                                dy += 1
                            Next
                        Else
                            mysheetIn.Cells(6, 2 + k).Value = ""
                            mysheetIn.Cells(7, 2 + k).Value = ""
                            mysheetIn.Cells(8, 2 + k).Value = ""
                            Dim dy As Integer = 0
                            For Each comp As BaseClasses.Compound In Me.PropertyPackage.CurrentMaterialStream.Phases(0).Compounds.Values
                                mysheetIn.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetOut.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetIn.Cells(12 + dy, 2 + k).Value = ""
                                mysheetIn.Cells(12 + dy, 3 + k).Value = ""
                                dy += 1
                            Next
                        End If
                    Next

                    xcl.Calculate()

                    '======= read results from Excel =============================================================
                    Dim Vmol As New Dictionary(Of String, Double)
                    Dim v As Double
                    Dim SMass, SMole As Double

                    For k = 0 To 3 'run through all streams to execute TP-flash
                        If Me.GraphicObject.OutputConnectors(k).IsAttached Then
                            Me.PropertyPackage.CurrentMaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                            T2 = mysheetOut.Cells(6, 2 + k).Value
                            P2 = mysheetOut.Cells(7, 2 + k).Value

                            'Atribuir valores a corrente de materia conectada a jusante
                            With Me.PropertyPackage.CurrentMaterialStream
                                .Phases(0).Properties.temperature = T2
                                .Phases(0).Properties.pressure = P2

                                Dim comp As BaseClasses.Compound
                                i = 0
                                SMole = 0
                                SMass = 0
                                Vmol.Clear()
                                For Each comp In .Phases(0).Compounds.Values
                                    v = mysheetOut.Cells(12 + i, 2 + k).Value
                                    Vmol.Add(comp.Name, v)
                                    SMole += Vmol(comp.Name)
                                    SMass += Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / 1000
                                    i += 1
                                Next
                                For Each comp In .Phases(0).Compounds.Values
                                    comp.MoleFraction = Vmol(comp.Name) / SMole
                                    comp.MassFraction = Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                                Next
                                .Phases(0).Properties.massflow = SMass

                                Try
                                    Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, P2, T2, 0)
                                    H2 = tmp.CalculatedEnthalpy
                                    .Phases(0).Properties.enthalpy = H2
                                Catch ex As Exception
                                    mybook.Close(saveChanges:=True)
                                    xcl.Quit()
                                    xcl.Dispose()
                                    Throw New Exception("Flash calculation error")
                                End Try

                                Hout += H2 * SMass
                                Wout += SMass
                            End With

                        End If
                    Next

                    '======= caclculate output stream data ====================================================

                    Me.DeltaQ = Hout - Hin

                    'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
                    With es
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                    '======== read output parameters from Excel table =========================================
                    k = 0
                    OutputParams.Clear()
                    Do
                        Dim ExlPar As New ExcelParameter

                        ParName = mysheetOut.Cells(5 + k, 7).Value
                        If ParName <> "" Then
                            ExlPar.Name = ParName
                            ExlPar.Value = mysheetOut.Cells(5 + k, 8).Value
                            ExlPar.Unit = mysheetOut.Cells(5 + k, 9).Value
                            ExlPar.Annotation = mysheetOut.Cells(5 + k, 10).Value
                            OutputParams.Add(ExlPar.Name, ExlPar)

                            k += 1
                        End If
                    Loop While ParName <> ""


                    '=============== clean up Excel stuff ================================================================
                    mybook.Close(saveChanges:=True)
                    xcl.Quit()
                    'xcl.Dispose()

                    MassBal = 100 * (Wout - Win) / (Win)
                    If Math.Abs(MassBal) > 0.001 Then
                        FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", IFlowsheet.MessageType.GeneralError)
                    End If

                End Using

            Else

                'use GemBox to read and write data

                GS.SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY")

                Dim xcl As GS.ExcelFile = Nothing

                Dim AppPath = Application.StartupPath

                'Load Excel definition file
                If My.Computer.FileSystem.FileExists(Filename) Then
                    xcl = GS.ExcelFile.Load(Filename)
                Else
                    Throw New Exception("Definition file '" & Filename & "' :" & FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"))
                End If

                Dim mysheetIn As GS.ExcelWorksheet = xcl.Worksheets("Input")
                Dim mysheetOut As GS.ExcelWorksheet = xcl.Worksheets("Output")
                '=====================================================================================================

                If Not Me.GraphicObject.InputConnectors(4).IsAttached Then 'Check if Energy stream existing
                    Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow1"))
                End If

                'check if at least one input and output connection is available
                For k = 0 To 3
                    If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                    If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                Next
                If ci = 0 Or co = 0 Then
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If

                Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double
                Dim es As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
                Dim ParName As String
                Dim i As Integer

                '======= write data to Excel ==============================================================
                mysheetIn.Cells.GetSubrange("B5", "E8").Value = "" 'delete Name, T, P, H of streams
                mysheetIn.Cells.GetSubrange("A12", "E150").Value = "" 'delete molar flows of streams

                '======= write stream names to Excel =========
                For k = 0 To 3
                    If GraphicObject.InputConnectors(k).IsAttached Then
                        mysheetIn.Cells(4, 1 + k).Value = Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Tag
                    Else
                        mysheetIn.Cells(4, 1 + k).Value = ""
                    End If
                    If GraphicObject.OutputConnectors(k).IsAttached Then
                        mysheetOut.Cells(4, 1 + k).Value = Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Tag
                    Else
                        mysheetOut.Cells(4, 1 + k).Value = ""
                    End If
                Next

                '======== write input parameters ============
                k = 0
                For Each EP As ExcelParameter In InputParams.Values
                    mysheetIn.Cells(4 + k, 7).Formula = EP.Value
                    k += 1
                Next

                '======== Input streams to unit =======
                Dim S As MaterialStream
                For k = 0 To 3
                    If Me.GraphicObject.InputConnectors(k).IsAttached Then
                        S = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                        Me.PropertyPackage.CurrentMaterialStream = S
                        Ti = S.Phases(0).Properties.temperature.GetValueOrDefault
                        Pi = S.Phases(0).Properties.pressure.GetValueOrDefault
                        Hi = S.Phases(0).Properties.enthalpy.GetValueOrDefault
                        Wi = S.Phases(0).Properties.massflow.GetValueOrDefault
                        Hin += Hi * Wi
                        Win += Wi

                        '======= transfer data to Excel ===========================================================
                        mysheetIn.Cells(5, 1 + k).Value = Ti
                        mysheetIn.Cells(6, 1 + k).Value = Pi
                        mysheetIn.Cells(7, 1 + k).Value = Hi

                        Dim dy As Integer = 0
                        For Each comp As BaseClasses.Compound In S.Phases(0).Compounds.Values
                            mysheetIn.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetOut.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetIn.Cells(11 + dy, 1 + k).Value = comp.MolarFlow.GetValueOrDefault
                            dy += 1
                        Next
                    Else
                        mysheetIn.Cells(5, 1 + k).Value = ""
                        mysheetIn.Cells(6, 1 + k).Value = ""
                        mysheetIn.Cells(7, 1 + k).Value = ""
                        Dim dy As Integer = 0
                        For Each comp As BaseClasses.Compound In Me.PropertyPackage.CurrentMaterialStream.Phases(0).Compounds.Values
                            mysheetIn.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetOut.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetIn.Cells(11 + dy, 1 + k).Value = ""
                            mysheetIn.Cells(11 + dy, 2 + k).Value = ""
                            dy += 1
                        Next
                    End If
                Next

                'open spreadsheet to be calculated manually by the user.

                xcl.Save(Filename)

                If Calculator.IsRunningOnMono Then
                    Dim p As New Process()
                    With p
                        .StartInfo.FileName = "xdg-open"
                        .StartInfo.Arguments = Filename
                        .StartInfo.UseShellExecute = False
                        .Start()
                        MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                    End With
                Else
                    Process.Start(Filename)
                    MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                End If

                'Load Excel definition file
                xcl = GS.ExcelFile.Load(Filename)

                mysheetIn = xcl.Worksheets("Input")
                mysheetOut = xcl.Worksheets("Output")

                '======= read results from sheet =============================================================
                Dim Vmol As New Dictionary(Of String, Double)
                Dim v As Double
                Dim SMass, SMole As Double

                For k = 0 To 3 'run through all streams to execute TP-flash
                    If Me.GraphicObject.OutputConnectors(k).IsAttached Then
                        Me.PropertyPackage.CurrentMaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                        T2 = mysheetOut.Cells(5, 1 + k).Value
                        P2 = mysheetOut.Cells(6, 1 + k).Value

                        'Atribuir valores a corrente de materia conectada a jusante
                        With Me.PropertyPackage.CurrentMaterialStream
                            .Phases(0).Properties.temperature = T2
                            .Phases(0).Properties.pressure = P2

                            Dim comp As BaseClasses.Compound
                            i = 0
                            SMole = 0
                            SMass = 0
                            Vmol.Clear()
                            For Each comp In .Phases(0).Compounds.Values
                                v = mysheetOut.Cells(11 + i, 1 + k).Value
                                Vmol.Add(comp.Name, v)
                                SMole += Vmol(comp.Name)
                                SMass += Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / 1000
                                i += 1
                            Next
                            For Each comp In .Phases(0).Compounds.Values
                                comp.MoleFraction = Vmol(comp.Name) / SMole
                                comp.MassFraction = Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                            Next
                            .Phases(0).Properties.massflow = SMass

                            Try
                                Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, P2, T2, 0)
                                H2 = tmp.CalculatedEnthalpy
                                .Phases(0).Properties.enthalpy = H2
                            Catch ex As Exception
                                Throw New Exception("Flash calculation error")
                            End Try

                            Hout += H2 * SMass
                            Wout += SMass
                        End With

                    End If
                Next

                '======= caclculate output stream data ====================================================

                Me.DeltaQ = Hout - Hin

                'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
                With es
                    .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With

                '======== read output parameters from Excel table =========================================
                k = 0
                OutputParams.Clear()
                Do
                    Dim ExlPar As New ExcelParameter

                    ParName = mysheetOut.Cells(4 + k, 6).Value
                    If ParName <> "" Then
                        ExlPar.Name = ParName
                        ExlPar.Value = mysheetOut.Cells(4 + k, 7).Value
                        ExlPar.Unit = mysheetOut.Cells(4 + k, 8).Value
                        ExlPar.Annotation = mysheetOut.Cells(4 + k, 9).Value
                        OutputParams.Add(ExlPar.Name, ExlPar)

                        k += 1
                    End If
                Loop While ParName <> ""

                MassBal = 100 * (Wout - Win) / (Win)
                If Math.Abs(MassBal) > 0.001 Then
                    FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", IFlowsheet.MessageType.GeneralError)
                End If

            End If


        End Sub

        Public Overrides Sub DeCalculate()

            Dim k As Integer

            For k = 0 To 3
                If Me.GraphicObject.OutputConnectors(k).IsAttached Then

                    'Zerar valores da corrente de materia conectada a jusante
                    With Me.GetOutletMaterialStream(k)
                        .Phases(0).Properties.temperature = Nothing
                        .Phases(0).Properties.pressure = Nothing
                        .Phases(0).Properties.enthalpy = Nothing
                        .Phases(0).Properties.molarfraction = 1
                        .Phases(0).Properties.massfraction = 1
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = 0
                            comp.MassFraction = 0
                            i += 1
                        Next
                        .Phases(0).Properties.massflow = Nothing
                        .Phases(0).Properties.molarflow = Nothing
                        .GraphicObject.Calculated = False
                    End With

                End If
            Next

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With Me.GetEnergyStream
                    .EnergyFlow = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, DeltaQ.GetValueOrDefault)
                Case "In"
                    value = InputParams(propID).Value
                Case "Out"
                    value = OutputParams(propID).Value
            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()

            Dim proplist As New ArrayList

            Select Case proptype
                Case PropertyType.RO
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
                Case PropertyType.RW
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                Case PropertyType.WR
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                Case PropertyType.ALL
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
            End Select

            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    DeltaQ = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case "In"
                    InputParams(propID).Value = propval
                Case "Out"
                    OutputParams(propID).Value = propval
            End Select

            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    value = su.heatflow
                Case "In"
                    value = InputParams(propID).Unit
                Case "Out"
                    value = OutputParams(propID).Unit
            End Select

            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_SpreadsheetUO With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_SpreadsheetUO With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.uo_excel_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("EXLUO_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("EXLUO_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

End Namespace
