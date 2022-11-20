Imports DWSIM.Thermodynamics.BaseClasses
Imports Ciloci.Flee
Imports System.Math
Imports System.Linq
Imports DWSIM.MathOps.MathEx.Common
Imports DotNumerics.Optimization
Imports DWSIM.MathOps.MathEx
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics
Imports scaler = DotNumerics.Scaling.Scaler
Imports DWSIM.MathOps
Imports SkiaSharp
Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports Python.Runtime
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports System.Text

Namespace Reactors

    <System.Serializable()> Public Class Reactor_ReaktoroGibbs

        Inherits Reactor

        Implements DWSIM.Interfaces.IExternalUnitOperation

        Private ImagePath As String = ""

        Private Image As SKImage

        Public Property EmbeddedImageData As String = ""

        Public Property UseEmbeddedImage As Boolean = False

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ReaktoroGibbs

        Public Property ReaktoroPath As String = "reaktoro"

        Public Property DatabaseName As String = "supcrt07.xml"

        Public Property Prefix As String = "RK-" Implements IExternalUnitOperation.Prefix

        Public Overrides Property ComponentName As String = GetDisplayName()

        Public Overrides Property ComponentDescription As String = GetDisplayDescription()

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = False

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        Private ReadOnly Property IExternalUnitOperation_Name As String = GetDisplayName() Implements IExternalUnitOperation.Name

        Public ReadOnly Property Description As String = GetDisplayDescription() Implements IExternalUnitOperation.Description

        Public Overrides ReadOnly Property MobileCompatible As Boolean = False

        Public Property CompoundsList As New List(Of String)

        Public Property ElementsList As New List(Of String)

        Public Property AqueousPhase As Boolean = True

        Public Property GaseousPhase As Boolean = True

        Public Property LiquidPhase As Boolean = False

        Public Property MineralPhase As Boolean = False

        Public Property CompoundNames As New Dictionary(Of String, String)

        Public Property SpeciesMaps As New Dictionary(Of String, String)

        Public Property CompoundConversions As New Dictionary(Of String, Double)

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Function GetDisplayName() As String

            Return "Gibbs Reactor (Reaktoro)"

        End Function

        Public Overrides Function GetDisplayDescription() As String

            Return "Gibbs Reactor (Reaktoro)"

        End Function

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Reactor_ReaktoroGibbs()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_ReaktoroGibbs)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Sub Validate()

        End Sub

        Public Overrides Sub PerformPostCalcValidation()

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Settings.RunningPlatform() = Settings.Platform.Windows Then

                DWSIM.GlobalSettings.Settings.ShutdownPythonEnvironment()

                If Settings.GetEnvironment() = 32 Then
                    Throw New Exception("Reaktoro is not supported on 32-bit environments.")
                End If

                ReaktoroPath = Path.Combine(SharedClasses.Utility.GetDwsimRootDirectory(), "PythonEnvs", "reaktoro")

                If Not Directory.Exists(ReaktoroPath) Then
                    Throw New Exception("Please install DWSIM Python Environments Add-On and try again.")
                End If

                DWSIM.GlobalSettings.Settings.InitializePythonEnvironment(ReaktoroPath)

                DWSIM.GlobalSettings.Settings.InitializePythonEnvironment()

            Else

                Throw New Exception("This Unit Operation is not available on Linux/macOS.")

            End If


            Dim msin = GetInletMaterialStream(0)
            Dim msout = GetOutletMaterialStream(0)

            Dim esout = GetOutletEnergyStream(1)

            Using Py.GIL

                Dim reaktoro As Object = Py.Import("reaktoro")

                'Initialize a thermodynamic database
                Dim db = reaktoro.Database(DatabaseName)

                'Define the chemical system
                Dim editor = reaktoro.ChemicalEditor(db)

                Dim elstring As String = ""

                For Each el In ElementsList
                    elstring += el + " "
                Next
                elstring = elstring.Trim()

                If GaseousPhase Then editor.addGaseousPhaseWithElements(elstring)

                If AqueousPhase Then
                    Dim aqueousPhase = editor.addAqueousPhaseWithElements(elstring)
                    aqueousPhase.setChemicalModelHKF()
                    aqueousPhase.setActivityModelDrummondCO2()
                End If

                If LiquidPhase Then editor.addLiquidPhaseWithElements(elstring)

                If MineralPhase Then editor.addMineralPhaseWithElements(elstring)

                'Construct the chemical system

                Dim mySystem = reaktoro.ChemicalSystem(editor)

                'Define the chemical equilibrium problem

                Dim problem = reaktoro.EquilibriumProblem(mySystem)

                problem.setTemperature(msin.GetTemperature(), "kelvin")
                problem.setPressure(msin.GetPressure(), "pascal")

                For Each item In CompoundsList
                    If FlowSheet.SelectedCompounds.ContainsKey(item) Then
                        Dim compound = FlowSheet.SelectedCompounds(item)
                        problem.add(CompoundNames(item), msin.Phases(0).Compounds(item).MolarFlow.GetValueOrDefault(), "mol")
                    End If
                Next

                'Calculate the chemical equilibrium state

                Dim state = reaktoro.equilibrate(problem)

                Dim properties = state.properties

                Dim species = mySystem.species()

                Dim amounts = state.speciesAmounts()

                Dim speciesAmountsFinal As New Dictionary(Of String, Double)
                Dim compoundAmountsFinal As New Dictionary(Of String, Double)

                Dim i As Integer

                Dim newspecies As New List(Of String)

                For i = 0 To species.Length - 1
                    Dim name = species(i).name.ToString()
                    newspecies.Add(name)
                    If Not SpeciesMaps.ContainsKey(name) Then
                        SpeciesMaps.Add(name, "")
                    End If
                    If SpeciesMaps(name) <> "" Then
                        speciesAmountsFinal.Add(name, amounts(i).ToString().ToDoubleFromInvariant())
                        If Not compoundAmountsFinal.ContainsKey(SpeciesMaps(name)) Then
                            compoundAmountsFinal.Add(SpeciesMaps(name), 0.0)
                        End If
                        compoundAmountsFinal(SpeciesMaps(name)) += amounts(i).ToString().ToDoubleFromInvariant()
                    End If
                Next

                Dim oldspecies = SpeciesMaps.Keys.ToList()

                For Each sp In oldspecies
                    If Not newspecies.Contains(sp) Then
                        Try
                            SpeciesMaps.Remove(sp)
                        Catch ex As Exception
                        End Try
                    End If
                Next

                Dim names = msin.Phases(0).Compounds.Keys.ToList()

                Dim N0 = msin.Phases(0).Compounds.Values.Select(Function(c) c.MolarFlow.GetValueOrDefault()).ToList()

                Dim Nf = New List(Of Double)(N0)

                For i = 0 To N0.Count - 1
                    If compoundAmountsFinal.ContainsKey(names(i)) Then
                        Nf(i) = compoundAmountsFinal(names(i))
                    Else
                        Nf(i) = N0(i)
                    End If
                Next

                'conversions

                ComponentConversions.Clear()
                For i = 0 To N0.Count - 1
                    Dim conv = (N0(i) - Nf(i)) / N0(i)
                    If conv > 0 Then
                        ComponentConversions.Add(names(i), conv)
                    End If
                Next

                'reaction heat

                Dim DHr As Double = 0

                For Each sb As Compound In msin.Phases(0).Compounds.Values
                    If compoundAmountsFinal.ContainsKey(sb.Name) Then
                        DHr += -sb.ConstantProperties.IG_Enthalpy_of_Formation_25C * sb.ConstantProperties.Molar_Weight * (Nf(names.IndexOf(sb.Name)) - N0(names.IndexOf(sb.Name))) / 1000.0
                    End If
                Next

                esout.EnergyFlow = DHr

                msout.Clear()
                msout.ClearAllProps()

                msout.SetOverallComposition(Nf.ToArray().MultiplyConstY(1.0 / Nf.Sum))
                msout.SetMolarFlow(Nf.Sum)
                msout.SetPressure(msin.GetPressure - DeltaP.GetValueOrDefault())
                msout.SetTemperature(msin.GetTemperature)
                msout.SetFlashSpec("PT")

                msout.AtEquilibrium = False

            End Using


        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ReaktoroGibbs With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReaktoroGibbs With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
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

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object

            Return My.Resources.reactor_reaktoro

        End Function

        Public Sub Draw(g As Object) Implements IExternalUnitOperation.Draw

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            If UseEmbeddedImage = True AndAlso EmbeddedImageData <> "" Then

                Dim p As New SKPaint
                With p
                    p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    p.FilterQuality = SKFilterQuality.High
                End With

                Using image As SKImage = EmbeddedImageGraphic.Base64ToImage(EmbeddedImageData)
                    canvas.DrawImage(image, New SKRect(GraphicObject.X, GraphicObject.Y, GraphicObject.X + GraphicObject.Width, GraphicObject.Y + GraphicObject.Height), p)
                End Using

            Else

                If Image Is Nothing Then

                    ImagePath = SharedClasses.Utility.GetTempFileName()
                    My.Resources.reactor_reaktoro.Save(ImagePath)

                    Using streamBG = New FileStream(ImagePath, FileMode.Open)
                        Using bitmap = SKBitmap.Decode(streamBG)
                            Image = SKImage.FromBitmap(bitmap)
                        End Using
                    End Using

                    Try
                        File.Delete(ImagePath)
                    Catch ex As Exception
                    End Try

                End If

                Using p As New SKPaint With {.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias, .FilterQuality = SKFilterQuality.High}
                    canvas.DrawImage(Image, New SKRect(GraphicObject.X, GraphicObject.Y, GraphicObject.X + GraphicObject.Width, GraphicObject.Y + GraphicObject.Height), p)
                End Using

            End If

        End Sub

        Public Sub CreateConnectors() Implements IExternalUnitOperation.CreateConnectors

            Dim w, h, x, y As Double
            w = GraphicObject.Width
            h = GraphicObject.Height
            x = GraphicObject.X
            y = GraphicObject.Y

            Dim myIC1 As New ConnectionPoint

            myIC1.Position = New Point(x, y + h / 2)
            myIC1.Type = ConType.ConIn
            myIC1.Direction = ConDir.Right

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(x + w, y + h / 2)
            myOC1.Type = ConType.ConOut
            myOC1.Direction = ConDir.Right

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(x + w / 2, y + h)
            myOC2.Type = ConType.ConOut
            myOC2.Direction = ConDir.Down
            myOC2.Type = ConType.ConEn

            With GraphicObject.InputConnectors
                If .Count = 1 Then
                    .Item(0).Position = New Point(x, y + h / 2)
                Else
                    .Add(myIC1)
                End If
                .Item(0).ConnectorName = "Inlet"
            End With

            With GraphicObject.OutputConnectors
                If .Count = 2 Then
                    .Item(0).Position = New Point(x + w, y + h / 2)
                    .Item(1).Position = New Point(x + w / 2, y + h)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                End If
                .Item(0).ConnectorName = "Outlet"
                .Item(1).ConnectorName = "Heat Outlet"
            End With

            Me.GraphicObject.EnergyConnector.Active = False

        End Sub

        Public Sub PopulateEditorPanel(container As Object) Implements IExternalUnitOperation.PopulateEditorPanel

        End Sub

        Public Function ReturnInstance(typename As String) As Object Implements IExternalUnitOperation.ReturnInstance

            Return New Reactor_ReaktoroGibbs

        End Function

        Public Overrides Function GetProperties(proptype As PropertyType) As String()

            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.WR
                    proplist.Add("Pressure Drop")
                Case PropertyType.ALL
                    For Each item In ComponentConversions
                        proplist.Add(item.Key + ": Conversion")
                    Next
            End Select

            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing

        End Function

        Public Overrides Function GetPropertyUnit(prop As String, Optional su As IUnitsOfMeasure = Nothing) As String

            If su Is Nothing Then su = New SystemsOfUnits.SI()
            If prop.Contains("Conversion") Then
                Return "%"
            ElseIf prop.Equals("Pressure Drop") Then
                Return su.deltaP
            End If

        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI()
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                Dim value As Double
                If prop.Contains("Conversion") Then
                    Dim comp = prop.Split(": ")(0)
                    If ComponentConversions.ContainsKey(comp) Then
                        value = ComponentConversions(comp) * 100
                    Else
                        value = 0.0
                    End If
                ElseIf prop.Equals("Pressure Drop") Then
                    Return DeltaP.GetValueOrDefault().ConvertFromSI(su.deltaP)
                End If
                Return value
            End If

        End Function

        Public Overrides Function SetPropertyValue(prop As String, propval As Object, Optional su As IUnitsOfMeasure = Nothing) As Boolean

            If su Is Nothing Then su = New SystemsOfUnits.SI()
            If prop.Equals("Pressure Drop") Then
                DeltaP = Convert.ToDouble(propval).ConvertToSI(su.deltaP)
            End If
            Return True

        End Function

        Public Function GetListOfCompounds() As String


            DWSIM.GlobalSettings.Settings.ShutdownPythonEnvironment()

            ReaktoroPath = Path.Combine(SharedClasses.Utility.GetDwsimRootDirectory(), "PythonEnvs", "reaktoro")

            If Not Directory.Exists(ReaktoroPath) Then
                Throw New Exception("Please install DWSIM Python Environments Add-On and try again.")
            End If

            DWSIM.GlobalSettings.Settings.InitializePythonEnvironment(ReaktoroPath)

            Using Py.GIL

                Dim reaktoro As Object = Py.Import("reaktoro")

                'Initialize a thermodynamic database
                Dim db As Object = reaktoro.Database(DatabaseName)

                Dim aql As Object = db.aqueousSpecies()
                Dim gql As Object = db.gaseousSpecies()
                Dim lql As Object = db.liquidSpecies()
                Dim mql As Object = db.mineralSpecies()

                Dim sb As New StringBuilder
                Dim i As Integer = 0

                sb.AppendLine("Aqueous Species:")
                sb.AppendLine()
                For i = 0 To aql.Length - 1
                    sb.AppendLine(aql(i).name.ToString() + " (" + aql(i).formula.ToString() + ")")
                Next
                sb.AppendLine()
                sb.AppendLine("Gaseous Species:")
                sb.AppendLine()
                For i = 0 To gql.Length - 1
                    sb.AppendLine(gql(i).name.ToString() + " (" + gql(i).formula.ToString() + ")")
                Next
                sb.AppendLine()
                sb.AppendLine("Liquid (Non-Aqueous) Species:")
                sb.AppendLine()
                For i = 0 To lql.Length - 1
                    sb.AppendLine(lql(i).name.ToString() + " (" + lql(i).formula.ToString() + ")")
                Next
                sb.AppendLine()
                sb.AppendLine("Mineral Species:")
                sb.AppendLine()
                For i = 0 To mql.Length - 1
                    sb.AppendLine(mql(i).name.ToString() + " (" + mql(i).formula.ToString() + ")")
                Next

                Return sb.ToString()

            End Using

        End Function

    End Class

End Namespace