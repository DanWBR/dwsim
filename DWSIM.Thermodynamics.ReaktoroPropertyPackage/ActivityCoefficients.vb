Imports System.IO
Imports DWSIM.Thermodynamics.PropertyPackages
Imports Python.Runtime
Imports DWSIM.ExtensionMethods
Imports DWSIM.GlobalSettings

Public Class ActivityCoefficients

    Public Overloads Function Calculate(Vx As Double(), T As Double, P As Double, pp As PropertyPackage) As Double()

        If Vx.SumY = 0.0 Then Return pp.RET_UnitaryVector()

        Dim i As Integer

        Dim CompProps = pp.DW_GetConstantProperties()
        Dim saltonly As Boolean = False
        For i = 0 To Vx.Length - 1
            If Vx(i) > 0 And CompProps(i).IsSalt Then
                saltonly = True
            ElseIf Vx(i) > 0 And Not CompProps(i).IsSalt Then
                saltonly = False
                Exit For
            End If
        Next

        If saltonly Then Return pp.RET_UnitaryVector()

        Dim CompoundMaps = New CompoundMapper()
        Dim Setschenow As New SetschenowCoefficients()

        Dim n As Integer = Vx.Length - 1
        Dim activcoeff(n) As Double

        Dim names = pp.RET_VNAMES().ToList
        Dim formulas As New List(Of String)

        For Each na In names
            If Not CompoundMaps.Maps.ContainsKey(na) Then
                'Throw New Exception(String.Format("Compound {0} is not supported by this Property Package [{1}].", na, pp.ComponentName))
            End If
        Next

        Settings.InitializePythonEnvironment()

        Dim speciesPhases As New Dictionary(Of String, String)
        Dim speciesAmounts As New Dictionary(Of String, Double)
        Dim speciesAmountsFinal As New Dictionary(Of String, Double)
        Dim compoundAmountsFinal As New Dictionary(Of String, Double)
        Dim inverseMaps As New Dictionary(Of String, String)

        Dim aqueous As String = ""

        i = 0
        For Each na In names
            formulas.Add(CompoundMaps.Maps(na).Formula)
            speciesAmounts.Add(CompoundMaps.Maps(na).Formula, Vx(i))
            If CompoundMaps.Maps(na).AqueousName <> "" Then
                aqueous += CompoundMaps.Maps(na).AqueousName + " "
                speciesPhases.Add(CompoundMaps.Maps(na).AqueousName, "L")
                inverseMaps.Add(CompoundMaps.Maps(na).AqueousName, CompoundMaps.Maps(na).Formula)
            Else
                speciesPhases.Add(CompoundMaps.Maps(na).AqueousName, "")
            End If
            i += 1
        Next
        aqueous = aqueous.TrimEnd()

        Dim libpath = ReaktoroLoader.Initialize()

        Dim pystate = Py.GIL()

        Dim ex0 As Exception = Nothing

        Try

            If libpath <> "" Then

                Dim sys As Object = Py.Import("sys")
                sys.path.append(libpath)

                Dim os As Object = Py.Import("os")

                Dim dllpath = Path.Combine(libpath, "reaktoro")
                Dim shareddllpath = Path.Combine(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location), "python_packages", "reaktoro_shared")

                os.add_dll_directory(dllpath)
                os.add_dll_directory(shareddllpath)
                os.add_dll_directory(Settings.PythonPath)

            End If

            Dim reaktoro As Object = Py.Import("reaktoro")
            Dim np As Object = Py.Import("numpy")

            'Initialize a thermodynamic database
            Dim db = reaktoro.Database("supcrt07-organics.xml")

            'Define the chemical system
            Dim editor = reaktoro.ChemicalEditor(db)

            Dim aqueousPhase = editor.addAqueousPhase(aqueous)

            aqueousPhase.setChemicalModelHKF()
            aqueousPhase.setActivityModelDrummondCO2()
            'i = 0
            'For Each na In names
            '    If CompoundMaps.Maps(na).AqueousName <> "" And na <> "Water" And
            '        Not CompProps(i).IsIon And Not CompProps(i).IsSalt Then
            '        aqueousPhase.setActivityModelSetschenow(CompoundMaps.Maps(na).AqueousName, Setschenow.GetValue(na))
            '    End If
            '    i += 1
            'Next

            'Construct the chemical system
            Dim mySystem = reaktoro.ChemicalSystem(editor)

            Dim mols = np.fromiter(speciesAmounts.Values.ToArray(), np.float64)

            Dim props = reaktoro.ChemicalProperties(mySystem)
            props.update(T, P, mols)

            Dim species = mySystem.species()

            Dim ac = props.lnActivityCoefficients().val


            For i = 0 To ac.Length - 1
                If speciesPhases(species(i).name.ToString()) = "L" Then
                    Dim index As Integer = formulas.IndexOf(inverseMaps(species(i).name.ToString()))
                    activcoeff(index) = Math.Exp(ac(i).ToString().ToDoubleFromInvariant())
                    'If names(i) = "Ammonia" Then
                    '    'ammonia act coefficient
                    '    activcoeff(index) = 1.68734806901 * Math.Exp(-790.33175622 / T + 4.12597652879 * Vx(index))
                    'End If
                End If
            Next


        Catch ex As Exception

            pp.Flowsheet?.ShowMessage("Reaktoro error: " + ex.Message, DWSIM.Interfaces.IFlowsheet.MessageType.GeneralError)
            ex0 = ex

        Finally

            pystate?.Dispose()
            pystate = Nothing

        End Try

        If ex0 IsNot Nothing Then
            Throw ex0
        Else
            Return activcoeff.ExpY()
        End If

    End Function


End Class
