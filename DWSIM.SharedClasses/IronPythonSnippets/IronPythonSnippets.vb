Imports System.IO
Imports System.Reflection
Imports DWSIM.Interfaces

Namespace Scripts

    Public Class IronPythonSnippet

        Public Property Name As String = ""

        Public Property Category1 As String = ""

        Public Property Category2 As String = ""

        Public Property Scope As String = ""

        Public Property Snippet As String = ""

    End Class

    Public Class IronPythonSnippets

        Public Sub New()

        End Sub

        Private Shared Function GetSnippetsXML() As XDocument


            Dim xmldoc = New XDocument()

            Using filestr As Stream = Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.SharedClasses.IronPythonSnippets.xml")
                xmldoc = XDocument.Load(filestr)
            End Using

            Return xmldoc

        End Function

        Public Shared Function GetSnippets() As List(Of IronPythonSnippet)

            Dim list As New List(Of IronPythonSnippet)

            Dim xml = GetSnippetsXML()

            For Each node As XElement In xml.Elements.First.Elements

                Dim snippet As New IronPythonSnippet()
                snippet.Name = node.Elements("A").Value
                snippet.Category1 = node.Elements("B").Value
                snippet.Category2 = node.Elements("C").Value
                snippet.Scope = node.Elements("D").Value
                snippet.Snippet = node.Elements("E").Value

                list.Add(snippet)

            Next

            list.Remove(list.First)

            Return list

        End Function

        Public Shared Sub PopulateWithDynamicSnippets(contextmenu As Eto.Forms.ContextMenu, fs As IFlowsheet, InsertText As Action(Of String))

            Dim gettsmi = New Eto.Forms.ButtonMenuItem With {.Text = "Get Object Property"}
            Dim settsmi = New Eto.Forms.ButtonMenuItem With {.Text = "Set Object Property"}

            For Each item In fs.SimulationObjects.Values.OrderBy(Function(x) x.GraphicObject.Tag)

                Dim itemtsmig As New Eto.Forms.ButtonMenuItem
                itemtsmig.Text = item.GraphicObject.Tag

                gettsmi.Items.Add(itemtsmig)

                Dim itemtsmis As New Eto.Forms.ButtonMenuItem
                itemtsmis.Text = item.GraphicObject.Tag

                settsmi.Items.Add(itemtsmis)

                If TypeOf item Is IMaterialStream Then

                    ' set overall properties

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Temperature", Sub()
                                                                 InsertText("# Define Stream Temperature")
                                                                 InsertText(System.Environment.NewLine)
                                                                 InsertText(System.Environment.NewLine)
                                                                 InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                 InsertText(System.Environment.NewLine)
                                                                 InsertText(String.Format("stream.SetTemperature(value) # value must be in K"))
                                                                 InsertText(System.Environment.NewLine)
                                                             End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Pressure", Sub()
                                                              InsertText("# Define Stream Pressure")
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream.SetPressure(value) # value must be in Pa"))
                                                              InsertText(System.Environment.NewLine)
                                                          End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Enthalpy", Sub()
                                                              InsertText("# Define Stream Enthalpy")
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream.SetMassEnthalpy(value) # value must be in kJ/kg"))
                                                              InsertText(System.Environment.NewLine)
                                                          End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Entropy", Sub()
                                                             InsertText("# Define Stream Entropy")
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(String.Format("stream.SetMassEntropy(value) # value must be in kJ/[kg.K]"))
                                                             InsertText(System.Environment.NewLine)
                                                         End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Mass Flow", Sub()
                                                               InsertText("Define Stream Mass Flow")
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(String.Format("stream.SetMassFlow(value) # value must be in kg/s"))
                                                               InsertText(System.Environment.NewLine)
                                                           End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Molar Flow", Sub()
                                                                InsertText("# Define Stream Molar Flow")
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("stream.SetMolarFlow(value) # value must be in mol/s"))
                                                                InsertText(System.Environment.NewLine)
                                                            End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Volumetric Flow", Sub()
                                                                     InsertText("# Define Stream Volumetric Flow")
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(String.Format("stream.SetVolumetricFlow(value) # value must be in m3/s"))
                                                                     InsertText(System.Environment.NewLine)
                                                                 End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Vapor Molar Fraction", Sub()
                                                                          InsertText("# Set Stream Vapor Molar Fraction")
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("obj.GetPhase('Vapor').Properties.molarfraction = value # number ranging from 0.0 to 1.0"))
                                                                          InsertText(System.Environment.NewLine)
                                                                      End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Molar Composition", Sub()
                                                                       InsertText("# Define Stream Molar Composition")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("value = System.Array[double]([0.1, ..., x])"))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("obj.SetOverallComposition(value) # value must be an array of mole fractions"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub, fs))

                    itemtsmis.Items.Add(
                        CreateMenuItem("Stream Flash Specification", Sub()
                                                                         InsertText("# Define Stream Flash Specification")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("obj.SpecType = value # number from 0 to 5"))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# Accepted values:")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 0: Temperature_and_Pressure")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 1: Pressure_and_Enthalpy")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 2: Pressure_and_Entropy")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 3: Pressure_and_VaporFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 4: Temperature_and_VaporFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 5: Pressure_and_SolidFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                     End Sub, fs))

                    'get overall properties

                    itemtsmig.Items.Add(
                       CreateMenuItem("Stream Temperature", Sub()
                                                                InsertText("# Get Stream Temperature")
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("value = stream.GetTemperature() # in K"))
                                                                InsertText(System.Environment.NewLine)
                                                            End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Pressure", Sub()
                                                              InsertText("# Get Stream Pressure")
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("value = stream.GetPressure() # in Pa"))
                                                              InsertText(System.Environment.NewLine)
                                                          End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Enthalpy", Sub()
                                                              InsertText("# Get Stream Enthalpy")
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                              InsertText(System.Environment.NewLine)
                                                              InsertText(String.Format("value = stream.GetMassEnthalpy() # in kJ/kg"))
                                                              InsertText(System.Environment.NewLine)
                                                          End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Entropy", Sub()
                                                             InsertText("# Get Stream Entropy")
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                             InsertText(System.Environment.NewLine)
                                                             InsertText(String.Format("value = stream.GetMassEntropy() # in kJ/[kg.K]"))
                                                             InsertText(System.Environment.NewLine)
                                                         End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Mass Flow", Sub()
                                                               InsertText("Get Stream Mass Flow")
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                               InsertText(System.Environment.NewLine)
                                                               InsertText(String.Format("value = stream.GetMassFlow() # in kg/s"))
                                                               InsertText(System.Environment.NewLine)
                                                           End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Molar Flow", Sub()
                                                                InsertText("# Get Stream Molar Flow")
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                InsertText(System.Environment.NewLine)
                                                                InsertText(String.Format("value = stream.GetMolarFlow() # in mol/s"))
                                                                InsertText(System.Environment.NewLine)
                                                            End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Volumetric Flow", Sub()
                                                                     InsertText("# Get Stream Volumetric Flow")
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                     InsertText(System.Environment.NewLine)
                                                                     InsertText(String.Format("value = stream.GetVolumetricFlow() # in m3/s"))
                                                                     InsertText(System.Environment.NewLine)
                                                                 End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Vapor Molar Fraction", Sub()
                                                                          InsertText("# Get Stream Vapor Molar Fraction")
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("value = obj.GetPhase('Vapor').Properties.molarfraction # number ranging from 0.0 to 1.0"))
                                                                          InsertText(System.Environment.NewLine)
                                                                      End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Molar Composition", Sub()
                                                                       InsertText("# Get Stream Molar Composition")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("value = obj.GetOverallComposition() # array of mole fractions"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub, fs))

                    itemtsmig.Items.Add(
                        CreateMenuItem("Stream Flash Specification", Sub()
                                                                         InsertText("# Get Stream Flash Specification")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("value = obj.SpecType # number from 0 to 5"))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# Current values:")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 0: Temperature_and_Pressure")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 1: Pressure_and_Enthalpy")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 2: Pressure_and_Entropy")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 3: Pressure_and_VaporFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 4: Temperature_and_VaporFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText("# 5: Pressure_and_SolidFraction")
                                                                         InsertText(System.Environment.NewLine)
                                                                     End Sub, fs))

                    Dim p1 = CreateMenuItem(("Phase Properties") & " - " & ("Overall"), Nothing, fs)
                    Dim p2 = CreateMenuItem(("Phase Properties") & " - " & ("Vapor"), Nothing, fs)
                    Dim p3 = CreateMenuItem(("Phase Properties") & " - " & ("Overall Liquid"), Nothing, fs)
                    Dim p4 = CreateMenuItem(("Phase Properties") & " - " & ("Liquid 1"), Nothing, fs)
                    Dim p5 = CreateMenuItem(("Phase Properties") & " - " & ("Liquid 2"), Nothing, fs)
                    Dim p6 = CreateMenuItem(("Phase Properties") & " - " & ("Solid"), Nothing, fs)

                    Dim pprops = GetType(IPhaseProperties).GetRuntimeProperties()

                    For Each pitem In pprops
                        p1.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('Overall').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                        p2.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('Vapor').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                        p3.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('OverallLiquid').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                        p4.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('Liquid1').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                        p5.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('Liquid2').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                        p6.Items.Add(CreateMenuItem(pitem.Name, Sub()
                                                                    InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                    InsertText(System.Environment.NewLine)
                                                                    InsertText(String.Format("value = obj.GetPhase('Solid').Properties.{0}", pitem.Name))
                                                                    InsertText(System.Environment.NewLine)
                                                                End Sub, fs))
                    Next

                    Dim pc = CreateMenuItem("Compounds", Nothing, fs)

                    itemtsmig.Items.AddRange({pc, p1, p2, p3, p4, p5, p6})

                    Dim ccprops = GetType(Interfaces.ICompoundConstantProperties).GetRuntimeProperties()
                    Dim cpprops = GetType(Interfaces.ICompound).GetRuntimeProperties()

                    For Each c In fs.SelectedCompounds.Values
                        Dim cx = CreateMenuItem(c.Name, Nothing, fs)
                        Dim c1 = CreateMenuItem(("Constant Properties"), Nothing, fs)
                        Dim c2 = CreateMenuItem(("Phase Properties"), Nothing, fs)
                        pc.Items.Add(cx)
                        cx.Items.Add(c1)
                        cx.Items.Add(c2)
                        For Each cc1 In ccprops
                            c1.Items.Add(CreateMenuItem(cc1.Name, Sub()
                                                                      InsertText("# Get Compound Constant Property: " & cc1.Name)
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("compound = Flowsheet.SelectedCompounds['{0}']", c.Name))
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("propval = compound.{0}", cc1.Name))
                                                                      InsertText(System.Environment.NewLine)
                                                                  End Sub, fs))
                        Next
                        For Each cp1 In cpprops
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Mixture Phase)", Sub()
                                                                                           InsertText("# Get Compound Property in Mixture (Overall) Phase: " & cp1.Name)
                                                                                           InsertText(System.Environment.NewLine)
                                                                                           InsertText(System.Environment.NewLine)
                                                                                           InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                           InsertText(System.Environment.NewLine)
                                                                                           InsertText(String.Format("propval = obj.GetPhase('Overall').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                           InsertText(System.Environment.NewLine)
                                                                                       End Sub, fs))
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Vapor Phase)", Sub()
                                                                                         InsertText("# Get Compound Property in Vapor Phase: " & cp1.Name)
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(String.Format("propval = obj.GetPhase('Vapor').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                         InsertText(System.Environment.NewLine)
                                                                                     End Sub, fs))
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Overall Liquid Phase)", Sub()
                                                                                                  InsertText("# Get Compound Property in Overall Liquid Phase: " & cp1.Name)
                                                                                                  InsertText(System.Environment.NewLine)
                                                                                                  InsertText(System.Environment.NewLine)
                                                                                                  InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                  InsertText(System.Environment.NewLine)
                                                                                                  InsertText(String.Format("propval = obj.GetPhase('OverallLiquid').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                  InsertText(System.Environment.NewLine)
                                                                                              End Sub, fs))
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Liquid Phase 1)", Sub()
                                                                                            InsertText("# Get Compound Property in Liquid Phase 1: " & cp1.Name)
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(String.Format("propval = obj.GetPhase('Liquid1').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                            InsertText(System.Environment.NewLine)
                                                                                        End Sub, fs))
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Liquid Phase 2)", Sub()
                                                                                            InsertText("# Get Compound Property in Liquid Phase 2: " & cp1.Name)
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                            InsertText(System.Environment.NewLine)
                                                                                            InsertText(String.Format("propval = obj.GetPhase('Liquid2').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                            InsertText(System.Environment.NewLine)
                                                                                        End Sub, fs))
                            c2.Items.Add(CreateMenuItem(cp1.Name & " (Solid Phase)", Sub()
                                                                                         InsertText("# Get Compound Property in Solid Phase: " & cp1.Name)
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                         InsertText(System.Environment.NewLine)
                                                                                         InsertText(String.Format("propval = obj.GetPhase('Solid').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                         InsertText(System.Environment.NewLine)
                                                                                     End Sub, fs))
                        Next
                    Next

                Else

                    Dim itemprops = item.GetType.GetRuntimeProperties()
                    Dim itemfields = item.GetType.GetRuntimeFields()

                    For Each pitem In itemprops

                        itemtsmis.Items.Add(
                        CreateMenuItem("Object Property: " & pitem.Name, Sub()
                                                                             InsertText("# Define Object Property: " & pitem.Name)
                                                                             InsertText(System.Environment.NewLine)
                                                                             InsertText(System.Environment.NewLine)
                                                                             InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                             InsertText(System.Environment.NewLine)
                                                                             InsertText(String.Format("obj.{0} = value", pitem.Name))
                                                                             InsertText(System.Environment.NewLine)
                                                                             If pitem.PropertyType.BaseType Is GetType([Enum]) Then
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText("# This property is an Enumeration (Enum) type.")
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(String.Format("# Full type name: {0}", pitem.PropertyType.ToString.Replace("+", ".")))
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText("# Accepted enumeration values:")
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 For Each etype In [Enum].GetNames(pitem.PropertyType)
                                                                                     InsertText(String.Format("# {0}.{1}", pitem.PropertyType.ToString.Replace("+", "."), etype))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 Next
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText("# example usage:")
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(String.Format("# obj.{0} = {1}.{2}", pitem.Name, pitem.PropertyType.ToString.Replace("+", "."), [Enum].GetNames(pitem.PropertyType)(0)))
                                                                             End If
                                                                         End Sub, fs))

                        itemtsmig.Items.Add(
                            CreateMenuItem("Object Property: " & pitem.Name, Sub()
                                                                                 InsertText("# Get Object Property: " & pitem.Name)
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 InsertText(String.Format("value = obj.{0}", pitem.Name))
                                                                                 InsertText(System.Environment.NewLine)
                                                                                 If pitem.PropertyType.BaseType Is GetType([Enum]) Then
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText("# This property is an Enumeration (Enum) type.")
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("# Full type name: {0}", pitem.PropertyType.ToString.Replace("+", ".")))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText("# Possible enumeration values:")
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     For Each etype In [Enum].GetNames(pitem.PropertyType)
                                                                                         InsertText(String.Format("# {0}.{1}", pitem.PropertyType.ToString.Replace("+", "."), etype))
                                                                                         InsertText(System.Environment.NewLine)
                                                                                     Next
                                                                                 End If
                                                                             End Sub, fs))

                    Next

                End If

            Next

            contextmenu.Items.Insert(0, gettsmi)
            contextmenu.Items.Insert(1, settsmi)

        End Sub

        Private Shared Function CreateMenuItem(text As String, clickaction As Action, flowsheet As IFlowsheet) As Eto.Forms.ButtonMenuItem

            Dim tsmi As New Eto.Forms.ButtonMenuItem With {.Text = text}
            AddHandler tsmi.Click, Sub()
                                       If clickaction IsNot Nothing Then flowsheet.RunCodeOnUIThread(clickaction)
                                   End Sub

            Return tsmi

        End Function

    End Class

End Namespace
