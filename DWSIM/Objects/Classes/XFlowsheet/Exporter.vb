Imports DWSIM.XFlowsheet.Implementation
Imports DWSIM.XFlowsheet.Interfaces
Imports System.Linq

Namespace XFlowsheet

    Public Class Exporter

        Public Shared Function Export(fs As Global.DWSIM.Interfaces.IFlowsheet) As String

            Dim fsx As New DefaultImplementations.Flowsheet With {
                .ID = Guid.NewGuid().ToString(),
                .Name = fs.FlowsheetOptions.SimulationName,
                .Description = fs.FlowsheetOptions.SimulationComments,
                .Compounds = fs.SelectedCompounds.Keys.ToList()
            }

            For Each pp In fs.PropertyPackages
                Dim ppx = New DefaultImplementations.PropertyPackage With {
                    .ID = pp.Value.UniqueID,
                    .Name = DirectCast(pp.Value, PropertyPackages.PropertyPackage).ComponentName,
                    .Description = DirectCast(pp.Value, PropertyPackages.PropertyPackage).ComponentDescription
                }
                Dim ppname = pp.Value.GetType().Name
                Select Case ppname
                    Case "PengRobinsonPropertyPackage"
                        ppx.Model = PropPackageModel.PR_EOS
                    Case "SRKPropertyPackage"
                        ppx.Model = PropPackageModel.SRK_EOS
                    Case "NRTLPropertyPackage"
                        ppx.Model = PropPackageModel.NRTL
                    Case "UNIQUACPropertyPackage"
                        ppx.Model = PropPackageModel.UNIQUAC
                    Case "UNIFACPropertyPackage"
                        ppx.Model = PropPackageModel.UNIFAC
                    Case "MODFACPropertyPackage"
                        ppx.Model = PropPackageModel.Mod_UNIFAC_Dortmund
                    Case "NISTMFACPropertyPackage"
                        ppx.Model = PropPackageModel.Mod_UNIFAC_NIST
                    Case "RaoultPropertyPackage"
                        ppx.Model = PropPackageModel.Ideal
                    Case "LKPPropertyPackage"
                        ppx.Model = PropPackageModel.Lee_Kesler_Plocker
                    Case "ChaoSeaderPropertyPackage"
                        ppx.Model = PropPackageModel.Chao_Seader
                    Case "GraysonStreedPropertyPackage"
                        ppx.Model = PropPackageModel.Grayson_Streed
                End Select
                fsx.PropertyPackages.Add(ppx)
            Next

            For Each sobj In fs.SimulationObjects.Values
                Dim sobjx = New DefaultImplementations.SimulationObject With {
                    .ID = sobj.Name,
                    .Name = sobj.GraphicObject.Tag,
                    .PFDObjectID = sobj.Name
                }
                Select Case sobj.GraphicObject.ObjectType
                    Case ObjectType.MaterialStream
                        Dim obj = DirectCast(sobj, Streams.MaterialStream)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.ObjectType = ObjType.MaterialStream
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Temperature",
                            .Name = .ID,
                            .Description = "Stream Temperature",
                            .Value = obj.GetTemperature(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Pressure",
                            .Name = .ID,
                            .Description = "Stream Pressure",
                            .Value = obj.GetPressure(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "MassEnthalpy",
                            .Name = .ID,
                            .Description = "Stream Mass Enthalpy",
                            .Value = obj.GetMassEnthalpy(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "MassEntropy",
                            .Name = .ID,
                            .Description = "Stream Mass Entropy",
                            .Value = obj.GetMassEntropy(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "VaporFraction",
                            .Name = .ID,
                            .Description = "Stream Vapor Fraction",
                            .Value = obj.Phases(2).Properties.molarfraction.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "OverallMolarFlow",
                            .Name = .ID,
                            .Description = "Stream Molar Flow",
                            .Value = obj.GetMolarFlow(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "OverallMolarComposition",
                            .Name = .ID,
                            .Description = "Stream Molar Composition",
                            .Value = obj.GetOverallComposition().ToList(),
                            .ValueType = ParamValueType.Type_ListDouble
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "FlashSpec",
                            .Name = .ID,
                            .Description = "Stream Flash Spec",
                            .Value = obj.GetFlashSpec(),
                            .ValueType = ParamValueType.Type_String
                        })
                    Case ObjectType.EnergyStream
                        Dim obj = DirectCast(sobj, EnergyStream)
                        sobjx.ObjectType = ObjType.EnergyStream
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "EnergyFlow",
                            .Name = .ID,
                            .Description = "Energy Flow",
                            .Value = obj.EnergyFlow.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Mixer
                        sobjx.ObjectType = ObjType.Mixer
                    Case ObjectType.Splitter
                        sobjx.ObjectType = ObjType.Splitter
                        Dim obj = DirectCast(sobj, UnitOperations.UnitOperations.Splitter)
                        sobjx.ObjectType = ObjType.Splitter
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "SplitRatios",
                            .Name = .ID,
                            .Description = "Split Ratios (0-1)",
                            .Value = obj.Ratios.ToDoubleArray().ToList(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Pump
                        sobjx.ObjectType = ObjType.Pump
                        Dim obj = DirectCast(sobj, Pump)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "PressureIncrease",
                            .Name = .ID,
                            .Description = "Pressure Increase",
                            .Value = obj.DeltaP.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Efficiency",
                            .Name = .ID,
                            .Description = "Efficiency",
                            .Value = obj.Eficiencia.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Valve
                        sobjx.ObjectType = ObjType.Valve
                        Dim obj = DirectCast(sobj, Valve)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "PressureDecrease",
                            .Name = .ID,
                            .Description = "Pressure Decrease",
                            .Value = obj.DeltaP.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Compressor
                        sobjx.ObjectType = ObjType.Compressor
                        Dim obj = DirectCast(sobj, Compressor)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "PressureIncrease",
                            .Name = .ID,
                            .Description = "Pressure Increase",
                            .Value = obj.DeltaP,
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "AdiabaticEfficiency",
                            .Name = .ID,
                            .Description = "Adiabatic Efficiency",
                            .Value = obj.AdiabaticEfficiency,
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Expander
                        sobjx.ObjectType = ObjType.Expander
                        Dim obj = DirectCast(sobj, Expander)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "PressureDecrease",
                            .Name = .ID,
                            .Description = "Pressure Decrease",
                            .Value = obj.DeltaP,
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "AdiabaticEfficiency",
                            .Name = .ID,
                            .Description = "Adiabatic Efficiency",
                            .Value = obj.AdiabaticEfficiency,
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Heater
                        sobjx.ObjectType = ObjType.Heater
                        Dim obj = DirectCast(sobj, Heater)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "HeatDuty",
                            .Name = .ID,
                            .Description = "Heat Duty",
                            .Value = obj.DeltaQ.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Efficiency",
                            .Name = .ID,
                            .Description = "Efficiency",
                            .Value = obj.Eficiencia.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Cooler
                        sobjx.ObjectType = ObjType.Cooler
                        Dim obj = DirectCast(sobj, Cooler)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "HeatDuty",
                            .Name = .ID,
                            .Description = "Heat Duty",
                            .Value = obj.DeltaQ.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Efficiency",
                            .Name = .ID,
                            .Description = "Efficiency",
                            .Value = obj.Eficiencia.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.HeatExchanger
                        sobjx.ObjectType = ObjType.HeatExchanger
                        Dim obj = DirectCast(sobj, HeatExchanger)
                        sobjx.PropertyPackageID = obj.PropertyPackage.UniqueID
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "HeatDuty",
                            .Name = .ID,
                            .Description = "Heat Duty",
                            .Value = obj.Q.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "OverallHTC",
                            .Name = .ID,
                            .Description = "Overall Heat Transfer Coefficient",
                            .Value = obj.OverallCoefficient.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "Efficiency",
                            .Name = .ID,
                            .Description = "Thermal Efficiency",
                            .Value = obj.ThermalEfficiency,
                            .ValueType = ParamValueType.Type_Double
                        })
                        sobjx.Parameters.Add(New DefaultImplementations.Parameter With {
                            .ID = "ExchangeArea",
                            .Name = .ID,
                            .Description = "Thermal Exchange Area",
                            .Value = obj.Area.GetValueOrDefault(),
                            .ValueType = ParamValueType.Type_Double
                        })
                    Case ObjectType.Vessel
                        sobjx.ObjectType = ObjType.SeparatorVessel
                End Select
                fsx.SimulationObjects.Add(sobjx)
            Next

            For Each gobj In fs.GraphicObjects.Values
                Select Case gobj.ObjectType
                    Case ObjectType.MaterialStream, ObjectType.EnergyStream, ObjectType.NodeIn, ObjectType.NodeOut,
                         ObjectType.Pump, ObjectType.Valve, ObjectType.Compressor, ObjectType.Expander,
                         ObjectType.HeatExchanger, ObjectType.Vessel, ObjectType.Heater, ObjectType.Cooler
                        Dim gobjx = New DefaultImplementations.PFDObject With {
                            .ID = gobj.Name,
                            .Name = gobj.Tag,
                            .Description = gobj.Description,
                            .HasAssociatedSimulationObject = True,
                            .AssociatedSimulationObjectID = gobj.Name,
                            .Height = gobj.Height,
                            .Width = gobj.Width,
                            .X = gobj.X,
                            .Y = gobj.Y
                        }
                        If gobj.ObjectType = ObjectType.MaterialStream Then
                            gobjx.ObjectType = ObjType.MaterialStream
                        ElseIf gobj.ObjectType = ObjectType.EnergyStream Then
                            gobjx.ObjectType = ObjType.EnergyStream
                        ElseIf gobj.ObjectType = ObjectType.NodeIn Then
                            gobjx.ObjectType = ObjType.Mixer
                        ElseIf gobj.ObjectType = ObjectType.NodeOut Then
                            gobjx.ObjectType = ObjType.Splitter
                        ElseIf gobj.ObjectType = ObjectType.Compressor Then
                            gobjx.ObjectType = ObjType.Compressor
                        ElseIf gobj.ObjectType = ObjectType.Expander Then
                            gobjx.ObjectType = ObjType.Expander
                        ElseIf gobj.ObjectType = ObjectType.Pump Then
                            gobjx.ObjectType = ObjType.Pump
                        ElseIf gobj.ObjectType = ObjectType.Valve Then
                            gobjx.ObjectType = ObjType.Valve
                        ElseIf gobj.ObjectType = ObjectType.Heater Then
                            gobjx.ObjectType = ObjType.Heater
                        ElseIf gobj.ObjectType = ObjectType.Cooler Then
                            gobjx.ObjectType = ObjType.Cooler
                        ElseIf gobj.ObjectType = ObjectType.HeatExchanger Then
                            gobjx.ObjectType = ObjType.HeatExchanger
                        ElseIf gobj.ObjectType = ObjectType.Vessel Then
                            gobjx.ObjectType = ObjType.SeparatorVessel
                        End If
                        For Each ip In gobj.InputConnectors
                            Dim ipx As New DefaultImplementations.ConnectionPort With {
                                .ID = Guid.NewGuid().ToString(),
                                .Name = ip.ConnectorName,
                                .Description = "",
                                .IsConnected = ip.IsAttached,
                                .RelativeX = ip.Position.X - gobj.X,
                                .RelativeY = ip.Position.Y - gobj.Y,
                                .IsInput = True
                            }
                            If ip.IsAttached Then
                                ipx.ConnectedToObjectID = ip.AttachedConnector.AttachedFrom.Name
                                ipx.ConnectedToObjectPortIndex = ip.AttachedConnector.AttachedFromConnectorIndex
                            End If
                            gobjx.Ports.Add(ipx)
                        Next
                        For Each ip In gobj.OutputConnectors
                            Dim ipx As New DefaultImplementations.ConnectionPort With {
                                .ID = Guid.NewGuid().ToString(),
                                .Name = ip.ConnectorName,
                                .Description = "",
                                .IsConnected = ip.IsAttached,
                                .RelativeX = ip.Position.X - gobj.X,
                                .RelativeY = ip.Position.Y - gobj.Y,
                                .IsOutput = True
                            }
                            If ip.IsAttached Then
                                ipx.ConnectedToObjectID = ip.AttachedConnector.AttachedTo.Name
                                ipx.ConnectedToObjectPortIndex = ip.AttachedConnector.AttachedToConnectorIndex
                            End If
                            gobjx.Ports.Add(ipx)
                        Next
                        fsx.PFDObjects.Add(gobjx)
                End Select
            Next

            Dim soptions As New Newtonsoft.Json.JsonSerializerSettings
            With soptions
                .Formatting = Newtonsoft.Json.Formatting.Indented
                .TypeNameHandling = Newtonsoft.Json.TypeNameHandling.Auto
            End With

            Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(fsx, soptions)

            Return jsondata

        End Function

    End Class

End Namespace

