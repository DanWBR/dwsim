using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using s = DWSIM.UI.Shared.Common;
using Eto.Drawing;

using StringResources = DWSIM.UI.Desktop.Shared.StringArrays;

namespace DWSIM.UI.Desktop.Editors
{
    public class GeneralEditors
    {

        public ISimulationObject SimObject;

        public DynamicLayout container;

        public GeneralEditors(ISimulationObject selectedobject, DynamicLayout layout)
        {
            SimObject = selectedobject;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat;
            var nff = SimObject.GetFlowsheet().FlowsheetOptions.FractionNumberFormat;

            s.CreateAndAddLabelRow(container, "OBJECT ID");

            s.CreateAndAddStringEditorRow(container, "Name", SimObject.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                SimObject.GraphicObject.Tag = arg3.Text;
            });

            s.CreateAndAddLabelRow(container, "PROPERTY PACKAGE");

            var proppacks = SimObject.GetFlowsheet().PropertyPackages.Values.Select((x) => x.Tag).ToList();

            if (proppacks.Count == 0)
            {
                SimObject.GetFlowsheet().ShowMessage("Error: please add at least one Property Package before continuing.", IFlowsheet.MessageType.GeneralError);
            }
            else
            {
                var pp = SimObject.PropertyPackage;
                string selectedpp = "";
                if (pp != null) selectedpp = pp.Tag;
                s.CreateAndAddDropDownRow(container, "Property Package", proppacks, proppacks.IndexOf(selectedpp), (DropDown arg1, EventArgs ev) =>
                {
                    SimObject.PropertyPackage = (IPropertyPackage)SimObject.GetFlowsheet().PropertyPackages.Values.Where((x) => x.Tag == proppacks[arg1.SelectedIndex]).FirstOrDefault();
                });
            }


            int posf = 0;
            switch (SimObject.PreferredFlashAlgorithmTag)
            {
                case "Default":
                case "":
                    posf = 0;
                    break;
                case "VLE":
                    posf = 1;
                    break;
                case "VLLE":
                    posf = 2;
                    break;
                case "LLE":
                    posf = 3;
                    break;
                case "SVLE":
                case "VSLE":
                case "VLSE":
                    posf = 4;
                    break;
            }
            s.CreateAndAddDropDownRow(container, "Flash Algorithm", StringResources.flashalg().ToList(), posf, (DropDown arg3, EventArgs ev) =>
            {
                switch (arg3.SelectedIndex)
                {
                    case 0:
                        SimObject.PreferredFlashAlgorithmTag = "Default";
                        break;
                    case 1:
                        SimObject.PreferredFlashAlgorithmTag = "VLE";
                        break;
                    case 2:
                        SimObject.PreferredFlashAlgorithmTag = "VLLE";
                        break;
                    case 3:
                        SimObject.PreferredFlashAlgorithmTag = "LLE";
                        break;
                    case 4:
                        SimObject.PreferredFlashAlgorithmTag = "SVLE";
                        break;
                }
            });

            s.CreateAndAddLabelRow(container, "OBJECT PROPERTIES");

            double val;
            switch (SimObject.GraphicObject.ObjectType)
            {
                case ObjectType.SolidSeparator:
                    var ss = (SolidsSeparator)SimObject;
                    s.CreateAndAddTextBoxRow(container, nf, "Solids Separation Efficiency", ss.SeparationEfficiency,
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           ss.SeparationEfficiency = Double.Parse(arg3.Text.ToString());
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Solids Separation Efficiency"));
                    s.CreateAndAddTextBoxRow(container, nf, "Liquids Separation Efficiency", ss.LiquidSeparationEfficiency,
                                        (TextBox arg3, EventArgs ev) =>
                                        {
                                            if (Double.TryParse(arg3.Text.ToString(), out val))
                                            {
                                                arg3.TextColor = (SystemColors.ControlText);
                                                ss.LiquidSeparationEfficiency = Double.Parse(arg3.Text.ToString());
                                            }
                                            else
                                            {
                                                arg3.TextColor = (Colors.Red);
                                            }
                                        });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Liquids Separation Efficiency"));
                    break;
                case ObjectType.EnergyStream:
                    var es = (EnergyStream)SimObject;
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Flow (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, es.EnergyFlow.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           es.EnergyFlow = cv.ConvertToSI(su.heatflow, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Heat Flow"));
                    break;
                case ObjectType.Compressor:
                    var ce = (Compressor)SimObject;
                    int pos1 = 0;
                    switch (ce.CalcMode)
                    {
                        case Compressor.CalculationMode.OutletPressure:
                            pos1 = 0;
                            break;
                        case Compressor.CalculationMode.Delta_P:
                            pos1 = 1;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.comprcalcmode().ToList(), pos1, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                ce.CalcMode =  Compressor.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                ce.CalcMode =  Compressor.CalculationMode.Delta_P;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Increase (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, ce.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           ce.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Increase"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, ce.POut.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.POut = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Efficiency (%)", ce.EficienciaAdiabatica.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.EficienciaAdiabatica = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Efficiency (%)"));
                    break;
                case ObjectType.Expander:
                    var xe = (UnitOperations.UnitOperations.Expander)SimObject;
                    int pos1e = 0;
                    switch (xe.CalcMode)
                    {
                        case UnitOperations.UnitOperations.Expander.CalculationMode.OutletPressure:
                            pos1e = 0;
                            break;
                        case UnitOperations.UnitOperations.Expander.CalculationMode.Delta_P:
                            pos1e = 1;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.comprcalcmode().ToList(), pos1e, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                xe.CalcMode = UnitOperations.UnitOperations.Expander.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                xe.CalcMode = UnitOperations.UnitOperations.Expander.CalculationMode.Delta_P;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Decrease (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, xe.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           xe.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Decrease"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, xe.POut.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.POut = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Efficiency (%)", xe.EficienciaAdiabatica.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.EficienciaAdiabatica = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Efficiency (%)"));
                    break;
                case ObjectType.Heater:
                    var hc = (Heater)SimObject;
                    int pos3 = 0;
                    switch (hc.CalcMode)
                    {
                        case Heater.CalculationMode.HeatAdded:
                            pos3 = 0;
                            break;
                        case Heater.CalculationMode.OutletTemperature:
                            pos3 = 1;
                            break;
                        case Heater.CalculationMode.OutletVaporFraction:
                            pos3 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.heatercalcmode().ToList(), pos3, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                hc.CalcMode = Heater.CalculationMode.HeatAdded;
                                break;
                            case 1:
                                hc.CalcMode = Heater.CalculationMode.OutletTemperature;
                                break;
                            case 2:
                                hc.CalcMode = Heater.CalculationMode.OutletVaporFraction;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, hc.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           hc.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Drop"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, hc.OutletTemperature.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Added (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, hc.DeltaQ.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.DeltaQ = cv.ConvertToSI(su.heatflow, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Heat Added"));
                    s.CreateAndAddTextBoxRow(container, nf, "Efficiency (%)", hc.Eficiencia.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.Eficiencia = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Vapor Fraction", hc.OutletVaporFraction.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.OutletVaporFraction = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Vapor Fraction"));
                    break;
                case ObjectType.Cooler:
                    var cc = (Cooler)SimObject;
                    int pos3c = 0;
                    switch (cc.CalcMode)
                    {
                        case Cooler.CalculationMode.HeatRemoved:
                            pos3c = 0;
                            break;
                        case Cooler.CalculationMode.OutletTemperature:
                            pos3c = 1;
                            break;
                        case Cooler.CalculationMode.OutletVaporFraction:
                            pos3c = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.heatercalcmode().ToList(), pos3c, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                cc.CalcMode = Cooler.CalculationMode.HeatRemoved;
                                break;
                            case 1:
                                cc.CalcMode = Cooler.CalculationMode.OutletTemperature;
                                break;
                            case 2:
                                cc.CalcMode = Cooler.CalculationMode.OutletVaporFraction;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, cc.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           cc.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Drop"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, cc.OutletTemperature.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Removed (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, cc.DeltaQ.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.DeltaQ = cv.ConvertToSI(su.heatflow, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Heat Removed"));
                    s.CreateAndAddTextBoxRow(container, nf, "Efficiency (%)", cc.Eficiencia.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.Eficiencia = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Vapor Fraction", cc.OutletVaporFraction.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.OutletVaporFraction = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Vapor Fraction"));
                    break;
                case ObjectType.Pump:
                    var pump = (Pump)SimObject;
                    int pos4 = 0;
                    switch (pump.CalcMode)
                    {
                        case Pump.CalculationMode.OutletPressure:
                            pos4 = 0;
                            break;
                        case Pump.CalculationMode.Delta_P:
                            pos4 = 1;
                            break;
                        case Pump.CalculationMode.Power:
                            pos4 = 2;
                            break;
                        case Pump.CalculationMode.EnergyStream:
                            pos4 = 3;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.pumpcalcmode().ToList(), pos4, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                pump.CalcMode = Pump.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                pump.CalcMode = Pump.CalculationMode.Delta_P;
                                break;
                            case 2:
                                pump.CalcMode = Pump.CalculationMode.Power;
                                break;
                            case 3:
                                pump.CalcMode = Pump.CalculationMode.EnergyStream;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Increase (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, pump.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (Double.TryParse(arg3.Text.ToString(), out val))
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           pump.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Increase"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, pump.Pout),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.Pout = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Efficiency (%)", pump.Eficiencia.GetValueOrDefault(),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.Eficiencia = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Power (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, pump.DeltaQ.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.DeltaQ = cv.ConvertToSI(su.heatflow, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Power"));
                    break;
                case ObjectType.NodeIn:
                    var mix = (Mixer)SimObject;
                    int pos2 = 0;
                    switch (mix.PressureCalculation)
                    {
                        case Mixer.PressureBehavior.Minimum:
                            pos2 = 0;
                            break;
                        case Mixer.PressureBehavior.Average:
                            pos2 = 1;
                            break;
                        case Mixer.PressureBehavior.Maximum:
                            pos2 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Pressure Calculation Mode", StringResources.mixercalcmode().ToList(), pos2, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                mix.PressureCalculation = Mixer.PressureBehavior.Minimum;
                                break;
                            case 1:
                                mix.PressureCalculation = Mixer.PressureBehavior.Average;
                                break;
                            case 2:
                                mix.PressureCalculation = Mixer.PressureBehavior.Maximum;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Calculation Mode"));
                    break;
                case ObjectType.Valve:
                    var valve = (Valve)SimObject;
                    int pos5 = 0;
                    switch (valve.CalcMode)
                    {
                        case Valve.CalculationMode.DeltaP:
                            pos5 = 1;
                            break;
                        case Valve.CalculationMode.OutletPressure:
                            pos5 = 0;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.valvecalcmode().ToList(), pos5, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                valve.CalcMode = Valve.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                valve.CalcMode = Valve.CalculationMode.DeltaP;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, valve.OutletPressure.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               valve.OutletPressure = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, valve.DeltaP.GetValueOrDefault()),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       valve.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    break;
                case ObjectType.ShortcutColumn:
                    var sc = (ShortcutColumn)SimObject;
                    int poshk, poslk;
                    var comps = SimObject.GetFlowsheet().SelectedCompounds.Values.Select((x) => x.Name).ToList();
                    comps.Insert(0, "");
                    poslk = comps.ToList().IndexOf(sc.m_lightkey);
                    poshk = comps.ToList().IndexOf(sc.m_heavykey);
                    s.CreateAndAddDropDownRow(container, "Light Key Compound", comps, poslk, (DropDown arg3, EventArgs ev) =>
                    {
                        sc.m_lightkey = comps.ToList()[arg3.SelectedIndex];
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Light Key Compound"));
                    s.CreateAndAddDropDownRow(container, "Heavy Key Compound", comps, poshk, (DropDown arg3, EventArgs ev) =>
                    {
                        sc.m_heavykey = comps.ToList()[arg3.SelectedIndex];
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Heavy Key Compound"));
                    s.CreateAndAddTextBoxRow(container, nf, "LK Mole Fraction in Bottoms", sc.m_lightkeymolarfrac,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_lightkeymolarfrac = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("LK Mole Fraction in Bottoms"));
                    s.CreateAndAddTextBoxRow(container, nf, "HK Mole Fraction in Distillate", sc.m_heavykeymolarfrac,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_heavykeymolarfrac = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("HK Mole Fraction in Bottoms"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reflux Ratio", sc.m_refluxratio,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_refluxratio = Double.Parse(arg3.Text.ToString());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reflux Ratio"));
                    s.CreateAndAddTextBoxRow(container, nf, "Condenser Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, sc.m_condenserpressure),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_condenserpressure = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Condenser Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reboiler Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, sc.m_boilerpressure),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_boilerpressure = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reboiler Pressure"));
                    int pos6 = 0;
                    switch (sc.condtype)
                    {
                        case ShortcutColumn.CondenserType.TotalCond:
                            pos6 = 0;
                            break;
                        case ShortcutColumn.CondenserType.PartialCond:
                            pos6 = 1;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Condenser Type", StringResources.condensertype().ToList(), pos6, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                sc.condtype = ShortcutColumn.CondenserType.TotalCond;
                                break;
                            case 1:
                                sc.condtype = ShortcutColumn.CondenserType.PartialCond;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Condenser Type"));
                    break;
                case ObjectType.HeatExchanger:
                    var hx = (HeatExchanger)SimObject;
                    int pos7 = 0;
                    switch (hx.CalculationMode)
                    {
                        case HeatExchangerCalcMode.CalcTempColdOut:
                            pos7 = 0;
                            break;
                        case HeatExchangerCalcMode.CalcTempHotOut:
                            pos7 = 1;
                            break;
                        case HeatExchangerCalcMode.CalcBothTemp:
                            pos7 = 2;
                            break;
                        case HeatExchangerCalcMode.CalcArea:
                            pos7 = 3;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.hxcalcmode().ToList(), pos7, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcTempColdOut;
                                break;
                            case 1:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcTempHotOut;
                                break;
                            case 2:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcBothTemp;
                                break;
                            case 3:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcArea;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    int pos9 = 0;
                    switch (hx.FlowDir)
                    {
                        case FlowDirection.CoCurrent:
                            pos9 = 0;
                            break;
                        case FlowDirection.CounterCurrent:
                            pos9 = 1;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Flow Direction", StringResources.hxflowdir().ToList(), pos9, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                hx.FlowDir = FlowDirection.CoCurrent;
                                break;
                            case 1:
                                hx.FlowDir = FlowDirection.CounterCurrent;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Flow Direction"));
                    int pos8 = 0;
                    switch (hx.DefinedTemperature)
                    {
                        case SpecifiedTemperature.Cold_Fluid:
                            pos8 = 0;
                            break;
                        case SpecifiedTemperature.Hot_Fluid:
                            pos8 = 1;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Defined Temperature (for Calc Area Mode)", StringResources.hxspectemp().ToList(), pos8, (DropDown arg3, EventArgs e2v) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                hx.DefinedTemperature = SpecifiedTemperature.Cold_Fluid;
                                break;
                            case 1:
                                hx.DefinedTemperature = SpecifiedTemperature.Hot_Fluid;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Defined Temperature (for Calc Area Mode)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (Hot Fluid) (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, hx.HotSidePressureDrop),
                               (TextBox arg3, EventArgs ev2) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       hx.HotSidePressureDrop = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop (Hot Fluid)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (Cold Fluid) (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, hx.ColdSidePressureDrop),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       hx.ColdSidePressureDrop = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop (Cold Fluid)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (Cold Fluid) (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, hx.ColdSideOutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.ColdSideOutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature (Cold Fluid)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (Hot Fluid) (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, hx.HotSideOutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.HotSideOutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature (Hot Fluid)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Overall HTC (" + su.heat_transf_coeff + ")", cv.ConvertFromSI(su.heat_transf_coeff, hx.OverallCoefficient.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.OverallCoefficient = cv.ConvertToSI(su.heat_transf_coeff, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Overall HTC"));
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Exchange Area (" + su.area + ")", cv.ConvertFromSI(su.heat_transf_coeff, hx.Area.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.Area = cv.ConvertToSI(su.area, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Heat Exchange Area"));
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Exchanged (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, hx.Q.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.Q = cv.ConvertToSI(su.heatflow, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Heat Exchanged"));
                    break;
                case ObjectType.RCT_Conversion:
                    var reactor = (Reactor_Conversion)SimObject;
                    int pos10 = 0;
                    switch (reactor.ReactorOperationMode)
                    {
                        case OperationMode.Adiabatic:
                            pos10 = 0;
                            break;
                        case OperationMode.Isothermic:
                            pos10 = 1;
                            break;
                        case OperationMode.OutletTemperature:
                            pos10 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode().ToList(), pos10, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                reactor.ReactorOperationMode = OperationMode.Adiabatic;
                                break;
                            case 1:
                                reactor.ReactorOperationMode = OperationMode.Isothermic;
                                break;
                            case 2:
                                reactor.ReactorOperationMode = OperationMode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, reactor.DeltaP.GetValueOrDefault()),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    break;
                case ObjectType.RCT_Equilibrium:
                    var reactor2 = (Reactor_Equilibrium)SimObject;
                    int pos11 = 0;
                    switch (reactor2.ReactorOperationMode)
                    {
                        case OperationMode.Adiabatic:
                            pos11 = 0;
                            break;
                        case OperationMode.Isothermic:
                            pos11 = 1;
                            break;
                        case OperationMode.OutletTemperature:
                            pos11 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode().ToList(), pos11, (DropDown arg3, EventArgs ve) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                reactor2.ReactorOperationMode = OperationMode.Adiabatic;
                                break;
                            case 1:
                                reactor2.ReactorOperationMode = OperationMode.Isothermic;
                                break;
                            case 2:
                                reactor2.ReactorOperationMode = OperationMode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor2.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor2.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, reactor2.DeltaP.GetValueOrDefault()),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor2.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    break;
                case ObjectType.RCT_Gibbs:
                    var reactor2g = (Reactor_Gibbs)SimObject;
                    int pos11g = 0;
                    switch (reactor2g.ReactorOperationMode)
                    {
                        case OperationMode.Adiabatic:
                            pos11g = 0;
                            break;
                        case OperationMode.Isothermic:
                            pos11g = 1;
                            break;
                        case OperationMode.OutletTemperature:
                            pos11g = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode().ToList(), pos11g, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                reactor2g.ReactorOperationMode = OperationMode.Adiabatic;
                                break;
                            case 1:
                                reactor2g.ReactorOperationMode = OperationMode.Isothermic;
                                break;
                            case 2:
                                reactor2g.ReactorOperationMode = OperationMode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor2g.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor2g.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, reactor2g.DeltaP.GetValueOrDefault()),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor2g.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    int i, j;
                    string elmatrix, elements;
                    elements = "";
                    for (i = 0; i < reactor2g.Elements.Count(); i++)
                    {
                        elements += reactor2g.Elements[i] + " ";
                    }
                    elements = elements.TrimEnd(' ');
                    elmatrix = "";
                    for (i = 0; i < reactor2g.ComponentIDs.Count; i++)
                    {
                        for (j = 0; j < reactor2g.Elements.Count(); j++)
                        {
                            try
                            {
                                elmatrix += reactor2g.ElementMatrix[j, i].ToString("G") + " ";
                            }
                            catch (Exception) { }
                        }
                        elmatrix = elmatrix.TrimEnd(' ');
                        elmatrix += "\n";
                    }
                    elmatrix = elmatrix.TrimEnd('\n');
                    var compounds = SimObject.GetFlowsheet().SelectedCompounds.Values.Select((x) => x.Name).ToList();
                    s.CreateAndAddLabelRow(container, "Reacting Compounds");
                    s.CreateAndAddDescriptionRow(container, "If you add or remove compounds from the reacting compounds " +
                                                 "list, close and reopen the editor before setting the element list and element matrix.");
                    var ids = reactor2g.ComponentIDs.ToArray();
                    string comptext = "";
                    foreach (string compi in ids)
                    {
                        comptext += compi + ", ";
                        if (!compounds.Contains(compi))
                        {
                            reactor2g.ComponentIDs.Remove(compi);
                        }
                    }
                    comptext = comptext.TrimEnd(' ').TrimEnd(',');
                    foreach (string comp in compounds)
                        s.CreateAndAddCheckBoxRow(container, 
                                                   comp,
                                                   reactor2g.ComponentIDs.Contains(comp),
                                                   (CheckBox arg2, EventArgs ev) =>
                                                   {
                                                       if (reactor2g.ComponentIDs.Contains(comp))
                                                       {
                                                           reactor2g.ComponentIDs.Remove(comp);
                                                       }
                                                       else
                                                       {
                                                           reactor2g.ComponentIDs.Add(comp);
                                                       }
                                                   });
                    s.CreateAndAddLabelRow(container, "Elements");
                    s.CreateAndAddDescriptionRow(container, "Enter the list of elements, separated by spaces");
                    var txtel = s.CreateAndAddFullTextBoxRow(container, elements,
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       try
                                       {
                                           var els = arg3.Text.Trim().Split(' ');
                                           reactor2g.Elements = els;
                                       }
                                       catch (Exception)
                                       {
                                           //Toast.MakeText(this.Context, "Error parsing element list: " + ex.Message, ToastLength.Long).Show();
                                       }
                                   });
                    txtel.PlaceholderText = "Enter the list of elements, separated by spaces";
                    s.CreateAndAddLabelRow(container, "Element Matrix");
                    s.CreateAndAddDescriptionRow(container, "Element Matrix for compounds in the following order: " + comptext);
                    var txtelm = s.CreateAndAddFullTextBoxRow(container, elmatrix, (TextBox arg3, EventArgs ev) =>
                    {
                        try
                        {
                            reactor2g.ElementMatrix = new Double[reactor2g.Elements.Count(), reactor2g.ComponentIDs.Count];
                            var ell = arg3.Text.Split('\n');
                            int i2, j2;
                            i2 = 0;
                            foreach (string line in ell)
                            {
                                j2 = 0;
                                var els = line.Split(' ');
                                foreach (string el in els)
                                {
                                    if (s.IsValidDouble(el)) { reactor2g.ElementMatrix[j2, i2] = Double.Parse(el); j2 += 1; }
                                }
                                i2 += 1;
                            }
                        }
                        catch (Exception)
                        {
                            //Toast.MakeText(this.Context, "Error parsing element matrix: " + ex.Message, ToastLength.Long).Show();
                        }
                    });
                    txtelm.PlaceholderText = "Enter the matrix of element amounts, separated by spaces, one line for each compound";
                    break;
                case ObjectType.RCT_CSTR:
                    var reactor3 = (Reactor_CSTR)SimObject;
                    int pos12 = 0;
                    switch (reactor3.ReactorOperationMode)
                    {
                        case OperationMode.Adiabatic:
                            pos12 = 0;
                            break;
                        case OperationMode.Isothermic:
                            pos12 = 1;
                            break;
                        case OperationMode.OutletTemperature:
                            pos12 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode().ToList(), pos12, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                reactor3.ReactorOperationMode = OperationMode.Adiabatic;
                                break;
                            case 1:
                                reactor3.ReactorOperationMode = OperationMode.Isothermic;
                                break;
                            case 2:
                                reactor3.ReactorOperationMode = OperationMode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor3.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor3.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, reactor3.DeltaP.GetValueOrDefault()),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.DeltaP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reactor Volume (" + su.volume + ")", cv.ConvertFromSI(su.volume, reactor3.Volume),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.Volume = cv.ConvertToSI(su.volume, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Volume"));
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Amount (" + su.mass + ")", cv.ConvertFromSI(su.mass, reactor3.CatalystAmount),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.CatalystAmount = cv.ConvertToSI(su.mass, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Catalyst Amount"));
                    break;
                case ObjectType.RCT_PFR:
                    var reactor4 = (Reactor_PFR)SimObject;
                    int pos13 = 0;
                    switch (reactor4.ReactorOperationMode)
                    {
                        case OperationMode.Adiabatic:
                            pos13 = 0;
                            break;
                        case OperationMode.Isothermic:
                            pos13 = 1;
                            break;
                        case OperationMode.OutletTemperature:
                            pos13 = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode().ToList(), pos13, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                reactor4.ReactorOperationMode = OperationMode.Adiabatic;
                                break;
                            case 1:
                                reactor4.ReactorOperationMode = OperationMode.Isothermic;
                                break;
                            case 2:
                                reactor4.ReactorOperationMode = OperationMode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor4.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (Double.TryParse(arg3.Text.ToString(), out val))
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor4.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reactor Volume (" + su.volume + ")", cv.ConvertFromSI(su.volume, reactor4.Volume),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.Volume = cv.ConvertToSI(su.volume, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Volume"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reactor Length (" + su.distance + ")", cv.ConvertFromSI(su.distance, reactor4.Length),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.Length = cv.ConvertToSI(su.distance, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Length"));
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Loading (" + su.density + ")", cv.ConvertFromSI(su.volume, reactor4.CatalystLoading),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.CatalystLoading = cv.ConvertToSI(su.density, Double.Parse(arg3.Text.ToString()));
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Catalyst Loading"));
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Diameter (" + su.diameter + ")", cv.ConvertFromSI(su.diameter, reactor4.CatalystParticleDiameter),
                              (TextBox arg3, EventArgs ev) =>
                              {
                                  if (Double.TryParse(arg3.Text.ToString(), out val))
                                  {
                                      arg3.TextColor = (SystemColors.ControlText);
                                      reactor4.CatalystParticleDiameter = cv.ConvertToSI(su.diameter, Double.Parse(arg3.Text.ToString()));
                                  }
                                  else
                                  {
                                      arg3.TextColor = (Colors.Red);
                                  }
                              });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Catalyst Diameter"));
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Void Fraction", reactor4.CatalystVoidFraction,
                             (TextBox arg3, EventArgs ev) =>
                             {
                                 if (Double.TryParse(arg3.Text.ToString(), out val))
                                 {
                                     arg3.TextColor = (SystemColors.ControlText);
                                     reactor4.CatalystVoidFraction = Double.Parse(arg3.Text.ToString());
                                 }
                                 else
                                 {
                                     arg3.TextColor = (Colors.Red);
                                 }
                             });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Catalyst Void Fraction"));
                    break;
                case ObjectType.ComponentSeparator:
                    var csep = (ComponentSeparator)SimObject;
                    s.CreateAndAddDropDownRow(container, "Specified Stream", StringResources.csepspecstream().ToList(), csep.SpecifiedStreamIndex, (DropDown arg3, EventArgs ev) =>
                    {
                        csep.SpecifiedStreamIndex = (byte)arg3.SelectedIndex;
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Specified Stream"));
                    s.CreateAndAddLabelRow(container, "COMPOUND SEPARATION SPECS");
                    foreach (ICompoundConstantProperties comp in SimObject.GetFlowsheet().SelectedCompounds.Values)
                    {
                        if (!csep.ComponentSepSpecs.ContainsKey(comp.Name))
                        {
                            csep.ComponentSepSpecs.Add(comp.Name,
                                                       new DWSIM.UnitOperations.UnitOperations.Auxiliary.ComponentSeparationSpec(comp.Name, DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.PercentInletMassFlow, 0.0f, "%"));
                        }
                    }
                    foreach (DWSIM.UnitOperations.UnitOperations.Auxiliary.ComponentSeparationSpec cs in csep.ComponentSepSpecs.Values)
                    {
                        int posx = 0;
                        switch (cs.SepSpec)
                        {
                            case DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.PercentInletMassFlow:
                                posx = 0;
                                break;
                            case DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.PercentInletMolarFlow:
                                posx = 1;
                                break;
                            case DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.MassFlow:
                                posx = 2;
                                break;
                            case DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.MolarFlow:
                                posx = 3;
                                break;
                        }
                        s.CreateAndAddDropDownRow(container, cs.ComponentID + " Separation Spec Type", StringResources.csepspectype().ToList(), posx, (DropDown arg3, EventArgs ev) =>
                        {
                            switch (arg3.SelectedIndex)
                            {
                                case 0:
                                    cs.SepSpec = DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.PercentInletMassFlow;
                                    break;
                                case 1:
                                    cs.SepSpec = DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.PercentInletMolarFlow;
                                    break;
                                case 2:
                                    cs.SepSpec = DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.MassFlow;
                                    break;
                                case 3:
                                    cs.SepSpec = DWSIM.UnitOperations.UnitOperations.Auxiliary.SeparationSpec.MolarFlow;
                                    break;
                            }
                        });
                        s.CreateAndAddTextBoxRow(container, nf, cs.ComponentID + " Separation Spec Value", cs.SpecValue,
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        cs.SpecValue = Double.Parse(arg3.Text.ToString());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                        var units = StringResources.cspecunit().ToList();
                        s.CreateAndAddDropDownRow(container, cs.ComponentID + " Separation Spec Units", units.ToList(), units.IndexOf(cs.SpecUnit), (DropDown arg3, EventArgs ev) =>
                        {
                            cs.SpecUnit = units[arg3.SelectedIndex];
                        });
                    };
                    break;
                case ObjectType.NodeOut:
                    var splitter = (DWSIM.UnitOperations.UnitOperations.Splitter)SimObject;
                    int poss = 0;
                    switch (splitter.OperationMode)
                    {
                        case DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.SplitRatios:
                            poss = 0;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec:
                            poss = 1;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec:
                            poss = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Specification", StringResources.splittercalcmode().ToList(), poss, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                splitter.OperationMode = UnitOperations.UnitOperations.Splitter.OpMode.SplitRatios;
                                break;
                            case 1:
                                splitter.OperationMode = UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec;
                                break;
                            case 2:
                                splitter.OperationMode = UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Specification"));
                    s.CreateAndAddTextBoxRow(container, nf, "Split Ratio Stream 1", (double)splitter.Ratios[0],
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[0] = Double.Parse(arg3.Text.ToString());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Split Ratio Stream 1"));
                    s.CreateAndAddTextBoxRow(container, nf, "Split Ratio Stream 2", (double)splitter.Ratios[1],
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[1] = Double.Parse(arg3.Text.ToString());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Split Ratio Stream 2"));
                    s.CreateAndAddTextBoxRow(container, nf, "Split Ratio Stream 3", (double)splitter.Ratios[2],
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[2] = Double.Parse(arg3.Text.ToString());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Split Ratio Stream 3"));
                    double sm1 = 0.0f;
                    if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                    {
                        sm1 = cv.ConvertFromSI(su.massflow, splitter.StreamFlowSpec);
                    }
                    else
                    {
                        sm1 = cv.ConvertFromSI(su.molarflow, splitter.StreamFlowSpec);
                    }
                    s.CreateAndAddTextBoxRow(container, nf, "Stream 1 Mass/Mole Flow Spec (" + su.massflow + "|" + su.molarflow + ")", sm1,
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                                        {
                                            splitter.StreamFlowSpec = cv.ConvertToSI(su.massflow, Double.Parse(arg3.Text.ToString()));
                                        }
                                        else
                                        {
                                            splitter.StreamFlowSpec = cv.ConvertToSI(su.molarflow, Double.Parse(arg3.Text.ToString()));
                                        }
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Stream 1 Mass/Mole Flow Spec"));
                    double sm2 = 0.0f;
                    if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                    {
                        sm2 = cv.ConvertFromSI(su.massflow, splitter.Stream2FlowSpec);
                    }
                    else
                    {
                        sm2 = cv.ConvertFromSI(su.molarflow, splitter.Stream2FlowSpec);
                    }
                    s.CreateAndAddTextBoxRow(container, nf, "Stream 2 Mass/Mole Flow Spec (" + su.massflow + "|" + su.molarflow + ")", sm2,
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                                        {
                                            splitter.Stream2FlowSpec = cv.ConvertToSI(su.massflow, Double.Parse(arg3.Text.ToString()));
                                        }
                                        else
                                        {
                                            splitter.Stream2FlowSpec = cv.ConvertToSI(su.molarflow, Double.Parse(arg3.Text.ToString()));
                                        }
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Stream 2 Mass/Mole Flow Spec"));
                    break;
                case ObjectType.Pipe:
                    var pipe = (Pipe)SimObject;
                    int posp = 0;
                    switch (pipe.Specification)
                    {
                        case Pipe.specmode.Length:
                            posp = 0;
                            break;
                        case Pipe.specmode.OutletPressure:
                            posp = 1;
                            break;
                        case Pipe.specmode.OutletTemperature:
                            posp = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.pipecalcmode().ToList(), posp, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                pipe.Specification = Pipe.specmode.Length;
                                break;
                            case 1:
                                pipe.Specification = Pipe.specmode.OutletPressure;
                                break;
                            case 2:
                                pipe.Specification = Pipe.specmode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, pipe.OutletPressure),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.OutletPressure = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, pipe.OutletTemperature),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.OutletTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Convergence Tolerance (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, pipe.TolP),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.TolP = cv.ConvertToSI(su.deltaP, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Convergence Tolerance"));
                    s.CreateAndAddTextBoxRow(container, nf, "Temperature Convergence Tolerance (" + su.deltaT + ")", cv.ConvertFromSI(su.deltaT, pipe.TolT),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.TolT = cv.ConvertToSI(su.deltaT, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Temperature Convergence Tolerance"));
                    s.CreateAndAddCheckBoxRow(container, "Include Joule-Thomson Effect", pipe.IncludeJTEffect, (CheckBox arg2, EventArgs ev) =>
                    {
                        pipe.IncludeJTEffect = arg2.Checked.GetValueOrDefault();
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Include Joule-Thomson Effect"));
                    s.CreateAndAddButtonRow(container, "Edit Hydraulic Profile", null, (Button arg1, EventArgs ev) =>
                    {
                        //var alert = new AlertDialog.Builder(this.Context);
                        //var myview = new PipeHydraulicProfileView(this.Context, SimObject.GetFlowsheet(), pipe.Profile);
                        //LayoutParams param = new LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent);
                        //myview.LayoutParameters = param;
                        //alert.SetView(myview);
                        //alert.Create().Show();
                    });
                    s.CreateAndAddButtonRow(container, "Edit Thermal Profile", null, (Button arg1, EventArgs ev) =>
                    {
                        //var alert = new AlertDialog.Builder(this.Context);
                        //var myview = new PipeThermalProfileView(this.Context, SimObject.GetFlowsheet(), pipe.ThermalProfile);
                        //LayoutParams param = new LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent);
                        //myview.LayoutParameters = param;
                        //alert.SetView(myview);
                        //alert.Create().Show();
                    });
                    break;
                case ObjectType.Vessel:
                    var vessel = (Vessel)SimObject;
                    s.CreateAndAddCheckBoxRow(container, "Override Separation Pressure", vessel.OverrideP, (CheckBox arg2, EventArgs ev) =>
                    {
                        vessel.OverrideP = arg2.Checked.GetValueOrDefault();
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Override Separation Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Separation Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, vessel.FlashPressure),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        vessel.FlashPressure = cv.ConvertToSI(su.pressure, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Separation Pressure"));
                    s.CreateAndAddCheckBoxRow(container, "Override Separation Temperature", vessel.OverrideT, (CheckBox arg2, EventArgs ev) =>
                    {
                        vessel.OverrideT = arg2.Checked.GetValueOrDefault();
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Override Separation Temperature"));
                    s.CreateAndAddTextBoxRow(container, nf, "Separation Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, vessel.FlashTemperature),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (Double.TryParse(arg3.Text.ToString(), out val))
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        vessel.FlashTemperature = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text.ToString()));
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Separation Temperature"));
                    break;
                case ObjectType.DistillationColumn:
                    //var editor = new DistillationColumnEditorView(this.Context, (DistillationColumn)SimObject);
                    //var ll = (LinearLayout)FindViewById(Resource.IdEditors.inputPropertiesContainer);
                    //ll.AddView(editor);
                    break;
                case ObjectType.AbsorptionColumn:
                    //var editor2 = new AbsColumnEditorView(this.Context, (AbsorptionColumn)SimObject);
                    //var ll2 = (LinearLayout)FindViewById(Resource.IdEditors.inputPropertiesContainer);
                    //ll2.AddView(editor2);
                    break;
                case ObjectType.OT_Adjust:
                    //var editor3 = new AdjustEditorView(this.Context, (Adjust)SimObject);
                    //var ll3 = (LinearLayout)FindViewById(Resource.IdEditors.inputPropertiesContainer);
                    //ll3.AddView(editor3);
                    break;
            }
        }

    }

}
