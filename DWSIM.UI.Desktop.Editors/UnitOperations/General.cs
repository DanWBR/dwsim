using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using s = DWSIM.UI.Shared.Common;
using Eto.Drawing;

using StringResources = DWSIM.UI.Desktop.Shared.StringArrays;
using System.Diagnostics;
using System.IO;

using DWSIM.ExtensionMethods;
using DWSIM.ExtensionMethods.Eto;

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

        void CallSolverIfNeeded()
        {
            if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke();
        }

        void Initialize()
        {

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat;
            var nff = SimObject.GetFlowsheet().FlowsheetOptions.FractionNumberFormat;

            s.CreateAndAddLabelRow(container, "Object Property Editor");

            s.CreateAndAddDescriptionRow(container, "Property values are updated/stored as they are changed/edited. There's no need to press ENTER to commit the changes.");

            if ((Inspector.Host.Items.Where(x => x.Name.Contains(SimObject.GraphicObject.Tag)).Count() > 0))
            {
                var ctn = new DynamicLayout();
                ctn.BackgroundColor = Colors.LightGrey;
                s.CreateAndAddLabelRow(ctn, "Inspector Reports");
                s.CreateAndAddLabelAndButtonRow(ctn, "An Inspector Report is ready for viewing.", "View Report", null, (btn, e) =>
                {
                    var f = s.GetDefaultEditorForm("Inspector Report for '" + SimObject.GraphicObject.Tag + "'", 1024, 768, Inspector.Window2_Eto.GetInspectorWindow(SimObject), false);
                    f.Show();
                });
                container.Add(ctn);
            }

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Type", SimObject.GetDisplayName());

            s.CreateAndAddTwoLabelsRow(container, "Status", SimObject.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", SimObject.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                SimObject.GraphicObject.Tag = arg3.Text;
                SimObject.GetFlowsheet().UpdateInterface();
            }, () =>
            {
                SimObject.GetFlowsheet().UpdateOpenEditForms();
            });

            s.CreateAndAddLabelRow(container, "Property Package");

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
                    if (proppacks.Count > 0) SimObject.PropertyPackage = (IPropertyPackage)SimObject.GetFlowsheet().PropertyPackages.Values.Where((x) => x.Tag == proppacks[arg1.SelectedIndex]).FirstOrDefault();
                });
            }

            s.CreateAndAddLabelRow(container, "Object Properties");

            switch (SimObject.GraphicObject.ObjectType)
            {
                case ObjectType.External:
                    ((IExternalUnitOperation)SimObject)?.PopulateEditorPanel(container);
                    break;
                case ObjectType.CapeOpenUO:
                    s.CreateAndAddDescriptionRow(container, "CAPE-OPEN Unit Operations are supported on the Classic User Interface (Classic UI) running on Windows only.");
                    break;
                case ObjectType.SolidSeparator:
                    var ss = (SolidsSeparator)SimObject;
                    s.CreateAndAddTextBoxRow(container, nf, "Solids Separation Efficiency", ss.SeparationEfficiency,
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           ss.SeparationEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
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
                                            if (arg3.Text.IsValidDoubleExpression())
                                            {
                                                arg3.TextColor = (SystemColors.ControlText);
                                                ss.LiquidSeparationEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
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
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           es.EnergyFlow = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                        case Compressor.CalculationMode.PowerRequired:
                            pos1 = 2;
                            break;
                        case Compressor.CalculationMode.EnergyStream:
                            pos1 = 3;
                            break;
                        case Compressor.CalculationMode.Head:
                            pos1 = 4;
                            break;
                        case Compressor.CalculationMode.Curves:
                            pos1 = 5;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.comprcalcmode().ToList(), pos1, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                ce.CalcMode = Compressor.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                ce.CalcMode = Compressor.CalculationMode.Delta_P;
                                break;
                            case 2:
                                ce.CalcMode = Compressor.CalculationMode.PowerRequired;
                                break;
                            case 3:
                                ce.CalcMode = Compressor.CalculationMode.EnergyStream;
                                break;
                            case 4:
                                ce.CalcMode = Compressor.CalculationMode.Head;
                                break;
                            case 5:
                                ce.CalcMode = Compressor.CalculationMode.Curves;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddDropDownRow(container, "Thermodynamic Path", new List<string>(new[] { "Adiabatic", "Polytropic" }), (int)ce.ProcessPath, (DropDown arg3, EventArgs ev) =>
                     {
                         switch (arg3.SelectedIndex)
                         {
                             case 0:
                                 ce.ProcessPath = Compressor.ProcessPathType.Adiabatic;
                                 break;
                             case 1:
                                 ce.ProcessPath = Compressor.ProcessPathType.Polytropic;
                                 break;
                         }
                     });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Thermodynamic Path"));
                    s.CreateAndAddButtonRow(container, "Edit Performance Curves", null, (btn, ea) =>
                    {
                        var f = new EditingForm_CompressorExpander_Curves { simobj = ce };
                        f.ShowDialog();
                    });
                    s.CreateAndAddTextBoxRow(container, nf, "Rotation Speed", ce.Speed,
                        (TextBox arg3, EventArgs ev) =>
                        {
                            if (arg3.Text.IsValidDoubleExpression())
                            {
                                arg3.TextColor = (SystemColors.ControlText);
                                ce.Speed = (int)arg3.Text.ToString().ParseExpressionToDouble();
                            }
                            else
                            {
                                arg3.TextColor = (Colors.Red);
                            }
                        });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Rotation Speed"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Increase (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, ce.DeltaP),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           ce.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Increase"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, ce.POut),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.POut = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Power Required (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, ce.DeltaQ),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.DeltaQ = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Power Required"));
                    s.CreateAndAddTextBoxRow(container, nf, "Adiabatic Efficiency (%)", ce.AdiabaticEfficiency,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.AdiabaticEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Adiabatic Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Polytropic Efficiency (%)", ce.PolytropicEfficiency,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.PolytropicEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Polytropic Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Adiabatic Head (" + su.distance + ")", cv.ConvertFromSI(su.distance, ce.AdiabaticHead),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.AdiabaticHead = cv.ConvertToSI(su.distance, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Adiabatic Head"));
                    s.CreateAndAddTextBoxRow(container, nf, "Polytropic Head (" + su.distance + ")", cv.ConvertFromSI(su.distance, ce.PolytropicHead),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               ce.PolytropicHead = cv.ConvertToSI(su.distance, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Polytropic Head"));
                    break;
                case ObjectType.Expander:
                    var xe = (DWSIM.UnitOperations.UnitOperations.Expander)SimObject;
                    int pos1e = 0;
                    switch (xe.CalcMode)
                    {
                        case DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.OutletPressure:
                            pos1e = 0;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Delta_P:
                            pos1e = 1;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.PowerGenerated:
                            pos1 = 2;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Head:
                            pos1 = 3;
                            break;
                        case DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Curves:
                            pos1 = 4;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.expndrcalcmode().ToList(), pos1e, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                xe.CalcMode = DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.OutletPressure;
                                break;
                            case 1:
                                xe.CalcMode = DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Delta_P;
                                break;
                            case 2:
                                xe.CalcMode = DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.PowerGenerated;
                                break;
                            case 3:
                                xe.CalcMode = DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Head;
                                break;
                            case 4:
                                xe.CalcMode = DWSIM.UnitOperations.UnitOperations.Expander.CalculationMode.Curves;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddButtonRow(container, "Edit Performance Curves", null, (btn, ea) =>
                    {
                        var f = new EditingForm_CompressorExpander_Curves { simobj = xe };
                        f.ShowDialog();
                    });
                    s.CreateAndAddDropDownRow(container, "Thermodynamic Path", new List<string>(new[] { "Adiabatic", "Polytropic" }), (int)xe.ProcessPath, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                xe.ProcessPath = DWSIM.UnitOperations.UnitOperations.Expander.ProcessPathType.Adiabatic;
                                break;
                            case 1:
                                xe.ProcessPath = DWSIM.UnitOperations.UnitOperations.Expander.ProcessPathType.Polytropic;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Thermodynamic Path"));
                    s.CreateAndAddTextBoxRow(container, nf, "Rotation Speed", xe.Speed,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.Speed = (int)arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Rotation Speed"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Decrease (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, xe.DeltaP),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           xe.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                       }
                                       else
                                       {
                                           arg3.TextColor = (Colors.Red);
                                       }
                                   });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Pressure Decrease"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, xe.POut),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.POut = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Outlet Pressure"));
                    s.CreateAndAddTextBoxRow(container, nf, "Power Generated (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, xe.DeltaQ),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.DeltaQ = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Power Generated"));
                    s.CreateAndAddTextBoxRow(container, nf, "Adiabatic Efficiency (%)", xe.AdiabaticEfficiency,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.AdiabaticEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Adiabatic Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Polytropic Efficiency (%)", xe.PolytropicEfficiency,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.PolytropicEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Polytropic Efficiency (%)"));
                    s.CreateAndAddTextBoxRow(container, nf, "Adiabatic Head (" + su.distance + ")", cv.ConvertFromSI(su.distance, xe.AdiabaticHead),
                        (TextBox arg3, EventArgs ev) =>
                        {
                            if (arg3.Text.IsValidDoubleExpression())
                            {
                                arg3.TextColor = (SystemColors.ControlText);
                                xe.AdiabaticHead = cv.ConvertToSI(su.distance, arg3.Text.ToString().ParseExpressionToDouble());
                            }
                            else
                            {
                                arg3.TextColor = (Colors.Red);
                            }
                        });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Adiabatic Head"));
                    s.CreateAndAddTextBoxRow(container, nf, "Polytropic Head (" + su.distance + ")", cv.ConvertFromSI(su.distance, xe.PolytropicHead),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               xe.PolytropicHead = cv.ConvertToSI(su.distance, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Polytropic Head"));
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
                        case Heater.CalculationMode.EnergyStream:
                            pos3 = 3;
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
                            case 3:
                                hc.CalcMode = Heater.CalculationMode.EnergyStream;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, hc.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           hc.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.DeltaQ = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.Eficiencia = arg3.Text.ToString().ParseExpressionToDouble();
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hc.OutletVaporFraction = arg3.Text.ToString().ParseExpressionToDouble();
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
                        case Cooler.CalculationMode.EnergyStream:
                            pos3c = 3;
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
                            case 3:
                                cc.CalcMode = Cooler.CalculationMode.EnergyStream;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                             SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, cc.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           cc.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.DeltaQ = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.Eficiencia = arg3.Text.ToString().ParseExpressionToDouble();
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               cc.OutletVaporFraction = arg3.Text.ToString().ParseExpressionToDouble();
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
                    Button btn1 = null;
                    DropDown drop1 = null;
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
                        case Pump.CalculationMode.Curves:
                            pos4 = 4;
                            break;
                    }
                    drop1 = s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.pumpcalcmode().ToList(), pos4, (DropDown arg3, EventArgs ev) =>
                    {
                        btn1.Enabled = false;
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
                            case 4:
                                pump.CalcMode = Pump.CalculationMode.Curves;
                                btn1.Enabled = true;
                                break;
                        }
                    });

                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));

                    btn1 = s.CreateAndAddLabelAndButtonRow(container, "Pump Performance Curves", "Edit Curves", null, (sender, e) =>
                    {
                        var editor = new DWSIM.UnitOperations.EditingForm_Pump_Curves { selectedpump = pump };
                        editor.ShowDialog();
                    });
                    btn1.Enabled = drop1.SelectedIndex == 4;

                    s.CreateAndAddDescriptionRow(container, "Pump Performance Curves need to be set only if you chose this calculation mode.");

                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Increase (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, pump.DeltaP.GetValueOrDefault()),
                                   (TextBox arg3, EventArgs ev) =>
                                   {
                                       if (arg3.Text.IsValidDoubleExpression())
                                       {
                                           arg3.TextColor = (SystemColors.ControlText);
                                           pump.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.Pout = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.Eficiencia = arg3.Text.ToString().ParseExpressionToDouble();
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               pump.DeltaQ = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                        case Valve.CalculationMode.Kv_General:
                            pos5 = 5;
                            break;
                        case Valve.CalculationMode.Kv_Steam:
                            pos5 = 4;
                            break;
                        case Valve.CalculationMode.Kv_Gas:
                            pos5 = 3;
                            break;
                        case Valve.CalculationMode.Kv_Liquid:
                            pos5 = 2;
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
                            case 2:
                                valve.CalcMode = Valve.CalculationMode.Kv_Liquid;
                                break;
                            case 3:
                                valve.CalcMode = Valve.CalculationMode.Kv_Gas;
                                break;
                            case 4:
                                valve.CalcMode = Valve.CalculationMode.Kv_Steam;
                                break;
                            case 5:
                                valve.CalcMode = Valve.CalculationMode.Kv_General;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, valve.OutletPressure.GetValueOrDefault()),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               valve.OutletPressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       valve.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    s.CreateAndAddDropDownRow(container, "Flow Coefficient Type", new List<string>(new[] { "Kv", "Cv" }), (int)valve.FlowCoefficient,
                        (sender, e) => {
                            valve.FlowCoefficient = sender.SelectedIndex.ToEnum<Valve.FlowCoefficientType>();
                        });
                    s.CreateAndAddTextBoxRow(container, nf, "Kv[Cv](max)", valve.Kv,
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       valve.Kv = arg3.Text.ToString().ParseExpressionToDouble();
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddCheckBoxRow(container, "Use Opening (%) versus Kv[Cv]/Kv[Cv]max (%) relationship", valve.EnableOpeningKvRelationship, (sender, e) => { valve.EnableOpeningKvRelationship = sender.Checked.GetValueOrDefault(); });
                    s.CreateAndAddTextBoxRow(container, nf, "Opening (%)", valve.OpeningPct,
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       valve.OpeningPct = arg3.Text.ToString().ParseExpressionToDouble();
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDropDownRow(container, "Opening/Kv[Cv] Relationship Type", StringResources.valveopkvrelmode().ToList(),
                        (int)valve.DefinedOpeningKvRelationShipType, (DropDown arg3, EventArgs ev) =>
                        {
                            valve.DefinedOpeningKvRelationShipType = arg3.SelectedIndex.ToEnum<Valve.OpeningKvRelationshipType>();
                        });
                    s.CreateAndAddTextBoxRow(container, nf, "Characteristic Parameter", valve.CharacteristicParameter,
                      (TextBox arg3, EventArgs ev) =>
                      {
                          if (arg3.Text.IsValidDoubleExpression())
                          {
                              arg3.TextColor = (SystemColors.ControlText);
                              valve.CharacteristicParameter = arg3.Text.ToString().ParseExpressionToDouble();
                          }
                          else
                          {
                              arg3.TextColor = (Colors.Red);
                          }
                      });
                    s.CreateAndAddStringEditorRow(container, "Kv[Cv]/Kv[Cv]max (%) = f(OP(%))", valve.PercentOpeningVersusPercentKvExpression, (sender, e) => { valve.PercentOpeningVersusPercentKvExpression = sender.Text; });
                    s.CreateAndAddButtonRow(container, "Opening/Kv User Data Table", null, (btn, e) =>
                    {
                        var editor = new Editors.UnitOperations.ValveDataEditor(valve);
                        var form = s.GetDefaultEditorForm("Valve Data Editor", 300, 250, editor, false);
                        form.Closing += (s, e2) => {
                            Application.Instance.Invoke(() => {
                                editor.Save();
                            });
                        };
                        form.Center();
                        form.Show();
                    });
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_lightkeymolarfrac = arg3.Text.ToString().ParseExpressionToDouble();
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_heavykeymolarfrac = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("HK Mole Fraction in Distillate"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reflux Ratio", sc.m_refluxratio,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_refluxratio = arg3.Text.ToString().ParseExpressionToDouble();
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_condenserpressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               sc.m_boilerpressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                        case HeatExchangerCalcMode.CalcTempHotOut:
                            pos7 = 0;
                            break;
                        case HeatExchangerCalcMode.CalcTempColdOut:
                            pos7 = 1;
                            break;
                        case HeatExchangerCalcMode.CalcBothTemp:
                            pos7 = 2;
                            break;
                        case HeatExchangerCalcMode.CalcBothTemp_UA:
                            pos7 = 3;
                            break;
                        case HeatExchangerCalcMode.CalcArea:
                            pos7 = 4;
                            break;
                        case HeatExchangerCalcMode.ShellandTube_Rating:
                            pos7 = 5;
                            break;
                        case HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor:
                            pos7 = 6;
                            break;
                        case HeatExchangerCalcMode.PinchPoint:
                            pos7 = 7;
                            break;
                        case HeatExchangerCalcMode.ThermalEfficiency:
                            pos7 = 8;
                            break;
                        case HeatExchangerCalcMode.OutletVaporFraction1:
                            pos7 = 9;
                            break;
                        case HeatExchangerCalcMode.OutletVaporFraction2:
                            pos7 = 10;
                            break;
                    }

                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.hxcalcmode().ToList(), pos7, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcTempHotOut;
                                break;
                            case 1:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcTempColdOut;
                                break;
                            case 2:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcBothTemp;
                                break;
                            case 3:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcBothTemp_UA;
                                break;
                            case 4:
                                hx.CalculationMode = HeatExchangerCalcMode.CalcArea;
                                break;
                            case 5:
                                hx.CalculationMode = HeatExchangerCalcMode.ShellandTube_Rating;
                                break;
                            case 6:
                                hx.CalculationMode = HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor;
                                break;
                            case 7:
                                hx.CalculationMode = HeatExchangerCalcMode.PinchPoint;
                                break;
                            case 8:
                                hx.CalculationMode = HeatExchangerCalcMode.ThermalEfficiency;
                                break;
                            case 9:
                                hx.CalculationMode = HeatExchangerCalcMode.OutletVaporFraction1;
                                break;
                            case 10:
                                hx.CalculationMode = HeatExchangerCalcMode.OutletVaporFraction2;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    var strdescr = new StringBuilder();
                    strdescr.AppendLine("Required input parameters for each calculation mode:");
                    strdescr.AppendLine();
                    strdescr.AppendLine("Hot Fluid Outlet Temperature: Cold Fluid Outlet Temperature, Overall HTC and Area");
                    strdescr.AppendLine("Cold Fluid Outlet Temperature: Hot Fluid Outlet Temperature, Overall HTC and Area");
                    strdescr.AppendLine("Outlet Temperatures: Area and Heat Exchanged");
                    strdescr.AppendLine("Outlet Temperatures (UA): Overall HTC and Area");
                    strdescr.AppendLine("Area: Overall HTC and Outlet Temperature for one of the fluids");
                    strdescr.AppendLine("Shell and Tube (Rating): Exchanger Geometry (input on separate window)");
                    strdescr.AppendLine("Shell and Tube (Design): Outlet Temperatures and Exchanger Geometry (input on separate tab)");
                    strdescr.AppendLine("Pinch Point: Overall HTC and MITA");
                    strdescr.AppendLine("Thermal Efficiency: HX Efficiency and Overall HTC");
                    strdescr.AppendLine("Outlet Vapor Fraction: Vapor Fraction and Area");
                    strdescr.AppendLine("*Pressure drop is required for both fluids except for Shell and Tube Rating mode.");

                    s.CreateAndAddDescriptionRow(container, strdescr.ToString());

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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       hx.HotSidePressureDrop = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       hx.ColdSidePressureDrop = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.ColdSideOutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.HotSideOutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.OverallCoefficient = cv.ConvertToSI(su.heat_transf_coeff, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.Area = cv.ConvertToSI(su.area, arg3.Text.ToString().ParseExpressionToDouble());
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.Q = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Heat Exchanged"));
                    s.CreateAndAddTextBoxRow(container, nf, "Heat Loss (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, hx.HeatLoss),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.HeatLoss = cv.ConvertToSI(su.heatflow, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddTextBoxRow(container, nf, "Minimum Temperature Difference (" + su.deltaT + ")", cv.ConvertFromSI(su.deltaT, hx.MITA),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.MITA = cv.ConvertToSI(su.deltaT, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("MITA"));
                    s.CreateAndAddCheckBoxRow(container, "Force Pinch Point Location to Outlets", hx.PinchPointAtOutlets,
                        (chk, e) =>
                        {
                            hx.PinchPointAtOutlets = chk.Checked.GetValueOrDefault();
                        });

                    s.CreateAndAddTextBoxRow(container, nf, "Heat Transfer Efficiency (%)", hx.ThermalEfficiency,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.ThermalEfficiency = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Vapor Fraction (Stream 1)", hx.OutletVaporFraction1,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.OutletVaporFraction1 = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Vapor Fraction (Stream 2)", hx.OutletVaporFraction2,
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               hx.OutletVaporFraction1 = arg3.Text.ToString().ParseExpressionToDouble();
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddCheckBoxRow(container, "Ignore LMTD Error", hx.IgnoreLMTDError,
                          (chk, e) =>
                          {
                              hx.IgnoreLMTDError = chk.Checked.GetValueOrDefault();
                          });

                    s.CreateAndAddCheckBoxRow(container, "Calculate Heat Exchange Profile", hx.CalculateHeatExchangeProfile,
                        (chk, e) =>
                        {
                            hx.CalculateHeatExchangeProfile = chk.Checked.GetValueOrDefault();
                        });
                    s.CreateAndAddDescriptionRow(container, "Enable this to calculate the Heat Exchange Profile for all operating modes. The profile will always be calculated for Pinch Point mode regardless of this setting.");

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
                    var rsets = SimObject.GetFlowsheet().ReactionSets.Values.Select((x) => x.Name).ToList();
                    if (!SimObject.GetFlowsheet().ReactionSets.ContainsKey(reactor.ReactionSetID)) reactor.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Keys.First();
                    var selname = SimObject.GetFlowsheet().ReactionSets[reactor.ReactionSetID].Name;
                    s.CreateAndAddDropDownRow(container, "Reaction Set", rsets, rsets.IndexOf(selname), (sender, e) => reactor.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Values.Where((x) => x.Name == sender.SelectedValue.ToString()).FirstOrDefault().ID);
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                    var rsets2 = SimObject.GetFlowsheet().ReactionSets.Values.Select((x) => x.Name).ToList();
                    if (!SimObject.GetFlowsheet().ReactionSets.ContainsKey(reactor2.ReactionSetID)) reactor2.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Keys.First();
                    var selname2 = SimObject.GetFlowsheet().ReactionSets[reactor2.ReactionSetID].Name;
                    s.CreateAndAddDropDownRow(container, "Reaction Set", rsets2, rsets2.IndexOf(selname2), (sender, e) => reactor2.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Values.Where((x) => x.Name == sender.SelectedValue.ToString()).FirstOrDefault().ID);
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor2.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor2.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    s.CreateAndAddLabelRow(container, "Convergence Parameters");
                    s.CreateAndAddCheckBoxRow(container, "Initialize from a Previous Solution", reactor2.UsePreviousSolution, (sender, e) => reactor2.UsePreviousSolution = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddTextBoxRow(container, nf, "Maximum Internal Iterations", reactor2.InternalLoopMaximumIterations, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2.InternalLoopMaximumIterations = int.Parse(sender.Text); });
                    s.CreateAndAddTextBoxRow(container, nf, "Maximum External Iterations", reactor2.ExternalLoopMaximumIterations, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2.ExternalLoopMaximumIterations = int.Parse(sender.Text); });
                    s.CreateAndAddTextBoxRow(container, nf, "Minimum Internal Error", reactor2.InternalLoopTolerance, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2.InternalLoopTolerance = sender.Text.ParseExpressionToDouble(); });
                    s.CreateAndAddTextBoxRow(container, nf, "Minimum External Error", reactor2.ExternalLoopTolerance, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2.ExternalLoopTolerance = sender.Text.ParseExpressionToDouble(); });
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor2g.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor2g.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Pressure Drop"));
                    var compounds = SimObject.GetFlowsheet().SelectedCompounds.Values.Select((x) => x.Name).ToList();
                    s.CreateAndAddLabelRow(container, "Reacting Compounds");
                    var ids = reactor2g.ComponentIDs.ToArray();                    
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
                                                       reactor2g.CreateElementMatrix();
                                                       reactor2g.FlowSheet.ShowMessage(String.Format("{0}: element matrix was rebuilt/reset.", reactor2g.GraphicObject.Tag), IFlowsheet.MessageType.Information);
                                                   });
                    s.CreateAndAddLabelRow(container, "Element Matrix");
                    s.CreateAndAddButtonRow(container, "Setup Element Matrix", null, (btn, e) =>
                    {
                        var editor = new Editors.UnitOperations.ElementMatrixEditor(reactor2g);
                        var form = s.GetDefaultEditorForm("Element Matrix Editor", 600, 300, editor, false);
                        form.Center();
                        form.Show();
                    });
                    s.CreateAndAddLabelRow(container, "Convergence Parameters");
                    s.CreateAndAddCheckBoxRow(container, "Initialize from Previous Solution", reactor2g.InitializeFromPreviousSolution, (sender, e) => reactor2g.InitializeFromPreviousSolution = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddTextBoxRow(container, nf, "Maximum Iterations", reactor2g.MaximumInternalIterations, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2g.MaximumInternalIterations = int.Parse(sender.Text); });
                    s.CreateAndAddTextBoxRow(container, nf, "Error Tolerance", reactor2g.InternalTolerance, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) reactor2g.InternalTolerance = sender.Text.ParseExpressionToDouble(); });
                    s.CreateAndAddCheckBoxRow(container, "Use IPOPT Solver", reactor2g.UseIPOPTSolver, (sender, e) => reactor2g.UseIPOPTSolver = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddCheckBoxRow(container, "Use Alternate Solving Method", reactor2g.AlternateSolvingMethod, (sender, e) => reactor2g.AlternateSolvingMethod = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddDropDownRow(container, "Reactive Phase Behavior", StringResources.rgrpb().ToList(), (int)reactor2g.ReactivePhaseBehavior, (DropDown arg3, EventArgs ev) =>
                    {
                        reactor2g.ReactivePhaseBehavior = arg3.SelectedIndex.ToEnum<Reactor_Gibbs.ReactivePhaseType>();
                    });
                    break;
                case ObjectType.RCT_CSTR:
                    var reactor3 = (Reactor_CSTR)SimObject;
                    var rsets3 = SimObject.GetFlowsheet().ReactionSets.Values.Select((x) => x.Name).ToList();
                    if (!SimObject.GetFlowsheet().ReactionSets.ContainsKey(reactor3.ReactionSetID)) reactor3.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Keys.First();
                    var selname3 = SimObject.GetFlowsheet().ReactionSets[reactor3.ReactionSetID].Name;
                    s.CreateAndAddDropDownRow(container, "Reaction Set", rsets3, rsets3.IndexOf(selname3), (sender, e) => reactor3.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Values.Where((x) => x.Name == sender.SelectedValue.ToString()).FirstOrDefault().ID);
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
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor3.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.DeltaP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.Volume = cv.ConvertToSI(su.volume, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Volume"));
                    s.CreateAndAddTextBoxRow(container, nf, "Reactor Headspace (" + su.volume + ")", cv.ConvertFromSI(su.volume, reactor3.Headspace),
                              (TextBox arg3, EventArgs ev) =>
                              {
                                  if (arg3.Text.IsValidDoubleExpression())
                                  {
                                      arg3.TextColor = (SystemColors.ControlText);
                                      reactor3.Headspace = cv.ConvertToSI(su.volume, arg3.Text.ToString().ParseExpressionToDouble());
                                  }
                                  else
                                  {
                                      arg3.TextColor = (Colors.Red);
                                  }
                              });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Headspace"));
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Amount (" + su.mass + ")", cv.ConvertFromSI(su.mass, reactor3.CatalystAmount),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor3.CatalystAmount = cv.ConvertToSI(su.mass, arg3.Text.ToString().ParseExpressionToDouble());
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
                    var rsets4 = SimObject.GetFlowsheet().ReactionSets.Values.Select((x) => x.Name).ToList();
                    if (!SimObject.GetFlowsheet().ReactionSets.ContainsKey(reactor4.ReactionSetID)) reactor4.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Keys.First();
                    var selname4 = SimObject.GetFlowsheet().ReactionSets[reactor4.ReactionSetID].Name;
                    s.CreateAndAddDropDownRow(container, "Reaction Set", rsets4, rsets4.IndexOf(selname4), (sender, e) => reactor4.ReactionSetID = SimObject.GetFlowsheet().ReactionSets.Values.Where((x) => x.Name == sender.SelectedValue.ToString()).FirstOrDefault().ID);
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
                        case OperationMode.NonIsothermalNonAdiabatic:
                            pos13 = 3;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.rctcalcmode2().ToList(), pos13, (DropDown arg3, EventArgs ev) =>
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
                            case 3:
                                reactor4.ReactorOperationMode = OperationMode.NonIsothermalNonAdiabatic;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, reactor4.OutletTemperature),
                       (TextBox arg3, EventArgs ev) =>
                       {
                           if (arg3.Text.IsValidDoubleExpression())
                           {
                               arg3.TextColor = (SystemColors.ControlText);
                               reactor4.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
                           }
                           else
                           {
                               arg3.TextColor = (Colors.Red);
                           }
                       });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Outlet Temperature"));
                    s.CreateAndAddLabelRow(container, "Sizing Information");
                    s.CreateAndAddNumericEditorRow2(container, "Number of Tubes", reactor4.NumberOfTubes, 1, 10000, 0, (ns, e) =>
                    {
                        reactor4.NumberOfTubes = (int)ns.Text.ToDoubleFromCurrent();
                    });
                    s.CreateAndAddTextBoxRow(container, nf, "Reactive Volume (" + su.volume + ")", cv.ConvertFromSI(su.volume, reactor4.Volume),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.Volume = cv.ConvertToSI(su.volume, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Volume"));
                    s.CreateAndAddDropDownRow(container, "Sizing Information", new List<string> { "Length", "Diameter" },
                        reactor4.ReactorSizingType == Reactor_PFR.SizingType.Length ? 0 : 1,
                         (dd, e) => reactor4.ReactorSizingType = dd.SelectedIndex.ToEnum<Reactor_PFR.SizingType>());
                    s.CreateAndAddTextBoxRow(container, nf, "Tube Length (" + su.distance + ")", cv.ConvertFromSI(su.distance, reactor4.Length),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.Length = cv.ConvertToSI(su.distance, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddTextBoxRow(container, nf, "Tube Diameter (" + su.diameter + ")", cv.ConvertFromSI(su.diameter, reactor4.Diameter),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.Diameter = cv.ConvertToSI(su.diameter, arg3.Text.ToString().ParseExpressionToDouble());
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Reactor Length"));
                    s.CreateAndAddLabelRow(container, "Catalyst Loading");
                    s.CreateAndAddDescriptionRow(container, "Catalyst Loading information is required when dealing with Heterogeneous Catalytic reactions.");
                    s.CreateAndAddTextBoxRow(container, nf, "Catalyst Loading (" + su.density + ")", cv.ConvertFromSI(su.volume, reactor4.CatalystLoading),
                               (TextBox arg3, EventArgs ev) =>
                               {
                                   if (arg3.Text.IsValidDoubleExpression())
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       reactor4.CatalystLoading = cv.ConvertToSI(su.density, arg3.Text.ToString().ParseExpressionToDouble());
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
                                  if (arg3.Text.IsValidDoubleExpression())
                                  {
                                      arg3.TextColor = (SystemColors.ControlText);
                                      reactor4.CatalystParticleDiameter = cv.ConvertToSI(su.diameter, arg3.Text.ToString().ParseExpressionToDouble());
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
                                 if (arg3.Text.IsValidDoubleExpression())
                                 {
                                     arg3.TextColor = (SystemColors.ControlText);
                                     reactor4.CatalystVoidFraction = arg3.Text.ToString().ParseExpressionToDouble();
                                 }
                                 else
                                 {
                                     arg3.TextColor = (Colors.Red);
                                 }
                             });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Catalyst Void Fraction"));
                    s.CreateAndAddCheckBoxRow(container, "Use Constant Linear Pressure Drop", reactor4.UseUserDefinedPressureDrop, 
                        (chk, e) => {
                            reactor4.UseUserDefinedPressureDrop = chk.Checked.GetValueOrDefault();
                        });
                    s.CreateAndAddTextBoxRow(container, nf, "Constant Linear Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, reactor4.UserDefinedPressureDrop),
                              (TextBox arg3, EventArgs ev) =>
                              {
                                  if (arg3.Text.IsValidDoubleExpression())
                                  {
                                      arg3.TextColor = (SystemColors.ControlText);
                                      reactor4.UserDefinedPressureDrop = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
                                  }
                                  else
                                  {
                                      arg3.TextColor = (Colors.Red);
                                  }
                              });
                    s.CreateAndAddDropDownRow(container, "Slurry Viscosity Correction", new List<string> { "Disabled", "Yoshida et al" },
                        reactor4.SlurryViscosityMode,
                         (dd, e) => reactor4.SlurryViscosityMode = dd.SelectedIndex);
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        cs.SpecValue = arg3.Text.ToString().ParseExpressionToDouble();
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
                                splitter.OperationMode = DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.SplitRatios;
                                break;
                            case 1:
                                splitter.OperationMode = DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec;
                                break;
                            case 2:
                                splitter.OperationMode = DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Specification"));
                    s.CreateAndAddTextBoxRow(container, nf, "Split Ratio Stream 1", (double)splitter.Ratios[0],
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[0] = arg3.Text.ToString().ParseExpressionToDouble();
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[1] = arg3.Text.ToString().ParseExpressionToDouble();
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        splitter.Ratios[2] = arg3.Text.ToString().ParseExpressionToDouble();
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
                    s.CreateAndAddTextBoxRow(container, nf, "S1 Flow (" + su.massflow + "|" + su.molarflow + ")", sm1,
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                                        {
                                            splitter.StreamFlowSpec = cv.ConvertToSI(su.massflow, arg3.Text.ToString().ParseExpressionToDouble());
                                        }
                                        else
                                        {
                                            splitter.StreamFlowSpec = cv.ConvertToSI(su.molarflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                    s.CreateAndAddTextBoxRow(container, nf, "S2 Flow (" + su.massflow + "|" + su.molarflow + ")", sm2,
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        if (splitter.OperationMode == DWSIM.UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec)
                                        {
                                            splitter.Stream2FlowSpec = cv.ConvertToSI(su.massflow, arg3.Text.ToString().ParseExpressionToDouble());
                                        }
                                        else
                                        {
                                            splitter.Stream2FlowSpec = cv.ConvertToSI(su.molarflow, arg3.Text.ToString().ParseExpressionToDouble());
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
                        case Pipe.Specmode.Length:
                            posp = 0;
                            break;
                        case Pipe.Specmode.OutletPressure:
                            posp = 1;
                            break;
                        case Pipe.Specmode.OutletTemperature:
                            posp = 2;
                            break;
                    }
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", StringResources.pipecalcmode().ToList(), posp, (DropDown arg3, EventArgs ev) =>
                    {
                        switch (arg3.SelectedIndex)
                        {
                            case 0:
                                pipe.Specification = Pipe.Specmode.Length;
                                break;
                            case 1:
                                pipe.Specification = Pipe.Specmode.OutletPressure;
                                break;
                            case 2:
                                pipe.Specification = Pipe.Specmode.OutletTemperature;
                                break;
                        }
                    });
                    s.CreateAndAddDescriptionRow(container, SimObject.GetPropertyDescription("Calculation Mode"));
                    s.CreateAndAddDropDownRow(container, "Pressure Drop Calculation Model", new List<string>() { "Beggs & Brill", "Lockhart & Martinelli", "Petalas & Aziz" }, (int)pipe.SelectedFlowPackage, (sender, e) => pipe.SelectedFlowPackage = (FlowPackage)sender.SelectedIndex);
                    s.CreateAndAddTextBoxRow(container, nf, "Outlet Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, pipe.OutletPressure),
                                (TextBox arg3, EventArgs ev) =>
                                {
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.OutletPressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.OutletTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.TolP = cv.ConvertToSI(su.deltaP, arg3.Text.ToString().ParseExpressionToDouble());
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        pipe.TolT = cv.ConvertToSI(su.deltaT, arg3.Text.ToString().ParseExpressionToDouble());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Temperature Convergence Tolerance"));
                    s.CreateAndAddCheckBoxRow(container, "Include Joule-Thomson Effect", pipe.IncludeEmulsion, (CheckBox arg2, EventArgs ev) =>
                    {
                        pipe.IncludeEmulsion = arg2.Checked.GetValueOrDefault();
                    });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Include Joule-Thomson Effect"));
                    s.CreateAndAddDropDownRow(container, "Slurry Viscosity Correction", new List<string> { "Disabled", "Yoshida et al" },
                        pipe.SlurryViscosityMode,
                         (dd, e) => pipe.SlurryViscosityMode = dd.SelectedIndex);
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        vessel.FlashPressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
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
                                    if (arg3.Text.IsValidDoubleExpression())
                                    {
                                        arg3.TextColor = (SystemColors.ControlText);
                                        vessel.FlashTemperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
                                    }
                                    else
                                    {
                                        arg3.TextColor = (Colors.Red);
                                    }
                                });
                    s.CreateAndAddDescriptionRow(container,
                                                 SimObject.GetPropertyDescription("Separation Temperature"));
                    s.CreateAndAddLabelRow(container, "Sizing Parameters");
                    s.CreateAndAddTextBoxRow(container, nf, "Length/Height over Diameter Ratio", vessel.DimensionRatio, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) vessel.DimensionRatio = sender.Text.ParseExpressionToDouble(); });
                    s.CreateAndAddTextBoxRow(container, nf, "Surge Factor", vessel.SurgeFactor, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) vessel.SurgeFactor = sender.Text.ParseExpressionToDouble(); });
                    s.CreateAndAddTextBoxRow(container, nf, "Liquid Residence Time (" + su.time + ")", cv.ConvertFromSI(su.time, vessel.ResidenceTime), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) vessel.DimensionRatio = cv.ConvertToSI(su.time, sender.Text.ParseExpressionToDouble()); });
                    break;
                case ObjectType.CustomUO:
                    var scriptuo = (CustomUO)SimObject;
                    s.CreateAndAddLabelRow(container, "Python Script Repositories");
                    s.CreateAndAddButtonRow(container, "FOSSEE's Custom Modeling Repository", null, (btn, e) =>
                    {
                        Process.Start("https://dwsim.fossee.in/custom-model");
                    });
                    s.CreateAndAddLabelRow(container, "Flowsheet Object");
                    s.CreateAndAddLabelAndButtonRow(container, "Embedded Image Icon", "Load File", null, (s, e) =>
                    {
                        var searchdialog = new OpenFileDialog() { Title = "Open Image File", MultiSelect = false };
                        searchdialog.Filters.Add(new FileFilter("Image Files", new[] { ".png", ".jpg" }));
                        if (searchdialog.ShowDialog(container) == DialogResult.Ok)
                        {
                            try
                            {
                                using (var bmp = (System.Drawing.Bitmap)System.Drawing.Bitmap.FromFile(searchdialog.FileName))
                                {
                                    using (var img = SkiaSharp.Views.Desktop.Extensions.ToSKImage(bmp))
                                    {
                                        scriptuo.EmbeddedImageData = DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.EmbeddedImageGraphic.ImageToBase64(img, SkiaSharp.SKEncodedImageFormat.Png);
                                        MessageBox.Show("Image data read successfully.", MessageBoxButtons.OK);
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                MessageBox.Show("Error reading image data.", MessageBoxButtons.OK);
                            }
                        }
                    });
                    s.CreateAndAddCheckBoxRow(container, "Use Embedded Image Icon", scriptuo.UseEmbeddedImage, (c, e) =>
                    {
                        scriptuo.UseEmbeddedImage = c.Checked.GetValueOrDefault();
                    });
                    s.CreateAndAddLabelRow(container, "Python Script");
                    s.CreateAndAddDropDownRow(container, "Python Interpreter", new List<string> { "IronPython", "Python.NET" }, (int)scriptuo.ExecutionEngine, (sender, e) => scriptuo.ExecutionEngine = (DWSIM.UnitOperations.UnitOperations.CustomUO.PythonExecutionEngine)sender.SelectedIndex);
                    s.CreateAndAddLabelRow(container, "Script Variables");
                    var tabc = new TabControl { Height = 400 };
                    var tab1 = new TabPage { Text = "Input Vars" };
                    var tab2 = new TabPage { Text = "Input Strings" };
                    var tab3 = new TabPage { Text = "Output Vars" };
                    tabc.Pages.Add(tab1);
                    tabc.Pages.Add(tab2);
                    tabc.Pages.Add(tab3);

                    int is1 = 0;

                    var c1 = s.GetDefaultContainer();
                    string t1 = "";
                    foreach (var obj in scriptuo.InputVariables)
                    {
                        t1 += obj.Key + "\t" + obj.Value.ToString() + "\n";
                    }
                    s.CreateAndAddDescriptionRow(c1, "Enter one variable per line, separating its name (no special characters or spaces) from its value with a tab or a single space.");
                    s.CreateAndAddMultilineMonoSpaceTextBoxRow(c1, t1, 300, false, (s, e) =>
                    {
                        if (s.Text == "")
                        {
                            scriptuo.InputVariables.Clear();
                            return;
                        }
                        var col = new Dictionary<string, double>();
                        is1 = 0;
                        foreach (string line in s.Text.Split('\n'))
                        {
                            try
                            {
                                var val = line.Trim().Split(new[] { ' ', '\t' });
                                col.Add(val[0], val[1].ToDoubleFromCurrent());
                                scriptuo.FlowSheet.ShowMessage(String.Format("Variable '{0}' parsed successfully.", val[0]), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                scriptuo.FlowSheet.ShowMessage(String.Format("Error parsing variable at line {0}: " + ex.Message.ToString(), is1), IFlowsheet.MessageType.Information);
                            }
                            is1 += 1;
                        }
                        scriptuo.InputVariables = col;
                    });

                    var c2 = s.GetDefaultContainer();
                    string t2 = "";
                    foreach (var obj in scriptuo.InputStringVariables)
                    {
                        t2 += obj.Key + "\t" + obj.Value + "\n";
                    }
                    s.CreateAndAddDescriptionRow(c2, "Enter one variable per line, separating its name (no special characters or spaces) from its value with a tab or a single space.");
                    s.CreateAndAddMultilineMonoSpaceTextBoxRow(c2, t2, 300, false, (s, e) =>
                    {
                        if (s.Text == "")
                        {
                            scriptuo.InputStringVariables.Clear();
                            return;
                        }
                        var col = new Dictionary<string, string>();
                        is1 = 0;
                        foreach (string line in s.Text.Split('\n'))
                        {
                            try
                            {
                                var val = line.Trim().Split(new[] { ' ', '\t' });
                                col.Add(val[0], val[1]);
                                scriptuo.FlowSheet.ShowMessage(String.Format("Variable '{0}' parsed successfully.", val[0]), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                scriptuo.FlowSheet.ShowMessage(String.Format("Error parsing variable at line {0}: " + ex.Message.ToString(), is1), IFlowsheet.MessageType.Information);
                            }
                            is1 += 1;
                        }
                        scriptuo.InputStringVariables = col;
                    });

                    var c3 = s.GetDefaultContainer();
                    string t3 = "";
                    foreach (var obj in scriptuo.OutputVariables)
                    {
                        t3 += obj.Key + "\t" + obj.Value.ToString() + "\n";
                    }
                    s.CreateAndAddMultilineMonoSpaceTextBoxRow(c3, t3, 300, true, null);

                    tab1.Content = c1;
                    tab2.Content = c2;
                    tab3.Content = c3;

                    s.CreateAndAddControlRow(container, tabc);
                    break;
                case ObjectType.ExcelUO:
                    var exceluo = (ExcelUO)SimObject;
                    s.CreateAndAddLabelRow(container, "Spreadsheet File");
                    TextBox tbox = null;

                    s.CreateAndAddDropDownRow(container, "File Source", new List<string>(new []{ "Embedded", "External" }), exceluo.FileIsEmbedded ? 0 : 1, (dd, e) => {
                        exceluo.FileIsEmbedded = dd.SelectedIndex == 0 ? true : false;
                    });

                    var files = exceluo.GetFlowsheet().FileDatabaseProvider.GetFiles();
                    files.Insert(0, "");
                    var selected = exceluo.EmbeddedFileName;
                    if (!files.Contains(selected)) selected = "";

                    s.CreateAndAddDropDownRow(container, "Embedded File", files, files.IndexOf(selected), (dd, e) => {
                        exceluo.EmbeddedFileName = files[dd.SelectedIndex];
                    });

                    tbox = s.CreateAndAddLabelAndTextBoxAndButtonRow(container, "External File Path", exceluo.Filename, "Search", null, (sender, e) => exceluo.Filename = sender.Text, (sender, e) =>
                    {
                        var searchdialog = new OpenFileDialog() { Title = "Search", FileName = exceluo.Filename, MultiSelect = false };
                        if (searchdialog.ShowDialog(container) == DialogResult.Ok)
                        {
                            tbox.Text = searchdialog.FileName;
                        }
                    });
                    s.CreateAndAddButtonRow(container, "Edit Spreadsheet", null, (sender, e) =>
                    {
                        if (!DWSIM.GlobalSettings.Settings.IsRunningOnMono())
                        {
                            Process.Start(exceluo.Filename);
                        }
                        else
                        {
                            Process.Start(new ProcessStartInfo("xdg-open", exceluo.Filename) { UseShellExecute = false });
                        }
                    });
                    s.CreateAndAddButtonRow(container, "Create New Spreadsheet", null, (sender, e) =>
                    {
                        var OpenFileDialog1 = new OpenFileDialog();
                        OpenFileDialog1.Title = "New Spreadsheet";
                        OpenFileDialog1.Filters.Add(new FileFilter("Spreadsheet files", new string[] { "*.xlsx", "*.xls", "*.ods" }));
                        OpenFileDialog1.CheckFileExists = false;
                        OpenFileDialog1.Directory = new Uri(Path.GetDirectoryName(exceluo.Filename));

                        if (OpenFileDialog1.ShowDialog(container) == DialogResult.Ok)
                        {
                            string str = OpenFileDialog1.FileName;
                            if (Path.GetExtension(str).ToLower() == ".ods")
                            {
                                File.Copy(AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "TemplateExcelUO.ods", str);
                            }
                            else if (Path.GetExtension(str).ToLower() == ".xls")
                            {
                                File.Copy(AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "TemplateExcelUO.xls", str);
                            }
                            else
                            {
                                File.Copy(AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "TemplateExcelUO.xlsx", str);
                            }
                            tbox.Text = str;
                            exceluo.ParamsLoaded = false;
                        }
                    });
                    exceluo.ReadExcelParams();
                    s.CreateAndAddLabelRow(container, "Input Parameters");
                    foreach (var par in exceluo.InputParams.Values)
                    {
                        s.CreateAndAddTextBoxRow(container, nf, par.Name + " (" + par.Unit + ")", par.Value, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) par.Value = sender.Text.ParseExpressionToDouble(); });
                    }
                    break;
                case ObjectType.Filter:
                    var filter = (Filter)SimObject;
                    s.CreateAndAddDropDownRow(container, "Calculation Mode", new List<string> { "Sizing", "Evaluation" }, (int)filter.CalcMode, (sender, e) => filter.CalcMode = (Filter.CalculationMode)sender.SelectedIndex);
                    s.CreateAndAddTextBoxRow(container, nf, "Medium Resistance (" + su.mediumresistance + ")", cv.ConvertFromSI(su.mediumresistance, filter.FilterMediumResistance), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.FilterMediumResistance = cv.ConvertToSI(su.mediumresistance, sender.Text.ParseExpressionToDouble()); });
                    s.CreateAndAddTextBoxRow(container, nf, "Cake Resistance (" + su.cakeresistance + ")", cv.ConvertFromSI(su.cakeresistance, filter.SpecificCakeResistance), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.SpecificCakeResistance = cv.ConvertToSI(su.cakeresistance, sender.Text.ParseExpressionToDouble()); });
                    s.CreateAndAddTextBoxRow(container, nf, "Cycle Time (" + su.time + ")", cv.ConvertFromSI(su.time, filter.FilterCycleTime), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.FilterCycleTime = cv.ConvertToSI(su.time, sender.Text.ParseExpressionToDouble()); });
                    s.CreateAndAddTextBoxRow(container, nf, "Total Filtering Area (" + su.area + ")", cv.ConvertFromSI(su.area, filter.TotalFilterArea), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.TotalFilterArea = cv.ConvertToSI(su.area, sender.Text.ParseExpressionToDouble()); });
                    s.CreateAndAddTextBoxRow(container, nf, "Pressure Drop (" + su.deltaP + ")", cv.ConvertFromSI(su.deltaP, filter.PressureDrop), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.PressureDrop = cv.ConvertToSI(su.deltaP, sender.Text.ParseExpressionToDouble()); });
                    s.CreateAndAddTextBoxRow(container, nf, "Cake Humidity (%)", filter.CakeRelativeHumidity, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.CakeRelativeHumidity = sender.Text.ParseExpressionToDouble(); });
                    s.CreateAndAddTextBoxRow(container, nf, "Submerged Fraction", filter.SubmergedAreaFraction, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) filter.SubmergedAreaFraction = sender.Text.ParseExpressionToDouble(); });
                    break;
                case ObjectType.FlowsheetUO:
                    var fsuo = (Flowsheet)SimObject;


                    s.CreateAndAddDropDownRow(container, "File Source", new List<string>(new[] { "Embedded", "External" }), fsuo.FileIsEmbedded ? 0 : 1, (dd, e) => {
                        fsuo.FileIsEmbedded = dd.SelectedIndex == 0 ? true : false;
                    });

                    var files2 = fsuo.GetFlowsheet().FileDatabaseProvider.GetFiles();
                    files2.Insert(0, "");
                    var selected2 = fsuo.EmbeddedFileName;
                    if (!files2.Contains(selected2)) selected2 = "";

                    s.CreateAndAddDropDownRow(container, "Embedded File", files2, files2.IndexOf(selected2), (dd, e) => {
                        fsuo.EmbeddedFileName = files2[dd.SelectedIndex];
                    });

                    TextBox tbox2 = null;
                    tbox2 = s.CreateAndAddLabelAndTextBoxAndButtonRow(container, "Flowsheet Path", fsuo.SimulationFile, "Search", null, (sender, e) => fsuo.SimulationFile = sender.Text, (sender, e) =>
                    {
                        var fpath = fsuo.SimulationFile;
                        if (fpath == "") fpath = System.Environment.CurrentDirectory;
                        var searchdialog = new OpenFileDialog() { Title = "Search", FileName = fpath, MultiSelect = false };
                        if (searchdialog.ShowDialog(container) == DialogResult.Ok)
                        {
                            tbox2.Text = searchdialog.FileName;
                        }
                    });
                    s.CreateAndAddCheckBoxRow(container, "Initialize on Load", fsuo.InitializeOnLoad, (sender, e) => fsuo.InitializeOnLoad = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddCheckBoxRow(container, "Update Process Data when Saving", fsuo.UpdateOnSave, (sender, e) => fsuo.UpdateOnSave = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddCheckBoxRow(container, "Redirect Flowsheet Calculator Messages", fsuo.RedirectOutput, (sender, e) => fsuo.RedirectOutput = sender.Checked.GetValueOrDefault());
                    s.CreateAndAddButtonRow(container, "Open Control Panel", null, (sender, e) =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            //System.Windows.Forms.Application.EnableVisualStyles();
                            //System.Windows.Forms.Application.SetCompatibleTextRenderingDefault(false);
                            var editor = new DWSIM.UnitOperations.EditingForm_Flowsheet_Editor { fsuo = fsuo };
                            editor.ShowDialog();
                            //System.Windows.Forms.Application.Run(editor);
                            //System.Windows.Forms.Application.ExitThread();
                        });
                    });
                    s.CreateAndAddLabelRow(container, "Linked Input Variables");
                    foreach (var item in fsuo.InputParams)
                    {
                        if (fsuo.Fsheet != null && fsuo.Fsheet.SimulationObjects.ContainsKey(item.Value.ObjectID))
                        {
                            var name = fsuo.Fsheet.SimulationObjects[item.Value.ObjectID].GraphicObject.Tag + ", " + fsuo.GetFlowsheet().GetTranslatedString(item.Value.ObjectProperty);
                            var value = (double)fsuo.Fsheet.SimulationObjects[item.Value.ObjectID].GetPropertyValue(item.Value.ObjectProperty, su);
                            var units = fsuo.Fsheet.SimulationObjects[item.Value.ObjectID].GetPropertyUnit(item.Value.ObjectProperty, su);
                            s.CreateAndAddTextBoxRow(container, nf, name + " (" + units + ")", value, (sender, e) =>
                            {
                                if (sender.Text.IsValidDoubleExpression())
                                {
                                    fsuo.Fsheet.SimulationObjects[item.Value.ObjectID].SetPropertyValue(item.Value.ObjectProperty, sender.Text.ParseExpressionToDouble(), su);
                                };
                            });
                        }
                    }
                    break;
                case ObjectType.Tank:
                    var tank = (Tank)SimObject;
                    s.CreateAndAddTextBoxRow(container, nf, "Volume (" + su.volume + ")", cv.ConvertFromSI(su.volume, tank.Volume), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) tank.Volume = cv.ConvertToSI(su.volume, sender.Text.ParseExpressionToDouble()); });
                    break;
                case ObjectType.OrificePlate:
                    var op = (OrificePlate)SimObject;
                    s.CreateAndAddDropDownRow(container, "Pressure Tappings", new List<string>() { "Corner", "Flange", "Radius" }, (int)op.OrifType, (sender, e) =>
                    {
                        op.OrifType = (DWSIM.UnitOperations.UnitOperations.OrificePlate.OrificeType)sender.SelectedIndex;
                    });
                    s.CreateAndAddTextBoxRow(container, nf, "Orifice Diameter (" + su.diameter + ")", op.OrificeDiameter, (sender, e) =>
                    {
                        if (sender.Text.IsValidDoubleExpression()) op.OrificeDiameter = sender.Text.ParseExpressionToDouble();
                    });
                    s.CreateAndAddTextBoxRow(container, nf, "Internal Pipe Diameter (" + su.diameter + ")", op.InternalPipeDiameter, (sender, e) =>
                    {
                        if (sender.Text.IsValidDoubleExpression()) op.InternalPipeDiameter = sender.Text.ParseExpressionToDouble();
                    });
                    s.CreateAndAddTextBoxRow(container, nf, "Correction Factor", op.CorrectionFactor, (sender, e) =>
                    {
                        if (sender.Text.IsValidDoubleExpression()) op.CorrectionFactor = sender.Text.ParseExpressionToDouble();
                    });
                    break;
            }
            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);
        }

    }

}
