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
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Interfaces.Enums;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class MaterialStreamEditor
    {

        public MaterialStream MatStream;

        public DynamicLayout container;

        public MaterialStreamEditor(ISimulationObject selectedobject, DynamicLayout layout)
        {
            MatStream = (MaterialStream)selectedobject;
            container = layout;
            Initialize();
        }
        void CallSolverIfNeeded()
        {
            if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke();
        }

        void Initialize()
        {

            var su = MatStream.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = MatStream.GetFlowsheet().FlowsheetOptions.NumberFormat;
            var nff = MatStream.GetFlowsheet().FlowsheetOptions.FractionNumberFormat;

            s.CreateAndAddLabelRow(container, "Material Stream Property Editor");

            s.CreateAndAddDescriptionRow(container, "Except for compound amounts, property values are updated/stored as they are changed/edited.");

            s.CreateAndAddLabelRow(container, "Material Stream Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", MatStream.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", MatStream.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                MatStream.GraphicObject.Tag = arg3.Text;
            }, () => CallSolverIfNeeded());

            s.CreateAndAddDropDownRow(container, "Compound Amount Basis",
            new List<string>() { "Molar Fractions", "Mass Fractions", "Volumetric Fractions", "Molar Flows", "Mass Flows", "Volumetric Flows", "Default" },
            (int)MatStream.FloatingTableAmountBasis, (sender, e) =>
            {
                MatStream.FloatingTableAmountBasis = (DWSIM.Interfaces.Enums.CompositionBasis)sender.SelectedIndex;
            });
            s.CreateAndAddDescriptionRow(container, "Select the basis to display compound amounts in floating tables, if enabled.");

            s.CreateAndAddLabelRow(container, "Property Package");

            var proppacks = MatStream.GetFlowsheet().PropertyPackages.Values.Select((x) => x.Tag).ToList();

            if (proppacks.Count == 0)
            {
                MatStream.GetFlowsheet().ShowMessage("Error: please add at least one Property Package before continuing.", IFlowsheet.MessageType.GeneralError);
            }
            else
            {
                var selectedpp = MatStream.PropertyPackage.Tag;
                s.CreateAndAddDropDownRow(container, "Property Package", proppacks, proppacks.IndexOf(selectedpp), (DropDown arg1, EventArgs ev) =>
                {
                    MatStream.PropertyPackage = (PropertyPackage)MatStream.GetFlowsheet().PropertyPackages.Values.Where((x) => x.Tag == proppacks[arg1.SelectedIndex]).FirstOrDefault();
                }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); } );
            }

            var flashalgos = MatStream.GetFlowsheet().FlowsheetOptions.FlashAlgorithms.Select(x => x.Tag).ToList();
            flashalgos.Insert(0, "Default");

            var cbFlashAlg = s.CreateAndAddDropDownRow(container, "Flash Algorithm", flashalgos, 0, null);

            if (!string.IsNullOrEmpty(MatStream.PreferredFlashAlgorithmTag))
                cbFlashAlg.SelectedIndex = Array.IndexOf(flashalgos.ToArray(), MatStream.PreferredFlashAlgorithmTag);
            else
                cbFlashAlg.SelectedIndex = 0;

            cbFlashAlg.SelectedIndexChanged += (sender, e) =>
            {
                MatStream.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedValue.ToString();
                if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke();
            };

            s.CreateAndAddLabelRow(container, "Properties");

            switch (MatStream.GraphicObject.ObjectType)
            {
                case ObjectType.MaterialStream:
                    var ms = MatStream;
                    int position = 0;
                    Double val;
                    switch (ms.SpecType)
                    {
                        case StreamSpec.Temperature_and_Pressure:
                            position = 0;
                            break;
                        case StreamSpec.Temperature_and_VaporFraction:
                            position = 1;
                            break;
                        case StreamSpec.Pressure_and_VaporFraction:
                            position = 2;
                            break;
                        case StreamSpec.Pressure_and_Enthalpy:
                            position = 3;
                            break;
                        case StreamSpec.Pressure_and_Entropy:
                            position = 4;
                            break;
                    }
                    if (ms.GraphicObject.InputConnectors[0].IsAttached &&
                        ms.GraphicObject.InputConnectors[0].AttachedConnector.AttachedFrom.ObjectType != ObjectType.OT_Recycle)
                    {
                        s.CreateAndAddDescriptionRow(container, "This Material Stream is connected to another object through its inlet port and cannot be edited.");
                    }
                    else
                    {
                        s.CreateAndAddDropDownRow(container, "Flash Specification", StringResources.flash_spec().ToList(), position, (DropDown arg3, EventArgs ev) =>
                        {
                            switch (arg3.SelectedIndex)
                            {
                                case 0:
                                    ms.SpecType = StreamSpec.Temperature_and_Pressure;
                                    break;
                                case 1:
                                    ms.SpecType = StreamSpec.Temperature_and_VaporFraction;
                                    break;
                                case 2:
                                    ms.SpecType = StreamSpec.Pressure_and_VaporFraction;
                                    break;
                                case 3:
                                    ms.SpecType = StreamSpec.Pressure_and_Enthalpy;
                                    break;
                                case 4:
                                    ms.SpecType = StreamSpec.Pressure_and_Entropy;
                                    break;
                            }
                        });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Flash Specification"));
                        s.CreateAndAddTextBoxRow(container, nf, "Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, ms.Phases[0].Properties.temperature.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.temperature = cv.ConvertToSI(su.temperature, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Temperature"));
                        s.CreateAndAddTextBoxRow(container, nf, "Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, ms.Phases[0].Properties.pressure.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.pressure = cv.ConvertToSI(su.pressure, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Pressure"));
                        var txtW = s.CreateAndAddTextBoxRow(container, nf, "Mass Flow (" + su.massflow + ")", cv.ConvertFromSI(su.massflow, ms.Phases[0].Properties.massflow.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.volumetric_flow = null;
                                                       ms.Phases[0].Properties.molarflow = null;
                                                       ms.Phases[0].Properties.massflow = cv.ConvertToSI(su.massflow, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Mass Flow"));
                        var txtQ = s.CreateAndAddTextBoxRow(container, nf, "Molar Flow (" + su.molarflow + ")", cv.ConvertFromSI(su.molarflow, ms.Phases[0].Properties.molarflow.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.massflow = null;
                                                       ms.Phases[0].Properties.volumetric_flow = null;
                                                       ms.Phases[0].Properties.molarflow = cv.ConvertToSI(su.molarflow, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Molar Flow"));
                        s.CreateAndAddTextBoxRow(container, nf, "Volumetric Flow (" + su.volumetricFlow + ")", cv.ConvertFromSI(su.volumetricFlow, ms.Phases[0].Properties.volumetric_flow.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.massflow = null;
                                                       ms.Phases[0].Properties.molarflow = null;
                                                       ms.Phases[0].Properties.volumetric_flow = cv.ConvertToSI(su.volumetricFlow, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Volumetric Flow"));
                        s.CreateAndAddTextBoxRow(container, nf, "Vapor Phase Mole Fraction (spec)", ms.Phases[2].Properties.molarfraction.GetValueOrDefault(),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[2].Properties.molarfraction = arg3.Text.ToString().ParseExpressionToDouble();
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });

                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Vapor Phase Mole Fraction (spec)"));
                        s.CreateAndAddTextBoxRow(container, nf, "Specific Enthalpy (" + su.enthalpy + ")", cv.ConvertFromSI(su.enthalpy, ms.Phases[0].Properties.enthalpy.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.enthalpy = cv.ConvertToSI(su.enthalpy, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Specific Enthalpy"));
                        s.CreateAndAddTextBoxRow(container, nf, "Specific Entropy (" + su.entropy + ")", cv.ConvertFromSI(su.entropy, ms.Phases[0].Properties.entropy.GetValueOrDefault()),
                                               (TextBox arg3, EventArgs ev) =>
                                               {
                                                   if (arg3.Text.IsValidDoubleExpression())
                                                   {
                                                       arg3.TextColor = (SystemColors.ControlText);
                                                       ms.Phases[0].Properties.entropy = cv.ConvertToSI(su.entropy, arg3.Text.ToString().ParseExpressionToDouble());
                                                   }
                                                   else
                                                   {
                                                       arg3.TextColor = (Colors.Red);
                                                   }
                                               }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke(); });
                        s.CreateAndAddDescriptionRow(container, ms.GetPropertyDescription("Specific Entropy"));

                        s.CreateAndAddLabelRow(container, "Mixture Composition");

                        s.CreateAndAddDescriptionRow(container, "Composition changes will only be committed after clicking on the 'Accept' button.");

                        DropDown spinner1 = s.CreateAndAddDropDownRow(container, "Amount Basis", StringResources.mscompinputtype().ToList(), 0, null);

                        var tblist = new List<TextBox>();

                        foreach (var comp in ms.Phases[0].Compounds)
                        {
                            var tbox = s.CreateAndAddTextBoxRow(container, nf, comp.Key, comp.Value.MoleFraction.GetValueOrDefault(),
                                                   (TextBox arg3, EventArgs ev) => { });
                            tbox.Tag = comp.Key;
                            tblist.Add(tbox);
                        }

                        spinner1.SelectedIndexChanged += (sender, e) =>
                        {
                            var W = ms.Phases[0].Properties.massflow.GetValueOrDefault();
                            var Q = ms.Phases[0].Properties.molarflow.GetValueOrDefault();
                            switch (spinner1.SelectedIndex)
                            {
                                case 0:
                                    foreach (var etext in tblist)
                                    {
                                        etext.Text = ms.Phases[0].Compounds[(String)etext.Tag].MoleFraction.GetValueOrDefault().ToString(nff);
                                    }
                                    break;
                                case 1:
                                    foreach (var etext in tblist)
                                    {
                                        etext.Text = ms.Phases[0].Compounds[(String)etext.Tag].MassFraction.GetValueOrDefault().ToString(nff);
                                    }
                                    break;
                                case 2:
                                    foreach (var etext in tblist)
                                    {
                                        etext.Text = (ms.Phases[0].Compounds[(String)etext.Tag].MoleFraction.GetValueOrDefault() * Q).ToString(nff);
                                    }
                                    break;
                                case 3:
                                    foreach (var etext in tblist)
                                    {
                                        etext.Text = (ms.Phases[0].Compounds[(String)etext.Tag].MassFraction.GetValueOrDefault() * W).ToString(nff);
                                    }
                                    break;
                            }
                        };
                        
                        Double total = 0.0f;

                        var btnNormalize = s.CreateAndAddButtonRow(container, "Normalize", null, null);
                        btnNormalize.Font = new Font(SystemFont.Default, s.GetEditorFontSize());

                        btnNormalize.Click += (sender, e) =>
                        {
                            total = 0.0f;
                            foreach (var etext in tblist)
                            {
                                if (Double.TryParse(etext.Text.ToString(), out val))
                                {
                                    etext.TextColor = (SystemColors.ControlText);
                                    total += Double.Parse(etext.Text.ToString());
                                }
                                else
                                {
                                    etext.TextColor = (Colors.Red);
                                    //Toast.MakeText(this.Context, "Error parsing '" + etext.Text + "' for " + (String)etext.Tag + ", not a valid number. Please input a valid number and try again.", ToastLength.Short).Show();
                                }
                            }
                            foreach (var etext in tblist)
                            {
                                if (Double.TryParse(etext.Text.ToString(), out val))
                                {
                                    etext.Text = (Double.Parse(etext.Text.ToString()) / total).ToString(nff);
                                }
                            }
                        };

                        var btnEqualize = s.CreateAndAddButtonRow(container, "Equalize", null, null);
                        btnEqualize.Font = new Font(SystemFont.Default, s.GetEditorFontSize());

                        btnEqualize.Click += (sender, e) =>
                        {
                            foreach (var etext in tblist)
                            {
                                etext.Text = (1.0 / tblist.Count).ToString(nff);
                            }
                        };

                        var btnClear = s.CreateAndAddButtonRow(container, "Clear", null, null);
                        btnClear.Font = new Font(SystemFont.Default, s.GetEditorFontSize());

                        btnClear.Click += (sender, e) =>
                        {
                            foreach (var etext in tblist)
                            {
                                etext.Text = 0.0f.ToString(nff);
                            }
                        };

                        var btnAccept = s.CreateAndAddButtonRow(container, "Accept/Update", null, null);
                        btnAccept.Font = new Font(SystemFont.Default, s.GetEditorFontSize());

                        btnAccept.Click += (sender, e) =>
                        {

                            Double W, Q, mtotal = 0.0f, mmtotal = 0.0f;

                            total = 0.0f;

                            switch (spinner1.SelectedIndex)
                            {

                                case 0:

                                    btnNormalize.PerformClick();
                                    foreach (var etext in tblist)
                                    {
                                        if (Double.TryParse(etext.Text.ToString(), out val))
                                        {
                                            MatStream.Phases[0].Compounds[(String)etext.Tag].MoleFraction = Double.Parse(etext.Text.ToString());
                                        }
                                        else
                                        {
                                            //Toast.MakeText(this.Context, "Error parsing '" + etext.Text + "' for " + (String)etext.Tag + ", not a valid number. Please input a valid number and try again.", ToastLength.Short).Show();
                                        }
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        mtotal += comp.MoleFraction.GetValueOrDefault() * comp.ConstantProperties.Molar_Weight;
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault() * comp.ConstantProperties.Molar_Weight / mtotal;
                                    }

                                    break;

                                case 1:

                                    btnNormalize.PerformClick();
                                    foreach (var etext in tblist)
                                    {
                                        if (Double.TryParse(etext.Text.ToString(), out val))
                                        {
                                            MatStream.Phases[0].Compounds[(String)etext.Tag].MassFraction = Double.Parse(etext.Text.ToString());
                                        }
                                        else
                                        {
                                            //Toast.MakeText(this.Context, "Error parsing '" + etext.Text + "' for " + (String)etext.Tag + ", not a valid number. Please input a valid number and try again.", ToastLength.Short).Show();
                                        }
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        mmtotal += comp.MassFraction.GetValueOrDefault() / comp.ConstantProperties.Molar_Weight;
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        comp.MoleFraction = comp.MassFraction.GetValueOrDefault() / comp.ConstantProperties.Molar_Weight / mmtotal;
                                    }

                                    break;

                                case 2:

                                    total = 0;
                                    foreach (var etext in tblist)
                                    {
                                        if (Double.TryParse(etext.Text.ToString(), out val))
                                        {
                                            total += Double.Parse(etext.Text.ToString());
                                        }
                                        else
                                        {
                                            //Toast.MakeText(this.Context, "Error parsing '" + etext.Text + "' for " + (String)etext.Tag + ", not a valid number. Please input a valid number and try again.", ToastLength.Short).Show();
                                        }
                                    }

                                    Q = cv.ConvertToSI(su.molarflow, total);
                                    foreach (var etext in tblist)
                                    {
                                        MatStream.Phases[0].Compounds[(String)etext.Tag].MoleFraction = Double.Parse(etext.Text.ToString()) / total;
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        mtotal += comp.MoleFraction.GetValueOrDefault() * comp.ConstantProperties.Molar_Weight;
                                    }

                                    W = 0;
                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        comp.MassFraction = comp.MoleFraction.GetValueOrDefault() * comp.ConstantProperties.Molar_Weight / mtotal;
                                        W += comp.MoleFraction.GetValueOrDefault() * comp.ConstantProperties.Molar_Weight / 1000 * Q;
                                    }

                                    MatStream.Phases[0].Properties.molarflow = Q;
                                    MatStream.Phases[0].Properties.massflow = W;

                                    txtQ.Text = cv.ConvertFromSI(su.molarflow, Q).ToString(nf);

                                    break;

                                case 3:

                                    total = 0;
                                    foreach (var etext in tblist)
                                    {
                                        if (Double.TryParse(etext.Text.ToString(), out val))
                                        {
                                            total += Double.Parse(etext.Text.ToString());
                                        }
                                        else
                                        {
                                            //Toast.MakeText(this.Context, "Error parsing '" + etext.Text + "' for " + (String)etext.Tag + ", not a valid number. Please input a valid number and try again.", ToastLength.Short).Show();
                                        }
                                    }

                                    W = cv.ConvertToSI(su.massflow, total);
                                    foreach (var etext in tblist)
                                    {
                                        MatStream.Phases[0].Compounds[(String)etext.Tag].MassFraction = Double.Parse(etext.Text.ToString()) / total;
                                    }

                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        mmtotal += comp.MassFraction.GetValueOrDefault() / comp.ConstantProperties.Molar_Weight;
                                    }

                                    Q = 0;
                                    foreach (var comp in MatStream.Phases[0].Compounds.Values)
                                    {
                                        comp.MoleFraction = comp.MassFraction.GetValueOrDefault() / comp.ConstantProperties.Molar_Weight / mmtotal;
                                        Q += comp.MassFraction.GetValueOrDefault() * W / comp.ConstantProperties.Molar_Weight * 1000;
                                    }

                                    MatStream.Phases[0].Properties.molarflow = Q;
                                    MatStream.Phases[0].Properties.massflow = W;

                                    txtW.Text = cv.ConvertFromSI(su.massflow, W).ToString(nf);

                                    break;
                            }

                            if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)MatStream.GetFlowsheet()).HighLevelSolve.Invoke();

                        };

                        s.CreateAndAddEmptySpace(container);
                        s.CreateAndAddEmptySpace(container);
                        s.CreateAndAddEmptySpace(container);
                        s.CreateAndAddEmptySpace(container);

                    }
                    break;
            }
        }


    }

}

