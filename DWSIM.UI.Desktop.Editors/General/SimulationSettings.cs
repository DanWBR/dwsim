using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.SharedClasses.SystemsOfUnits;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class SimulationSettings
    {

        public DWSIM.UI.Desktop.Shared.Flowsheet flowsheet;
        public DynamicLayout container;

        public SimulationSettings(DWSIM.UI.Desktop.Shared.Flowsheet fs, DynamicLayout layout)
        {
            flowsheet = fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            s.CreateAndAddLabelRow(container, "General");

            s.CreateAndAddStringEditorRow(container, "Simulation Name", flowsheet.FlowsheetOptions.SimulationName, (sender, e) => flowsheet.FlowsheetOptions.SimulationName = sender.Text);

            s.CreateAndAddDescriptionRow(container, "The simulation name will be used for report identification and file name during saving.");

            s.CreateAndAddCheckBoxRow(container, "Enable Undo/Redo",
                flowsheet.FlowsheetOptions.EnabledUndoRedo,
                (chk, e) => flowsheet.FlowsheetOptions.EnabledUndoRedo = chk.Checked.GetValueOrDefault());

            s.CreateAndAddDescriptionRow(container, "Enables Undo/Redo feature and starts listening for reversible actions.");

            var avunits = flowsheet.AvailableSystemsOfUnits.Select((x) => x.Name).ToList();

            s.CreateAndAddLabelRow(container, "Flowsheet Objects");

            s.CreateAndAddCheckBoxRow(container, "Force calculation of flowsheet objects even when input data doesn`t change",
                flowsheet.FlowsheetOptions.ForceObjectSolving, 
                (chk, e) => flowsheet.FlowsheetOptions.ForceObjectSolving = chk.Checked.GetValueOrDefault());

            s.CreateAndAddLabelRow(container, "Specification Blocks");

            s.CreateAndAddDropDownRow(container, "Calculation Mode", new List<string> { "After Source Object", "Before Target Object", "Before Flowsheet", "After Flowsheet" },
                (int)flowsheet.FlowsheetOptions.SpecCalculationMode, (dd, e) => {
                    flowsheet.FlowsheetOptions.SpecCalculationMode = (DWSIM.Interfaces.Enums.SpecCalcMode)dd.SelectedIndex;
                });

            s.CreateAndAddLabelRow(container, "System of Units");

            Button btnEdit = null;
            DropDown uselector = null;

            uselector = s.CreateAndAddDropDownRow(container, "System of Units", avunits, avunits.IndexOf(flowsheet.FlowsheetOptions.SelectedUnitSystem.Name), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.SelectedUnitSystem = flowsheet.AvailableSystemsOfUnits.Where((x) => x.Name == avunits[sender.SelectedIndex]).FirstOrDefault();
                btnEdit.Enabled = !new string[] { "SI", "CGS", "ENG" }.Contains(uselector.SelectedValue.ToString());
            });

            s.CreateAndAddDescriptionRow(container, "Select the System of Units to be used on this simulation");

            btnEdit = s.CreateAndAddLabelAndButtonRow(container, "Edit System of Units", "Edit Selected", null, (sender, e) =>
            {
                var editcontainer = new Editors.UnitSetEditorView((DWSIM.SharedClasses.SystemsOfUnits.Units)flowsheet.FlowsheetOptions.SelectedUnitSystem);
                var form = s.GetDefaultEditorForm("Edit System of Units", 400, 600, editcontainer);
                form.Closed += (sender2, e2) =>
                {
                    container.RemoveAll();
                    container.Clear();
                    Initialize();
                    container.Create();
                };
                form.Show();
            });

            s.CreateAndAddLabelAndButtonRow(container, "Create New System of Units", "Create New", null, (sender, e) =>
            {
                var newsystem = new DWSIM.SharedClasses.SystemsOfUnits.SI { Name = "NewUnitSet" };
                flowsheet.AvailableSystemsOfUnits.Add(newsystem);
                flowsheet.FlowsheetOptions.SelectedUnitSystem = newsystem;
                var editcontainer = new Editors.UnitSetEditorView(newsystem);
                var form = s.GetDefaultEditorForm("Create New System of Units", 400, 600, editcontainer);
                form.Closed += (sender2, e2) =>
                {
                    container.RemoveAll();
                    container.Clear();
                    Initialize();
                    container.Create();
                };
                form.Show();
            });

            btnEdit.Enabled = !new string[] { "SI", "CGS", "ENG" }.Contains(uselector.SelectedValue.ToString());

            var nformats = new[] { "F", "G", "G2", "G4", "G6", "G8", "G10", "N", "N2", "N4", "N6", "R", "E", "E1", "E2", "E3", "E4", "E6" };

            s.CreateAndAddLabelRow(container, "Mass and Energy Balances");

            s.CreateAndAddDropDownRow(container, "Flowsheet object mass balance check", new List<string>() { "Ignore", "Show Warning", "Throw Exception" }, (int)flowsheet.FlowsheetOptions.MassBalanceCheck, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.MassBalanceCheck = (DWSIM.Interfaces.Enums.WarningType)sender.SelectedIndex;
            });

            s.CreateAndAddTextBoxRow(container, "G", "Mass balance relative tolerance", flowsheet.FlowsheetOptions.MassBalanceRelativeTolerance, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) flowsheet.FlowsheetOptions.MassBalanceRelativeTolerance = sender.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddDropDownRow(container, "Flowsheet object energy balance check", new List<string>() { "Ignore", "Show Warning", "Throw Exception" }, (int)flowsheet.FlowsheetOptions.EnergyBalanceCheck, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.EnergyBalanceCheck = (DWSIM.Interfaces.Enums.WarningType)sender.SelectedIndex;
            });

            s.CreateAndAddTextBoxRow(container, "G", "Energy balance relative tolerance", flowsheet.FlowsheetOptions.EnergyBalanceRelativeTolerance, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) flowsheet.FlowsheetOptions.EnergyBalanceRelativeTolerance = sender.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddLabelRow(container, "Number Formats");

            s.CreateAndAddDropDownRow(container, "General", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.NumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.NumberFormat = sender.SelectedValue.ToString();
            });

            s.CreateAndAddDescriptionRow(container, "Select the formatting scheme for general numbers.");

            s.CreateAndAddDropDownRow(container, "Compound Amounts", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.FractionNumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.FractionNumberFormat = sender.SelectedValue.ToString();
            });

            s.CreateAndAddDescriptionRow(container, "Select the formatting scheme for compound amounts in Material Stream reports.");

            s.CreateAndAddLabelRow(container, "Display Real-Time Properties");

            s.CreateAndAddLabelRow2(container, "Material Streams");

            s.CreateAndAddCheckBoxRow(container, "Temperature", flowsheet.FlowsheetOptions.DisplayMaterialStreamTemperatureValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamTemperatureValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Pressure", flowsheet.FlowsheetOptions.DisplayMaterialStreamPressureValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamPressureValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Mass Flow", flowsheet.FlowsheetOptions.DisplayMaterialStreamMassFlowValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamMassFlowValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Molar Flow", flowsheet.FlowsheetOptions.DisplayMaterialStreamMolarFlowValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamMolarFlowValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Volumetric Flow", flowsheet.FlowsheetOptions.DisplayMaterialStreamVolFlowValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamVolFlowValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Energy Flow", flowsheet.FlowsheetOptions.DisplayMaterialStreamEnergyFlowValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamEnergyFlowValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Energy Flow", flowsheet.FlowsheetOptions.DisplayMaterialStreamEnergyFlowValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayMaterialStreamEnergyFlowValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow2(container, "Energy Streams");

            s.CreateAndAddCheckBoxRow(container, "Power/Heat/Energy Flow", flowsheet.FlowsheetOptions.DisplayEnergyStreamPowerValue, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayEnergyStreamPowerValue = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow2(container, "Unit Operations");

            s.CreateAndAddCheckBoxRow(container, "Dynamic Mode Properties", flowsheet.FlowsheetOptions.DisplayDynamicPropertyValues, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayDynamicPropertyValues = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow(container, "Floating Tables and Anchored Property Lists");

            s.CreateAndAddCheckBoxRow(container, "Display Floating Tables", flowsheet.FlowsheetOptions.DisplayFloatingPropertyTables, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayFloatingPropertyTables = sender.Checked.GetValueOrDefault();
                var surface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)(flowsheet.GetSurface());
                surface.DrawFloatingTable = flowsheet.FlowsheetOptions.DisplayFloatingPropertyTables;
            });

            s.CreateAndAddCheckBoxRow(container, "Display Compound Amount Floating Tables for Material Streams", flowsheet.FlowsheetOptions.DisplayFloatingTableCompoundAmounts, (sender, e) =>
            {
                flowsheet.Options.DisplayFloatingTableCompoundAmounts = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddDropDownRow(container, "Compound Amount Default Basis", 
                new List<string>() { "Molar Fractions", "Mass Fractions","Volumetric Fractions","Molar Flows","Mass Flows","Volumetric Flows" }, 
                (int)flowsheet.FlowsheetOptions.DefaultFloatingTableCompoundAmountBasis, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DefaultFloatingTableCompoundAmountBasis = (DWSIM.Interfaces.Enums.CompositionBasis)sender.SelectedIndex;
            });

            s.CreateAndAddCheckBoxRow(container, "Display Anchored Property Lists", flowsheet.FlowsheetOptions.DisplayCornerPropertyList, (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayCornerPropertyList = sender.Checked.GetValueOrDefault();
                var surface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)(flowsheet.GetSurface());
                surface.DrawPropertyList = flowsheet.FlowsheetOptions.DisplayCornerPropertyList;
            });

            s.CreateAndAddLabelRow(container, "Anchored Property List Settings");

            var fonts = Eto.Drawing.Fonts.AvailableFontFamilies.Select((x) => x.Name.ToLower()).ToList();

            if (!fonts.Contains(flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName.ToLower()))
            {
                flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName = Eto.Drawing.FontFamilies.MonospaceFamilyName;
            }

            var fp = new FontPicker(new Eto.Drawing.Font(flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName, flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontSize));

            fp.ValueChanged += (sender, e) =>
            {
                if (fp.Value != null)
                {
                    flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontName = fp.Value.FamilyName;
                    flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontSize = (int)fp.Value.Size;
                }
            };

            s.CreateAndAddLabelAndControlRow(container, "Font Name and Size", fp);

            var colors = new SkiaSharp.SKColors();

            var fontcolors = colors.GetType().GetFields().Select((x) => x.Name).ToList();

            s.CreateAndAddDropDownRow(container, "Font Color", fontcolors, fontcolors.IndexOf(flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontColor),
            (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayCornerPropertyListFontColor = fontcolors[sender.SelectedIndex];
            });

            var paddings = new List<string>() { "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" };

            s.CreateAndAddDropDownRow(container, "Line Padding", paddings, paddings.IndexOf(flowsheet.FlowsheetOptions.DisplayCornerPropertyListPadding.ToString()),
            (sender, e) =>
            {
                flowsheet.FlowsheetOptions.DisplayCornerPropertyListPadding = int.Parse(paddings[sender.SelectedIndex]);
            });

        }

    }

}