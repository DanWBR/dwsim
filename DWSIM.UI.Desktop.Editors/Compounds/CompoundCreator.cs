using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using Eto.Drawing;
using Eto.Forms;
using c = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UI.Desktop.Shared;

using DWSIM.ExtensionMethods;
using System.IO;

namespace DWSIM.UI.Desktop.Editors
{
    public class CompoundCreatorView : DynamicLayout
    {

        public Flowsheet flowsheet;

        private DWSIM.Thermodynamics.Utilities.Hypos.Methods.Joback joback = new  Thermodynamics.Utilities.Hypos.Methods.Joback();
        private DWSIM.Thermodynamics.BaseClasses.ConstantProperties comp = new  ConstantProperties();

        public CompoundCreatorView(Flowsheet fs) :
            base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            comp.OriginalDB = "User";
            comp.CurrentDB = "User";
            comp.Name = "MyCompound";

            var nf = flowsheet.Options.NumberFormat;
            var su = flowsheet.Options.SelectedUnitSystem;

            c.CreateAndAddLabelRow(this, "Identification");

            c.CreateAndAddStringEditorRow(this, "Name", comp.Name, (arg1, arg2) =>
            {
                comp.Name = arg1.Text;
            });
            c.CreateAndAddDescriptionRow(this, "Name is required.");

            c.CreateAndAddStringEditorRow(this, "CAS ID", comp.CAS_Number, (arg1, arg2) =>
            {
                comp.CAS_Number = arg1.Text;
            });
            c.CreateAndAddDescriptionRow(this, "CAS ID is optional.");

            c.CreateAndAddStringEditorRow(this, "Formula", comp.Formula, (arg1, arg2) =>
            {
                comp.Formula = arg1.Text;
            });
            c.CreateAndAddDescriptionRow(this, "Formula is optional.");

            c.CreateAndAddButtonRow(this, "Enter/Edit UNIFAC Information", null, (arg1, arg2) =>
            {
                var ll = c.GetDefaultContainer();
                c.CreateAndAddDescriptionRow(ll, "Enter the amount of each group in the molecule.");
                foreach (var item in joback.UNIFACLines)
                {
                    var id = Int32.Parse(item.Split(',')[1]);
                    var desc = item.Split(',')[3];
                    c.CreateAndAddTextBoxRow(ll, "0", desc, comp.UNIFACGroups.ContainsKey(id) ? comp.UNIFACGroups[id].ToString().ToDoubleFromCurrent() : 0d, (arg11, arg22) =>
                    {
                        if (c.IsValidDouble(arg11.Text)) comp.UNIFACGroups[id] = Int32.Parse(arg11.Text);
                    });
                }
                var alert = c.GetDefaultEditorForm("Edit UNIFAC Group Information", 300, 500, ll);
                alert.Show();
            });

            c.CreateAndAddButtonRow(this, "Enter/Edit Modified UNIFAC Information", null, (arg1, arg2) =>
            {
                var ll = c.GetDefaultContainer();
                c.CreateAndAddDescriptionRow(ll,  "Enter the amount of each group in the molecule.");
                foreach (var item in joback.MODFACLines)
                {
                    var id = Int32.Parse(item.Split(';')[3]);
                    var desc = item.Split(';')[2];
                    c.CreateAndAddTextBoxRow(ll, "0", desc, comp.MODFACGroups.ContainsKey(id) ? comp.MODFACGroups[id].ToString().ToDoubleFromCurrent() : 0d, (arg11, arg22) =>
                    {
                        if (c.IsValidDouble(arg11.Text)) comp.MODFACGroups[id] = Int32.Parse(arg11.Text);
                    });
                }
                var alert = c.GetDefaultEditorForm("Edit MODFAC Group Information", 300, 500, ll);
                alert.Show();
            });

            c.CreateAndAddLabelRow(this, "Basic Properties");

            TextBox mw = new TextBox(),
                tc = new TextBox(),
                pc = new TextBox(),
                zc = new TextBox(),
                zr = new TextBox(),
                af = new TextBox(),
                nbp = new TextBox(),
                gf = new TextBox(),
                tf = new TextBox(),
                hfus = new TextBox(),
                hf = new TextBox();

            c.CreateAndAddButtonRow(this, "Estimate from UNIFAC Information", null, (arg1, arg2) =>
            {
                var uf = joback.GetUNIFACList(comp.UNIFACGroups);
                var jc = joback.GetJCFromUNIFAC(uf);
                var acl = joback.GetACLFromUNIFAC(uf);
                mw.Text = joback.CalcMW(acl).ToString(nf);
                nbp.Text = cv.ConvertFromSI(su.temperature, joback.CalcTb(jc)).ToString(nf);
                tc.Text = cv.ConvertFromSI(su.temperature, joback.CalcTc(comp.Normal_Boiling_Point, jc)).ToString(nf);
                pc.Text = cv.ConvertFromSI(su.pressure, joback.CalcPc(jc)).ToString(nf);
                tf.Text = cv.ConvertFromSI(su.temperature, joback.CalcTf(jc)).ToString(nf);
                hfus.Text = (joback.CalcHf(jc)).ToString(nf);
                hf.Text = cv.ConvertFromSI(su.enthalpy, joback.CalcDHf(jc) / comp.Molar_Weight).ToString(nf);
                gf.Text = cv.ConvertFromSI(su.enthalpy, joback.CalcDGf(jc) / comp.Molar_Weight).ToString(nf);
                comp.Critical_Volume = joback.CalcVc(jc);
                zc.Text = (comp.Critical_Pressure * comp.Critical_Volume / comp.Critical_Temperature / 8.314 / 1000).ToString(nf);
                zr.Text = zc.Text;
                af.Text = DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods.PropertyMethods.AcentricFactor_LeeKesler(comp.Critical_Temperature, comp.Critical_Pressure, comp.Normal_Boiling_Point).ToString(nf);
                comp.IdealgasCpEquation = "4";
                comp.Ideal_Gas_Heat_Capacity_Const_A = joback.CalcCpA(jc) * 1000;
                comp.Ideal_Gas_Heat_Capacity_Const_B = joback.CalcCpB(jc) * 1000;
                comp.Ideal_Gas_Heat_Capacity_Const_C = joback.CalcCpC(jc) * 1000;
                comp.Ideal_Gas_Heat_Capacity_Const_D = joback.CalcCpD(jc) * 1000;
            });
            c.CreateAndAddDescriptionRow(this, "This will estimate most properties using the informed UNIFAC structure.");


            mw = c.CreateAndAddTextBoxRow(this, nf, "Molecular Weight" + FormatUnit(su.molecularWeight), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Molar_Weight = cv.ConvertToSI(su.molecularWeight, arg1.Text.ToDoubleFromCurrent());
            });

            nbp = c.CreateAndAddTextBoxRow(this, nf, "Normal Boiling Point" + FormatUnit(su.temperature), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Normal_Boiling_Point = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            tc = c.CreateAndAddTextBoxRow(this, nf, "Critical Temperature" + FormatUnit(su.temperature), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Temperature = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            pc = c.CreateAndAddTextBoxRow(this, nf, "Critical Pressure" + FormatUnit(su.pressure), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Pressure = cv.ConvertToSI(su.pressure, arg1.Text.ToDoubleFromCurrent());
            });

            zc = c.CreateAndAddTextBoxRow(this, nf, "Critical Compressibility Factor", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Compressibility = arg1.Text.ToDoubleFromCurrent();
            });

            af = c.CreateAndAddTextBoxRow(this, nf, "Acentric Factor", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Acentric_Factor = arg1.Text.ToDoubleFromCurrent();
            });

            hf = c.CreateAndAddTextBoxRow(this, nf, "Enthalpy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.IG_Enthalpy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
            });

            gf = c.CreateAndAddTextBoxRow(this, nf, "Gibbs Energy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.IG_Gibbs_Energy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddLabelRow(this, "Model-Specific parameters");

            zr = c.CreateAndAddTextBoxRow(this, nf, "Rackett Parameter", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Z_Rackett = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddTextBoxRow(this, nf, "UNIQUAC Q", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.UNIQUAC_Q = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddTextBoxRow(this, nf, "UNIQUAC R", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.UNIQUAC_R = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddLabelRow(this, "Solid Phase Properties");

            tf = c.CreateAndAddTextBoxRow(this, nf, "Temperature of Fusion" + FormatUnit(su.temperature), 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.TemperatureOfFusion = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            hfus = c.CreateAndAddTextBoxRow(this, nf, "Enthalpy of Fusion (kJ/mol)", 0d, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.EnthalpyOfFusionAtTf = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddLabelRow(this, "Temperature-Dependent Properties");
            c.CreateAndAddDescriptionRow(this, "For temperature-dependent properties, you can let DWSIM estimate them or input an expression using T as variable. Use dot ('.') as the decimal separator when entering numerical values.");

            var options = new string[] { "Estimate", "Use Expression" };

            TextBox ev = new TextBox(),
                eigcp = new TextBox(),
                eld = new TextBox(),
                elcp = new TextBox(),
                elv = new TextBox(),
                esd = new TextBox(),
                escp = new TextBox();

            c.CreateAndAddDropDownRow(this, "Vapor Pressure", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        ev.Visible = false;
                        break;
                    case 1:
                        ev.Visible = true;
                        ev.Focus();
                        break;
                }
            });

            ev = c.CreateAndAddFullTextBoxRow(this, comp.VaporPressureEquation, (arg1, arg2) =>
            {
                comp.VaporPressureEquation = arg1.Text;
            });
            ev.Visible = false;
            ev.PlaceholderText = "T in K, Pvap in Pa";

            c.CreateAndAddDropDownRow(this, "Ideal Gas Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        eigcp.Visible = false;
                        break;
                    case 1:
                        eigcp.Visible = true;
                        eigcp.Focus();
                        break;
                }
            });

            eigcp = c.CreateAndAddFullTextBoxRow(this, comp.IdealgasCpEquation, (arg1, arg2) =>
            {
                comp.IdealgasCpEquation = arg1.Text;
            });
            eigcp.Visible = false;
            eigcp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            c.CreateAndAddDropDownRow(this, "Liquid Density", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        eld.Visible = false;
                        break;
                    case 1:
                        eld.Visible = true;
                        eld.Focus();
                        break;
                }
            });

            eld = c.CreateAndAddFullTextBoxRow(this, comp.LiquidDensityEquation, (arg1, arg2) =>
            {
                comp.LiquidDensityEquation = arg1.Text;
            });
            eld.Visible = false;
            eld.PlaceholderText = "T in K, dens in kg/m3";

            c.CreateAndAddDropDownRow(this, "Liquid Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        elcp.Visible = false;
                        break;
                    case 1:
                        elcp.Visible = true;
                        elcp.Focus();
                        break;
                }
            });

            elcp = c.CreateAndAddFullTextBoxRow(this, comp.LiquidHeatCapacityEquation, (arg1, arg2) =>
            {
                comp.LiquidHeatCapacityEquation = arg1.Text;
            });
            elcp.Visible = false;
            elcp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            c.CreateAndAddDropDownRow(this, "Liquid Viscosity", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        elv.Visible = false;
                        break;
                    case 1:
                        elv.Visible = true;
                        elv.Focus();
                        break;
                }
            });

            elv = c.CreateAndAddFullTextBoxRow(this, comp.LiquidViscosityEquation, (arg1, arg2) =>
            {
                comp.LiquidViscosityEquation = arg1.Text;
            });
            elv.Visible = false;
            elv.PlaceholderText = "T in K, visc in Pa.s";

            c.CreateAndAddDropDownRow(this, "Solid Density", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        esd.Visible = false;
                        break;
                    case 1:
                        esd.Visible = true;
                        esd.Focus();
                        break;
                }
            });

            esd = c.CreateAndAddFullTextBoxRow(this, comp.SolidDensityEquation, (arg1, arg2) =>
            {
                comp.SolidDensityEquation = arg1.Text;
            });
            esd.Visible = false;
            esd.PlaceholderText = "T in K, dens in kg/m3";

            c.CreateAndAddDropDownRow(this, "Solid Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
            {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        escp.Visible = false;
                        break;
                    case 1:
                        escp.Visible = true;
                        escp.Focus();
                        break;
                }
            });

            escp = c.CreateAndAddFullTextBoxRow(this, comp.SolidHeatCapacityEquation, (arg1, arg2) =>
            {
                comp.SolidHeatCapacityEquation = arg1.Text;
            });
            escp.Visible = false;
            escp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            c.CreateAndAddLabelRow(this, "Export Options");

            c.CreateAndAddButtonRow(this, "Export Compound to JSON File", null, (arg1, arg2) =>
            {
                var dialog = new SaveFileDialog();
                dialog.Title = "Save Compound to JSON File";
                dialog.Filters.Add(new FileFilter("JSON File", new[] { ".json" }));
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(this) == DialogResult.Ok)
                {
                    try {
                        File.WriteAllText(dialog.FileName, Newtonsoft.Json.JsonConvert.SerializeObject(comp, Newtonsoft.Json.Formatting.Indented));
                        flowsheet.ShowMessage("Compound '" + comp.Name + "' successfully saved to JSON file.", IFlowsheet.MessageType.Information);
                    }
                    catch (Exception ex) {
                        flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);                    
                    }
                }
            });
            c.CreateAndAddDescriptionRow(this, "Export compound data to a JSON file for later use (recommended).");

            c.CreateAndAddButtonRow(this, "Add Compound to Simulation", null, (arg1, arg2) =>
            {
                if (flowsheet.AvailableCompounds.ContainsKey(comp.Name))
                {
                    flowsheet.ShowMessage("Compound '" + comp.Name + "' already exists.", IFlowsheet.MessageType.GeneralError);
                }
                else
                {
                    flowsheet.AvailableCompounds.Add(comp.Name, comp);
                    flowsheet.SelectedCompounds.Add(comp.Name, comp);
                    foreach (MaterialStream obj in flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream))
                    {
                        foreach (var phase in obj.Phases.Values)
                        {
                            phase.Compounds.Add(comp.Name, new DWSIM.Thermodynamics.BaseClasses.Compound(comp.Name, ""));
                            phase.Compounds[comp.Name].ConstantProperties = flowsheet.SelectedCompounds[comp.Name];
                        }
                    }
                    flowsheet.ShowMessage("Compound '" + comp.Name + "' added to the simulation.", IFlowsheet.MessageType.Information);
                }
            });
            c.CreateAndAddDescriptionRow(this, "Adds the compound directly to the current simulation.");

        }

        string FormatUnit(string units)
        {
            return " (" + units + ")";
        }
    }
}