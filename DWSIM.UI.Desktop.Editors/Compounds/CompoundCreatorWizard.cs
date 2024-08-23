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
using DWSIM.ExtensionMethods.Eto;
using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UI.Desktop.Shared;
using DWSIM.UI.Shared;

using DWSIM.ExtensionMethods;
using System.IO;
using DWSIM.SharedClassesCSharp.FilePicker;
using DWSIM.Thermodynamics.Databases.ChemeoLink;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Window;

namespace DWSIM.UI.Desktop.Editors
{
    public class CompoundCreatorWizard
    {

        public IFlowsheet flowsheet;

        private DWSIM.Thermodynamics.Utilities.Hypos.Methods.Joback joback = new Thermodynamics.Utilities.Hypos.Methods.Joback();
        private DWSIM.Thermodynamics.BaseClasses.ConstantProperties comp = new ConstantProperties();

        private static double sf = GlobalSettings.Settings.UIScalingFactor;
        private static double dpi = GlobalSettings.Settings.DpiScale;

        private int Width = (int)(800 * sf);
        private int Height = (int)(500 * sf);

        private int comptype = 0;
        private bool estimatefromunifac = true;
        private bool searchonline = false;

        private bool foundkdb = false, foundchemeo = false, foundddb = false;

        private string nf = "";
        private IUnitsOfMeasure su;

        ConstantProperties kdbc = null, chemeoc = null;
        Dictionary<string, List<string[]>> facdata = null;

        public CompoundCreatorWizard(IFlowsheet fs)
            : base()
        {

            if (flowsheet != null) flowsheet = fs;
            Init();

        }

        void Init()
        {

            if (GlobalSettings.Settings.OldUI)
            {
                Width = (int)(Width * dpi);
                Height = (int)(Height * dpi);
            }

            comp.ID = new Random().Next(700000, 800000);

            comp.OriginalDB = "User";
            comp.CurrentDB = "User";
            comp.Name = "MyCompound";

            if (flowsheet == null)
            {
                nf = "G";
                su = new SharedClasses.SystemsOfUnits.SI();
            }
            else
            {
                nf = flowsheet.FlowsheetOptions.NumberFormat;
                su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            }

        }

        public void SetupAndDisplayPage(int page)
        {
            switch (page)
            {
                case 1:
                    DisplayPage1();
                    break;
            }
        }

        private void DisplayPage1()
        {

            var page1 = new WizardPage();

            page1.hasBackButton = false;
            page1.hasCancelButton = true;
            page1.hasNextButton = true;
            page1.hasFinishButton = false;

            page1.cancelAction = () => page1.Close();
            page1.nextAction = () => { page1.Close(); DisplayPage2(); };

            page1.Title = "Compound Creator Wizard";
            page1.HeaderTitle = "Step 1 - Compound Type";
            page1.HeaderDescription = "Select the type of the compound to create.";
            page1.FooterText = "";

            page1.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Height = Height;
            dl.Width = Width - 20;

            var ctypes = new string[] { "Default", "Simplified", "Electrolyte", "Black-Oil" };

            c.CreateAndAddLabelRow(dl, "Compound Type");

            var drop = c.CreateAndAddDropDownRow(dl, "Compound Type", ctypes.ToList(), comptype, (sender, e) =>
            {
                comptype = sender.SelectedIndex;
            });

            drop.Size = new Size(200, 28);

            c.CreateAndAddLabelRow2(dl, "Default: a full data set is required to create the compound.");
            c.CreateAndAddLabelRow2(dl, "Simplified: a minimum data set is required to create the compound.");
            c.CreateAndAddLabelRow2(dl, "Electrolyte: the compound being created is an ion, salt or hydrated salt.");
            c.CreateAndAddLabelRow2(dl, "Black-Oil: the compound being created is based on black-oil data.");

            c.CreateAndAddLabelRow(dl, "Load Data");
            c.CreateAndAddLabelAndButtonRow(dl, "You can load compound data from an existing JSON file.", "Load Data", null, (sender, e) =>
            {
                if (GlobalSettings.Settings.OldUI)
                {
                    IFilePicker filePickerForm = FilePickerService.GetInstance().GetFilePicker();
                    IVirtualFile handler = filePickerForm.ShowOpenDialog(new List<FilePickerAllowedType> { new FilePickerAllowedType("JSON File", "*.json")});
                    if (handler != null)
                    {
                        try
                        {
                            comp = Newtonsoft.Json.JsonConvert.DeserializeObject<DWSIM.Thermodynamics.BaseClasses.ConstantProperties>(handler.ReadAllText());
                            estimatefromunifac = false;
                           MessageBox.Show("Data successfully loaded from JSON file.", "Information", MessageBoxButtons.OK, MessageBoxType.Information, MessageBoxDefaultButton.OK);
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show("Error loading compound data to JSON file: " + ex.ToString(), "Error", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                        }
                    }
                }
                else
                {
                    var dialog = new OpenFileDialog();
                    dialog.Title = "Load Compound Data";
                    dialog.Filters.Add(new FileFilter("JSON File", new[] { ".json" }));
                    dialog.CurrentFilterIndex = 0;
                    if (dialog.ShowDialog(page1) == DialogResult.Ok)
                    {
                        try
                        {
                            comp = Newtonsoft.Json.JsonConvert.DeserializeObject<DWSIM.Thermodynamics.BaseClasses.ConstantProperties>(File.ReadAllText(dialog.FileName));
                            estimatefromunifac = false;
                            MessageBox.Show("Data successfully loaded from JSON file.", "Information", MessageBoxButtons.OK, MessageBoxType.Information, MessageBoxDefaultButton.OK);
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show("Error loading compound data to JSON file: " + ex.ToString(), "Error", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                        }
                    }
                }
            });

            page1.SuspendLayout();
            page1.ContentContainer.Add(dl);
            page1.ResumeLayout();
            page1.SetFontAndPadding();
            page1.Show();

        }

        private void DisplayPage2()
        {

            var dl = c.GetDefaultContainer();

            var page2 = new WizardPage();

            page2.hasBackButton = true;
            page2.hasCancelButton = true;
            page2.hasNextButton = true;
            page2.hasFinishButton = false;

            page2.cancelAction = () => page2.Close();
            page2.backAction = () => { page2.Close(); DisplayPage1(); };
            page2.nextAction = () =>
            {
                if (searchonline)
                {
                    page2.btnBack.Enabled = false;
                    page2.btnNext.Enabled = false;
                    page2.btnCancel.Enabled = false;
                    dl.Enabled = false;
                    page2.footerSpinner.Visible = true;
                    page2.footerSpinner.Enabled = true;
                    page2.footerLabel.Text = "Searching compound data at online sources, please wait...";
                    var t1 = Task.Factory.StartNew(() => CheckOnlineData());
                    t1.ContinueWith((t) =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            page2.Close();
                            switch (comptype)
                            {
                                case 0:
                                    DisplayPage4();
                                    break;
                                case 1:
                                case 2:
                                case 3:
                                    DisplayPage3();
                                    break;
                            }
                        });
                    });
                }
                else
                {
                    page2.Close();
                    switch (comptype)
                    {
                        case 0:
                            DisplayPage4();
                            break;
                        case 1:
                        case 2:
                        case 3:
                            DisplayPage3();
                            break;
                    }
                }
            };

            page2.Title = "Compound Creator Wizard";
            page2.HeaderTitle = "Step 2 - Compound ID";
            page2.HeaderDescription = "Enter identification data for the compound.";
            page2.FooterText = "";

            page2.Init(Width, Height);

            dl.Width = Width - 20;

            dl.CreateAndAddLabelRow("Identification");
            dl.CreateAndAddStringEditorRow("Compound Name", comp.Name, (sender, e) => comp.Name = sender.Text);

            if (comptype < 2)
            {
                dl.CreateAndAddStringEditorRow("CAS ID (Optional)", comp.CAS_Number, (sender, e) => comp.CAS_Number = sender.Text);
                dl.CreateAndAddStringEditorRow("Chemical Formula (Optional)", comp.Formula, (sender, e) => comp.Formula = sender.Text);
                dl.CreateAndAddEmptySpace();
                dl.CreateAndAddEmptySpace();
                dl.CreateAndAddEmptySpace();
                dl.CreateAndAddEmptySpace();
                dl.CreateAndAddCheckBoxRow("Search Online Databases for Compound Data", searchonline, (sender, e) => searchonline = sender.Checked.GetValueOrDefault());
                dl.CreateAndAddLabelRow2("This will search selected online thermodynamic databases (KDB, Cheméo and DDB) for compound data according to its name and/or CAS ID.");
            }

            page2.ContentContainer.Add(new Scrollable { Content = dl, Border = BorderType.None, Height = Height, Width = Width });
            page2.SetFontAndPadding();
            page2.Show();

        }

        private void DisplayPage3()
        {

            var page2 = new WizardPage();

            page2.hasBackButton = true;
            page2.hasCancelButton = true;
            page2.hasNextButton = true;
            page2.hasFinishButton = false;

            page2.cancelAction = () => page2.Close();
            page2.backAction = () => { page2.Close(); DisplayPage2(); };
            page2.nextAction = () =>
            {
                page2.Close();
                switch (comptype)
                {
                    case 0:
                        break;
                    case 1:
                    case 2:
                    case 3:
                        DisplayLastPage();
                        break;
                }
            };

            page2.Title = "Compound Creator Wizard";
            page2.HeaderTitle = "Step 3 - Compound Properties";
            page2.HeaderDescription = "Define the basic properties of the compound.";
            if (foundkdb)
            {
                page2.FooterText = "DWSIM found compound data at KDB Online Database, check non-empty/non-zeroed fields.";
            }
            else if (foundchemeo)
            {
                page2.FooterText = "DWSIM found compound data at Cheméo Online Database, check non-empty/non-zeroed fields.";
            }
            else
            {
                page2.FooterText = "";
            }

            page2.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Width = Width - 20;

            switch (comptype)
            {
                case 0:
                    //default
                    break;
                case 1:
                    //simplified
                    dl.CreateAndAddLabelRow("Basic Properties");
                    dl.CreateAndAddLabelRow2("These are the minimum required set of properties.");
                    c.CreateAndAddTextBoxRow(dl, nf, "Molecular Weight" + FormatUnit(su.molecularWeight), comp.Molar_Weight, (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.Molar_Weight = cv.ConvertToSI(su.molecularWeight, arg1.Text.ToDoubleFromCurrent());
                    });
                    c.CreateAndAddTextBoxRow(dl, nf, "Normal Boiling Point" + FormatUnit(su.temperature), comp.Normal_Boiling_Point.ConvertFromSI(su.temperature), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.Normal_Boiling_Point = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
                    });
                    c.CreateAndAddTextBoxRow(dl, nf, "Critical Temperature" + FormatUnit(su.temperature), comp.Critical_Temperature.ConvertFromSI(su.temperature), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.Critical_Temperature = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
                    });
                    c.CreateAndAddTextBoxRow(dl, nf, "Critical Pressure" + FormatUnit(su.pressure), comp.Critical_Pressure.ConvertFromSI(su.pressure), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.Critical_Pressure = cv.ConvertToSI(su.pressure, arg1.Text.ToDoubleFromCurrent());
                    });
                    c.CreateAndAddTextBoxRow(dl, nf, "Acentric Factor", comp.Acentric_Factor, (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.Acentric_Factor = arg1.Text.ToDoubleFromCurrent();
                    });

                    dl.CreateAndAddLabelRow("Formation Properties");
                    dl.CreateAndAddLabelRow2("Formation properties are required if your compound will be part of a chemical reaction.");
                    c.CreateAndAddTextBoxRow(dl, nf, "Enthalpy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), comp.IG_Enthalpy_of_Formation_25C.ConvertFromSI(su.enthalpy), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.IG_Enthalpy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
                    });
                    c.CreateAndAddTextBoxRow(dl, nf, "Gibbs Energy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), comp.IG_Gibbs_Energy_of_Formation_25C.ConvertFromSI(su.enthalpy), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.IG_Gibbs_Energy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
                    });
                    dl.CreateAndAddLabelRow("Solid Phase Properties");
                    dl.CreateAndAddLabelRow2("Solid phase properties are required for solid prediction. If you don't have them, you can force this compound to be in the solid phase in the Property Package Advanced Settings editor.");
                    dl.CreateAndAddTextBoxRow(nf, "Temperature of Fusion" + FormatUnit(su.temperature), comp.TemperatureOfFusion.ConvertFromSI(su.temperature), (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.TemperatureOfFusion = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
                    });

                    dl.CreateAndAddTextBoxRow(nf, "Enthalpy of Fusion (kJ/mol)", comp.EnthalpyOfFusionAtTf, (arg1, arg2) =>
                    {
                        if (c.IsValidDouble(arg1.Text)) comp.EnthalpyOfFusionAtTf = arg1.Text.ToDoubleFromCurrent();
                    });
                    break;
                case 2:
                    //elec
                    dl.CreateAndAddLabelRow("Basic Properties");
                    var type = new string[] { "Ion", "Salt", "Hydrated Salt" };
                    var tval = 0;
                    if (comp.IsIon) tval = 0;
                    if (comp.IsSalt) tval = 1;
                    if (comp.IsHydratedSalt) tval = 2;
                    dl.CreateAndAddDropDownRow("Electrolyte Type", type.ToList(), tval, (sender, e) =>
                    {
                        switch (sender.SelectedIndex)
                        {
                            case 0:
                                comp.IsIon = true;
                                comp.IsSalt = false;
                                comp.IsHydratedSalt = false;
                                break;
                            case 1:
                                comp.IsIon = false;
                                comp.IsSalt = true;
                                comp.IsHydratedSalt = false;
                                break;
                            case 2:
                                comp.IsSalt = false;
                                comp.IsIon = false;
                                comp.IsHydratedSalt = true;
                                break;
                        }
                    });
                    dl.CreateAndAddTextBoxRow(nf, "Molecular Weight", comp.Molar_Weight, (sender, e) => comp.Molar_Weight = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.Molar_Weight);
                    dl.CreateAndAddStringEditorRow("Chemical Formula", comp.Formula, (sender, e) => comp.Formula = sender.Text);
                    dl.CreateAndAddStringEditorRow("Positive Ion Formula (if Salt)", comp.PositiveIon, (sender, e) => comp.PositiveIon = sender.Text);
                    dl.CreateAndAddTextBoxRow(nf, "Positive Ion Stoich. Coeff. (if Salt)", comp.PositiveIonStoichCoeff, (sender, e) => comp.PositiveIonStoichCoeff = sender.Text.IsValidDouble() ? (int)sender.Text.ToDoubleFromCurrent() : comp.PositiveIonStoichCoeff);
                    dl.CreateAndAddStringEditorRow("Negative Ion Formula (if Salt)", comp.NegativeIon, (sender, e) => comp.NegativeIon = sender.Text);
                    dl.CreateAndAddTextBoxRow(nf, "Negative Ion Stoich. Coeff. (if Salt)", comp.NegativeIonStoichCoeff, (sender, e) => comp.NegativeIonStoichCoeff = sender.Text.IsValidDouble() ? (int)sender.Text.ToDoubleFromCurrent() : comp.NegativeIonStoichCoeff);
                    dl.CreateAndAddTextBoxRow(nf, "Charge (if Ion)", comp.Charge, (sender, e) => comp.Charge = sender.Text.IsValidDouble() ? (int)sender.Text.ToDoubleFromCurrent() : comp.Charge);
                    dl.CreateAndAddTextBoxRow(nf, "Hydration Number (if Hydrated Salt)", comp.HydrationNumber, (sender, e) => comp.HydrationNumber = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.HydrationNumber);
                    dl.CreateAndAddLabelRow("Solid Phase / Precipitate Properties");
                    dl.CreateAndAddTextBoxRow(nf, "Temperature of Fusion (" + su.temperature + ")", comp.TemperatureOfFusion.ConvertFromSI(su.temperature), (sender, e) => comp.TemperatureOfFusion = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature) : comp.TemperatureOfFusion);
                    dl.CreateAndAddTextBoxRow(nf, "Enthalpy of Fusion (kJ/mol)", comp.EnthalpyOfFusionAtTf, (sender, e) => comp.EnthalpyOfFusionAtTf = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.EnthalpyOfFusionAtTf);
                    dl.CreateAndAddTextBoxRow(nf, "Solid Density (" + su.density + ")", comp.SolidDensityAtTs.ConvertFromSI(su.density), (sender, e) => comp.SolidDensityAtTs = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.density) : comp.SolidDensityAtTs);
                    dl.CreateAndAddTextBoxRow(nf, "Temperature of Solid Density (" + su.temperature + ")", comp.SolidTs.ConvertFromSI(su.temperature), (sender, e) => comp.SolidTs = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature) : comp.SolidTs);
                    dl.CreateAndAddLabelRow("Formation Properties");
                    dl.CreateAndAddTextBoxRow(nf, "Enthalpy of Formation @ 298 K (kJ/mol)", comp.Electrolyte_DelHF, (sender, e) => comp.Electrolyte_DelHF = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.Electrolyte_DelHF);
                    dl.CreateAndAddTextBoxRow(nf, "Gibbs Energy of Formation @ 298 K (kJ/mol)", comp.Electrolyte_DelGF, (sender, e) => comp.Electrolyte_DelGF = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.Electrolyte_DelGF);
                    dl.CreateAndAddTextBoxRow(nf, "Heat Capacity @ 298 K (kJ/[mol.K])", comp.Electrolyte_Cp0, (sender, e) => comp.Electrolyte_Cp0 = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.Electrolyte_Cp0);
                    break;
                case 3:
                    //blackoil
                    dl.CreateAndAddLabelRow("Required Properties");
                    dl.CreateAndAddTextBoxRow(nf, "Gas Specific Gravity", comp.BO_SGG, (sender, e) => comp.BO_SGG = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_SGG);
                    dl.CreateAndAddTextBoxRow(nf, "Oil Specific Gravity", comp.BO_SGO, (sender, e) => comp.BO_SGO = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_SGO);
                    dl.CreateAndAddTextBoxRow(nf, "GOR at Standard Conditions (" + su.gor + ")", comp.BO_GOR.ConvertFromSI(su.gor), (sender, e) => comp.BO_GOR = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.gor) : comp.BO_GOR);
                    dl.CreateAndAddTextBoxRow(nf, "Basic Sediments and Water (BSW) (%)", comp.BO_BSW, (sender, e) => comp.BO_BSW = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_BSW);
                    dl.CreateAndAddLabelRow("Optional Properties");
                    dl.CreateAndAddTextBoxRow(nf, "Oil Viscosity @ T1 (" + su.cinematic_viscosity + ")",
                        comp.BO_OilVisc1.ConvertFromSI(su.cinematic_viscosity),
                        (sender, e) => comp.BO_OilVisc1 = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.cinematic_viscosity) : comp.BO_OilVisc1);
                    dl.CreateAndAddTextBoxRow(nf, "Oil Viscosity T1 (" + su.temperature + ")",
                        comp.BO_OilVisc1.ConvertFromSI(su.cinematic_viscosity),
                        (sender, e) => comp.BO_OilViscTemp1 = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature) : comp.BO_OilViscTemp1);
                    dl.CreateAndAddTextBoxRow(nf, "Oil Viscosity @ T2 (" + su.cinematic_viscosity + ")",
                        comp.BO_OilVisc2.ConvertFromSI(su.cinematic_viscosity),
                        (sender, e) => comp.BO_OilVisc2 = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.cinematic_viscosity) : comp.BO_OilVisc2);
                    dl.CreateAndAddTextBoxRow(nf, "Oil Viscosity T2 (" + su.temperature + ")",
                        comp.BO_OilVisc2.ConvertFromSI(su.cinematic_viscosity),
                        (sender, e) => comp.BO_OilViscTemp2 = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature) : comp.BO_OilViscTemp2);
                    dl.CreateAndAddTextBoxRow(nf, "Oil PNA Analysis, Paraffins (%)", comp.BO_PNA_P, (sender, e) => comp.BO_PNA_P = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_PNA_P);
                    dl.CreateAndAddTextBoxRow(nf, "Oil PNA Analysis, Naphtenes (%)", comp.BO_PNA_N, (sender, e) => comp.BO_PNA_N = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_PNA_N);
                    dl.CreateAndAddTextBoxRow(nf, "Oil PNA Analysis, Aromatics (%)", comp.BO_PNA_A, (sender, e) => comp.BO_PNA_A = sender.Text.IsValidDouble() ? sender.Text.ToDoubleFromCurrent() : comp.BO_PNA_A);
                    break;
            }

            page2.ContentContainer.Add(new Scrollable { Content = dl, Border = BorderType.None, Height = Height, Width = Width });
            page2.SetFontAndPadding();
            page2.Show();

        }

        private void DisplayPage4()
        {

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = true;
            page.hasFinishButton = false;

            page.cancelAction = () => page.Close();

            page.backAction = () =>
            {
                page.Close();
                DisplayPage2();
            };

            page.nextAction = () =>
            {
                page.Close();
                DisplayPage5();
            };

            page.Title = "Compound Creator Wizard";
            page.HeaderTitle = "Step 3 - UNIFAC Structure";
            page.HeaderDescription = "Enter UNIFAC structure information, if available.";
            if (foundddb)
            {
                page.FooterText = "DWSIM found UNIFAC/MODFAC structure data at DDB Online Database, check non-empty/non-zeroed fields.";
            }
            else
            {
                page.FooterText = "";
            }

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Width = Width - 20;

            c.CreateAndAddLabelRow(dl, "UNIFAC Structure");
            c.CreateAndAddCheckBoxRow(dl, "Estimate Constant Properties from UNIFAC structure information", estimatefromunifac, (sender, e) => estimatefromunifac = sender.Checked.GetValueOrDefault());
            c.CreateAndAddLabelRow2(dl, "This will estimate most properties using the informed UNIFAC structure.");
            c.CreateAndAddLabelRow2(dl, "Enter the amount of each group in the molecule:");
            foreach (var item in joback.UNIFACLines)
            {
                var id = item.Split(',')[1];
                var desc = item.Split(',')[3];
                c.CreateAndAddTextBoxRow(dl, "0", desc, comp.UNIFACGroups.ContainsKey(id) ? comp.UNIFACGroups[id].ToString().ToDoubleFromCurrent() : 0d, (arg11, arg22) =>
                {
                    if (c.IsValidDouble(arg11.Text)) comp.UNIFACGroups[id] = Int32.Parse(arg11.Text);
                });
            }

            page.ContentContainer.Add(new Scrollable { Content = dl, Border = BorderType.None, Height = Height, Width = Width });
            page.SetFontAndPadding();
            page.Show();

        }

        private void DisplayPage5()
        {

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = true;
            page.hasFinishButton = false;

            page.cancelAction = () => page.Close();

            page.backAction = () =>
            {
                page.Close();
                DisplayPage4();
            };

            page.nextAction = () =>
            {
                page.Close();
                DisplayPage6();
            };

            page.Title = "Compound Creator Wizard";
            page.HeaderTitle = "Step 4 - Constant Properties";
            page.HeaderDescription = "Enter the required information.";
            if (!estimatefromunifac && foundkdb)
            {
                page.FooterText = "DWSIM found compound data at KDB Online Database, check non-empty/non-zeroed fields.";
            }
            else if (!estimatefromunifac && foundchemeo)
            {
                page.FooterText = "DWSIM found compound data at Cheméo Online Database, check non-empty/non-zeroed fields.";
            }
            else
            {
                page.FooterText = "";
            }

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Width = Width - 20;

            if (estimatefromunifac) EstimateFromUNIFAC();

            c.CreateAndAddLabelRow(dl, "Basic Properties");
            dl.CreateAndAddLabelRow2("These are the minimum required set of properties.");

            c.CreateAndAddTextBoxRow(dl, nf, "Molecular Weight" + FormatUnit(su.molecularWeight), comp.Molar_Weight, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Molar_Weight = cv.ConvertToSI(su.molecularWeight, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Normal Boiling Point" + FormatUnit(su.temperature), comp.Normal_Boiling_Point.ConvertFromSI(su.temperature), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Normal_Boiling_Point = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Critical Temperature" + FormatUnit(su.temperature), comp.Critical_Temperature.ConvertFromSI(su.temperature), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Temperature = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Critical Pressure" + FormatUnit(su.pressure), comp.Critical_Pressure.ConvertFromSI(su.pressure), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Pressure = cv.ConvertToSI(su.pressure, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Critical Compressibility Factor", comp.Critical_Compressibility, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Critical_Compressibility = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Acentric Factor", comp.Acentric_Factor, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Acentric_Factor = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddLabelRow(dl, "Formation Properties");
            dl.CreateAndAddLabelRow2("Formation properties are required if your compound will be part of a chemical reaction.");

            c.CreateAndAddTextBoxRow(dl, nf, "Enthalpy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), comp.IG_Enthalpy_of_Formation_25C.ConvertFromSI(su.enthalpy), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.IG_Enthalpy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Gibbs Energy of Formation (Ideal Gas)" + FormatUnit(su.enthalpy), comp.IG_Gibbs_Energy_of_Formation_25C.ConvertFromSI(su.enthalpy), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.IG_Gibbs_Energy_of_Formation_25C = cv.ConvertToSI(su.enthalpy, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddLabelRow(dl, "Model-Specific Parameters");
            c.CreateAndAddLabelRow2(dl, "This data is optional.");

            c.CreateAndAddTextBoxRow(dl, nf, "Rackett Parameter", comp.Z_Rackett, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.Z_Rackett = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddTextBoxRow(dl, nf, "UNIQUAC Q", comp.UNIQUAC_Q, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.UNIQUAC_Q = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddTextBoxRow(dl, nf, "UNIQUAC R", comp.UNIQUAC_R, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.UNIQUAC_R = arg1.Text.ToDoubleFromCurrent();
            });

            c.CreateAndAddLabelRow(dl, "Solid Phase Properties");
            dl.CreateAndAddLabelRow2("Solid phase properties are required for solid prediction.\nIf you don't have them, you can force this compound to be in the solid phase in the\nProperty Package Advanced Settings editor.");

            c.CreateAndAddTextBoxRow(dl, nf, "Temperature of Fusion" + FormatUnit(su.temperature), comp.TemperatureOfFusion.ConvertFromSI(su.temperature), (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.TemperatureOfFusion = cv.ConvertToSI(su.temperature, arg1.Text.ToDoubleFromCurrent());
            });

            c.CreateAndAddTextBoxRow(dl, nf, "Enthalpy of Fusion (kJ/mol)", comp.EnthalpyOfFusionAtTf, (arg1, arg2) =>
            {
                if (c.IsValidDouble(arg1.Text)) comp.EnthalpyOfFusionAtTf = arg1.Text.ToDoubleFromCurrent();
            });

            page.ContentContainer.Add(new Scrollable { Content = dl, Border = BorderType.None, Height = Height, Width = Width });
            page.SetFontAndPadding();
            page.Show();

        }

        private void DisplayPage6()
        {

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = true;
            page.hasFinishButton = false;

            page.cancelAction = () => page.Close();

            page.backAction = () =>
            {
                page.Close();
                DisplayPage5();
            };

            page.nextAction = () =>
            {
                page.Close();
                DisplayLastPage();
            };

            page.Title = "Compound Creator Wizard";
            page.HeaderTitle = "Step 5 - Temperature-dependent properties";
            page.HeaderDescription = "Enter the required information.";
            page.FooterText = "";

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Height = Height;
            dl.Width = Width - 20;

            c.CreateAndAddLabelRow2(dl, "For temperature-dependent properties, you can let DWSIM estimate them or input an expression using T as variable. Use dot ('.') as the decimal separator when entering numerical values.");

            var options = new string[] { "Estimate", "Use Expression" };

            TextBox ev = new TextBox(),
                eigcp = new TextBox(),
                eld = new TextBox(),
                elcp = new TextBox(),
                elv = new TextBox(),
                esd = new TextBox(),
                escp = new TextBox();

            c.CreateAndAddDropDownRow(dl, "Vapor Pressure", options.ToList(), 0, (arg1, arg2) =>
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

            ev = c.CreateAndAddFullTextBoxRow(dl, comp.VaporPressureEquation, (arg1, arg2) =>
            {
                comp.VaporPressureEquation = arg1.Text;
            });
            ev.Visible = false;
            ev.PlaceholderText = "T in K, Pvap in Pa";

            c.CreateAndAddDropDownRow(dl, "Ideal Gas Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
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

            eigcp = c.CreateAndAddFullTextBoxRow(dl, comp.IdealgasCpEquation, (arg1, arg2) =>
            {
                comp.IdealgasCpEquation = arg1.Text;
            });
            eigcp.Visible = false;
            eigcp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            c.CreateAndAddDropDownRow(dl, "Liquid Density", options.ToList(), 0, (arg1, arg2) =>
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

            eld = c.CreateAndAddFullTextBoxRow(dl, comp.LiquidDensityEquation, (arg1, arg2) =>
            {
                comp.LiquidDensityEquation = arg1.Text;
            });
            eld.Visible = false;
            eld.PlaceholderText = "T in K, dens in kg/m3";

            c.CreateAndAddDropDownRow(dl, "Liquid Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
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

            elcp = c.CreateAndAddFullTextBoxRow(dl, comp.LiquidHeatCapacityEquation, (arg1, arg2) =>
            {
                comp.LiquidHeatCapacityEquation = arg1.Text;
            });
            elcp.Visible = false;
            elcp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            c.CreateAndAddDropDownRow(dl, "Liquid Viscosity", options.ToList(), 0, (arg1, arg2) =>
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

            elv = c.CreateAndAddFullTextBoxRow(dl, comp.LiquidViscosityEquation, (arg1, arg2) =>
            {
                comp.LiquidViscosityEquation = arg1.Text;
            });
            elv.Visible = false;
            elv.PlaceholderText = "T in K, visc in Pa.s";

            c.CreateAndAddDropDownRow(dl, "Solid Density", options.ToList(), 0, (arg1, arg2) =>
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

            esd = c.CreateAndAddFullTextBoxRow(dl, comp.SolidDensityEquation, (arg1, arg2) =>
            {
                comp.SolidDensityEquation = arg1.Text;
            });
            esd.Visible = false;
            esd.PlaceholderText = "T in K, dens in kg/m3";

            c.CreateAndAddDropDownRow(dl, "Solid Heat Capacity", options.ToList(), 0, (arg1, arg2) =>
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

            escp = c.CreateAndAddFullTextBoxRow(dl, comp.SolidHeatCapacityEquation, (arg1, arg2) =>
            {
                comp.SolidHeatCapacityEquation = arg1.Text;
            });
            escp.Visible = false;
            escp.PlaceholderText = "T in K, Cp in kJ/[kg.K]";

            page.ContentContainer.Add(dl);
            page.SetFontAndPadding();
            page.Show();

        }

        private void DisplayLastPage()
        {

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = false;
            page.hasFinishButton = true;

            page.cancelAction = () => page.Close();
            page.finishAction = () => page.Close();

            page.backAction = () =>
            {
                page.Close();
                switch (comptype)
                {
                    case 0:
                        DisplayPage6();
                        break;
                    case 1:
                    case 2:
                    case 3:
                        DisplayPage3();
                        break;
                }
            };

            page.Title = "Compound Creator Wizard";
            page.HeaderTitle = "Final Step - Add Compound and Export Data";
            page.HeaderDescription = "Add the compound to the simulation and/or export the created data to a file.";
            page.FooterText = "Click 'Finish' to close this wizard.";

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Height = Height;
            dl.Width = Width - 20;

            dl.CreateAndAddLabelRow("Export Options");

            dl.CreateAndAddLabelAndButtonRow("Export Compound to JSON File", "Export to JSON", null, (arg1, arg2) =>
            {
                if (GlobalSettings.Settings.OldUI)
                {
                    IFilePicker filePickerForm = FilePickerService.GetInstance().GetFilePicker();
                    IVirtualFile handler = filePickerForm.ShowSaveDialog(new List<FilePickerAllowedType> { new FilePickerAllowedType("JSON File", "*.json") });

                    if (handler != null)
                    {
                        using (var stream = new System.IO.MemoryStream())
                        {
                            using (var writer = new StreamWriter(stream) { AutoFlush = true })
                            {
                                try
                                {
                                    var jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(comp, Newtonsoft.Json.Formatting.Indented);
                                    writer.Write(jsondata);
                                    handler.Write(stream);
                                    if (flowsheet == null)
                                    {
                                        MessageBox.Show("Compound '" + comp.Name + "' successfully saved to JSON file.");
                                    }
                                    else
                                    {
                                        flowsheet.ShowMessage("Compound '" + comp.Name + "' successfully saved to JSON file.", IFlowsheet.MessageType.Information);
                                    }
                                }
                                catch (Exception ex)
                                {
                                    if (flowsheet == null)
                                    {
                                        MessageBox.Show("Error saving compound to JSON file: " + ex.ToString());
                                    }
                                    else
                                    {
                                        flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    var dialog = new SaveFileDialog();
                    dialog.Title = "Save Compound to JSON File";
                    dialog.Filters.Add(new FileFilter("JSON File", new[] { ".json" }));
                    dialog.CurrentFilterIndex = 0;
                    if (dialog.ShowDialog(page) == DialogResult.Ok)
                    {
                        try
                        {
                            File.WriteAllText(dialog.FileName, Newtonsoft.Json.JsonConvert.SerializeObject(comp, Newtonsoft.Json.Formatting.Indented));
                            flowsheet.ShowMessage("Compound '" + comp.Name + "' successfully saved to JSON file.", IFlowsheet.MessageType.Information);
                        }
                        catch (Exception ex)
                        {
                            flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                        }
                    }
                }
            });
            dl.CreateAndAddLabelRow2("Export compound data to a JSON file for later use (recommended).");

            if (flowsheet != null)
            {

                dl.CreateAndAddLabelRow("Add to Simulation");

                dl.CreateAndAddLabelAndButtonRow("Add Compound to Simulation", "Add", null, (arg1, arg2) =>
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
                        if (flowsheet is Flowsheet)
                        {
                            ((Flowsheet)flowsheet).UpdateEditorPanels.Invoke();
                        }
                    }
                });
                dl.CreateAndAddLabelRow2("Adds the compound to the current simulation.");
            }

            page.ContentContainer.Add(dl);
            page.SetFontAndPadding();
            page.Show();


        }

        string FormatUnit(string units)
        {
            return " (" + units + ")";
        }

        void EstimateFromUNIFAC()
        {
            var uf = joback.GetUNIFACList(comp.UNIFACGroups);
            var jc = joback.GetJCFromUNIFAC(uf);
            var acl = joback.GetACLFromUNIFAC(uf);

            comp.Molar_Weight = joback.CalcMW(acl);
            comp.Normal_Boiling_Point = joback.CalcTb(jc);
            comp.Critical_Temperature = joback.CalcTc(comp.Normal_Boiling_Point, jc);
            comp.Critical_Pressure = joback.CalcPc(jc);
            comp.TemperatureOfFusion = joback.CalcTf(jc);
            comp.EnthalpyOfFusionAtTf = joback.CalcHf(jc);
            comp.IG_Enthalpy_of_Formation_25C = joback.CalcDHf(jc) / comp.Molar_Weight;
            comp.IG_Gibbs_Energy_of_Formation_25C = joback.CalcDGf(jc) / comp.Molar_Weight;
            comp.Critical_Volume = joback.CalcVc(jc);
            comp.Critical_Compressibility = (comp.Critical_Pressure * comp.Critical_Volume / comp.Critical_Temperature / 8.314 / 1000);
            comp.Z_Rackett = comp.Critical_Compressibility;
            comp.Acentric_Factor = DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods.PropertyMethods.AcentricFactor_LeeKesler(comp.Critical_Temperature, comp.Critical_Pressure, comp.Normal_Boiling_Point);

            comp.IdealgasCpEquation = "4";
            comp.Ideal_Gas_Heat_Capacity_Const_A = joback.CalcCpA(jc) * 1000;
            comp.Ideal_Gas_Heat_Capacity_Const_B = joback.CalcCpB(jc) * 1000;
            comp.Ideal_Gas_Heat_Capacity_Const_C = joback.CalcCpC(jc) * 1000;
            comp.Ideal_Gas_Heat_Capacity_Const_D = joback.CalcCpD(jc) * 1000;
        }

        void CheckOnlineData()
        {

            chemeoc = null;
            kdbc = null;
            facdata = null;

            foundchemeo = false;
            foundddb = false;
            foundkdb = false;

            string searchterm = "";

            if (comp.CAS_Number != "") searchterm = comp.CAS_Number; else searchterm = comp.Name;

            var t1 = Task.Factory.StartNew(() =>
            {
                try
                {
                    var cids = Thermodynamics.Databases.KDBLink.KDBParser.GetCompoundIDs(searchterm, false);
                    kdbc = Thermodynamics.Databases.KDBLink.KDBParser.GetCompoundData(int.Parse(cids[0][0]));
                }
                catch { }
            });

            var t2 = Task.Factory.StartNew(async () =>
            {
                try
                {
                    var cids = await Thermodynamics.Databases.ChemeoLink.ChemeoParser.GetCompoundIDs(comp.Name, false);
                    chemeoc = Thermodynamics.Databases.ChemeoLink.ChemeoParser.GetCompoundData(cids[0][0]);
                }
                catch { }
            });

            Task.WaitAll(new[] { t1, t2 }, 20000);

            if (kdbc != null && chemeoc == null)
            {
                if (comp.Formula == "") comp.Formula = kdbc.Formula;
                if (comp.CAS_Number == "") comp.CAS_Number = kdbc.CAS_Number;
                comp.Molar_Weight = kdbc.Molar_Weight;
                comp.Normal_Boiling_Point = kdbc.Normal_Boiling_Point;
                comp.NBP = kdbc.NBP;
                comp.TemperatureOfFusion = kdbc.TemperatureOfFusion;
                comp.Critical_Temperature = kdbc.Critical_Temperature;
                comp.Critical_Pressure = kdbc.Critical_Pressure;
                comp.Critical_Volume = kdbc.Critical_Volume;
                comp.Critical_Compressibility = kdbc.Critical_Compressibility;
                comp.Acentric_Factor = kdbc.Acentric_Factor;
                comp.Z_Rackett = kdbc.Z_Rackett;
                comp.IG_Enthalpy_of_Formation_25C = kdbc.IG_Enthalpy_of_Formation_25C;
                comp.IG_Entropy_of_Formation_25C = kdbc.IG_Entropy_of_Formation_25C;
                comp.IG_Gibbs_Energy_of_Formation_25C = kdbc.IG_Gibbs_Energy_of_Formation_25C;
                comp.UNIQUAC_Q = kdbc.UNIQUAC_Q;
                comp.UNIQUAC_R = kdbc.UNIQUAC_R;
                foundkdb = true;
            }
            else if (chemeoc != null)
            {
                if (comp.Formula == "") comp.Formula = chemeoc.Formula;
                if (comp.CAS_Number == "") comp.CAS_Number = chemeoc.CAS_Number;
                comp.InChI = chemeoc.InChI;
                comp.SMILES = chemeoc.SMILES;
                comp.Molar_Weight = chemeoc.Molar_Weight;
                comp.Normal_Boiling_Point = chemeoc.Normal_Boiling_Point;
                comp.NBP = chemeoc.NBP;
                comp.TemperatureOfFusion = chemeoc.TemperatureOfFusion;
                comp.Critical_Temperature = chemeoc.Critical_Temperature;
                comp.Critical_Pressure = chemeoc.Critical_Pressure;
                comp.Critical_Volume = chemeoc.Critical_Volume;
                comp.Critical_Compressibility = chemeoc.Critical_Compressibility;
                comp.Acentric_Factor = chemeoc.Acentric_Factor;
                comp.IG_Enthalpy_of_Formation_25C = chemeoc.IG_Enthalpy_of_Formation_25C;
                comp.IG_Entropy_of_Formation_25C = chemeoc.IG_Entropy_of_Formation_25C;
                comp.IG_Gibbs_Energy_of_Formation_25C = chemeoc.IG_Gibbs_Energy_of_Formation_25C;
                comp.EnthalpyOfFusionAtTf = chemeoc.EnthalpyOfFusionAtTf;
                foundchemeo = true;
            }


            try
            {
                var cid = Thermodynamics.Databases.DDBStructureLink.DDBStructureParser.GetID(comp.CAS_Number);
                facdata = Thermodynamics.Databases.DDBStructureLink.DDBStructureParser.GetData(cid);
            }
            catch { }

            if (facdata != null)
            {
                foundddb = true;
                if (facdata.ContainsKey("Original"))
                {
                    comp.UNIFACGroups = new System.Collections.SortedList();
                    comp.UNIFACGroups.Clear();
                    foreach (var item in facdata["Original"])
                    {
                        comp.UNIFACGroups.Add(item[1], item[2]);
                    }
                }
                if (facdata.ContainsKey("Modified"))
                {
                    comp.MODFACGroups = new System.Collections.SortedList();
                    comp.MODFACGroups.Clear();
                    comp.NISTMODFACGroups = new System.Collections.SortedList();
                    comp.NISTMODFACGroups.Clear();
                    foreach (var item in facdata["Modified"])
                    {
                        comp.MODFACGroups.Add(item[1], item[2]);
                        comp.NISTMODFACGroups.Add(item[1], item[2]);
                    }
                }

            }

        }

    }

}
