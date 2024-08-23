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
using DWSIM.UI.Shared;

using DWSIM.ExtensionMethods;
using System.IO;
using DWSIM.Thermodynamics.PropertyPackages;

namespace DWSIM.UI.Desktop.Editors
{
    public class SimulationSetupWizard
    {

        public IFlowsheet flowsheet;

        private static double sf = GlobalSettings.Settings.UIScalingFactor;

        private int Width = (int)(800 * sf);
        private int Height = (int)(500 * sf);

        private bool hasLowPressure = false;
        private bool hasHC = false;
        private bool hasHCW = false;
        private bool hasPolarChemicals = false;
        private bool hasRefrigeration = false;
        private bool hasSingleCompoundWater = false;
        private bool hasElectrolytes = false;

        DropDown ddpp = null;
        public SimulationSetupWizard(IFlowsheet fs)
            : base()
        {

            flowsheet = fs;
        }

        public void DisplayPage0(Control owner = null)
        {

            var page1 = new WizardPage();

            page1.hasBackButton = false;
            page1.hasCancelButton = true;
            page1.hasNextButton = true;
            page1.hasFinishButton = false;

            page1.cancelAction = () => page1.Close();
            page1.nextAction = () => { page1.Close(); DisplayPage1(); };

            page1.Title = "Simulation Setup Wizard";
            page1.HeaderTitle = "Simulation Setup Wizard";
            page1.HeaderDescription = "Welcome!";
            page1.FooterText = "Click 'Next' to continue.";

            page1.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Height = Height;
            dl.Width = Width;

            dl.CreateAndAddLabelRow2("Welcome to the Simulation Setup wizard. In the next steps you'll be able to configure the main simulation settings using a simplifed approach.");
            dl.CreateAndAddLabelRow2("You can close this wizard at your will. Many other (advanced) settings can be configured by using the appropriate editors, which can be found in the 'Setup' menu item.");
            dl.CreateAndAddLabelRow2("Click 'Next' to continue.");

            page1.ContentContainer.Add(dl);
            page1.Show();
            page1.Center();

        }

        private void DisplayPage1()
        {

            var page1 = new WizardPage();

            page1.hasBackButton = true;
            page1.hasCancelButton = true;
            page1.hasNextButton = true;
            page1.hasFinishButton = false;

            page1.cancelAction = () => page1.Close();
            page1.backAction = () => { page1.Close(); DisplayPage0(); };
            page1.nextAction = () => { page1.Close(); DisplayPage2(); };

            page1.Title = "Simulation Setup Wizard";
            page1.HeaderTitle = "Step 1 - Add Compounds";
            page1.HeaderDescription = "Select the compounds to add to the simulation. If your compound is not on the list, you can create and add a new one\nusing the Compound Creator Wizard ('Setup' > 'Compounds' > 'Compound Tools' > 'Compound Creator Wizard').";
            page1.FooterText = "Click 'Next' to continue.";

            page1.Init(Width, Height);

            var tl = new TableLayout() { Width = Width, Height = Height };

            new Compounds(flowsheet, tl);

            page1.ContentContainer.Add(tl);
            page1.Show();
            page1.Center();

        }

        private void DisplayPage2()
        {

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = true;
            page.hasFinishButton = false;

            page.cancelAction = () => { page.Close(); };
            page.backAction = () => { page.Close(); DisplayPage1(); };
            page.nextAction = () => { page.Close(); DisplayPage3(); };

            page.Title = "Simulation Setup Wizard";
            page.HeaderTitle = "Step 2 - Process Model details";
            page.HeaderDescription = "Configure process model details.";
            page.FooterText = "Click 'Next' to continue.";

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            
            dl.CreateAndAddLabelRow("Process Details");
            dl.CreateAndAddLabelRow2("Select an item according to your process charateristics and DWSIM will choose the best thermodynamic model setup for your simulation.");
            dl.CreateAndAddLabelRow2("If you prefer to add multiple Property Packages, close this wizard and go to 'Setup' > 'Basis'.");

            dl.CreateAndAddLabelRow("General Information");

            var rl = new RadioButtonList { Orientation = Orientation.Vertical };

            rl.Spacing = new Size(5, 5);

            rl.Items.Add("My process can be modeled using the Ideal Gas law for vapor phase and Ideal Solution Theory for liquid phase");
            rl.Items.Add("My process deals with hydrocarbons only");
            rl.Items.Add("My process has hydrocarbons and Water at higher pressures");
            rl.Items.Add("My process has polar chemicals");
            rl.Items.Add("My process deals with a refrigeration cycle");
            rl.Items.Add("This is a single Water/Steam simulation");
            rl.Items.Add("I'm simulating a process which involves aqueous electrolytes");
            rl.Items.Add("I want to select/use a specific Property Package");

            rl.SelectedIndexChanged += (s, e) => {
                switch (rl.SelectedIndex)
                {
                    case 0:
                        hasLowPressure = true;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 1:
                        hasLowPressure = false;
                        hasHC = true;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 2:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = true;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 3:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = true;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 4:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = true;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 5:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = true;
                        hasElectrolytes = false;
                        ddpp.Enabled = false;
                        break;
                    case 6:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = true;
                        ddpp.Enabled = false;
                        break;
                    case 7:
                        hasLowPressure = false;
                        hasHC = false;
                        hasHCW = false;
                        hasPolarChemicals = false;
                        hasRefrigeration = false;
                        hasSingleCompoundWater = false;
                        hasElectrolytes = false;
                        ddpp.Enabled = true;
                        break;
                }
            };

            dl.CreateAndAddControlRow(rl);

            ddpp = dl.CreateAndAddDropDownRow("Property Package", flowsheet.AvailablePropertyPackages.Keys.ToList(), 0, (dd, e) => { });
            ddpp.Enabled = false;
            
            if (!Application.Instance.Platform.IsGtk) ddpp.Width = 350;

            if (Application.Instance.Platform.IsGtk)
            {
                page.ContentContainer.Add(new Scrollable { Content = dl, Border = BorderType.None, Height = Height, Width = Width });
            }
            else
            {
                dl.Height = Height;
                dl.Width = Width;
                page.ContentContainer.Add(dl);
            }
            page.Show();
            page.Center();

        }

        private void DisplayPage3() {

            SetupPropertyPackage();

            var page = new WizardPage();

            page.hasBackButton = true;
            page.hasCancelButton = true;
            page.hasNextButton = false;
            page.hasFinishButton = true;

            page.cancelAction = () => { page.Close(); };
            page.finishAction = () => { page.Close(); };
            page.backAction = () => { page.Close(); DisplayPage2(); };

            page.Title = "Simulation Setup Wizard";
            page.HeaderTitle = "Step 3 - Other Settings";
            page.HeaderDescription = "Configure miscellaneous simulation settings.";
            page.FooterText = "Click 'Finish' to close this window and start building your process model.";

            page.Init(Width, Height);

            var dl = c.GetDefaultContainer();
            dl.Height = Height;
            dl.Width = Width;

            c.CreateAndAddLabelRow(dl, "General");

            c.CreateAndAddStringEditorRow(dl, "Simulation Name", flowsheet.FlowsheetOptions.SimulationName, (sender, e) => flowsheet.FlowsheetOptions.SimulationName = sender.Text);

            c.CreateAndAddLabelRow2(dl, "The simulation name will be used for report identification and file name during saving.");

            var avunits = flowsheet.AvailableSystemsOfUnits.Select((x) => x.Name).ToList();

            c.CreateAndAddLabelRow(dl, "System of Units");
            c.CreateAndAddLabelRow2(dl, "Select the System of Units to be used on this simulation.");

            c.CreateAndAddDropDownRow(dl, "System of Units", avunits, avunits.IndexOf(flowsheet.FlowsheetOptions.SelectedUnitSystem.Name), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.SelectedUnitSystem = flowsheet.AvailableSystemsOfUnits.Where((x) => x.Name == avunits[sender.SelectedIndex]).FirstOrDefault();
            });

            var nformats = new[] { "F", "G", "G2", "G4", "G6", "G8", "G10", "N", "N2", "N4", "N6", "R", "E", "E1", "E2", "E3", "E4", "E6" };

            c.CreateAndAddLabelRow(dl, "Number Formats");

            c.CreateAndAddDropDownRow(dl, "General", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.NumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.NumberFormat = sender.SelectedValue.ToString();
            });

            c.CreateAndAddDescriptionRow(dl, "Select the formatting scheme for general numbers.");

            c.CreateAndAddDropDownRow(dl, "Compound Amounts", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.FractionNumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.FractionNumberFormat = sender.SelectedValue.ToString();
            });

            c.CreateAndAddDescriptionRow(dl, "Select the formatting scheme for compound amounts in Material Stream reports.");

            page.ContentContainer.Add(dl);
            page.Show();
            page.Center();

        }

        private void SetupPropertyPackage()
        {

            flowsheet.PropertyPackages.Clear();

            if (hasLowPressure) {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["Raoult's Law"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (hasHC | hasHCW)
            {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["Peng-Robinson (PR)"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (hasPolarChemicals)
            {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["NRTL"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (hasRefrigeration)
            {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["CoolProp"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (hasSingleCompoundWater)
            {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["Steam Tables (IAPWS-IF97)"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (hasElectrolytes)
            {
                var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages["Ideal Solution (Aqueous Electrolytes)"].Clone();
                pp.UniqueID = Guid.NewGuid().ToString();
                pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(pp);
                return;
            }

            if (ddpp.Enabled)
            {
                var ppx = (PropertyPackage)flowsheet.AvailablePropertyPackages[ddpp.SelectedValue.ToString()].Clone();
                ppx.UniqueID = Guid.NewGuid().ToString();
                ppx.Tag = ppx.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                flowsheet.AddPropertyPackage(ppx);
                return;
            }

        }

    }
}
