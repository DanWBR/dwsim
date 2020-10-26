using System;
using System.Linq;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.UI.Desktop.Shared;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;


namespace DWSIM.UI.Desktop.Editors
{
    public class Models
    {

        public Flowsheet flowsheet;
        public DynamicLayout container;

        private DynamicLayout ppcontainer, facontainer;

        public Models(Flowsheet fs, DynamicLayout layout)
        {
            flowsheet = fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            ppcontainer = new DynamicLayout();
            facontainer = new DynamicLayout();

            var proppacks = flowsheet.AvailablePropertyPackages.Keys.ToList();

            proppacks.Insert(0, "Select an item...");

            s.CreateAndAddLabelRow(container, "Property Packages");

            s.CreateAndAddDescriptionRow(container, "A Property Package is a set of " +
                "models and methods/equations which are responsible for the calculation of compound and phase properties and for providing " +
                "thermodynamic properties for Unit Operation calculations, like enthalpy and entropy.\n\n" +
                "You need to add at least one Property Package to your simulation.");

            s.CreateAndAddDropDownRow(container, "Add New Property Package", proppacks, 0, (sender, e) =>
            {
                var item = sender.SelectedValue.ToString();
                if (item != "Select an item...")
                {
                    var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages[item].Clone();
                    pp.UniqueID = Guid.NewGuid().ToString();
                    pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                    flowsheet.AddPropertyPackage(pp);
                    Application.Instance.AsyncInvoke(() =>
                    {
                        AddPropPackItem(pp);
                        flowsheet.UpdateEditorPanels.Invoke();
                        ppcontainer.Create();
                        ppcontainer.Invalidate();
                    });
                    sender.SelectedIndex = 0;
                }
            });

            s.CreateAndAddLabelRow(container, "Added Property Packages");

            s.CreateAndAddControlRow(container, ppcontainer);

            foreach (PropertyPackage pp in flowsheet.PropertyPackages.Values)
            {
                AddPropPackItem(pp);
            }

            s.CreateAndAddCheckBoxRow(container, "Skip equilibrium calculations on well-defined Material Streams",
                flowsheet.Options.SkipEquilibriumCalculationOnDefinedStreams,
                (chk, e) => flowsheet.Options.SkipEquilibriumCalculationOnDefinedStreams = chk.Checked.GetValueOrDefault());

            s.CreateAndAddDescriptionRow(container, "Prevents DWSIM from recalculating the equilibrium phase distribution in some well-defined material streams, i.e. separator vessel outlets, distillation column products, etc.");

        }

        void AddPropPackItem(PropertyPackage pp)
        {
            pp.Flowsheet = flowsheet;
            var tr = new TableRow();
            tr = s.CreateAndAddTextBoxAndFourButtonsRow(ppcontainer, pp.Tag, "Edit", null, "Flash", null, "Advanced", null, "Remove", null,
                                                                (arg1, arg2) =>
                                                                {
                                                                    pp.Tag = arg1.Text;
                                                                },
                                                                (arg1, arg2) =>
                                                                {
                                                                    var supported = new string[] { "NRTL", "UNIQUAC", "Peng-Robinson (PR)", "Soave-Redlich-Kwong (SRK)", "Lee-Kesler-Plöcker" };
                                                                    if (supported.Contains(pp.ComponentName))
                                                                    {
                                                                        var cont = new PropertyPackageSettingsView(flowsheet, pp);
                                                                        var cont2 = new PropertyPackageIPView(flowsheet, pp);
                                                                        cont.Tag = "General Settings";
                                                                        cont2.Tag = "Interaction Parameters";
                                                                        var form = s.GetDefaultTabbedForm("Edit '" + pp.Tag + "' (" + pp.ComponentName + ")", 800, 500, new DynamicLayout[] { cont2, cont });
                                                                        form.Show();
                                                                    }
                                                                    else
                                                                    {
                                                                        Application.Instance.Invoke(() => { pp.DisplayEditingForm(); });
                                                                    }
                                                                },
                                                                 (arg1, arg2) =>
                                                                 {
                                                                     {
                                                                         Application.Instance.Invoke(() => { pp.DisplayFlashConfigForm(); });
                                                                     }
                                                                 },
                                                                (arg1, arg2) =>
                                                                {
                                                                    {
                                                                        Application.Instance.Invoke(() => { pp.DisplayAdvancedEditingForm(); });
                                                                    }
                                                                },
                                                               (arg1, arg2) =>
                                                               {
                                                                   if (MessageBox.Show("Confirm removal?", "Remove Property Package", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                                                   {
                                                                       ppcontainer.Remove(tr);
                                                                       flowsheet.PropertyPackages.Remove(pp.UniqueID);
                                                                       flowsheet.UpdateEditorPanels.Invoke();
                                                                   }
                                                               });
        }

    }

}