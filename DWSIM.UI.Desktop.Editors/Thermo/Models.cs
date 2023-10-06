using System;
using System.Linq;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.UI.Desktop.Shared;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.ExtensionMethods.Eto;


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

            var proppacks = flowsheet.AvailablePropertyPackages.Keys.OrderBy(x => x).ToList();

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
                    flowsheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.PropertyPackages);
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

            int fp = 0;
            switch (flowsheet.Options.ForceStreamPhase)
            {
                case Interfaces.Enums.ForcedPhase.None:
                    fp = 0;
                    break;
                case Interfaces.Enums.ForcedPhase.Vapor:
                    fp = 1;
                    break;
                case Interfaces.Enums.ForcedPhase.Liquid:
                    fp = 2;
                    break;
                case Interfaces.Enums.ForcedPhase.Solid:
                    fp = 3;
                    break;
            }

            s.CreateAndAddDropDownRow(container, "Force Phase in Material Streams",
                new System.Collections.Generic.List<string>{ "Do Not Force", "Vapor", "Liquid", "Solid" }, fp, (dd, e) => {
                    switch (dd.SelectedIndex)
                    {
                        case 0:
                            flowsheet.Options.ForceStreamPhase = Interfaces.Enums.ForcedPhase.None;
                            break;
                        case 1:
                            flowsheet.Options.ForceStreamPhase = Interfaces.Enums.ForcedPhase.Vapor;
                            break;
                        case 2:
                            flowsheet.Options.ForceStreamPhase = Interfaces.Enums.ForcedPhase.Liquid;
                            break;
                        case 3:
                            flowsheet.Options.ForceStreamPhase = Interfaces.Enums.ForcedPhase.Solid;
                            break;
                    }
                });

            s.CreateAndAddDescriptionRow(container, "Forces the fluid in all Material Streams in this Flowsheet to stay in the specified phase.");

        }

        void AddPropPackItem(PropertyPackage pp)
        {
            pp.Flowsheet = flowsheet;
            var tr = new TableRow();
            tr = s.CreateAndAddTextBoxAndTwoButtonsRow(ppcontainer, pp.Tag, "Edit", null, "Remove", null,
                                                                (arg1, arg2) =>
                                                                {
                                                                    flowsheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.PropertyPackages);
                                                                    pp.Tag = arg1.Text;
                                                                },
                                                                (arg1, arg2) =>
                                                                {
                                                                    flowsheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.PropertyPackages);
                                                                    var supported = new string[] { "NRTL", "UNIQUAC", "Peng-Robinson (PR)", "Soave-Redlich-Kwong (SRK)", "Lee-Kesler-Plöcker" };
                                                                    var cont = new PropertyPackageSettingsView(flowsheet, pp);
                                                                    cont.Tag = "Property Calculations";
                                                                    var cont3 = new FlashSettingsEditor(flowsheet, pp);
                                                                    cont3.Tag = "Equilibrium Calculations";
                                                                    var advcont = pp.GetAdvancedEditingContainers();
                                                                    if (supported.Contains(pp.ComponentName))
                                                                    {
                                                                        var cont2 = new PropertyPackageIPView(flowsheet, pp);
                                                                        cont2.Tag = "Interaction Parameters";
                                                                        var form = s.GetDefaultTabbedForm("Edit '" + pp.Tag + "' (" + pp.ComponentName + ")", 800, 500, new DynamicLayout[] { cont2, cont3, cont, advcont[0], advcont[1] });
                                                                        form.Show();
                                                                        form.Center();
                                                                    }
                                                                    else
                                                                    {
                                                                        var form = s.GetDefaultTabbedForm("Edit '" + pp.Tag + "' (" + pp.ComponentName + ")", 800, 500, new DynamicLayout[] { cont3, cont, advcont[0], advcont[1] });
                                                                        form.Show();
                                                                        form.Center();
                                                                    }
                                                                },                                                                
                                                               (arg1, arg2) =>
                                                               {
                                                                   if (MessageBox.Show("Confirm removal?", "Remove Property Package", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                                                   {
                                                                       flowsheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.PropertyPackages);
                                                                       ppcontainer.Remove(tr);
                                                                       flowsheet.PropertyPackages.Remove(pp.UniqueID);
                                                                       flowsheet.UpdateEditorPanels.Invoke();
                                                                   }
                                                               });
        }

    }

}