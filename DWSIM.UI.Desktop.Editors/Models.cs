using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class Models
    {

        public IFlowsheet flowsheet;
        public DynamicLayout container;

        private TableLayout ppcontainer, facontainer;

        private ObservableCollection<CompoundItem> obslist = new ObservableCollection<CompoundItem>();

        public Models(IFlowsheet fs, DynamicLayout layout)
        {
            flowsheet = fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            ppcontainer = new TableLayout();
            facontainer = new TableLayout();

            var proppacks = flowsheet.AvailablePropertyPackages.Keys.ToList();

            proppacks.Insert(0, "Select an item...");

            s.CreateAndAddLabelRow(container, "Add Property Package");

            s.CreateAndAddDropDownRow(container, "Add New Property Package", proppacks, 0, (sender, e) => {
                var item = sender.SelectedValue.ToString();
                if (item != "Select an item...")
                {                   
                    var pp = (PropertyPackage)flowsheet.AvailablePropertyPackages[item].Clone();
                    pp.UniqueID = Guid.NewGuid().ToString();
                    pp.Tag = pp.ComponentName + " (" + (flowsheet.PropertyPackages.Count + 1).ToString() + ")";
                    flowsheet.AddPropertyPackage(pp);
                    AddPropPackItem(pp);
                    sender.SelectedIndex = 0;
                }
            });

            s.CreateAndAddLabelRow(container, "Added Property Packages");

            s.CreateAndAddControlRow(container, ppcontainer);

            foreach (PropertyPackage pp in flowsheet.PropertyPackages.Values)
            {
                AddPropPackItem(pp);
            }

            var flashalgos = flowsheet.AvailableFlashAlgorithms.Keys.ToList();

            flashalgos.Insert(0, "Select an item...");

            s.CreateAndAddLabelRow(container, "Add Flash Algorithm");

            s.CreateAndAddDropDownRow(container, "Add New Flash Algorithm", flashalgos, 0, (sender, e) =>
            {
                var item = sender.SelectedValue.ToString();
                if (item != "Select an item...")
                {
                    var fa = (IFlashAlgorithm)flowsheet.AvailableFlashAlgorithms[item].Clone();
                    fa.Tag = fa.Name + " (" + (flowsheet.FlowsheetOptions.FlashAlgorithms.Count + 1).ToString() + ")";
                    flowsheet.FlowsheetOptions.FlashAlgorithms.Add(fa);
                    AddFlashAlgorithmItem(fa);
                    sender.SelectedIndex = 0;
                }
            });

            s.CreateAndAddLabelRow(container, "Added Flash Algorithms");

            s.CreateAndAddControlRow(container, facontainer);

            foreach (IFlashAlgorithm fa in flowsheet.FlowsheetOptions.FlashAlgorithms)
            {
                AddFlashAlgorithmItem(fa);
            }

        }

        void AddFlashAlgorithmItem(IFlashAlgorithm fa)
        {

            var tr = new TableRow();
            tr = s.CreateAndAddLabelAndTwoButtonsRow(facontainer, fa.Tag, "Edit", null, "Remove", null,
                                                               (arg1, arg2) =>
                                                               {
                                                                   //var alert = new AlertDialog.Builder(this.Context);
                                                                   //var myview = new PropertyPackageSettingsView(this.Context, flowsheet, pp);
                                                                   //alert.SetView(myview);
                                                                   //alert.Create().Show();
                                                               },
                                                               (arg1, arg2) =>
                                                               {
                                                                   if (MessageBox.Show("Confirm removal?", "Remove Flash Algorithm", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes){
                                                                       facontainer.Remove(tr);
                                                                       flowsheet.FlowsheetOptions.FlashAlgorithms.Remove(fa);
                                                                   }
                                                               });
        }

        void AddPropPackItem(PropertyPackage pp)
        {

            var tr = new TableRow();
            tr = s.CreateAndAddLabelAndTwoButtonsRow(ppcontainer, pp.Tag, "Edit", null, "Remove", null,
                                                               (arg1, arg2) =>
                                                               {
                                                                   //var alert = new AlertDialog.Builder(this.Context);
                                                                   //var myview = new PropertyPackageSettingsView(this.Context, flowsheet, pp);
                                                                   //alert.SetView(myview);
                                                                   //alert.Create().Show();
                                                               },
                                                               (arg1, arg2) =>
                                                               {
                                                                   if (MessageBox.Show("Confirm removal?", "Remove Property Package", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                                                   {
                                                                       ppcontainer.Remove(tr);
                                                                       flowsheet.PropertyPackages.Remove(pp.UniqueID);
                                                                   }
                                                               });
        }

    }

}