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

            var flashalgos = flowsheet.AvailableFlashAlgorithms.Values.Where((x) => !x.InternalUseOnly).Select((x) => x.Name).ToList();

            flashalgos.Insert(0, "Select an item...");

            s.CreateAndAddLabelRow(container, "Flash Algorithms");

            s.CreateAndAddDescriptionRow(container, "The Flash Algorithms in DWSIM are the components responsible for determining a particular set " +
            "of phases at thermodynamic equilibrium, their amounts (and the amounts of the compounds on each phase) at the specified conditions like " +
            "Temperature, Pressure, Total Enthalpy and Total Entropy. Some Flash Algorithms are capable of predicting equilibrium between one vapor " +
            "and one liquid phase, while others support another co-existing liquid and/or solid phase. As the amount of phases considered in " +
            "equilibrium increases, the calculation time/complexity also increases while the results' reliability decreases.\n\n" +
            "Some flash algorithms are more capable/reliable than others, depending on the mixture for which the flash calculation request is being " +
            "requested. DWSIM features a selection of flash algorithms that are capable of calculating VLE, VLLE and SLE.\n\n" +
            "The 'Nested Loops (VLE)' algorithm satisfies the requirements of most Vapor-Liquid Equilibria systems.");

            s.CreateAndAddDropDownRow(container, "Add New Flash Algorithm", flashalgos, 0, (sender, e) =>
            {
                var item = sender.SelectedValue.ToString();
                if (item != "Select an item...")
                {
                    var fa = (IFlashAlgorithm)flowsheet.AvailableFlashAlgorithms[item].Clone();
                    fa.Tag = fa.Name + " (" + (flowsheet.FlowsheetOptions.FlashAlgorithms.Count + 1).ToString() + ")";
                    flowsheet.FlowsheetOptions.FlashAlgorithms.Add(fa);
                    Application.Instance.AsyncInvoke(() =>
                    {
                        AddFlashAlgorithmItem(fa);
                        flowsheet.UpdateEditorPanels.Invoke();
                        facontainer.Create();
                        facontainer.Invalidate();
                    });
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
            tr = s.CreateAndAddTextBoxAndTwoButtonsRow(facontainer, fa.Tag, "Edit", null, "Remove", null,
                                                               (arg1, arg2) =>
                                                               {
                                                                   fa.Tag = arg1.Text;
                                                               },
                                                               (arg1, arg2) =>
                                                               {
                                                                   //if (Application.Instance.Platform.IsMac)
                                                                   //{
                                                                   //    flowsheet.ShowMessage("Sorry, editing a Flash Algorithm is not yet possible on the macOS platform.", IFlowsheet.MessageType.GeneralError);
                                                                   //    return;
                                                                   //}
                                                                   Application.Instance.Invoke(() =>
                                                                   {
                                                                       Thermodynamics.FlashAlgorithmConfig f = new Thermodynamics.FlashAlgorithmConfig
                                                                       {
                                                                           Settings = fa.FlashSettings,
                                                                           AvailableCompounds = flowsheet.SelectedCompounds.Values.Select(x => x.Name).ToList(),
                                                                           FlashAlgo = fa
                                                                       };

                                                                       if (fa is DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.CAPEOPEN_Equilibrium_Server)
                                                                       {
                                                                           var coflash = (DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.CAPEOPEN_Equilibrium_Server)fa;

                                                                           f._coes = coflash._coes;
                                                                           f._coppm = coflash._coppm;
                                                                           f._selppm = coflash._selppm;
                                                                           f._esname = coflash._esname;
                                                                           f._mappings = coflash._mappings;
                                                                           f._phasemappings = coflash._phasemappings;

                                                                           f.ShowDialog();

                                                                           coflash._coes = f._coes;
                                                                           coflash._coppm = f._coppm;
                                                                           coflash._selppm = f._selppm;
                                                                           coflash._esname = f._esname;
                                                                           coflash._mappings = f._mappings;
                                                                           coflash._phasemappings = f._phasemappings;

                                                                           fa.FlashSettings = f.Settings;

                                                                           f.Dispose();
                                                                           f = null;
                                                                       }
                                                                       else
                                                                       {
                                                                           f.ShowDialog();
                                                                           fa.FlashSettings = f.Settings;
                                                                           f.Dispose();
                                                                           f = null;
                                                                       }
                                                                   });
                                                               },
                                                               (arg1, arg2) =>
                                                               {
                                                                   if (MessageBox.Show("Confirm removal?", "Remove Flash Algorithm", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                                                   {
                                                                       facontainer.Remove(tr);
                                                                       flowsheet.FlowsheetOptions.FlashAlgorithms.Remove(fa);
                                                                       flowsheet.UpdateEditorPanels.Invoke();
                                                                   }
                                                               });
        }

        void AddPropPackItem(PropertyPackage pp)
        {
            pp.Flowsheet = flowsheet;
            var tr = new TableRow();
            tr = s.CreateAndAddTextBoxAndThreeButtonsRow(ppcontainer, pp.Tag, "Edit", null, "Advanced", null, "Remove", null,
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
                                                                        var form = s.GetDefaultEditorForm("Edit '" + pp.Tag + "' (" + pp.ComponentName + ")", 800, 500, cont);
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