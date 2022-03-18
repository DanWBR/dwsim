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
using s = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UI.Desktop.Shared;

using DWSIM.ExtensionMethods;
using System.IO;
using DWSIM.Interfaces.Enums;
using DWSIM.UI.Desktop.Shared.Controls;
using DWSIM.Thermodynamics.Utilities.PetroleumCharacterization;

namespace DWSIM.UI.Desktop.Editors
{
    public class BulkC7PCharacterization : DynamicLayout
    {

        public Flowsheet flowsheet;

        private String assayname;

        int ncomps = 10;
        double t1, t2, v1, v2, mw0, sg0, nbp0;
        string Tccorr = "Riazi-Daubert (1985)", Pccorr = "Riazi-Daubert (1985)", AFcorr = "Lee-Kesler (1976)", MWcorr = "Winn (1956)";
        bool adjustAf = true, adjustZR = true;

        Nullable<double> mw, sg, nbp;


        public BulkC7PCharacterization(Flowsheet fs) : base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            mw0 = 80.0f;
            sg0 = 0.70f;
            nbp0 = 333.0f;

            t1 = 38 + 273.15;
            t2 = 98.9 + 273.15;

            assayname = "OIL";
            v1 = 0;
            v2 = 0;

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            s.CreateAndAddLabelRow(this, "Assay Identification");
            s.CreateAndAddStringEditorRow(this, "Assay Name", assayname, (arg3, arg2) =>
            {
                assayname = arg3.Text;
            });
            s.CreateAndAddDescriptionRow(this, "Enter the name of the assay. It will be used to identify the Material Stream on the flowsheet and the associated compounds as well.");

            s.CreateAndAddLabelRow(this, "Property Methods");

            s.CreateAndAddDescriptionRow(this, "Select the methods to calculate compound properties.");

            s.CreateAndAddDropDownRow(this, "Molecular Weight", new List<string>() { "Winn (1956)", "Riazi (1986)", "Lee-Kesler (1974)" }, 0, (arg3, arg2) =>
            {
                MWcorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Critical Temperature", new List<string>() { "Riazi-Daubert (1985)", "Riazi (2005)", "Lee-Kesler (1976)", "Farah (2006)" }, 0, (arg3, arg2) =>
            {
                Tccorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Critical Pressure", new List<string>() { "Riazi-Daubert (1985)", "Lee-Kesler (1976)", "Farah (2006)" }, 0, (arg3, arg2) =>
            {
                Pccorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Acentric Factor", new List<string>() { "Lee-Kesler (1976)", "Korsten (2000)" }, 0, (arg3, arg2) =>
            {
                AFcorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddCheckBoxRow(this, "Adjust Acentric Factors to match Normal Boiling Temperatures", adjustAf, (arg1, arg2) =>
            {
                adjustAf = arg1.Checked.GetValueOrDefault();
            }, null);
            s.CreateAndAddCheckBoxRow(this, "Adjust Rackett Parameters to match Specific Gravities", adjustZR, (arg1, arg2) =>
            {
                adjustZR = arg1.Checked.GetValueOrDefault();
            }, null);

            s.CreateAndAddLabelRow(this, "Assay Properties");
            s.CreateAndAddDescriptionRow(this, "Define at least one of the following three properties in order to calculate a property distribution.");

            s.CreateAndAddTextBoxRow(this, nf, "Molar Weight", mw.GetValueOrDefault(), (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    mw = Double.Parse(arg3.Text);
                }
            });
            s.CreateAndAddDescriptionRow(this, "Leave it unchanged if not available.");
            s.CreateAndAddTextBoxRow(this, nf, "Specific Gravity", sg.GetValueOrDefault(), (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    sg = Double.Parse(arg3.Text);
                }
            });
            s.CreateAndAddDescriptionRow(this, "Leave it unchanged if not available.");
            s.CreateAndAddTextBoxRow(this, nf, "Average NBP (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, nbp.GetValueOrDefault()), (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    nbp = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text));
                }
            });
            s.CreateAndAddDescriptionRow(this, "Leave it unchanged if not available.");


            s.CreateAndAddLabelRow(this, "Initial Values for Property Distribution");
            s.CreateAndAddTextBoxRow(this, nf, "Molar Weight", mw0, (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    mw0 = Double.Parse(arg3.Text);
                }
            });
            s.CreateAndAddDescriptionRow(this, "This defines the Molar Weight of the lightest compound in the assay.");
            s.CreateAndAddTextBoxRow(this, nf, "Specific Gravity", sg0, (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    sg0 = Double.Parse(arg3.Text);
                }
            });
            s.CreateAndAddDescriptionRow(this, "This defines the Specific Gravity of the lightest compound in the assay.");
            s.CreateAndAddTextBoxRow(this, nf, "Normal Boiling Point (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, nbp0), (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    nbp0 = cv.ConvertToSI(su.temperature, Double.Parse(arg3.Text));
                }
            });
            s.CreateAndAddDescriptionRow(this, "This defines the Normal Boiling Point of the lightest compound in the assay.");

            s.CreateAndAddLabelRow(this, "Pseudo Compounds");
            s.CreateAndAddTextBoxRow(this, "N0", "Number of Compounds", ncomps, (arg3, arg2) =>
            {
                if (s.IsValidDouble(arg3.Text))
                {
                    ncomps = int.Parse(arg3.Text);
                }
            });
            s.CreateAndAddDescriptionRow(this, "Specify the number of compounds to be generated that, together, will represent the assay. The generated compounds will be added to the simulation and a Material Stream will be created with distribution-defined amounts of these compounds.");

            s.CreateAndAddButtonRow(this, "Characterize Assay and Create Compounds", null, (arg3, arg2) =>
            {
                var dialog = ProgressDialog.Show(this, "Petroleum C7+ Characterization", "Generating compounds, please wait...", false);
                var comps = new Dictionary<string, ICompound>();
                Task.Factory.StartNew(() =>
                {
                    comps = new GenerateCompounds().GenerateCompounds(assayname, ncomps, Tccorr, Pccorr, AFcorr, MWcorr, adjustAf, adjustZR, mw, sg, nbp, v1, v2, t1, t2, mw0, sg0, nbp0);
                }).ContinueWith((t) =>
                {
                    Application.Instance.Invoke(() => { dialog.Close(); });
                    if (t.Exception == null)
                    {
                        var assay = new DWSIM.SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay(mw.GetValueOrDefault(), sg.GetValueOrDefault(), nbp.GetValueOrDefault(), t1, t2, v1, v2);
                        var ms2 = new MaterialStream("", "");
                        ms2.SetFlowsheet(flowsheet);
                        if (flowsheet.PropertyPackages.Count > 0)
                        {
                            ms2.SetPropertyPackage(flowsheet.PropertyPackages.Values.First());
                        }
                        else
                        {
                            ms2.SetPropertyPackage(new Thermodynamics.PropertyPackages.PengRobinsonPropertyPackage());
                        }
                        foreach (var subst in comps.Values)
                        {
                            ms2.Phases[0].Compounds.Add(subst.Name, subst);
                            ms2.Phases[1].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[2].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[3].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[4].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[5].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[6].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[7].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                        }
                        var qc = new Thermodynamics.QualityCheck(assay, ms2);
                        qc.DoQualityCheck();
                        Application.Instance.Invoke(() =>
                        {
                            qc.DisplayForm((c) =>
                        {
                            Application.Instance.Invoke(() =>
                            {
                                var form = s.GetDefaultEditorForm("Compound Properties: " + c.Name, 800, 600, new CompoundViewer((Flowsheet)flowsheet, c), false);
                                form.Show();
                            });
                        }, () =>
                        {
                            foreach (var comp in comps.Values)
                            {
                                if (!flowsheet.AvailableCompounds.ContainsKey(comp.Name))
                                {
                                    flowsheet.AvailableCompounds.Add(comp.Name, comp.ConstantProperties);
                                }
                                flowsheet.SelectedCompounds.Add(comp.Name, flowsheet.AvailableCompounds[comp.Name]);
                                foreach (MaterialStream obj in flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream))
                                {
                                    foreach (var phase in obj.Phases.Values)
                                    {
                                        phase.Compounds.Add(comp.Name, new Thermodynamics.BaseClasses.Compound(comp.Name, ""));
                                        phase.Compounds[comp.Name].ConstantProperties = flowsheet.SelectedCompounds[comp.Name];
                                    }
                                }
                            }
                            var ms = (MaterialStream)flowsheet.AddObject(ObjectType.MaterialStream, 100, 100, assayname);
                            double wtotal = comps.Values.Select((x) => x.MoleFraction.GetValueOrDefault() * x.ConstantProperties.Molar_Weight).Sum();
                            foreach (var c in ms.Phases[0].Compounds.Values)
                            {
                                c.MassFraction = 0.0f;
                                c.MoleFraction = 0.0f;
                            }
                            foreach (var c in comps.Values)
                            {
                                c.MassFraction = c.MoleFraction.GetValueOrDefault() * c.ConstantProperties.Molar_Weight / wtotal;
                                ms.Phases[0].Compounds[c.Name].MassFraction = c.MassFraction.GetValueOrDefault();
                                ms.Phases[0].Compounds[c.Name].MoleFraction = c.MoleFraction.GetValueOrDefault();
                            }
                            Application.Instance.Invoke(() =>
                            {
                                flowsheet.UpdateInterface();
                                flowsheet.UpdateEditorPanels.Invoke();
                                flowsheet.ShowMessage("Material Stream '" + assayname + "' added successfully. " + ncomps.ToString() + " compounds created.", IFlowsheet.MessageType.Information);

                                if (MessageBox.Show("Do you want to export the created compounds to a XML database?", "Petroleum C7+ Characterization", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.Yes) == DialogResult.Yes)
                                {
                                    try
                                    {
                                        var compstoexport = comps.Values.Select((x) => x.ConstantProperties).ToArray();
                                        var savedialog = new SaveFileDialog();
                                        savedialog.Title = "Save Compounds to XML Database";
                                        savedialog.Filters.Add(new FileFilter("XML File", new[] { ".xml" }));
                                        savedialog.CurrentFilterIndex = 0;
                                        if (savedialog.ShowDialog(this) == DialogResult.Ok)
                                        {
                                            try
                                            {
                                                if (!File.Exists(savedialog.FileName))
                                                {
                                                    File.WriteAllText(savedialog.FileName, "");
                                                    Thermodynamics.Databases.UserDB.CreateNew(savedialog.FileName, "compounds");
                                                }
                                                using (var stream = new FileStream(savedialog.FileName, FileMode.OpenOrCreate))
                                                {
                                                    Thermodynamics.Databases.UserDB.AddCompounds(compstoexport, stream, true);
                                                }
                                                flowsheet.ShowMessage("Compounds successfully saved to XML file.", IFlowsheet.MessageType.Information);
                                            }
                                            catch (Exception ex)
                                            {
                                                flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                                            }
                                        }
                                    }
                                    catch (Exception ex)
                                    {
                                        flowsheet.ShowMessage("Error saving data: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                                    }
                                }
                            });
                        });
                        });
                    }
                    else
                    {
                        Application.Instance.Invoke(() =>
                    {
                        flowsheet.ShowMessage("Error saving data: " + t.Exception.GetBaseException().Message, IFlowsheet.MessageType.GeneralError);
                    });
                    }
                });
            });
        }
    }
}
