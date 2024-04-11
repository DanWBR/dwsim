using DWSIM.Interfaces;
using DWSIM.SharedClasses.DWSIM.Flowsheet;
using DWSIM.Thermodynamics.PropertyPackages;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace DWSIM.ProFeatures
{

    public class Functions
    {

        public static Func<IFlowsheet, bool> TransitionAction;

        public static bool ProcessTransition(IFlowsheet flowsheet)
        {

            return (bool)TransitionAction?.Invoke(flowsheet);

        }

        public static void DisplayTransitionForm(IFlowsheet flowsheet, string featurename, bool? skipIntro = false)
        {

            var fp = new FormBridgeToPro(skipIntro);
            fp.lblFeature.Text = featurename;
            fp.CurrentFlowsheet = flowsheet;
            fp.ShowDialog();

        }

        public static void AddProUnitOps(FlowLayoutPanel[] Panels)
        {

            foreach (var panel in Panels)
            {
                switch (panel.Name)
                {
                    case "PanelPressure":
                        AddUnitOp("Pipe Network", "DWSIM.UnitOperations.PipeNetworkUnitOperation", My.Resources.Resources.pipe_network_icon, panel);
                        break;
                    case "PanelMixers":
                        AddUnitOp("Energy Stream Splitter", "DWSIM.AdditionalUnitOperations.EnergySplitter", My.Resources.Resources.uo_split_32, panel);
                        break;
                    case "PanelExchangers":
                        AddUnitOp("Falling Film Evaporator", "DWSIM.AdditionalUnitOperations.FallingFilmEvaporator", My.Resources.Resources.fallingfilm_icon, panel);
                        break;
                    case "PanelColumns":
                        AddUnitOp("PPBDesigner Column", "ProExtensions.PPBDesigner.PPBDesignerUnitOperation", My.Resources.Resources.ppbdesigner, panel);
                        AddUnitOp("Three-Phase/Reactive Column (Pro)", "ProExtensions.UnitOperations.ProRigorousColumns.DistillationColumnPro", My.Resources.Resources.col_dc_32, panel);
                        break;
                    case "PanelUser":
                        AddUnitOp("Neural Network (Pro)", "DWSIM.UnitOperations.NeuralNetworkUnitOperation", My.Resources.Resources.icons8_artificial_intelligence, panel);
                        break;
                    case "PanelLogical":
                        AddUnitOp("Material Stream Switch", "DWSIM.AdditionalUnitOperations.MaterialStreamSwitch", My.Resources.Resources.switch_material, panel);
                        AddUnitOp("Energy Stream Switch", "DWSIM.AdditionalUnitOperations.EnergyStreamSwitch", My.Resources.Resources.switch_energy, panel);
                        break;
                    case "PanelReactors":
                        AddUnitOp("(Semi)Batch Reactor", "ProExtensions.UnitOperations.SemiBatchReactor.SemiBatchReactor", My.Resources.Resources.jacketedreactor_icon, panel);
                        break;
                }
            }

        }

        private static void AddUnitOp(string name, string typename, Image image, FlowLayoutPanel panel)
        {

            var li = new ListItem();
            li.lblName.Text = name;
            li.lblName.Tag = typename;
            li.lblName.Font = new Font(SystemFonts.MessageBoxFont.FontFamily, 7.0f, FontStyle.Bold);
            li.ToolTip1.SetToolTip(li.lblName, "Upgrade to DWSIM Pro");
            li.Image.Image = image;
            li.ToolTip1.SetToolTip(li.Image, "Upgrade to DWSIM Pro");
            li.ObjectTypeInfo = li.GetType();
            li.Tag = DWSIM.Interfaces.Enums.SimulationObjectClass.None;
            panel.Controls.Add(li);

        }

        public static void AddProPPs(DataGridView grid)
        {

            grid.Rows.Add(new object[] { PackageType.ActivityCoefficient, 0, null, My.Resources.Resources.Icon1281, "Amines", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            grid.Rows.Add(new object[] { PackageType.ActivityCoefficient, 0, null, My.Resources.Resources.Icon1281, "Peng-Robinson 1978 (PR78) for Petroleum Industry", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "NRTL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIQUAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIFAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIFAC-LL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Modified UNIFAC (Dortmund) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Modified UNIFAC (NIST) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            grid.Rows.Add(new object[] { PackageType.Miscelaneous, 0, null, My.Resources.Resources.Icon1281, "REFPROP", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            grid.Rows.Add(new object[] { PackageType.Miscelaneous, 0, null, My.Resources.Resources.Icon1281, "Extended CoolProp", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            // grid.Rows.Add(New Object() {PackageType.EOS, 0, Nothing, My.Resources.Icon1281, "Peng-Robinson 1978 Pro (PR78 Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {PackageType.EOS, 0, Nothing, My.Resources.Icon1281, "Soave-Redlich-Kwong Pro (SRK Pro)", "Upgrade to DWSIM Pro to use this Property Package."})

        }

        public static void AddProPPs2(DataGridView grid)
        {

            grid.Rows.Add(new object[] { "", My.Resources.Resources.Icon1281, "Amines", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            grid.Rows.Add(new object[] { "", My.Resources.Resources.Icon1281, "Peng-Robinson 1978 (PR78) for Petroleum Industry", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "NRTL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIQUAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIFAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIFAC-LL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Modified UNIFAC (Dortmund) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Modified UNIFAC (NIST) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            grid.Rows.Add(new object[] { "", My.Resources.Resources.Icon1281, "REFPROP", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            grid.Rows.Add(new object[] { "", My.Resources.Resources.Icon1281, "Extended CoolProp", "Upgrade to DWSIM Pro to use this Property Package." });
            grid.Rows[grid.Rows.Count - 1].DefaultCellStyle.BackColor = Color.Honeydew;
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Peng-Robinson 1978 Pro (PR78 Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
            // grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Soave-Redlich-Kwong Pro (SRK Pro)", "Upgrade to DWSIM Pro to use this Property Package."})

        }

        public static void CreateTransitionObject(IFlowsheet fs, string feature, string @type, string action, string location, double[] position)
        {

            var ts = new FlowsheetTransitionRestore();
            ts.FeatureName = feature;
            ts.FeatureType = type;
            ts.Action = action;
            ts.Location = location;
            if (position is not null)
                ts.Position = new List<double>(position);
            fs.FlowsheetOptions.FlowsheetTransitionObject = ts;

        }

    }
}