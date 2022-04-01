using DWSIM.Interfaces;
using Eto.Forms;
using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using s = DWSIM.UI.Shared.Common;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UI.Shared;
using DWSIM.ExtensionMethods;
using System.Collections.Generic;
using System.Text;
using System;
using System.Linq;

namespace DWSIM.UI.Desktop.Editors
{
    public class RigorousColumnShared
    {

        public Column column;

        public DynamicLayout container;


        public static DynamicLayout GetInitialEstimatesEditor(Column column)
        {

            var su = column.FlowSheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = column.FlowSheet.FlowsheetOptions.NumberFormat;
            var nff = column.FlowSheet.FlowsheetOptions.FractionNumberFormat;

            var dl = s.GetDefaultContainer();

            dl.CreateAndAddLabelRow("Initial Estimates");
            dl.CreateAndAddDescriptionRow("This feature allows you to override the initial estimates for temperature, flow and composition profiles in order to facilitate convergence.");

            dl.CreateAndAddDescriptionRow("Current Temmperature Units: " + su.temperature);
            dl.CreateAndAddDescriptionRow("Current Molar Flow Units: " + su.molarflow);
            dl.CreateAndAddDescriptionRow("Phase Composition (Liquid/Vapor/Liquid2) in Molar Fractions.");

            dl.CreateAndAddCheckBoxRow("Update Automatically", column.AutoUpdateInitialEstimates, (sender, e) => column.AutoUpdateInitialEstimates = sender.Checked.GetValueOrDefault());

            dl.CreateAndAddCheckBoxRow("Override Temperature Estimates", column.UseTemperatureEstimates, (sender, e) => column.UseTemperatureEstimates = sender.Checked.GetValueOrDefault());
            dl.CreateAndAddCheckBoxRow("Override Liquid Flow Estimates", column.UseLiquidFlowEstimates, (sender, e) => column.UseLiquidFlowEstimates = sender.Checked.GetValueOrDefault());
            dl.CreateAndAddCheckBoxRow("Override Vapor/Liquid2 Estimates", column.UseVaporFlowEstimates, (sender, e) => column.UseVaporFlowEstimates = sender.Checked.GetValueOrDefault());
            dl.CreateAndAddCheckBoxRow("Override Composition Estimates", column.UseCompositionEstimates, (sender, e) => column.UseCompositionEstimates = sender.Checked.GetValueOrDefault());

            var dl1 = s.GetDefaultContainer();
            dl1.Tag = "Variables";

            var dl2 = s.GetDefaultContainer();
            dl2.Tag = "Liquid Composition";

            var dl3 = s.GetDefaultContainer();
            dl3.Tag = "Vapor/Liquid2 Composition";

            if (column.InitialEstimates.StageTemps.Count == 0) column.RebuildEstimates();

            int n = column.InitialEstimates.StageTemps.Count;
            int j = 0;
            int m = column.InitialEstimates.LiqCompositions[0].Keys.Count;

            var sb = new StringBuilder();

            var ie = column.InitialEstimates;

            // temperature and flows

            sb.AppendLine("stage\ttemp\tliqflow\tvapliq2flow");
            int i = 0;
            for (i = 0; i < n; i++)
            {
                sb.AppendLine(string.Format("{0}\t{1}\t{2}\t{3}", (i + 1), ie.StageTemps[i].Value.ConvertFromSI(su.temperature).ToString(nf),
                    ie.LiqMolarFlows[i].Value.ConvertFromSI(su.molarflow).ToString(nf),
                    ie.VapMolarFlows[i].Value.ConvertFromSI(su.molarflow).ToString(nf)));
            }

            var ta1 = dl1.CreateAndAddMultilineMonoSpaceTextBoxRow(sb.ToString(), 400, false, null);
            var tstatus1 = dl1.CreateAndAddDescriptionRow("Current text input is OK.");

            ta1.TextChanged += (sender, e) =>
            {
                try
                {
                    var datalines = ta1.Text.Split('\n');
                    i = 0;
                    foreach (string line in datalines)
                    {
                        if (i > 0 && i <= n)
                        {
                            var val = line.Trim().Split(new[] { ' ', '\t' });
                            column.InitialEstimates.StageTemps[i - 1].Value = cv.ConvertToSI(su.temperature, Convert.ToDouble(val[1]));
                            column.InitialEstimates.LiqMolarFlows[i - 1].Value = cv.ConvertToSI(su.molarflow, Convert.ToDouble(val[2]));
                            column.InitialEstimates.VapMolarFlows[i - 1].Value = cv.ConvertToSI(su.molarflow, Convert.ToDouble(val[3]));
                        }
                        i += 1;
                    }
                    tstatus1.Text = "Current text input is OK.";
                }
                catch (Exception ex)
                {
                    tstatus1.Text = String.Format("Error in current input, line {0}: " + ex.Message, i + 1);
                }
            };

            // liquid compositions

            sb.Clear();
            sb.Append("stage");
            foreach (var key in ie.LiqCompositions[0].Keys)
            {
                sb.Append("\t" + key);
            }
            sb.Append("\n");
            i = 0;
            for (i = 0; i < n; i++)
            {
                sb.Append((i + 1).ToString());
                foreach (var value in ie.LiqCompositions[i].Values)
                {
                    sb.Append("\t" + value.Value.ToString(nff));
                }
                sb.Append("\n");
            }

            var ta2 = dl2.CreateAndAddMultilineMonoSpaceTextBoxRow(sb.ToString(), 400, false, null);
            var tstatus2 = dl2.CreateAndAddDescriptionRow("Current text input is OK.");

            var comps = ie.LiqCompositions[0].Keys.ToArray();

            ta2.TextChanged += (sender, e) =>
            {
                try
                {
                    var datalines = ta2.Text.Split('\n');
                    i = 0;
                    foreach (string line in datalines)
                    {
                        if (i > 0 && i <= n)
                        {
                            var val = line.Trim().Split(new[] { ' ', '\t' });
                            for (j = 0; j < m; j++)
                            {
                                ie.LiqCompositions[i - 1][comps[j]].Value = Convert.ToDouble(val[j + 1]);
                                j += 1;
                            }
                        }
                        i += 1;
                    }
                    tstatus2.Text = "Current text input is OK.";
                }
                catch (Exception ex)
                {
                    tstatus2.Text = String.Format("Error in current input, line {0}, column {1}: " + ex.Message, i + 1, j + 2);
                }
            };

            // vapor/liquid 2 compositions

            sb.Clear();
            sb.Append("stage");
            foreach (var key in ie.VapCompositions[0].Keys)
            {
                sb.Append("\t" + key);
            }
            sb.Append("\n");
            i = 0;
            for (i = 0; i < n; i++)
            {
                sb.Append((i + 1).ToString());
                foreach (var value in ie.VapCompositions[i].Values)
                {
                    sb.Append("\t" + value.Value.ToString(nff));
                }
                sb.Append("\n");
            }

            var ta3 = dl3.CreateAndAddMultilineMonoSpaceTextBoxRow(sb.ToString(), 400, false, null);
            var tstatus3 = dl3.CreateAndAddDescriptionRow("Current text input is OK.");

            ta3.TextChanged += (sender, e) =>
            {
                try
                {
                    var datalines = ta3.Text.Split('\n');
                    i = 0;
                    foreach (string line in datalines)
                    {
                        if (i > 0 && i <= n)
                        {
                            var val = line.Trim().Split(new[] { ' ', '\t' });
                            for (j = 0; j < m; j++)
                            {
                                ie.VapCompositions[i - 1][comps[j]].Value = Convert.ToDouble(val[j + 1]);
                                j += 1;
                            }
                        }
                        i += 1;
                    }
                    tstatus3.Text = "Current text input is OK.";
                }
                catch (Exception ex)
                {
                    tstatus3.Text = String.Format("Error in current input, line {0}, column {1}: " + ex.Message, i + 1, j + 2);
                }
            };

            dl.CreateAndAddButtonRow("Update Estimates from Current Solution", null, (sender, e) =>
            {
                try
                {
                    column.RebuildEstimates();
                    for (i = 0; i < n; i++)
                    {
                        ie.StageTemps[i].Value = column.Tf[i];
                        ie.VapMolarFlows[i].Value = column.Vf[i];
                        ie.LiqMolarFlows[i].Value = column.Lf[i];
                        for (j = 0; j < m; j++)
                        {
                            ie.LiqCompositions[i][comps[j]].Value = ((double[])column.xf.ToArray()[i])[j];
                            ie.VapCompositions[i][comps[j]].Value = ((double[])column.yf.ToArray()[i])[j];
                        }
                    }
                    // temperature and flows
                    sb.Clear();
                    sb.AppendLine("stage\ttemp\tliqflow\tvapliq2flow");
                    for (i = 0; i < n; i++)
                    {
                        sb.AppendLine(string.Format("{0}\t{1}\t{2}\t{3}", (i + 1), ie.StageTemps[i].Value.ConvertFromSI(su.temperature).ToString(nf),
                            ie.LiqMolarFlows[i].Value.ConvertFromSI(su.molarflow).ToString(nf),
                            ie.VapMolarFlows[i].Value.ConvertFromSI(su.molarflow).ToString(nf)));
                    }
                    ta1.Text = sb.ToString();
                    // liquid compositions
                    sb.Clear();
                    sb.Append("stage");
                    foreach (var key in ie.LiqCompositions[0].Keys)
                    {
                        sb.Append("\t" + key);
                    }
                    sb.Append("\n");
                    i = 0;
                    for (i = 0; i < n; i++)
                    {
                        sb.Append((i + 1).ToString());
                        foreach (var value in ie.LiqCompositions[i].Values)
                        {
                            sb.Append("\t" + value.Value.ToString(nff));
                        }
                        sb.Append("\n");
                    }
                    ta2.Text = sb.ToString();
                    // vapor/liquid 2 compositions
                    sb.Clear();
                    sb.Append("stage");
                    foreach (var key in ie.VapCompositions[0].Keys)
                    {
                        sb.Append("\t" + key);
                    }
                    sb.Append("\n");
                    i = 0;
                    for (i = 0; i < n; i++)
                    {
                        sb.Append((i + 1).ToString());
                        foreach (var value in ie.VapCompositions[i].Values)
                        {
                            sb.Append("\t" + value.Value.ToString(nff));
                        }
                        sb.Append("\n");
                    }
                    ta3.Text = sb.ToString();
                }
                catch (Exception ex) {
                    MessageBox.Show("Error reading current solution (is the column solved already?): " + ex.Message, "Error", MessageBoxType.Error);
                }
            });

            var tv = new TabControl();
            tv.Pages.Add(new TabPage(dl1) { Text = dl1.Tag.ToString() });
            tv.Pages.Add(new TabPage(dl2) { Text = dl2.Tag.ToString() });
            tv.Pages.Add(new TabPage(dl3) { Text = dl3.Tag.ToString() });

            dl.Add(tv);

            return dl;

        }

    }
}
