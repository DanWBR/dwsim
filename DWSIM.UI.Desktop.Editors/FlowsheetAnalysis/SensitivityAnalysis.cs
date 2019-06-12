using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using DWSIM.SharedClasses.SystemsOfUnits;
using DWSIM.Interfaces.Enums;
using DWSIM.Interfaces;

using System.Threading.Tasks;
using OxyPlot;
using OxyPlot.Axes;

using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.UI.Desktop.Shared;

using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class SensAnalysisView : DynamicLayout
    {

        public Flowsheet flowsheet;

        public DWSIM.SharedClasses.Flowsheet.Optimization.SensitivityAnalysisCase mycase;

        private TextArea resulttextbox;
        private Eto.OxyPlot.Plot resultschart;

        public SensAnalysisView(IFlowsheet fs)
            : base()
        {
            flowsheet = (Flowsheet)fs;
            Init();
        }

        void Init()
        {
            
            DynamicLayout p1, p2;

            StackLayout t1;

            p1 = UI.Shared.Common.GetDefaultContainer();
            p2 = UI.Shared.Common.GetDefaultContainer();

            p1.Width = 380;
            p2.Width = 380;

            t1 = new StackLayout();
            t1.Items.Add(new StackLayoutItem(p1));
            t1.Items.Add(new StackLayoutItem(p2));
            t1.Orientation = Orientation.Horizontal;
            
            Padding = new Padding(10);

            if (flowsheet.SensAnalysisCollection.Count == 0)
            {
                flowsheet.SensAnalysisCollection.Add(new DWSIM.SharedClasses.Flowsheet.Optimization.SensitivityAnalysisCase());
            }

            mycase = flowsheet.SensAnalysisCollection.First();

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            s.CreateAndAddLabelRow2(this, "Use the Sensitivity Analysis tool to study/analyze the influence of a process variable on other variables in the flowsheet.");
            
            this.Add(t1);

            s.CreateAndAddLabelRow(p1, "Case ID");
            s.CreateAndAddFullTextBoxRow(p1, mycase.name, (arg3, arg2) => { mycase.name = arg3.Text; });

            s.CreateAndAddLabelRow(p1, "Case Description");
            s.CreateAndAddFullTextBoxRow(p1, mycase.description, (arg3, arg2) => { mycase.description = arg3.Text; });

            s.CreateAndAddLabelRow(p1, "Independent Variable");

            var objlist = flowsheet.SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");

            var spinner = s.CreateAndAddDropDownRow(p1, "Object", objlist, 0, null);

            var spinner2 = s.CreateAndAddDropDownRow(p1, "Property", new List<string>(), 0, null);

            List<string> proplist = new List<string>();

            spinner.SelectedIndexChanged += (sender, e) =>
            {
                if (spinner.SelectedIndex > 0)
                {
                    mycase.iv1.objectID = flowsheet.GetFlowsheetSimulationObject(objlist[spinner.SelectedIndex]).Name;
                    mycase.iv1.objectTAG = objlist[spinner.SelectedIndex];
                    proplist = flowsheet.GetFlowsheetSimulationObject(objlist[spinner.SelectedIndex]).GetProperties(PropertyType.WR).ToList();
                    proplist.Insert(0, "");
                    spinner2.Items.Clear();
                    spinner2.Items.AddRange(proplist.Select(x => new ListItem { Key = x, Text = flowsheet.GetTranslatedString(x) }).ToList());
                    spinner2.SelectedIndex = (proplist.IndexOf(mycase.iv1.propID));
                }
                else
                {
                    spinner2.Items.Clear();
                }
            };
            if (flowsheet.SimulationObjects.ContainsKey(mycase.iv1.objectID))
            {
                spinner.SelectedIndex = (objlist.IndexOf(flowsheet.SimulationObjects[mycase.iv1.objectID].GraphicObject.Tag));
            }

            var txtLowerLimit = s.CreateAndAddTextBoxRow(p1, nf, "Initial Value", 0, null);
            var txtUpperLimit = s.CreateAndAddTextBoxRow(p1, nf, "Final Value", 0, null);
            var txtSteps = s.CreateAndAddTextBoxRow(p1, "0", "Number of Steps", 0, null);

            txtLowerLimit.Text = mycase.iv1.lowerlimit.GetValueOrDefault().ToString(nf);
            txtUpperLimit.Text = mycase.iv1.upperlimit.GetValueOrDefault().ToString(nf);
            txtSteps.Text = mycase.iv1.points.ToString();

            var labelUnits = s.CreateAndAddTwoLabelsRow(p1, "Property Units", "");

            spinner2.SelectedIndexChanged += (sender, e) =>
            {
                if (spinner2.SelectedIndex > 0)
                {
                    mycase.iv1.propID = proplist[spinner2.SelectedIndex];
                    mycase.iv1.unit = flowsheet.GetFlowsheetSimulationObject(objlist[spinner.SelectedIndex]).GetPropertyUnit(proplist[spinner2.SelectedIndex], su);
                    labelUnits.Text = mycase.iv1.unit;
                }
            };

            double dummy = 0.0f;

            txtLowerLimit.TextChanged += (sender2, e2) =>
            {
                if (double.TryParse(txtLowerLimit.Text.ToString(), out dummy))
                {
                    mycase.iv1.lowerlimit = Double.Parse(txtLowerLimit.Text);
                }
            };

            txtUpperLimit.TextChanged += (sender2, e2) =>
            {
                if (double.TryParse(txtUpperLimit.Text.ToString(), out dummy))
                {
                    mycase.iv1.upperlimit = Double.Parse(txtUpperLimit.Text);
                }
            };

            txtSteps.TextChanged += (sender2, e2) =>
            {
                if (double.TryParse(txtSteps.Text.ToString(), out dummy))
                {
                    mycase.iv1.points = Int32.Parse(txtSteps.Text);
                }
            };

            s.CreateAndAddEmptySpace(p1);
            s.CreateAndAddEmptySpace(p1);
            s.CreateAndAddEmptySpace(p1);
            s.CreateAndAddEmptySpace(p1);
            s.CreateAndAddEmptySpace(p1);
            s.CreateAndAddEmptySpace(p1);

            var btnAddDepVar = s.CreateAndAddBoldLabelAndButtonRow(p2, "Dependent Variables", "Add New", null, null);
           
            var ll = new StackLayout { Orientation = Orientation.Vertical, Padding = new Eto.Drawing.Padding(2), Spacing = 10 };
            ll.RemoveAll();

            var sc = new Scrollable { Border = BorderType.None, Content = ll };

            s.CreateAndAddControlRow(p2, sc);

            t1.SizeChanged += (sender, e) => {
                if (p1.ParentWindow != null)
                {
                    p1.Width = (int)(p1.ParentWindow.Width / 2 - 10);
                    p2.Width = (int)(p2.ParentWindow.Width / 2 - 10);
                    p1.Height = p1.ParentWindow.Height - 170;
                    p2.Height = p1.Height;
                    sc.Height = p2.Height - btnAddDepVar.Height - 30;
                    foreach (var item in ll.Items)
                    {
                        item.Control.Width = sc.Width - 25;
                    }
                }
            };

            foreach (var dvar in mycase.depvariables.Values)
            {
                AddDepVar(ll, dvar, objlist);
            }

            btnAddDepVar.Click += (sender2, e2) =>
            {
                var depvar = new DWSIM.SharedClasses.Flowsheet.Optimization.SAVariable();
                depvar.id = Guid.NewGuid().ToString();
                mycase.depvariables.Add(depvar.id, depvar);
                AddDepVar(ll, depvar, objlist);
            };

            var btnRun = s.CreateAndAddButtonRow(this, "Run Analysis", null, null);

            var btnAbort = s.CreateAndAddButtonRow(this, "Abort Run", null, null);

            btnAbort.Enabled = false;

            resulttextbox = new TextArea { Height = 400, Text = "", Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize), ReadOnly = true };

            s.CreateAndAddLabelRow(this, "Results");

            var btnrt = s.CreateAndAddLabelAndButtonRow(this, "View Results Report", "View Report", null, (sender, e) =>
            {
                var form = s.GetDefaultEditorForm("Sensitivity Analysis Report", 500, 500, resulttextbox, true);
                form.Show();
            });
            btnrt.Enabled = false;

            resultschart = new Eto.OxyPlot.Plot { Height = 400 };

            var btnrc = s.CreateAndAddLabelAndButtonRow(this, "View Results Chart", "View Chart", null, (sender, e) =>
            {
                var form = s.GetDefaultEditorForm("Sensitivity Analysis Result", 500, 500, resultschart, true);
                form.Show();
            });
            btnrc.Enabled = false;

            btnRun.Click += (sender2, e2) =>
            {

                int iv1np, i;
                double iv1ll, iv1ul, iv1val, iv1val0;
                string iv1id, iv1prop;
                List<List<double>> results = new List<List<double>>();

                iv1ll = Converter.ConvertToSI(mycase.iv1.unit, mycase.iv1.lowerlimit.GetValueOrDefault());
                iv1ul = Converter.ConvertToSI(mycase.iv1.unit, mycase.iv1.upperlimit.GetValueOrDefault());
                iv1np = mycase.iv1.points - 1;
                iv1id = mycase.iv1.objectID;
                iv1prop = mycase.iv1.propID;

                flowsheet.SupressMessages = true;

                Application.Instance.Invoke(() =>
                {
                    btnAbort.Enabled = true;
                    btnrt.Enabled = false;
                    btnrc.Enabled = false;
                    flowsheet.ShowMessage("Starting Sensitivity Analysis, please wait...", IFlowsheet.MessageType.Information);
                });

                var task = new Task(() =>
                {

                    flowsheet.SolveFlowsheet(true);

                    iv1val0 = Convert.ToDouble(flowsheet.SimulationObjects[iv1id].GetPropertyValue(iv1prop));

                    for (i = 0; i <= iv1np; i++)
                    {
                        iv1val = iv1ll + i * (iv1ul - iv1ll) / iv1np;
                        flowsheet.SimulationObjects[iv1id].SetPropertyValue(iv1prop, iv1val);
                        flowsheet.SolveFlowsheet(true);
                        List<double> depvarvals = new List<double>();
                        foreach (var depvar in mycase.depvariables.Values)
                        {
                            depvar.currentvalue = Convert.ToDouble(flowsheet.SimulationObjects[depvar.objectID].GetPropertyValue(depvar.propID));
                            depvarvals.Add(depvar.currentvalue);
                        }
                        results.Add(depvarvals);
                    }

                    mycase.results = new System.Collections.ArrayList();

                    foreach (var res in results)
                    {
                        mycase.results.Add(res.ToArray());
                    }

                    flowsheet.SimulationObjects[iv1id].SetPropertyValue(iv1prop, iv1val0);
                    flowsheet.SolveFlowsheet(true);

                }, GlobalSettings.Settings.TaskCancellationTokenSource.Token);
                                
                task.ContinueWith((t) =>
                {
                    flowsheet.SupressMessages = false;

                    if (t.Exception != null)
                    {
                        Application.Instance.Invoke(() =>
                         {

                             btnAbort.Enabled = false;
                             btnrt.Enabled = false;
                             btnrc.Enabled = false;

                             flowsheet.ShowMessage("Error: " + t.Exception.Message, IFlowsheet.MessageType.GeneralError);
                         });
                    }
                    else
                    {
                        Application.Instance.Invoke(() =>
                         {

                             btnAbort.Enabled = false;
                             btnrt.Enabled = true;
                             btnrc.Enabled = true;

                             flowsheet.ShowMessage("Sensitivity Analysis finished successfully.", IFlowsheet.MessageType.Information);
                             if (t.Status == TaskStatus.RanToCompletion)
                             {
                                 var str = new System.Text.StringBuilder();
                                 str.AppendLine("Sensitivity Analysis Run Results");
                                 str.AppendLine();
                                 str.AppendLine("Independent Variable: " + flowsheet.SimulationObjects[iv1id].GraphicObject.Tag + " / " + flowsheet.GetTranslatedString(mycase.iv1.propID));
                                 str.AppendLine();
                                 str.AppendLine("Range: " + mycase.iv1.lowerlimit.GetValueOrDefault() + " to " + mycase.iv1.upperlimit.GetValueOrDefault() + " " + mycase.iv1.unit + ", " + mycase.iv1.points + " steps");
                                 str.AppendLine();
                                 str.AppendLine("Dependent Variables:");
                                 int count = 1;
                                 str.AppendLine();
                                 foreach (var dvar in mycase.depvariables.Values)
                                 {
                                     str.AppendLine(count + " - " + flowsheet.SimulationObjects[dvar.objectID].GraphicObject.Tag + " / " + flowsheet.GetTranslatedString(dvar.propID) + " (" + dvar.unit + ")");
                                     count += 1;
                                 }
                                 str.AppendLine();
                                 str.AppendLine("Ind var\t\tDep. vars");
                                 int cnt = 0;
                                 List<double> px = new List<double>();
                                 List<List<double>> py = new List<List<double>>();
                                 foreach (var dvar in mycase.depvariables.Values)
                                 {
                                     py.Add(new List<double>());
                                 }
                                 foreach (double[] res in mycase.results)
                                 {
                                     var dv = Converter.ConvertFromSI(mycase.iv1.unit, iv1ll + cnt * (iv1ul - iv1ll) / iv1np);
                                     px.Add(dv);
                                     string line = dv.ToString(nf) + "\t\t";
                                     int j = 0;
                                     foreach (var d in res)
                                     {
                                         py[j].Add(Converter.ConvertFromSI(mycase.depvariables.Values.ElementAt(j).unit, d));
                                         line += Converter.ConvertFromSI(mycase.depvariables.Values.ElementAt(j).unit, d).ToString(nf) + "\t\t";
                                         j += 1;
                                     }
                                     str.AppendLine(line);
                                     cnt += 1;
                                 }

                                 Application.Instance.Invoke(() => resulttextbox.Text = str.ToString());

                                 var model = new PlotModel() { Subtitle = "Sensitivity Analysis Run Results", Title = flowsheet.FlowsheetOptions.SimulationName };
                                 model.TitleFontSize = 14;
                                 model.SubtitleFontSize = 11;
                                 model.Axes.Add(new LinearAxis()
                                 {
                                     MajorGridlineStyle = LineStyle.Dash,
                                     MinorGridlineStyle = LineStyle.Dot,
                                     Position = AxisPosition.Bottom,
                                     FontSize = 12,
                                     Title = mycase.iv1.objectTAG + " / " + flowsheet.GetTranslatedString(mycase.iv1.propID) + " (" + mycase.iv1.unit + ")"
                                 });
                                 int cnt2 = 0;
                                 foreach (var dvar in mycase.depvariables.Values)
                                 {
                                     model.Axes.Add(new LinearAxis()
                                     {
                                         MajorGridlineStyle = LineStyle.Dash,
                                         MinorGridlineStyle = LineStyle.Dot,
                                         FontSize = 12,
                                         Title = dvar.objectTAG + " / " + flowsheet.GetTranslatedString(dvar.propID) + " (" + dvar.unit + ")"
                                     });
                                     model.Axes[cnt2 + 1].Key = cnt2.ToString();
                                     model.Axes[cnt2 + 1].PositionTier = cnt2;
                                     model.Axes[cnt2 + 1].AxislineStyle = LineStyle.Solid;
                                     model.AddLineSeries(px, py[cnt2]);
                                     model.Series[cnt2].Title = dvar.objectTAG + " / " + flowsheet.GetTranslatedString(dvar.propID);
                                     ((OxyPlot.Series.LineSeries)(model.Series[cnt2])).YAxisKey = cnt2.ToString();
                                     cnt2 += 1;
                                 }
                                 model.LegendFontSize = 11;
                                 model.LegendPlacement = LegendPlacement.Outside;
                                 model.LegendOrientation = LegendOrientation.Vertical;
                                 model.LegendPosition = LegendPosition.BottomCenter;
                                 model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView;

                                 Application.Instance.Invoke(() => { resultschart.Model = model; resultschart.Invalidate(); });

                             }
                         });
                    }
                });

                btnAbort.Click += (sender, e) =>
                {
                    GlobalSettings.Settings.TaskCancellationTokenSource.Cancel();
                };

                task.Start();

            };

        }

        private void AddDepVar(StackLayout container, DWSIM.SharedClasses.Flowsheet.Optimization.SAVariable depvar, List<string> objlist)
        {

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            List<string> proplist2 = new List<string>();
            var myview = new DynamicLayout { Padding = new Padding(5), Width = container.Width - 25};
            var slcontainer = new StackLayoutItem(myview);
            s.CreateAndAddLabelRow(myview, "Dependent Variable #" + (container.Items.Count + 1).ToString());
            var spinobj = s.CreateAndAddDropDownRow(myview, "Object", objlist, 0, null);
            var spinprop = s.CreateAndAddDropDownRow(myview, "Property", new List<string>(), 0, null);

            spinobj.SelectedIndexChanged += (sender2, e2) =>
            {
                if (spinobj.SelectedIndex > 0)
                {
                    depvar.objectID = flowsheet.GetFlowsheetSimulationObject(objlist[spinobj.SelectedIndex]).Name;
                    depvar.objectTAG = objlist[spinobj.SelectedIndex];
                    proplist2 = flowsheet.GetFlowsheetSimulationObject(objlist[spinobj.SelectedIndex]).GetProperties(PropertyType.ALL).ToList();
                    proplist2.Insert(0, "");
                    spinprop.Items.Clear();
                    spinprop.Items.AddRange(proplist2.Select(x => new ListItem { Key = x, Text = flowsheet.GetTranslatedString(x) }).ToList());
                    spinprop.SelectedIndex = (proplist2.IndexOf(depvar.propID));
                }
            };

            if (flowsheet.SimulationObjects.ContainsKey(depvar.objectID))
            {
                spinobj.SelectedIndex = (objlist.IndexOf(flowsheet.SimulationObjects[depvar.objectID].GraphicObject.Tag));
            }

            spinprop.SelectedIndexChanged += (sender2, e2) =>
            {
                if (spinprop.SelectedIndex > 0)
                {
                    depvar.propID = proplist2[spinprop.SelectedIndex];
                    depvar.unit = flowsheet.GetFlowsheetSimulationObject(objlist[spinobj.SelectedIndex]).GetPropertyUnit(proplist2[spinprop.SelectedIndex], su);
                }
            };

            var btnremove = s.CreateAndAddButtonRow(myview, "Remove Variable", null, null);
            btnremove.Click += (sender2, e2) =>
            {
                mycase.depvariables.Remove(depvar.id);
                container.Items.Remove(slcontainer);
            };

            container.Items.Add(slcontainer);

        }
    }
}