using DWSIM.UI.Desktop.Shared;
using DWSIM.UnitOperations.SpecialOps;
using Eto.Forms;
using System;
using System.Linq;
using System.Threading.Tasks;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using DWSIM.ExtensionMethods;
using Eto.Drawing;
using DWSIM.Interfaces;
using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using OxyPlot.Series;
using OxyPlot;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid.DataFormat;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;

namespace DWSIM.UI.Desktop.Editors.LogicalBlocks
{
    public class ControllerPanel : DynamicLayout
    {

        public Adjust controller;
        private Flowsheet flowsheet;
        private int convmethod, curriter;
        private double tol = 0.0001, step = 0.1, max = 1000000, min = 0;

        private Label currstatus;
        private TextBox tberror, tbcurrval;

        private Eto.OxyPlot.Plot chart;
        private DWSIM.CrossPlatform.UI.Controls.ReoGrid.ReoGridControl grid;

        private IUnitsOfMeasure su;
        private string nf;

        private bool running = false, cancel = false;

        public ControllerPanel(Adjust ctrlr) : base()
        {
            controller = ctrlr;
            flowsheet = (Flowsheet)controller.GetFlowsheet();
            Init();
        }

        public void Init()
        {

            Padding = new Padding(5);

            su = controller.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            nf = controller.GetFlowsheet().FlowsheetOptions.NumberFormat;

            this.CreateAndAddLabelRow("Settings");

            var methods = new[] { "Secant", "Brent", "Newton", "IPOPT" }.ToList();

            this.CreateAndAddDropDownRow("Convergence Method", methods, 0, (dd, e) => convmethod = dd.SelectedIndex);
            this.CreateAndAddNumericEditorRow("Maximum Iterations", controller.MaximumIterations, 5, 1000, 0, (ns, e) => controller.MaximumIterations = (int)ns.Value);
            this.CreateAndAddNumericEditorRow2("Error Tolerance", tol, Double.MinValue, Double.MaxValue, 6, (ns, e) => tol = ns.Text.ToDoubleFromCurrent());
            this.CreateAndAddNumericEditorRow2("Step Size", controller.StepSize, Double.MinValue, Double.MaxValue, 6, (ns, e) => controller.StepSize = ns.Text.ToDoubleFromCurrent());
            this.CreateAndAddNumericEditorRow2("Lower Bound", controller.MinVal.GetValueOrDefault(), Double.MinValue, Double.MaxValue, 6, (ns, e) => controller.MinVal = ns.Text.ToDoubleFromCurrent());
            this.CreateAndAddNumericEditorRow2("Upper Bound", controller.MaxVal.GetValueOrDefault(), Double.MinValue, Double.MaxValue, 6, (ns, e) => controller.MaxVal = ns.Text.ToDoubleFromCurrent());

            this.CreateAndAddLabelRow("Controls");

            this.CreateAndAddButtonRow("Start/Stop", null, (btn, e) => DoJob());

            this.CreateAndAddLabelRow("Live View");

            currstatus = this.CreateAndAddLabelRow2("Status: idle");

            tbcurrval = this.CreateAndAddTextBoxRow(nf, "Current Value", 0.0, null);
            tberror = this.CreateAndAddTextBoxRow(nf, "Current Error", 0.0, null);

            tbcurrval.ReadOnly = true;
            tberror.ReadOnly = true;

            var tabc = new TabControl { Height = 400 };

            chart = new Eto.OxyPlot.Plot();

            var gridc = GridControl.GetGridControl();
            grid = gridc.GridControl;
            var sheet = grid.Worksheets[0];

            sheet.SetRows(1000);
            sheet.SetCols(5);
            sheet.SetColumnsWidth(0, 5, 100);
            sheet.ColumnHeaders[0].Text = "Iter.";
            sheet.ColumnHeaders[1].Text = "MV";
            sheet.ColumnHeaders[2].Text = "CV";
            sheet.ColumnHeaders[3].Text = "SP";
            sheet.ColumnHeaders[4].Text = "SP-CV";

            sheet.SetRangeDataFormat(0, 0, 1000, 5, CellDataFormatFlag.Number,
            new NumberDataFormatter.NumberFormatArgs
            {
                DecimalPlaces = 6,
                UseSeparator = false,
                NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus
            });
            sheet.SetRangeStyles(0, 0, 1000, 5, new WorksheetRangeStyle
            {
                Flag = PlainStyleFlag.HorizontalAlign,
                HAlign = ReoGridHorAlign.Right
            });
            sheet.SetRangeStyles(0, 0, 1000, 5, new WorksheetRangeStyle
            {
                Flag = PlainStyleFlag.VerticalAlign,
                VAlign = ReoGridVerAlign.Middle
            });
            sheet.SetRangeStyles(0, 0, 1000, 5, new WorksheetRangeStyle
            {
                Flag = PlainStyleFlag.FontAll,
                FontName = SystemFonts.Message().FamilyName,
                FontSize = SystemFonts.Message().Size
            });

            tabc.Pages.Add(new TabPage { Content = chart, Text = "Chart" });
            tabc.Pages.Add(new TabPage { Content = gridc, Text = "Data" });
            this.CreateAndAddControlRow(tabc);
        }

        public void DoJob()
        {

            if (running) cancel = true;

            chart.Model = s.CreatePlotModel(controller.GraphicObject.Tag, "", "Iteration", "Variable");
            chart.Model.AddLineSeries(new double[] { }, new double[] { }, "MV");
            chart.Model.AddLineSeries(new double[] { }, new double[] { }, "CV");
            chart.Model.AddLineSeries(new double[] { }, new double[] { }, "SP");
            chart.Model.AddLineSeries(new double[] { }, new double[] { }, "SP-CV");

            grid.Worksheets[0].ClearRangeContent("A1:E1000", CrossPlatform.UI.Controls.ReoGrid.CellElementFlag.Data);

            double fval, cvVal, rfVal, mvVal, maxval, minval;

            double adjval = cv.ConvertFromSI(controller.ControlledObject.GetPropertyUnit(controller.ControlledObjectData.PropertyName, su), controller.AdjustValue);
            cvVal = (double)GetCtlVarValue();
            mvVal = (double)GetMnpVarValue();
            var mvVal0 = mvVal;
            maxval = controller.MaxVal.GetValueOrDefault();
            minval = controller.MinVal.GetValueOrDefault();
            if (controller.Referenced) rfVal = (double)GetRefVarValue();
            var maxits = controller.MaximumIterations;

            curriter = 0;

            tberror.Text = "";
            tbcurrval.Text = "";
            currstatus.Text = "Status: idle";

            Action<double, double, double, int> updateproc = (mv, cv, sp, c) =>
            {
                currstatus.Text = "Status: working...";
                tberror.Text = (adjval - cv).ToString(nf);
                tbcurrval.Text = cv.ToString(nf);

                ((LineSeries)chart.Model.Series[0]).Points.Add(new DataPoint(c, mv));
                ((LineSeries)chart.Model.Series[1]).Points.Add(new DataPoint(c, cv));
                ((LineSeries)chart.Model.Series[2]).Points.Add(new DataPoint(c, sp));
                ((LineSeries)chart.Model.Series[3]).Points.Add(new DataPoint(c, sp-cv));

                grid.Worksheets[0].Cells[c, 0].Data = c;
                grid.Worksheets[0].Cells[c, 1].Data = mv;
                grid.Worksheets[0].Cells[c, 2].Data = cv;
                grid.Worksheets[0].Cells[c, 3].Data = sp;
                grid.Worksheets[0].Cells[c, 4].Data = sp-cv;

            };

            Func<double, double> funcproc = (xval) =>
            {

                if (cancel) throw new TaskCanceledException(flowsheet.GetTranslatedString("Ajustecanceladopelou"));

                SetMnpVarValue(xval);

                FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);

                if (controller.Referenced)
                {
                    rfVal = (double)GetRefVarValue();
                    var punit = flowsheet.SimulationObjects[controller.ReferencedObjectData.ID].GetPropertyUnit(controller.ReferencedObjectData.PropertyName, su);
                    if (su.GetUnitType(punit) == Interfaces.Enums.UnitOfMeasure.temperature)
                        adjval = rfVal + cv.ConvertFromSI(punit + ".", controller.AdjustValue);
                    else
                        adjval = rfVal + cv.ConvertFromSI(punit, controller.AdjustValue);
                }

                cvVal = (double)GetCtlVarValue();

                fval = cvVal.ConvertToSI(controller.ControlledObject.GetPropertyUnit(controller.ControlledObjectData.PropertyName, su)) -
                       adjval.ConvertToSI(controller.ControlledObject.GetPropertyUnit(controller.ControlledObjectData.PropertyName, su));

                flowsheet.RunCodeOnUIThread(() => updateproc.Invoke((double)GetMnpVarValue(), cvVal, adjval, curriter));

                curriter += 1;
                return fval;
            };

            Action<double> funcrestore = (xvar) =>
            {
                SetMnpVarValue(xvar);
                FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
            };

            Action funcfinish = () =>
            {
                cancel = false;
                running = false;
            };

            running = true;

            switch (convmethod)
            {
                case 0:
                    Task.Factory.StartNew(() =>
                    {
                        mvVal = MathNet.Numerics.RootFinding.Secant.FindRoot(
                          (xval) =>
                          {
                              if (Double.IsNaN(xval) || Double.IsInfinity(xval))
                                  return 1.0E+20;
                              else
                                  return funcproc.Invoke(xval);
                          }, mvVal, mvVal * 1.01, min, max, tol, maxits);
                    }).ContinueWith((t) =>
                    {
                        flowsheet.RunCodeOnUIThread(() => flowsheet.UpdateOpenEditForms());
                        if (t.Exception != null)
                        {
                            funcrestore.Invoke(mvVal0);
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: an error has occurred or canceled by the user.");
                            MessageBox.Show(t.Exception.InnerException.Message, flowsheet.GetTranslatedString("Erro"),
                                            MessageBoxButtons.OK, MessageBoxType.Information);
                        }
                        else {
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: finished successfully!");
                        }
                        funcfinish.Invoke();
                    });
                    break;
                case 1:
                    Task.Factory.StartNew(() =>
                    {
                        mvVal = MathNet.Numerics.RootFinding.Brent.FindRoot(
                          (xval) =>
                          {
                              if (Double.IsNaN(xval) || Double.IsInfinity(xval))
                                  return 1.0E+20;
                              else
                                  return funcproc.Invoke(xval);
                          }, minval, maxval, tol, maxits);
                    }).ContinueWith((t) =>
                    {
                        flowsheet.RunCodeOnUIThread(() => flowsheet.UpdateOpenEditForms());
                        if (t.Exception != null)
                        {
                            funcrestore.Invoke(mvVal0);
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: an error has occurred or canceled by the user.");
                            MessageBox.Show(t.Exception.InnerException.Message, flowsheet.GetTranslatedString("Erro"),
                                            MessageBoxButtons.OK, MessageBoxType.Information);
                        }
                        else
                        {
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: finished successfully!");
                        }
                        funcfinish.Invoke();
                    });
                    break;
                case 2:
                    var nsolv = new MathOps.MathEx.Optimization.NewtonSolver();
                    nsolv.EnableDamping = false;
                    nsolv.MaxIterations = maxits;
                    nsolv.Tolerance = tol * tol;
                    Task.Factory.StartNew(() =>
                    {
                        mvVal = nsolv.Solve((xvars) => new double[] { funcproc.Invoke(xvars[0]) }, new double[] { mvVal })[0];
                    }).ContinueWith((t) =>
                    {
                        flowsheet.RunCodeOnUIThread(() => flowsheet.UpdateOpenEditForms());
                        if (t.Exception != null)
                        {
                            funcrestore.Invoke(mvVal0);
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: an error has occurred or canceled by the user.");
                            MessageBox.Show(t.Exception.InnerException.Message, flowsheet.GetTranslatedString("Erro"),
                                            MessageBoxButtons.OK, MessageBoxType.Information);
                        }
                        else
                        {
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: finished successfully!");
                        }
                        funcfinish.Invoke();
                    });
                    break;
                case 3:
                    var isolv = new MathOps.MathEx.Optimization.IPOPTSolver();
                    isolv.MaxIterations = maxits;
                    isolv.Tolerance = tol;
                    Task.Factory.StartNew(() =>
                    {
                        mvVal = isolv.Solve((xvars) => Math.Pow(funcproc.Invoke(xvars[0]), 2),
                            null, new double[] { mvVal }, new double[] { minval }, new double[] { maxval })[0];
                    }).ContinueWith((t) =>
                    {
                        flowsheet.RunCodeOnUIThread(() => flowsheet.UpdateOpenEditForms());
                        if (t.Exception != null)
                        {
                            funcrestore.Invoke(mvVal0);
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: an error has occurred or canceled by the user.");
                            MessageBox.Show(t.Exception.InnerException.Message, flowsheet.GetTranslatedString("Erro"),
                                            MessageBoxButtons.OK, MessageBoxType.Information);
                        }
                        else
                        {
                            flowsheet.RunCodeOnUIThread(() => currstatus.Text = "Status: finished successfully!");
                        }
                        funcfinish.Invoke();
                    });
                    break;
            }

        }

        private object GetCtlVarValue()
        {
            var data = controller.ControlledObjectData;
            return flowsheet.SimulationObjects[data.ID].GetPropertyValue(data.PropertyName, su);
        }

        private object GetMnpVarValue()
        {
            var data = controller.ManipulatedObjectData;
            return flowsheet.SimulationObjects[data.ID].GetPropertyValue(data.PropertyName, su);
        }

        private object GetRefVarValue()
        {
            var data = controller.ReferencedObjectData;
            return flowsheet.SimulationObjects[data.ID].GetPropertyValue(data.PropertyName, su);
        }

        private void SetMnpVarValue(double value)
        {
            var data = controller.ManipulatedObjectData;
            flowsheet.SimulationObjects[data.ID].SetPropertyValue(data.PropertyName, value, su);
        }

    }
}
