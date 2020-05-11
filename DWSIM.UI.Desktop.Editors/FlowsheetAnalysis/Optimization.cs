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
using DWSIM.UI.Desktop.Shared.Controls;
using DWSIM.SharedClasses.Flowsheet.Optimization;
using Ciloci.Flee;

namespace DWSIM.UI.Desktop.Editors
{
    public class OptimizerView : DynamicLayout
    {

        public Flowsheet flowsheet;
        private bool ongradient = false;
        Dialog dialog = null;

        private Task st;

        public DWSIM.SharedClasses.Flowsheet.Optimization.OptimizationCase mycase;

        public OptimizerView(IFlowsheet fs)
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

            p1.Height = 540;

            t1 = new StackLayout();
            t1.Items.Add(new StackLayoutItem(p1));
            t1.Items.Add(new StackLayoutItem(p2));
            t1.Orientation = Orientation.Horizontal;

            Padding = new Padding(10);

            if (flowsheet.OptimizationCollection.Count == 0)
            {
                flowsheet.OptimizationCollection.Add(new DWSIM.SharedClasses.Flowsheet.Optimization.OptimizationCase());
            }

            mycase = flowsheet.OptimizationCollection.First();

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var objlist = flowsheet.SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");

            s.CreateAndAddLabelRow2(this, "Use the optimizer to bring the flowsheet to a new 'state' regarding the maximum or minimum or value of a user-defined parameter.");

            this.Add(t1);

            s.CreateAndAddLabelRow(p1, "Case ID");
            s.CreateAndAddFullTextBoxRow(p1, mycase.name, (arg3, arg2) => { mycase.name = arg3.Text; });

            s.CreateAndAddLabelRow(p1, "Case Description");
            s.CreateAndAddFullTextBoxRow(p1, mycase.description, (arg3, arg2) => { mycase.description = arg3.Text; });

            s.CreateAndAddDropDownRow(p1, "Optimization Type", new[] { "Minimization", "Maximization" }.ToList(), (int)mycase.type, (arg3, arg2) => { mycase.type = (DWSIM.SharedClasses.Flowsheet.Optimization.OPTType)arg3.SelectedIndex; });
            s.CreateAndAddDropDownRow(p1, "Objective Function", new[] { "Variable", "Expression" }.ToList(), (int)mycase.objfunctype, (arg3, arg2) => { mycase.objfunctype = (DWSIM.SharedClasses.Flowsheet.Optimization.OPTObjectiveFunctionType)arg3.SelectedIndex; });

            s.CreateAndAddTextBoxRow(p1, nf, "Maximum Iterations", (double)mycase.maxits, (arg3, arg2) => { mycase.maxits = (int)arg3.Text.ToDoubleFromCurrent(); });
            s.CreateAndAddTextBoxRow(p1, nf, "Absolute Tolerance", mycase.tolerance, (arg3, arg2) => { mycase.tolerance = arg3.Text.ToDoubleFromCurrent(); });

            var varcontainer = new StackLayout { Orientation = Orientation.Horizontal, Padding = new Eto.Drawing.Padding(0), Spacing = 10 };

            var btnAddVar = s.CreateAndAddBoldLabelAndButtonRow(p2, "Variables", "Add Variable", null, (arg1, arg2) =>
            {

                var newiv = new DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariable();
                newiv.id = new Random().Next().ToString();
                newiv.name = "var" + mycase.variables.Count.ToString();
                mycase.variables.Add(newiv.id, newiv);

                AddVariable(newiv, varcontainer, objlist);

            });

            var desclbl = s.CreateAndAddDescriptionRow(p2, "Variables can be 'INDependent' (actual variables), 'AUXiliary' " +
                                         "(for usage as a parameter in the objective function expression, if applicable), " +
                                         "'DEPendent' (the parameter to be minimized/maximized, if applicable) or CONstraint, " +
                                         "if you don want it to go beyond the defined limits at the solution.");

            var sc = new Scrollable { Border = BorderType.None, Content = varcontainer };

            s.CreateAndAddControlRow(p2, sc);

            //t1.SizeChanged += (sender, e) =>
            //{
            //    if (p1.ParentWindow != null)
            //    {
            //        p1.Width = (int)(p1.ParentWindow.Width / 2 - 10);
            //        p2.Width = (int)(p2.ParentWindow.Width / 2 - 10);
            //        p1.Height = p1.ParentWindow.Height - 140;
            //        p2.Height = p1.Height;
            //        sc.Height = p2.Height - btnAddVar.Height - 35 - desclbl.Height;
            //        foreach (var item in varcontainer.Items)
            //        {
            //            item.Control.Width = sc.Width - 25;
            //        }
            //    }
            //};

            foreach (var item in mycase.variables.Values)
            {
                AddVariable(item, varcontainer, objlist);
            }

            s.CreateAndAddLabelRow(p1, "Expression");
            s.CreateAndAddDescriptionRow(p1, "Enter an expression to be minimized or maximized using IND and AUX variables as parameters, if the Objective Function is defined as 'Expression'.");
            s.CreateAndAddFullTextBoxRow(p1, mycase.expression, (arg3, arg2) => { mycase.expression = arg3.Text; });

            s.CreateAndAddLabelRow(p1, "Optimization Control");
            int solvm = ((int)mycase.solvm - 5);
            if (solvm < 0) solvm = 0;
            s.CreateAndAddDropDownRow(p1, "Method", new[] { "Simplex", "LBFGS", "Truncated Newton", "Simplex (Bounded)", "LBFGS (Bounded)", "Truncated Newton (Bounded)" }.ToList(), solvm, (arg3, arg2) => { mycase.solvm = (DWSIM.SharedClasses.Flowsheet.Optimization.OptimizationCase.SolvingMethod)(arg3.SelectedIndex + 5); });
            s.CreateAndAddTextBoxRow(p1, "R", "Numerical derivative step", mycase.epsilon, (arg3, arg2) => { mycase.epsilon = arg3.Text.ToDoubleFromCurrent(); });
            s.CreateAndAddTextBoxRow(p1, "R", "Barrier multiplier", mycase.barriermultiplier, (arg3, arg2) => { mycase.barriermultiplier = arg3.Text.ToDoubleFromCurrent(); });

            s.CreateAndAddEmptySpace(this);

            s.CreateAndAddButtonRow(this, "Run Optimization", null, (arg3, arg2) =>
            {
                StartTask();
            });

            s.CreateAndAddEmptySpace(this);

            s.CreateAndAddEmptySpace(this);

        }

        void AddVariable(DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariable newiv, StackLayout ivcontainer, List<string> objs)
        {

            Flowsheet fsheet = (Flowsheet)flowsheet;

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            List<string> proplist = new List<string>();
            List<string> proplist2 = new List<string>();

            proplist.Add("");
            proplist2.Add("");

            var container = new DynamicLayout { Padding = new Padding(5), Width = 300 };
            //container.BackgroundColor = Eto.Drawing.Colors.White;
            container.Tag = new Random().Next() + mycase.variables.Count + 1;

            var slcontainer = new StackLayoutItem(container);

            s.CreateAndAddLabelRow(container, "Optimization Variable #" + (ivcontainer.Items.Count + 1).ToString());

            var btnremove = s.CreateAndAddButtonRow(container, "Remove Variable", null, (arg1a, arg2a) =>
            {
                mycase.variables.Remove(newiv.id);
                ivcontainer.Items.Remove(slcontainer);
            });

            s.CreateAndAddLabelRow(container, "Definitions");

            s.CreateAndAddStringEditorRow(container, "Name", newiv.name, (arg3b, arg2b) =>
            {
                newiv.name = arg3b.Text;
            });

            s.CreateAndAddDropDownRow(container, "Type", new[] { "DEP", "IND", "AUX", "CON" }.ToList(), (int)newiv.type, (arg3, arg2) => { newiv.type = (DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType)arg3.SelectedIndex; });

            DropDown spinprop = null;

            int idx = 0;

            if (flowsheet.SimulationObjects.ContainsKey(newiv.objectID))
            {
                idx = objs.IndexOf(flowsheet.SimulationObjects[newiv.objectID].GraphicObject.Tag);
            }

            s.CreateAndAddLabelRow(container, "Flowsheet Object Property Link");

            s.CreateAndAddDropDownRow(container, "Object", objs, idx, (arg3c, arg2c) =>
            {
                if (arg3c.SelectedIndex > 0)
                {
                    var obj = flowsheet.GetFlowsheetSimulationObject(objs[arg3c.SelectedIndex]);
                    newiv.objectID = obj.Name;
                    if (newiv.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Independent)
                    {
                        proplist = obj.GetProperties(PropertyType.WR).ToList();
                    }
                    else
                    {
                        proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    }
                    proplist.Insert(0, "");
                    proplist2 = proplist.Select(x => flowsheet.GetTranslatedString(x)).ToList();

                    spinprop.Items.Clear();
                    spinprop.Items.AddRange(proplist2.Select((x) => new ListItem { Key = x, Text = x }));
                }
            });

            int idx2 = 0;

            if (flowsheet.SimulationObjects.ContainsKey(newiv.objectID))
            {
                var obj = flowsheet.SimulationObjects[newiv.objectID];
                if (newiv.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Independent)
                {
                    proplist = obj.GetProperties(PropertyType.WR).ToList();
                }
                else
                {
                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                }
                proplist.Insert(0, "");
                proplist2 = proplist.Select(x => flowsheet.GetTranslatedString(x)).ToList();
                idx2 = proplist.IndexOf(newiv.propID);
            }

            TextBox tviv = null;
            spinprop = s.CreateAndAddDropDownRow(container, "Property", proplist2, idx2, (arg3c, arg2c) =>
            {
                if (spinprop.SelectedIndex > 0)
                {
                    newiv.propID = proplist[arg3c.SelectedIndex];
                    newiv.unit = fsheet.SimulationObjects[newiv.objectID].GetPropertyUnit(newiv.propID, su);
                    newiv.initialvalue = (double)fsheet.SimulationObjects[newiv.objectID].GetPropertyValue(newiv.propID, su);
                    tviv.Text = newiv.initialvalue.ToString(nf);
                }
            });

            s.CreateAndAddLabelRow(container, "Variable Bounds");
            s.CreateAndAddDescriptionRow(container, "Bounds will be taken into account only if you use a bounded solver.");
            s.CreateAndAddTextBoxRow(container, nf, "Lower Bound", newiv.lowerlimit.GetValueOrDefault(), (arg3, arg2) => { newiv.lowerlimit = arg3.Text.ToDoubleFromCurrent(); });
            s.CreateAndAddTextBoxRow(container, nf, "Upper Bound", newiv.upperlimit.GetValueOrDefault(), (arg3, arg2) => { newiv.upperlimit = arg3.Text.ToDoubleFromCurrent(); });

            s.CreateAndAddLabelRow(container, "Initial Value");
            s.CreateAndAddDescriptionRow(container, "Set the initial value of the variable (IND only, other variable types will have their values read from the flowsheet).");
            tviv = s.CreateAndAddTextBoxRow(container, nf, "Initial Value", newiv.initialvalue, (arg3, arg2) => { newiv.initialvalue = arg3.Text.ToDoubleFromCurrent(); });

            ivcontainer.Items.Add(slcontainer);

        }

        void StartTask()
        {
            flowsheet.optimizing = true;
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.SolverBreakOnException = true;
            st = new Task(() =>
            {

                Application.Instance.Invoke(() =>
                {
                    dialog = ProgressDialog.ShowWithAbort(flowsheet.FlowsheetControl, "Optimizing flowsheet, please wait...", "", false, "Abort",
                                                  (sender, e) =>
                                                  {
                                                      dialog.Close();
                                                      GlobalSettings.Settings.CalculatorStopRequested = true;
                                                      if (GlobalSettings.Settings.TaskCancellationTokenSource != null)
                                                      {
                                                          try
                                                          {
                                                              GlobalSettings.Settings.TaskCancellationTokenSource.Cancel();
                                                          }
                                                          catch (Exception) { }
                                                      }
                                                  });
                });

                RunOpt();

            }, GlobalSettings.Settings.TaskCancellationTokenSource.Token);

            st.ContinueWith((t) =>
            {
                flowsheet.optimizing = true;
                Application.Instance.Invoke(() => dialog.Close());
                if (t.Exception == null)
                {
                    flowsheet.ShowMessage("Optimization finished successfully.", IFlowsheet.MessageType.Information);
                }
                else
                {
                    if (!flowsheet.SupressMessages)
                    {
                        foreach (Exception ex in t.Exception.InnerExceptions)
                        {
                            Exception ex2 = ex.InnerException;
                            if (ex2 != null)
                            {
                                while (ex2.InnerException != null)
                                {
                                    ex2 = ex2.InnerException;
                                }
                            }
                            else { ex2 = ex; }
                            if (ex2 is System.OperationCanceledException)
                            {
                                flowsheet.ShowMessage("Optimization aborted by the user.", IFlowsheet.MessageType.Information);
                            }
                            else
                            {
                                flowsheet.ShowMessage("Error running optimization: " + ex2.Message, IFlowsheet.MessageType.GeneralError);
                            }
                        }
                    }
                    GlobalSettings.Settings.CalculatorStopRequested = false;
                    GlobalSettings.Settings.CalculatorBusy = false;
                }
            });

            st.Start();

        }

        void RunOpt()
        {

            var indvars = mycase.variables.Values.Where(x => x.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Independent).ToList();

            List<DotNumerics.Optimization.OptVariable> unboundvars = new List<DotNumerics.Optimization.OptVariable>();
            List<DotNumerics.Optimization.OptBoundVariable> boundvars = new List<DotNumerics.Optimization.OptBoundVariable>();

            foreach (var item in indvars)
            {
                unboundvars.Add(new DotNumerics.Optimization.OptVariable(item.initialvalue));
                boundvars.Add(new DotNumerics.Optimization.OptBoundVariable(item.initialvalue, item.lowerlimit.GetValueOrDefault(), item.upperlimit.GetValueOrDefault()));
            }

            switch (mycase.solvm)
            {
                case OptimizationCase.SolvingMethod.DN_NELDERMEAD_SIMPLEX:
                case OptimizationCase.SolvingMethod.AL_BRENT:
                case OptimizationCase.SolvingMethod.IPOPT:
                    var solver = new DotNumerics.Optimization.Simplex();
                    solver.Tolerance = mycase.tolerance;
                    solver.MaxFunEvaluations = mycase.maxits;
                    solver.ComputeMin(FunctionValue, unboundvars.ToArray());
                    break;
                case OptimizationCase.SolvingMethod.DN_LBFGS:
                case OptimizationCase.SolvingMethod.AL_LBFGS:
                    var solver2 = new DotNumerics.Optimization.L_BFGS_B();
                    solver2.Tolerance = mycase.tolerance;
                    solver2.MaxFunEvaluations = mycase.maxits;
                    solver2.ComputeMin(FunctionValue, FunctionGradient, unboundvars.ToArray());
                    break;
                case OptimizationCase.SolvingMethod.DN_TRUNCATED_NEWTON:
                    var solver3 = new DotNumerics.Optimization.TruncatedNewton();
                    solver3.Tolerance = mycase.tolerance;
                    solver3.MaxFunEvaluations = mycase.maxits;
                    solver3.ComputeMin(FunctionValue, FunctionGradient, unboundvars.ToArray());
                    break;
                case OptimizationCase.SolvingMethod.DN_NELDERMEAD_SIMPLEX_B:
                case OptimizationCase.SolvingMethod.AL_BRENT_B:
                    var solver4 = new DotNumerics.Optimization.Simplex();
                    solver4.Tolerance = mycase.tolerance;
                    solver4.MaxFunEvaluations = mycase.maxits;
                    solver4.ComputeMin(FunctionValue, boundvars.ToArray());
                    break;
                case OptimizationCase.SolvingMethod.DN_LBFGS_B:
                case OptimizationCase.SolvingMethod.AL_LBFGS_B:
                    var solver5 = new DotNumerics.Optimization.L_BFGS_B();
                    solver5.Tolerance = mycase.tolerance;
                    solver5.MaxFunEvaluations = mycase.maxits;
                    solver5.ComputeMin(FunctionValue, FunctionGradient, boundvars.ToArray());
                    break;
                case OptimizationCase.SolvingMethod.DN_TRUNCATED_NEWTON_B:
                    var solver6 = new DotNumerics.Optimization.TruncatedNewton();
                    solver6.Tolerance = mycase.tolerance;
                    solver6.MaxFunEvaluations = mycase.maxits;
                    solver6.ComputeMin(FunctionValue, FunctionGradient, unboundvars.ToArray());
                    break;
            }
        }

        double FunctionValue(double[] x)
        {

            var indvars = mycase.variables.Values.Where(x2 => x2.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Independent).ToList();
            var depvars = mycase.variables.Values.Where(x2 => x2.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Dependent).ToList();
            var auxvars = mycase.variables.Values.Where(x2 => x2.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Auxiliary).ToList();

            int i = 0;
            foreach (var item in indvars)
            {
                var value = cv.ConvertToSI(item.unit, x[i]);
                flowsheet.SimulationObjects[item.objectID].SetPropertyValue(item.propID, value);
                i += 1;
            }

            flowsheet.RequestCalculation();

            UpdateVariableValues();

            double objval = 0.0d;

            if (mycase.objfunctype == OPTObjectiveFunctionType.Expression)
            {
                mycase.econtext = new ExpressionContext();
                mycase.econtext.Imports.AddType(typeof(System.Math));

                foreach (var item in indvars)
                {
                    mycase.econtext.Variables.Add(item.name, item.currentvalue);
                }
                foreach (var item in depvars)
                {
                    mycase.econtext.Variables.Add(item.name, item.currentvalue);
                }
                foreach (var item in auxvars)
                {
                    mycase.econtext.Variables.Add(item.name, item.currentvalue);
                }

                mycase.exbase = mycase.econtext.CompileGeneric<Double>(mycase.expression);
                objval = mycase.exbase.Evaluate() + ReturnPenaltyValue();
            }
            else
            {
                objval = depvars[0].currentvalue + ReturnPenaltyValue();
            }

            if (!ongradient)
            {
                flowsheet.ShowMessage("Objective Function Value: " + objval.ToString("G8"), IFlowsheet.MessageType.Information);
            }

            if (mycase.type == OPTType.Maximization) { objval = -objval; }

            return objval;

        }

        double[] FunctionGradient(double[] x)
        {
            ongradient = true;
            double f1, f2;
            double[] g = new double[x.Length - 1], x2 = new double[x.Length - 1];
            f1 = FunctionValue(x);
            int i = 0;
            for (i = 0; i < x.Length; i++)
            {
                x2 = (double[])x.Clone();
                if (Math.Abs(x[i]) < double.Epsilon) { x2[i] = mycase.epsilon; } else { x2[i] = x[i] * (1 + mycase.epsilon); }
                f2 = FunctionValue(x2);
                g[i] = (f2 - f1) / (x2[i] - x[i]);
            }
            ongradient = false;
            return g;
        }

        double ReturnPenaltyValue()
        {

            var penm = mycase.barriermultiplier;
            var convars = mycase.variables.Values.Where(x2 => x2.type == DWSIM.SharedClasses.Flowsheet.Optimization.OPTVariableType.Constraint).ToList();

            double penval = 0.0d;
            foreach (var item in convars)
            {
                var delta1 = item.currentvalue - item.lowerlimit.GetValueOrDefault();
                var delta2 = item.currentvalue - item.upperlimit.GetValueOrDefault();
                if (delta1 < 0.0d) { penval += -delta1 * 1E6; }
                else if (delta2 > 1.0d) { penval += delta2 * 1E6; }
                else { penval += 1 / delta1 + 1 / delta2; }
            }

            penval *= penm;
            if (double.IsNaN(penval)) { penval = 0.0d; }

            return penval;

        }

        void UpdateVariableValues()
        {

            foreach (var item in mycase.variables.Values)
            {
                if (item.objectID != "")
                {
                    item.currentvalue = cv.ConvertFromSI(item.unit, (double)flowsheet.SimulationObjects[item.objectID].GetPropertyValue(item.propID));
                }
            }

        }
    }
}
