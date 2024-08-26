using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using Eto.Drawing;

using StringResources = DWSIM.UI.Desktop.Shared.StringArrays;
using System.Diagnostics;
using System.IO;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Interfaces.Enums;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DotNumerics.Optimization.TN;
using Newtonsoft.Json.Linq;

namespace DWSIM.UI.Desktop.Editors.LogicalBlocks
{
    public static class RecycleEditor
    {

        public static void Populate(ISimulationObject obj, DynamicLayout container)
        {

            var su = obj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = obj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var recycle = (Recycle)obj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", obj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", obj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                obj.GraphicObject.Tag = arg3.Text;
            });

            container.CreateAndAddLabelRow("Convergence Control");
            container.CreateAndAddCheckBoxRow("Converge using global solver",
                recycle.AccelerationMethod == Interfaces.Enums.AccelMethod.GlobalBroyden,
                (sender, e) => { if (sender.Checked.GetValueOrDefault()) recycle.AccelerationMethod = Interfaces.Enums.AccelMethod.GlobalBroyden; else recycle.AccelerationMethod = Interfaces.Enums.AccelMethod.None; });
            container.CreateAndAddCheckBoxRow("Legacy Mode", recycle.LegacyMode,
                (sender, e) => recycle.LegacyMode = sender.Checked.GetValueOrDefault());
            container.CreateAndAddTextBoxRow("N0", "Maximum Iterations", recycle.MaximumIterations,
                (sender, e) =>
                {
                    if (sender.Text.IsValidDouble()) recycle.MaximumIterations = int.Parse(sender.Text);
                });
            var nu = container.CreateAndAddNumericEditorRow("Smoothing Factor", recycle.SmoothingFactor, 0.1, 1.0, 1,
                (sender, e) =>
                {
                    recycle.SmoothingFactor= sender.Value;
                }
                );
            nu.Increment=0.1;
            container.CreateAndAddLabelRow("Convergence Tolerances");
            container.CreateAndAddTextBoxRow(nf, "Mass Flow", cv.ConvertFromSI(su.massflow, recycle.ConvergenceParameters.VazaoMassica),
                (sender, e) =>
                {
                    if (sender.Text.IsValidDouble()) recycle.ConvergenceParameters.VazaoMassica = cv.ConvertToSI(su.massflow, sender.Text.ToDoubleFromCurrent());
                });
            container.CreateAndAddTextBoxRow(nf, "Temperature", cv.ConvertFromSI(su.deltaT, recycle.ConvergenceParameters.Temperatura),
                (sender, e) =>
                {
                    if (sender.Text.IsValidDouble()) recycle.ConvergenceParameters.Temperatura = cv.ConvertToSI(su.deltaT, sender.Text.ToDoubleFromCurrent());
                });
            container.CreateAndAddTextBoxRow(nf, "Pressure", cv.ConvertFromSI(su.deltaP, recycle.ConvergenceParameters.Pressao),
               (sender, e) =>
               {
                   if (sender.Text.IsValidDouble()) recycle.ConvergenceParameters.Pressao = cv.ConvertToSI(su.deltaP, sender.Text.ToDoubleFromCurrent());
               });

        }

    }

    public static class EnergyRecycleEditor
    {

        public static void Populate(ISimulationObject obj, DynamicLayout container)
        {

            var su = obj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = obj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var recycle = (EnergyRecycle)obj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", obj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", obj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                obj.GraphicObject.Tag = arg3.Text;
            });
            container.CreateAndAddTextBoxRow("N0", "Maximum Iterations", recycle.MaximumIterations,
                (sender, e) =>
                {
                    if (sender.Text.IsValidDouble()) recycle.MaximumIterations = int.Parse(sender.Text);
                });
            container.CreateAndAddLabelRow("Convergence Tolerances");
            container.CreateAndAddTextBoxRow(nf, "Energy Flow", cv.ConvertFromSI(su.heatflow, recycle.ConvergenceParameters.Energy),
                (sender, e) =>
                {
                    if (sender.Text.IsValidDouble()) recycle.ConvergenceParameters.Energy = cv.ConvertToSI(su.heatflow, sender.Text.ToDoubleFromCurrent());
                });

        }

    }

    public static class AdjustEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var adjust = (Adjust)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = adjust.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).OrderBy((x3) => x3).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Manipulated Object");

            DropDown spin1 = null, spin2 = null;
            Label lblMv = null, lblCv = null, lblRv = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Manipulated Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ManipulatedObjectData.ID))
                    {
                        var prevobj = adjust.GetFlowsheet().SimulationObjects[adjust.ManipulatedObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = adjust.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    adjust.ManipulatedObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = adjust.Name;
                    obj.AdjustVarType = AdjustVarType.Manipulated;

                    adjust.ManipulatedObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)obj;
                    ((AdjustGraphic)adjust.GraphicObject).ConnectedToMv = (GraphicObject)obj.GraphicObject;

                    proplist = obj.GetProperties(PropertyType.WR).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = adjust.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (adjust.ManipulatedObjectData.PropertyName != "" && proplist.Contains(adjust.ManipulatedObjectData.PropertyName))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(adjust.ManipulatedObjectData.PropertyName));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Manipulated Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    adjust.ManipulatedObjectData.PropertyName = proplist[sender.SelectedIndex];
                    var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ManipulatedObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist[sender.SelectedIndex], su);
                    lblMv.Text = "Current Value: " + currval;
                }
            });

            lblMv = s.CreateAndAddLabelRow2(container, "");

            List<string> proplist2 = new List<string>();

            s.CreateAndAddLabelRow(container, "Controlled Object");

            DropDown spin3 = null, spin4 = null;

            spin3 = s.CreateAndAddDropDownRow(container, "Controlled Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
                    {
                        var prevobj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = adjust.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    adjust.ControlledObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = adjust.Name;
                    obj.AdjustVarType = AdjustVarType.Controlled;

                    adjust.ControlledObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                    ((AdjustGraphic)adjust.GraphicObject).ConnectedToCv = (GraphicObject)adjust.ControlledObject.GraphicObject;

                    proplist2 = adjust.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]).GetProperties(PropertyType.ALL).ToList();
                    proplist2.Insert(0, "");

                    spin4.Items.Clear();
                    spin4.Items.AddRange(proplist2.Select(x => new ListItem() { Text = adjust.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (adjust.ControlledObjectData.PropertyName != "" && proplist2.Contains(adjust.ControlledObjectData.PropertyName))
                    {
                        spin4.SelectedIndex = (proplist2.IndexOf(adjust.ControlledObjectData.PropertyName));
                    }
                }
                else
                {
                    spin4.Items.Clear();
                }
            });

            spin4 = s.CreateAndAddDropDownRow(container, "Controlled Property", proplist2, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    adjust.ControlledObjectData.PropertyName = proplist2[sender.SelectedIndex];
                    var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist2[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist2[sender.SelectedIndex], su);
                    lblCv.Text = "Current Value: " + currval;
                }
            });

            lblCv = s.CreateAndAddLabelRow2(container, "");

            List<string> proplist3 = new List<string>();

            s.CreateAndAddLabelRow(container, "Referenced Object");

            s.CreateAndAddCheckBoxRow(container, "Use Referenced Object", adjust.Referenced, (sender, e) => { adjust.Referenced = sender.Checked.GetValueOrDefault(); });

            s.CreateAndAddDescriptionRow(container, "When a Reference Object is used, the Adjust/Controller block will change the Manipulated Variable so the Controlled Variable matches the Referenced value plus/minus the defined Set-Point/Offset.");

            DropDown spin5 = null, spin6 = null;

            spin5 = s.CreateAndAddDropDownRow(container, "Referenced Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ReferencedObjectData.ID))
                    {
                        var prevobj = adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = adjust.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    adjust.ReferencedObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = adjust.Name;
                    obj.AdjustVarType = AdjustVarType.Reference;

                    adjust.ReferenceObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                    ((AdjustGraphic)adjust.GraphicObject).ConnectedToRv = (GraphicObject)adjust.ReferenceObject.GraphicObject;

                    proplist3 = adjust.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]).GetProperties(PropertyType.ALL).ToList();
                    proplist3.Insert(0, "");

                    spin6.Items.Clear();
                    spin6.Items.AddRange(proplist3.Select(x => new ListItem() { Text = adjust.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (adjust.ReferencedObjectData.PropertyName != "" && proplist3.Contains(adjust.ReferencedObjectData.PropertyName))
                    {
                        spin6.SelectedIndex = (proplist3.IndexOf(adjust.ReferencedObjectData.PropertyName));
                    }
                }
                else
                {
                    spin6.Items.Clear();
                }
            });

            spin6 = s.CreateAndAddDropDownRow(container, "Referenced Property", proplist3, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    adjust.ReferencedObjectData.PropertyName = proplist3[sender.SelectedIndex];
                    var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist3[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist3[sender.SelectedIndex], su);
                    lblRv.Text = "Current Value: " + currval;
                }
            });

            lblRv = s.CreateAndAddLabelRow2(container, "");

            s.CreateAndAddLabelRow(container, "Controller Parameters");

            var adjval = 0.0;

            if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
            {
                var obj0 = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                if (obj0 != null)
                {
                    if (adjust.Referenced)
                    {
                        obj0 = adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                        var punit = obj0.GetPropertyUnit(adjust.ReferencedObjectData.PropertyName, su);
                        if (su.GetUnitType(punit) == UnitOfMeasure.temperature)
                        {
                            adjval = adjust.AdjustValue.ConvertFromSI(punit + ".");
                        }
                        else
                        {
                            adjval = adjust.AdjustValue.ConvertFromSI(punit);
                        }
                    }
                    else
                    {
                        adjval = adjust.AdjustValue.ConvertFromSI(obj0.GetPropertyUnit(adjust.ControlledObjectData.PropertyName, su));
                    }
                }
            }
            else
            {
                adjval = adjust.AdjustValue;
            }

            var txtvalue = s.CreateAndAddTextBoxRow(container, nf, "Set-Point/Offset", adjval, (sender, e) =>
             {
                 if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
                 {
                     var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                     if (s.IsValidDouble(sender.Text))
                     {
                         if (adjust.Referenced)
                         {
                             obj = adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                             var punit = obj.GetPropertyUnit(adjust.ReferencedObjectData.PropertyName, su);
                             if (su.GetUnitType(punit) == UnitOfMeasure.temperature)
                             {
                                 adjust.AdjustValue = cv.ConvertToSI(punit + ".", sender.Text.ToDoubleFromCurrent());
                             }
                             else
                             {
                                 adjust.AdjustValue = cv.ConvertToSI(punit, sender.Text.ToDoubleFromCurrent());
                             }
                         }
                         else
                         {
                             adjust.AdjustValue = cv.ConvertToSI(obj.GetPropertyUnit(adjust.ControlledObjectData.PropertyName, su), sender.Text.ToDoubleFromCurrent());
                         }
                     }
                 }
             }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)adjust.GetFlowsheet()).HighLevelSolve.Invoke(); });

            if (adjust.ManipulatedObjectData.ID != "" && adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ManipulatedObjectData.ID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(adjust.GetFlowsheet().SimulationObjects[adjust.ManipulatedObjectData.ID].GraphicObject.Tag));
            }

            if (adjust.ControlledObjectData.ID != "" && adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
            {
                spin3.SelectedIndex = (objlist.IndexOf(adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID].GraphicObject.Tag));
                var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                txtvalue.Text = cv.ConvertFromSI(obj.GetPropertyUnit(adjust.ControlledObjectData.PropertyName, su), adjust.AdjustValue).ToString(nf);
            }

            if (adjust.ReferencedObjectData.ID != "" && adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ReferencedObjectData.ID))
            {
                spin5.SelectedIndex = (objlist.IndexOf(adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID].GraphicObject.Tag));
                var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ReferencedObjectData.ID];
                txtvalue.Text = cv.ConvertFromSI(obj.GetPropertyUnit(adjust.ReferencedObjectData.PropertyName, su), adjust.AdjustValue).ToString(nf);
            }

            s.CreateAndAddCheckBoxRow(container, "Run with the Simultaneous Adjust Solver", adjust.SimultaneousAdjust, (sender, e) => adjust.SimultaneousAdjust = sender.Checked.GetValueOrDefault());

            s.CreateAndAddButtonRow(container, "Open Control Panel", null, (btn, e) =>
            {
                if (adjust.ControlledObject == null) return;
                if (adjust.ManipulatedObject == null) return;
                var fcp = s.GetDefaultEditorForm("Control Panel: " + adjust.GraphicObject.Tag, 600, 600, new ControllerPanel(adjust));
                fcp.Show();
                fcp.Center();
            });

        }

    }

    public static class SpecEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (Spec)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).OrderBy((x3) => x3).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Source Object");

            DropDown spin1 = null, spin2 = null;
            Label txtval1 = null, txtval2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Source Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SourceObjectData.ID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SourceObjectData.ID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SourceObjectData.ID = obj.Name;

                    obj.IsSpecAttached = true;
                    obj.AttachedSpecId = spec.Name;
                    obj.SpecVarType = SpecVarType.Source;

                    spec.SourceObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)obj;
                    ((SpecGraphic)spec.GraphicObject).ConnectedToSv = (GraphicObject)obj.GraphicObject;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SourceObjectData.PropertyName != "" && proplist.Contains(spec.SourceObjectData.PropertyName))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SourceObjectData.PropertyName));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Source Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SourceObjectData.PropertyName = proplist[sender.SelectedIndex];
                    var obj = spec.GetFlowsheet().SimulationObjects[spec.SourceObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist[sender.SelectedIndex], su);
                    txtval1.Text = "Current Value: " + currval;
                }
            });

            txtval1 = container.CreateAndAddLabelRow2("Current Value: N/A");

            if (spec.SourceObjectData.ID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SourceObjectData.ID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SourceObjectData.ID].GraphicObject.Tag));
            }

            List<string> proplist2 = new List<string>();

            s.CreateAndAddLabelRow(container, "Target Object");

            DropDown spin3 = null, spin4 = null, spin5;

            spin3 = s.CreateAndAddDropDownRow(container, "Target Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.TargetObjectData.ID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.TargetObjectData.ID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.TargetObjectData.ID = obj.Name;

                    obj.IsSpecAttached = true;
                    obj.AttachedSpecId = spec.Name;
                    obj.SpecVarType = SpecVarType.Target;

                    spec.TargetObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)spec.GetFlowsheet().SimulationObjects[spec.TargetObjectData.ID];
                    ((SpecGraphic)spec.GraphicObject).ConnectedToTv = (GraphicObject)spec.TargetObject.GraphicObject;

                    proplist2 = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]).GetProperties(PropertyType.WR).ToList();
                    proplist2.Insert(0, "");

                    spin4.Items.Clear();
                    spin4.Items.AddRange(proplist2.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.TargetObjectData.PropertyName != "" && proplist2.Contains(spec.TargetObjectData.PropertyName))
                    {
                        spin4.SelectedIndex = (proplist2.IndexOf(spec.TargetObjectData.PropertyName));
                    }
                }
                else
                {
                    spin4.Items.Clear();
                }
            });

            spin4 = s.CreateAndAddDropDownRow(container, "Target Property", proplist2, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.TargetObjectData.PropertyName = proplist2[sender.SelectedIndex];
                    var obj = spec.GetFlowsheet().SimulationObjects[spec.TargetObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist2[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist2[sender.SelectedIndex], su);
                    txtval2.Text = "Current Value: " + currval;
                }
            });

            txtval2 = container.CreateAndAddLabelRow2("Current Value: N/A");

            if (spec.TargetObjectData.ID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.TargetObjectData.ID))
            {
                spin3.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.TargetObjectData.ID].GraphicObject.Tag));
            }

            container.CreateAndAddLabelRow("Behavior");

            container.CreateAndAddDropDownRow("Calculation Mode", spec.SpecCalculationMode.GetEnumNames(), (int)spec.SpecCalculationMode,
                (sender, e) =>
                {
                    spec.SpecCalculationMode= sender.SelectedIndex.ToEnum<Interfaces.Enums.SpecCalcMode2>();
                });

            spin5 = container.CreateAndAddDropDownRow("Reference Object", objlist, 0,
                (sender, e) =>
                {
                    if (sender.SelectedIndex > 0)
                        spec.ReferenceObjectID= spec.GetFlowsheet().GetObject(objlist[sender.SelectedIndex]).Name;
                });

            if (spec.ReferenceObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.ReferenceObjectID))
            {
                spin5.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.ReferenceObjectID].GraphicObject.Tag));
            }

            container.CreateAndAddLabelRow("Expression");

            container.CreateAndAddStringEditorRow2("Y = f(X) = ", "enter a math expression using X as the only variable", spec.Expression, (sender, e) => spec.Expression = sender.Text);

        }

    }

    public static class PIDControllerEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var pid = (PIDController)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = pid.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).OrderBy((x3) => x3).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Manipulated Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Manipulated Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ManipulatedObjectData.ID))
                    {
                        var prevobj = pid.GetFlowsheet().SimulationObjects[pid.ManipulatedObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    pid.ManipulatedObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = pid.Name;
                    obj.AdjustVarType = AdjustVarType.Manipulated;

                    pid.ManipulatedObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)obj;
                    ((PIDControllerGraphic)pid.GraphicObject).ConnectedToMv = (GraphicObject)obj.GraphicObject;

                    proplist = obj.GetProperties(PropertyType.WR).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = pid.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (pid.ManipulatedObjectData.PropertyName != "" && proplist.Contains(pid.ManipulatedObjectData.PropertyName))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(pid.ManipulatedObjectData.PropertyName));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Manipulated Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    pid.ManipulatedObjectData.PropertyName = proplist[sender.SelectedIndex];
                }
            });

            s.CreateAndAddStringEditorRow(container, "Manipulated Property Units",
                pid.ManipulatedObjectData.Units, (tb, e) =>
                {
                    pid.ManipulatedObjectData.Units = tb.Text;
                });

            List<string> proplist2 = new List<string>();

            s.CreateAndAddLabelRow(container, "Controlled Object");

            DropDown spin3 = null, spin4 = null;

            spin3 = s.CreateAndAddDropDownRow(container, "Controlled Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ControlledObjectData.ID))
                    {
                        var prevobj = pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    pid.ControlledObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = pid.Name;
                    obj.AdjustVarType = AdjustVarType.Controlled;

                    pid.ControlledObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                    ((PIDControllerGraphic)pid.GraphicObject).ConnectedToCv = (GraphicObject)pid.ControlledObject.GraphicObject;

                    proplist2 = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]).GetProperties(PropertyType.ALL).ToList();
                    proplist2.Insert(0, "");

                    spin4.Items.Clear();
                    spin4.Items.AddRange(proplist2.Select(x => new ListItem() { Text = pid.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (pid.ControlledObjectData.PropertyName != "" && proplist2.Contains(pid.ControlledObjectData.PropertyName))
                    {
                        spin4.SelectedIndex = (proplist2.IndexOf(pid.ControlledObjectData.PropertyName));
                    }
                }
                else
                {
                    spin4.Items.Clear();
                }
            });

            spin4 = s.CreateAndAddDropDownRow(container, "Controlled Property", proplist2, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    pid.ControlledObjectData.PropertyName = proplist2[sender.SelectedIndex];
                    var obj = pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                }
            });

            s.CreateAndAddStringEditorRow(container, "Controlled Property Units",
                pid.ControlledObjectData.Units, (tb, e) =>
                {
                    pid.ControlledObjectData.Units = tb.Text;
                });

            s.CreateAndAddLabelRow(container, "Controller Parameters");

            s.CreateAndAddCheckBoxRow(container, "Controller Active", pid.Active, (chk, e) =>
            {
                pid.Active = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Reverse Acting", pid.ReverseActing, (chk, e) =>
            {
                pid.ReverseActing = chk.Checked.GetValueOrDefault();
            });

            var txtvalue = s.CreateAndAddTextBoxRow(container, nf, "Set-Point", pid.AdjustValue, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.AdjustValue = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Offset (Bias)", pid.Offset, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.Offset = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Minimum Output Value", pid.OutputMin, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.OutputMin = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Maximum Output Value", pid.OutputMax, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.OutputMax = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Kp", pid.Kp, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.Kp = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Ki", pid.Ki, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.Ki = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Kd", pid.Kd, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.Kd = sender.Text.ToDoubleFromCurrent();
                }
            });

            s.CreateAndAddTextBoxRow(container, nf, "Wind-Up Guard", pid.WindupGuard, (sender, e) =>
            {
                if (s.IsValidDouble(sender.Text))
                {
                    pid.WindupGuard = sender.Text.ToDoubleFromCurrent();
                }
            });

            if (pid.ManipulatedObjectData.ID != "" && pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ManipulatedObjectData.ID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(pid.GetFlowsheet().SimulationObjects[pid.ManipulatedObjectData.ID].GraphicObject.Tag));
            }

            if (pid.ControlledObjectData.ID != "" && pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ControlledObjectData.ID))
            {
                spin3.SelectedIndex = (objlist.IndexOf(pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID].GraphicObject.Tag));
            }

        }

    }

    public static class PythonControllerEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var dc = new DocumentControl { DisplayArrows = true };
            var dp1 = new DocumentPage { Closable = false, Text = "General" };
            var dp2 = new DocumentPage { Closable = false, Text = "Python Script" };

            dc.Pages.Add(dp1);
            dc.Pages.Add(dp2);

            var lay1 = DWSIM.UI.Shared.Common.GetDefaultContainer();

            container.CreateAndAddControlRow(dc);

            var pid = (PythonController)simobj;

            s.CreateAndAddLabelRow(lay1, "Object Details");

            s.CreateAndAddTwoLabelsRow(lay1, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(lay1, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = pid.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).OrderBy((x3) => x3).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(lay1, "Manipulated Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(lay1, "Manipulated Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ManipulatedObjectData.ID))
                    {
                        var prevobj = pid.GetFlowsheet().SimulationObjects[pid.ManipulatedObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    pid.ManipulatedObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = pid.Name;
                    obj.AdjustVarType = AdjustVarType.Manipulated;

                    pid.ManipulatedObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)obj;
                    ((PythonControllerGraphic)pid.GraphicObject).ConnectedToMv = (GraphicObject)obj.GraphicObject;

                    proplist = obj.GetProperties(PropertyType.WR).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = pid.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (pid.ManipulatedObjectData.PropertyName != "" && proplist.Contains(pid.ManipulatedObjectData.PropertyName))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(pid.ManipulatedObjectData.PropertyName));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(lay1, "Manipulated Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    pid.ManipulatedObjectData.PropertyName = proplist[sender.SelectedIndex];
                }
            });

            s.CreateAndAddStringEditorRow(lay1, "Manipulated Property Units",
                pid.ManipulatedObjectData.Units, (tb, e) =>
                {
                    pid.ManipulatedObjectData.Units = tb.Text;
                });

            List<string> proplist2 = new List<string>();

            s.CreateAndAddLabelRow(lay1, "Controlled Object");

            DropDown spin3 = null, spin4 = null;

            spin3 = s.CreateAndAddDropDownRow(lay1, "Controlled Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ControlledObjectData.ID))
                    {
                        var prevobj = pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                        prevobj.IsAdjustAttached = false;
                        prevobj.AttachedAdjustId = "";
                        prevobj.AdjustVarType = AdjustVarType.None;
                    }

                    var obj = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    pid.ControlledObjectData.ID = obj.Name;

                    obj.IsAdjustAttached = true;
                    obj.AttachedAdjustId = pid.Name;
                    obj.AdjustVarType = AdjustVarType.Controlled;

                    pid.ControlledObject = (DWSIM.SharedClasses.UnitOperations.BaseClass)pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                    ((PythonControllerGraphic)pid.GraphicObject).ConnectedToCv = (GraphicObject)pid.ControlledObject.GraphicObject;

                    proplist2 = pid.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]).GetProperties(PropertyType.ALL).ToList();
                    proplist2.Insert(0, "");

                    spin4.Items.Clear();
                    spin4.Items.AddRange(proplist2.Select(x => new ListItem() { Text = pid.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (pid.ControlledObjectData.PropertyName != "" && proplist2.Contains(pid.ControlledObjectData.PropertyName))
                    {
                        spin4.SelectedIndex = (proplist2.IndexOf(pid.ControlledObjectData.PropertyName));
                    }
                }
                else
                {
                    spin4.Items.Clear();
                }
            });

            spin4 = s.CreateAndAddDropDownRow(lay1, "Controlled Property", proplist2, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    pid.ControlledObjectData.PropertyName = proplist2[sender.SelectedIndex];
                    var obj = pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID];
                }
            });

            s.CreateAndAddStringEditorRow(lay1, "Controlled Property Units",
                pid.ControlledObjectData.Units, (tb, e) =>
                {
                    pid.ControlledObjectData.Units = tb.Text;
                });

            s.CreateAndAddLabelRow(lay1, "Controller Parameters");

            s.CreateAndAddCheckBoxRow(lay1, "Controller Active", pid.Active, (chk, e) =>
            {
                pid.Active = chk.Checked.GetValueOrDefault();
            });

            if (pid.ManipulatedObjectData.ID != "" && pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ManipulatedObjectData.ID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(pid.GetFlowsheet().SimulationObjects[pid.ManipulatedObjectData.ID].GraphicObject.Tag));
            }

            if (pid.ControlledObjectData.ID != "" && pid.GetFlowsheet().SimulationObjects.ContainsKey(pid.ControlledObjectData.ID))
            {
                spin3.SelectedIndex = (objlist.IndexOf(pid.GetFlowsheet().SimulationObjects[pid.ControlledObjectData.ID].GraphicObject.Tag));
            }

            dp1.Content = lay1;

            var lay2 = new TableLayout() { Spacing = new Size(10, 10), Padding = new Padding(5) };
            var btn1 = new Button { Text = "Update" };
            var sed = new Eto.Forms.Controls.Scintilla.Shared.ScintillaControl();
            sed.ScriptText = pid.PythonScript;
            sed.Height = 500;
            btn1.Click += (s, e) => pid.PythonScript = sed.ScriptText;
            s.CreateAndAddDescriptionRow(lay2, "- Process Variable value is available on 'PV';\n" +
                "- Write the Set-Point value to 'SP' and the Controller Output to 'MV';\n"+
                "- Use the 'Flowsheet' object to access other objects and their properties.");
            lay2.Rows.Add(sed);
            lay2.Rows.Add(btn1);

            dp2.Content = lay2;
        }

    }

    public static class LevelGaugeEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (LevelGauge)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Source Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Source Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SelectedObjectID = obj.Name;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SelectedProperty != "" && proplist.Contains(spec.SelectedProperty))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SelectedProperty));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Source Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SelectedProperty = proplist[sender.SelectedIndex];
                }
            });

            if (spec.SelectedObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID].GraphicObject.Tag));
            }

            s.CreateAndAddStringEditorRow(container, "Source Property Units",
               spec.SelectedPropertyUnits, (tb, e) =>
               {
                   spec.SelectedPropertyUnits = tb.Text;
               });

            s.CreateAndAddTextBoxRow(container, nf, "Minimum Value",
              spec.MinimumValue, (tb, e) =>
              {
                  spec.MinimumValue = tb.Text.ToDoubleFromCurrent();
              });

            s.CreateAndAddTextBoxRow(container, nf, "Maximum Value",
              spec.MaximumValue, (tb, e) =>
              {
                  spec.MaximumValue = tb.Text.ToDoubleFromCurrent();
              });

            s.CreateAndAddCheckBoxRow(container, "Display in Percentage", spec.DisplayInPercent, (chk, e) =>
            {
                spec.DisplayInPercent = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow(container, "Alarm Parameters");

            s.CreateAndAddCheckBoxRow(container, "Show Alarm Indicators", spec.ShowAlarms, (chk, e) =>
            {
                spec.ShowAlarms = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Very Low", spec.VeryLowAlarmEnabled, (chk, e) =>
            {
                spec.VeryLowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very Low Value",
            spec.VeryLowAlarmValue, (tb, e) =>
            {
                spec.VeryLowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Low", spec.LowAlarmEnabled, (chk, e) =>
            {
                spec.LowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Low Value",
            spec.LowAlarmValue, (tb, e) =>
            {
                spec.LowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "High", spec.HighAlarmEnabled, (chk, e) =>
            {
                spec.HighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "High Value",
            spec.HighAlarmValue, (tb, e) =>
            {
                spec.HighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Very High", spec.VeryHighAlarmEnabled, (chk, e) =>
            {
                spec.VeryHighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very High Value",
            spec.VeryHighAlarmValue, (tb, e) =>
            {
                spec.VeryHighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

        }

    }

    public static class DigitalGaugeEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (DigitalGauge)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Source Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Source Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SelectedObjectID = obj.Name;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SelectedProperty != "" && proplist.Contains(spec.SelectedProperty))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SelectedProperty));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Source Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SelectedProperty = proplist[sender.SelectedIndex];
                }
            });

            if (spec.SelectedObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID].GraphicObject.Tag));
            }

            s.CreateAndAddStringEditorRow(container, "Source Property Units",
               spec.SelectedPropertyUnits, (tb, e) =>
               {
                   spec.SelectedPropertyUnits = tb.Text;
               });

            s.CreateAndAddNumericEditorRow2(container, "Integer Digits",
                spec.IntegralDigits, 1, 10, 0, (ns, e) =>
                {
                    spec.IntegralDigits = (int)ns.Text.ToDoubleFromCurrent();
                });

            s.CreateAndAddNumericEditorRow2(container, "Decimal Digits",
                spec.DecimalDigits, 1, 10, 0, (ns, e) =>
                {
                    spec.DecimalDigits = (int)ns.Text.ToDoubleFromCurrent();
                });

            s.CreateAndAddCheckBoxRow(container, "Display in Percentage", spec.DisplayInPercent, (chk, e) =>
            {
                spec.DisplayInPercent = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow(container, "Alarm Parameters");

            s.CreateAndAddCheckBoxRow(container, "Show Alarm Indicators", spec.ShowAlarms, (chk, e) =>
            {
                spec.ShowAlarms = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Very Low", spec.VeryLowAlarmEnabled, (chk, e) =>
            {
                spec.VeryLowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very Low Value",
            spec.VeryLowAlarmValue, (tb, e) =>
            {
                spec.VeryLowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Low", spec.LowAlarmEnabled, (chk, e) =>
            {
                spec.LowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Low Value",
            spec.LowAlarmValue, (tb, e) =>
            {
                spec.LowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "High", spec.HighAlarmEnabled, (chk, e) =>
            {
                spec.HighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "High Value",
            spec.HighAlarmValue, (tb, e) =>
            {
                spec.HighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Very High", spec.VeryHighAlarmEnabled, (chk, e) =>
            {
                spec.VeryHighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very High Value",
            spec.VeryHighAlarmValue, (tb, e) =>
            {
                spec.VeryHighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

        }

    }

    public static class AnalogGaugeEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (AnalogGauge)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Source Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Source Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SelectedObjectID = obj.Name;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SelectedProperty != "" && proplist.Contains(spec.SelectedProperty))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SelectedProperty));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Source Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SelectedProperty = proplist[sender.SelectedIndex];
                }
            });

            if (spec.SelectedObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID].GraphicObject.Tag));
            }

            s.CreateAndAddStringEditorRow(container, "Source Property Units",
               spec.SelectedPropertyUnits, (tb, e) =>
               {
                   spec.SelectedPropertyUnits = tb.Text;
               });

            s.CreateAndAddTextBoxRow(container, nf, "Minimum Value",
              spec.MinimumValue, (tb, e) =>
              {
                  spec.MinimumValue = tb.Text.ToDoubleFromCurrent();
              });

            s.CreateAndAddTextBoxRow(container, nf, "Maximum Value",
              spec.MaximumValue, (tb, e) =>
              {
                  spec.MaximumValue = tb.Text.ToDoubleFromCurrent();
              });

            s.CreateAndAddCheckBoxRow(container, "Display in Percentage", spec.DisplayInPercent, (chk, e) =>
            {
                spec.DisplayInPercent = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddLabelRow(container, "Alarm Parameters");

            s.CreateAndAddCheckBoxRow(container, "Show Alarm Indicators", spec.ShowAlarms, (chk, e) =>
            {
                spec.ShowAlarms = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Very Low", spec.VeryLowAlarmEnabled, (chk, e) =>
            {
                spec.VeryLowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very Low Value",
            spec.VeryLowAlarmValue, (tb, e) =>
            {
                spec.VeryLowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Low", spec.LowAlarmEnabled, (chk, e) =>
            {
                spec.LowAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Low Value",
            spec.LowAlarmValue, (tb, e) =>
            {
                spec.LowAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "High", spec.HighAlarmEnabled, (chk, e) =>
            {
                spec.HighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "High Value",
            spec.HighAlarmValue, (tb, e) =>
            {
                spec.HighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

            s.CreateAndAddCheckBoxRow(container, "Very High", spec.VeryHighAlarmEnabled, (chk, e) =>
            {
                spec.VeryHighAlarmEnabled = chk.Checked.GetValueOrDefault();
            });

            s.CreateAndAddTextBoxRow(container, nf, "Very High Value",
            spec.VeryHighAlarmValue, (tb, e) =>
            {
                spec.VeryHighAlarmValue = tb.Text.ToDoubleFromCurrent();
            });

        }

    }

    public static class SwitchEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (DWSIM.UnitOperations.UnitOperations.Switch)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Selected Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Selected Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SelectedObjectID = obj.Name;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SelectedProperty != "" && proplist.Contains(spec.SelectedProperty))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SelectedProperty));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Selected Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SelectedProperty = proplist[sender.SelectedIndex];
                }
            });

            if (spec.SelectedObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID].GraphicObject.Tag));
            }

            s.CreateAndAddStringEditorRow(container, "Property Units",
               spec.SelectedPropertyUnits, (tb, e) =>
               {
                   spec.SelectedPropertyUnits = tb.Text;
               });

            s.CreateAndAddTextBoxRow(container, nf, "Value when Off",
              spec.OffValue, (tb, e) =>
              {
                  spec.OffValue = tb.Text.ToDoubleFromCurrent();
              });

            s.CreateAndAddTextBoxRow(container, nf, "Value when On",
              spec.OnValue, (tb, e) =>
              {
                  spec.OnValue = tb.Text.ToDoubleFromCurrent();
              });

        }

    }

    public static class InputEditor
    {

        public static void Populate(ISimulationObject simobj, DynamicLayout container)
        {

            var su = simobj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = simobj.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var spec = (DWSIM.UnitOperations.UnitOperations.Input)simobj;

            s.CreateAndAddLabelRow(container, "Object Details");

            s.CreateAndAddTwoLabelsRow(container, "Status", simobj.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", simobj.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                simobj.GraphicObject.Tag = arg3.Text;
            });

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Selected Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Selected Object", objlist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    if (spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
                    {
                        var prevobj = spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID];
                        prevobj.IsSpecAttached = false;
                        prevobj.AttachedSpecId = "";
                        prevobj.SpecVarType = SpecVarType.None;
                    }

                    var obj = spec.GetFlowsheet().GetFlowsheetSimulationObject(objlist[sender.SelectedIndex]);
                    spec.SelectedObjectID = obj.Name;

                    proplist = obj.GetProperties(PropertyType.ALL).ToList();
                    proplist.Insert(0, "");

                    spin2.Items.Clear();
                    spin2.Items.AddRange(proplist.Select(x => new ListItem() { Text = spec.GetFlowsheet().GetTranslatedString(x) }).ToList());

                    if (spec.SelectedProperty != "" && proplist.Contains(spec.SelectedProperty))
                    {
                        spin2.SelectedIndex = (proplist.IndexOf(spec.SelectedProperty));
                    }

                }
                else
                {
                    spin2.Items.Clear();
                }
            });

            spin2 = s.CreateAndAddDropDownRow(container, "Selected Property", proplist, 0, (sender, e) =>
            {
                if (sender.SelectedIndex > 0)
                {
                    spec.SelectedProperty = proplist[sender.SelectedIndex];
                }
            });

            if (spec.SelectedObjectID != "" && spec.GetFlowsheet().SimulationObjects.ContainsKey(spec.SelectedObjectID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(spec.GetFlowsheet().SimulationObjects[spec.SelectedObjectID].GraphicObject.Tag));
            }

            s.CreateAndAddStringEditorRow(container, "Property Units",
               spec.SelectedPropertyUnits, (tb, e) =>
               {
                   spec.SelectedPropertyUnits = tb.Text;
               });

        }

    }

}
