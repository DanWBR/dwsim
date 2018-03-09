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
                (sender, e) => { if (sender.Checked.GetValueOrDefault())  recycle.AccelerationMethod = Interfaces.Enums.AccelMethod.GlobalBroyden; else recycle.AccelerationMethod = Interfaces.Enums.AccelMethod.None; });
            container.CreateAndAddLabelRow("Convergence Tolerances");
            container.CreateAndAddTextBoxRow(nf, "Mass Flow", cv.ConvertFromSI(su.massflow, recycle.ConvergenceParameters.VazaoMassica),
                (sender, e) => {
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

            var objlist = adjust.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
            objlist.Insert(0, "");
            List<string> proplist = new List<string>();

            s.CreateAndAddLabelRow(container, "Manipulated Object");

            DropDown spin1 = null, spin2 = null;

            spin1 = s.CreateAndAddDropDownRow(container, "Manipulated Object", objlist, 0, (sender, e) => {
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
                    spin2.Items.AddRange(proplist.Select(x => new ListItem(){ Text = adjust.GetFlowsheet().GetTranslatedString(x)}).ToList());

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

            spin2 = s.CreateAndAddDropDownRow(container, "Manipulated Property", proplist, 0, (sender, e) => {
                if (sender.SelectedIndex > 0)
                {
                    adjust.ManipulatedObjectData.PropertyName = proplist[sender.SelectedIndex];
                }
            });

            if (adjust.ManipulatedObjectData.ID != "" && adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ManipulatedObjectData.ID))
            {
                spin1.SelectedIndex = (objlist.IndexOf(adjust.GetFlowsheet().SimulationObjects[adjust.ManipulatedObjectData.ID].GraphicObject.Tag));
            }

            List<string> proplist2 = new List<string>();

            s.CreateAndAddLabelRow(container, "Controlled Object");

            DropDown spin3 = null, spin4 = null;

            spin3 = s.CreateAndAddDropDownRow(container, "Controlled Object", objlist, 0, (sender, e) => {
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

            Label txtval = null;

            spin4 = s.CreateAndAddDropDownRow(container, "Controlled Property", proplist2, 0, (sender, e) => {
                if (sender.SelectedIndex > 0)
                {
                    adjust.ControlledObjectData.PropertyName = proplist2[sender.SelectedIndex];
                    var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                    var currval = Convert.ToDouble(obj.GetPropertyValue(proplist2[sender.SelectedIndex], su)).ToString(nf) + " " + obj.GetPropertyUnit(proplist2[sender.SelectedIndex], su);
                    txtval.Text = "Current Value: " + currval;
                }
            });

            txtval = s.CreateAndAddLabelRow(container, "Current Value: N/A");
            
            var txtvalue = s.CreateAndAddTextBoxRow(container, nf, "Target Value", 0.0f, (sender, e) =>
            {
                if (adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
                {
                    var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                    if (s.IsValidDouble(sender.Text))
                    {
                        adjust.AdjustValue = cv.ConvertToSI(obj.GetPropertyUnit(adjust.ControlledObjectData.PropertyName, su), Double.Parse(sender.Text));
                    }
                }
            }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)adjust.GetFlowsheet()).HighLevelSolve.Invoke(); });

            if (adjust.ControlledObjectData.ID != "" && adjust.GetFlowsheet().SimulationObjects.ContainsKey(adjust.ControlledObjectData.ID))
            {
                spin3.SelectedIndex = (objlist.IndexOf(adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID].GraphicObject.Tag));
                var obj = adjust.GetFlowsheet().SimulationObjects[adjust.ControlledObjectData.ID];
                txtvalue.Text = cv.ConvertFromSI(obj.GetPropertyUnit(adjust.ControlledObjectData.PropertyName, su), adjust.AdjustValue).ToString(nf);
            }

            s.CreateAndAddCheckBoxRow(container, "Run with the Simultaneous Adjust Solver", adjust.SimultaneousAdjust, (sender, e) => adjust.SimultaneousAdjust = sender.Checked.GetValueOrDefault());

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

            var objlist = spec.GetFlowsheet().SimulationObjects.Values.Select((x2) => x2.GraphicObject.Tag).ToList();
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

            DropDown spin3 = null, spin4 = null;

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

            container.CreateAndAddLabelRow("Expression");

            container.CreateAndAddStringEditorRow2("Y = f(X) = ", "enter a math expression using X as the only variable", spec.Expression, (sender, e) => spec.Expression = sender.Text);

        }

    }

}
