using System.Collections.Generic;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.ExtensionMethods;
using DWSIM.ExtensionMethods.Eto;
using DWSIM.UI.Shared;
using Eto.Drawing;

namespace DWSIM.UI.Desktop.Editors
{
    public class DynamicPropertiesEditor
    {

        public ISimulationObject SimObject;

        public DynamicLayout container;

        public DynamicPropertiesEditor(ISimulationObject selectedobject, DynamicLayout layout)
        {
            SimObject = selectedobject;
            container = layout;
            Initialize();
        }

        void CallSolverIfNeeded()
        {
            if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke();
        }

        void Initialize()
        {

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat;
            var nff = SimObject.GetFlowsheet().FlowsheetOptions.FractionNumberFormat;

            s.CreateAndAddLabelRow(container, "Dynamic Properties Editor");

            s.CreateAndAddDescriptionRow(container, "Property values are updated/stored as they are changed/edited. There's no need to press ENTER to commit the changes.");

            switch (SimObject.GraphicObject.ObjectType)
            {
                case ObjectType.MaterialStream:
                    container.CreateAndAddDropDownRow("Dynamic Specification",
                        SimObject.DynamicsSpec.GetEnumNames(), (int)SimObject.DynamicsSpec, (dd, e) => {
                            SimObject.DynamicsSpec = dd.SelectedIndex.ToEnum<Interfaces.Enums.Dynamics.DynamicsSpecType>();
                            SimObject.GetFlowsheet().UpdateInterface();
                        });
                    break;
            }

            var col1 = (IDictionary<string, object>)SimObject.ExtraProperties;
            var col2 = (IDictionary<string, object>)SimObject.ExtraPropertiesDescriptions;
            var col3 = (IDictionary<string, object>)SimObject.ExtraPropertiesUnitTypes;

            foreach (var p in col1)
            {
                if (col2.ContainsKey(p.Key) && col3.ContainsKey(p.Key))
                {
                    string unitsstring = "";
                    try
                    {
                        var utype = col3[p.Key];
                        unitsstring = su.GetCurrentUnits(int.Parse(utype.ToString()).ToEnum<Interfaces.Enums.UnitOfMeasure>());
                    }
                    catch { }
                    container.CreateAndAddTextBoxRow("G", unitsstring != "" ? p.Key + " (" + unitsstring + ")" : p.Key, 
                        p.Value.ToString().ToDoubleFromCurrent(), 
                        (tb, e) => {
                            try
                            {
                                col1[p.Key] = tb.Text.ToDoubleFromCurrent();
                                tb.TextColor = Colors.Blue;
                            }
                            catch
                            {
                                tb.TextColor = Colors.Red;
                            }
                        });
                    container.CreateAndAddDescriptionRow(col2[p.Key].ToString());
                    container.CreateAndAddEmptySpace();
                }
            }

            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);
        }

    }

}
