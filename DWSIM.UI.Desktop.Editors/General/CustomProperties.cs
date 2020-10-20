using System.Collections.Generic;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.ExtensionMethods;
using DWSIM.ExtensionMethods.Eto;
using DWSIM.UI.Shared;
using Eto.Drawing;
using System;

namespace DWSIM.UI.Desktop.Editors
{
    public class CustomPropertiesEditor
    {

        public ISimulationObject SimObject;

        public DynamicLayout container;

        public CustomPropertiesEditor(ISimulationObject selectedobject, DynamicLayout layout)
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

            s.CreateAndAddLabelRow(container, "Custom Properties Editor");

            s.CreateAndAddDescriptionRow(container, "Property values are updated/stored as they are changed/edited. There's no need to press ENTER to commit the changes.");

            var col1 = (IDictionary<string, object>)SimObject.ExtraProperties;
            var col2 = (IDictionary<string, object>)SimObject.ExtraPropertiesDescriptions;
            var col3 = (IDictionary<string, object>)SimObject.ExtraPropertiesUnitTypes;

            foreach (var p in col1)
            {
                if (!col2.ContainsKey(p.Key) && !col3.ContainsKey(p.Key))
                {
                    container.CreateAndAddStringEditorRow(p.Key,
                        p.Value.ToString(),
                        (tb, e) =>
                        {
                            try
                            {                                
                                col1[p.Key] = tb.Text;
                                tb.TextColor = Colors.Blue;
                            }
                            catch
                            {
                                tb.TextColor = Colors.Red;
                            }
                        });
                    container.CreateAndAddEmptySpace();
                }
            }

            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);
        }

    }

}
