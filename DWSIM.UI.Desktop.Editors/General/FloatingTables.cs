using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using Eto.Forms;
using Eto.Drawing;
using s = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;
using System.Reflection;
using System.IO;

namespace DWSIM.UI.Desktop.Editors
{
    public class FloatingTablesView : DynamicLayout
    {

        public IFlowsheet flowsheet;

        private Dictionary<string, string[]> availableproperties = new Dictionary<string, string[]>();
        private List<Type> aTypeList = new List<Type>();
      
        public FloatingTablesView(IFlowsheet fs)
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            var dir = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);

            var calculatorassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.Thermodynamics.dll"));
            var unitopassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.UnitOperations.dll"));

            aTypeList.Clear();
            aTypeList.AddRange(calculatorassembly.GetTypes().Where(x =>
            {
                if (x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }));
            aTypeList.AddRange(unitopassembly.GetTypes().Where(x =>
            {
                if (x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }));

            bool add = false;
            if (flowsheet.FlowsheetOptions.VisibleProperties.Count == 0) add = true;

            this.CreateAndAddDescriptionRow("Select the properties to show on Floating Property Tables according to object type.");

            var cbObjectType = this.CreateAndAddDropDownRow("Object Type", new List<string>(), 0, null);

            cbObjectType.Items.Clear();
            availableproperties.Clear();
            
            foreach (var item in aTypeList.OrderBy(x => x.Name))
            {
                if (!item.IsAbstract)
                {
                    var obj = (Interfaces.ISimulationObject)Activator.CreateInstance(item);
                    obj.SetFlowsheet(flowsheet);
                    cbObjectType.Items.Add(new ListItem{Key = item.Name, Text= obj.GetDisplayName()});
                    availableproperties.Add(item.Name, obj.GetProperties(DWSIM.Interfaces.Enums.PropertyType.ALL));
                    if (add) flowsheet.FlowsheetOptions.VisibleProperties.Add(item.Name, obj.GetDefaultProperties().ToList());
                    obj = null;
                }
            }

            cbObjectType.SelectedIndex = 0;

            var PropertyListView = new StackLayout();

            cbObjectType.SelectedIndexChanged += (sender, e) =>
            {
                if (cbObjectType.SelectedIndex < 0) return;
                var selvalue = cbObjectType.SelectedValue.ToString();
                var selkey = cbObjectType.SelectedKey;
                PropertyListView.SuspendLayout();
                PropertyListView.Items.Clear();
                foreach (var item in availableproperties[selkey])
                {
                    var check = new CheckBox {Text = flowsheet.GetTranslatedString(item), Tag = item, Checked = flowsheet.FlowsheetOptions.VisibleProperties[selkey].Contains(item) };
                    check.CheckedChanged += (sender2, e2) => {
                        if (check.Checked.GetValueOrDefault())
                        {
                            if (!flowsheet.FlowsheetOptions.VisibleProperties[selkey].Contains(check.Tag.ToString()))
                            {
                                flowsheet.FlowsheetOptions.VisibleProperties[selkey].Add(check.Tag.ToString());
                            }
                        }
                        else
                        {
                            if (flowsheet.FlowsheetOptions.VisibleProperties[selkey].Contains(check.Tag.ToString()))
                            {
                                flowsheet.FlowsheetOptions.VisibleProperties[selkey].Remove(check.Tag.ToString());
                            }
                        }
                    };
                    var newitem = new StackLayoutItem(check);
                    PropertyListView.Items.Add(newitem);
                }
                PropertyListView.ResumeLayout();
            };

            this.CreateAndAddControlRow(new Scrollable { Content = PropertyListView });

        }
    }
}
