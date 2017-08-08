using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.SharedClasses.SystemsOfUnits;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class SimulationSettings
    {

        public IFlowsheet flowsheet;
        public DynamicLayout container;

        public SimulationSettings(IFlowsheet fs, DynamicLayout layout)
        {
            flowsheet = fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {
            
            s.CreateAndAddLabelRow(container, "General");

            s.CreateAndAddStringEditorRow(container, "Simulation Name", flowsheet.FlowsheetOptions.SimulationName, (sender, e) => flowsheet.FlowsheetOptions.SimulationName = sender.Text);

            s.CreateAndAddDescriptionRow(container, "The simulation name will be used for report identification and file name during saving.");
                      
            var avunits = flowsheet.AvailableSystemsOfUnits.Select((x) => x.Name).ToList();

            s.CreateAndAddLabelRow(container, "System of Units");

            Button btnEdit = null;
            DropDown uselector = null;

            uselector = s.CreateAndAddDropDownRow(container, "System of Units", avunits, avunits.IndexOf(flowsheet.FlowsheetOptions.SelectedUnitSystem.Name), (sender, e) => {
                flowsheet.FlowsheetOptions.SelectedUnitSystem = flowsheet.AvailableSystemsOfUnits.Where((x) => x.Name == avunits[sender.SelectedIndex]).FirstOrDefault();
                btnEdit.Enabled = !new string[]{"SI", "CGS", "ENG"}.Contains(uselector.SelectedValue.ToString());
            });

            s.CreateAndAddDescriptionRow(container, "Select the System of Units to be used on this simulation");

            btnEdit = s.CreateAndAddLabelAndButtonRow(container, "Edit System of Units", "Edit Selected", null, (sender, e) =>
            {
                var editcontainer = new Editors.UnitSetEditorView((DWSIM.SharedClasses.SystemsOfUnits.Units)flowsheet.FlowsheetOptions.SelectedUnitSystem);
                var form = s.GetDefaultEditorForm("Edit System of Units", 400, 600, editcontainer);
                form.Closed += (sender2, e2) => {
                    container.RemoveAll();
                    container.Clear();
                    Initialize();
                    container.Create();
                };
                form.Show();
            });

            s.CreateAndAddLabelAndButtonRow(container, "Create New System of Units", "Create New", null, (sender, e) =>
            {
                var newsystem = new DWSIM.SharedClasses.SystemsOfUnits.SI { Name = "NewUnitSet" };
                flowsheet.AvailableSystemsOfUnits.Add(newsystem);
                flowsheet.FlowsheetOptions.SelectedUnitSystem = newsystem;
                var editcontainer = new Editors.UnitSetEditorView(newsystem);
                var form = s.GetDefaultEditorForm("Create New System of Units", 400, 600, editcontainer);
                form.Closed += (sender2, e2) =>
                {
                    container.RemoveAll();
                    container.Clear();
                    Initialize();
                    container.Create();
                };
                form.Show();
            });

            btnEdit.Enabled = !new string[] { "SI", "CGS", "ENG" }.Contains(uselector.SelectedValue.ToString());

            var nformats = new []{"F", "G","G2","G4","G6","G8","G10","N","N2","N4","N6","R","E","E1","E2","E3","E4","E6"};

            s.CreateAndAddLabelRow(container, "Number Formats");

            s.CreateAndAddDropDownRow(container, "General", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.NumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.NumberFormat = sender.SelectedValue.ToString();
            });

            s.CreateAndAddDescriptionRow(container, "Select the formatting scheme for general numbers.");

            s.CreateAndAddDropDownRow(container, "Compound Amounts", nformats.ToList(), nformats.ToList().IndexOf(flowsheet.FlowsheetOptions.FractionNumberFormat), (sender, e) =>
            {
                flowsheet.FlowsheetOptions.FractionNumberFormat = sender.SelectedValue.ToString();
            });

            s.CreateAndAddDescriptionRow(container, "Select the formatting scheme for compound amounts in Material Stream reports.");


        }

    }

}