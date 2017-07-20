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

            s.CreateAndAddDropDownRow(container, "System of Units", avunits, avunits.IndexOf(flowsheet.FlowsheetOptions.SelectedUnitSystem.Name), (sender, e) => {
                flowsheet.FlowsheetOptions.SelectedUnitSystem = (IUnitsOfMeasure)Activator.CreateInstance(flowsheet.AvailableSystemsOfUnits.Where((x) => x.Name == avunits[sender.SelectedIndex]).FirstOrDefault().GetType());
            });

            s.CreateAndAddDescriptionRow(container, "Select the System of Units to be used on this simulation");

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