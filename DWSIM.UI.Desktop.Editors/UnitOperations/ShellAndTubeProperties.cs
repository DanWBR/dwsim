using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;

using DWSIM.UI.Shared;

using DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe;
using DWSIM.UnitOperations.UnitOperations;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;

using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class ShellAndTubePropertiesView : DynamicLayout
    {

        public ISimulationObject SimObject;

        public ShellAndTubePropertiesView(ISimulationObject selectedobject)
            : base()
        {
            SimObject = selectedobject;
            Init();
        }

        void Init()
        {

            Padding = new Eto.Drawing.Padding(10);

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat;

            var st = ((HeatExchanger)SimObject).STProperties;

            this.CreateAndAddLabelRow("Shell Side Properties");

            var tbShellsInSeries = this.CreateAndAddTextBoxRow(nf, "Shells in Series", 0, null);
            var tbNumberOfShellPasses = this.CreateAndAddTextBoxRow(nf, "Shell Passes", 0, null);
            var tbShellDi = this.CreateAndAddTextBoxRow(nf, "Internal Diameter" + " (" + su.diameter + ")", 0, null);
            var tbShellFoulingFactor = this.CreateAndAddTextBoxRow(nf, "Fouling Factor" + " (" + su.foulingfactor + ")", 0, null);
            var tbBaffleSpacing = this.CreateAndAddTextBoxRow(nf, "Baffle Spacing" + " (" + su.thickness + ")", 0, null);
            var tbBaffleCut = this.CreateAndAddTextBoxRow(nf, "Baffle Cut (% diameter)", 0, null);

            var cbBaffleType = this.CreateAndAddDropDownRow("Baffle Type", new List<string>() { "Single", "Double", "Triple", "Quadruple" }, 0, null);
            var cbBaffleOrientation = this.CreateAndAddDropDownRow("Baffle Orientation", new List<string>() { "Horizontal", "Vertical" }, 0, null);

            cbBaffleType.Enabled = false;
            cbBaffleOrientation.Enabled = false;

            this.CreateAndAddLabelRow("Tube Side Properties");

            var tbTubeDi = this.CreateAndAddTextBoxRow(nf, "Internal Diameter" + " (" + su.diameter + ")", 0, null);
            var tbTubeDe = this.CreateAndAddTextBoxRow(nf, "External Diameter" + " (" + su.diameter + ")", 0, null);
            var tbTubeLength = this.CreateAndAddTextBoxRow(nf, "Length" + " (" + su.distance + ")", 0, null);
            var tbTubeFoulingFactor = this.CreateAndAddTextBoxRow(nf, "Fouling Factor" + " (" + su.foulingfactor + ")", 0, null);
            var tbTubeRoughness = this.CreateAndAddTextBoxRow(nf, "Roughness" + " (" + su.diameter + ")", 0, null);
            var tbTubeThermalCond = this.CreateAndAddTextBoxRow(nf, "Thermal Conductivity" + " (" + su.thermalConductivity + ")", 0, null);
            var tbTubePassesPerShell = this.CreateAndAddTextBoxRow(nf, "Passes per Shell", 0, null);
            var tbNumberOfTubesPerShell = this.CreateAndAddTextBoxRow(nf, "Tubes per Shell", 0, null);
            var tbTubePitch = this.CreateAndAddTextBoxRow(nf, "Tube Spacing" + " (" + su.thickness + ")", 0, null);

            var cbTubeLayout = this.CreateAndAddDropDownRow("Tube Layout", new List<string>() { "Triangle", "Rotated Triangle", "Square", "Rotated Square" }, 0, null);
            var cbFluidInTubes = this.CreateAndAddDropDownRow("Fluid in Tubes", new List<string>() { "Hot", "Cold" }, 0, null);

            this.CreateAndAddEmptySpace();
            this.CreateAndAddEmptySpace();

            tbBaffleCut.Text = st.Shell_BaffleCut.ToString(nf);
            tbBaffleSpacing.Text = cv.ConvertFromSI(su.thickness, st.Shell_BaffleSpacing).ToString(nf);
            tbNumberOfShellPasses.Text = st.Shell_NumberOfPasses.ToString();
            tbNumberOfTubesPerShell.Text = st.Tube_NumberPerShell.ToString();
            tbShellDi.Text = cv.ConvertFromSI(su.diameter, st.Shell_Di).ToString(nf);
            tbShellFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, st.Shell_Fouling).ToString(nf);
            tbShellsInSeries.Text = st.Shell_NumberOfShellsInSeries.ToString();
            tbTubeDe.Text = cv.ConvertFromSI(su.diameter, st.Tube_De).ToString(nf);
            tbTubeDi.Text = cv.ConvertFromSI(su.diameter, st.Tube_Di).ToString(nf);
            tbTubeFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, st.Tube_Fouling).ToString(nf);
            tbTubeLength.Text = cv.ConvertFromSI(su.distance, st.Tube_Length).ToString(nf);
            tbTubePassesPerShell.Text = st.Tube_PassesPerShell.ToString();
            tbTubePitch.Text = cv.ConvertFromSI(su.thickness, st.Tube_Pitch).ToString(nf);
            tbTubeRoughness.Text = cv.ConvertFromSI(su.diameter, st.Tube_Roughness).ToString(nf);
            tbTubeThermalCond.Text = cv.ConvertFromSI(su.thermalConductivity, st.Tube_ThermalConductivity).ToString(nf);
            cbBaffleOrientation.SelectedIndex = st.Shell_BaffleOrientation;
            cbBaffleType.SelectedIndex = st.Shell_BaffleType;
            cbTubeLayout.SelectedIndex = st.Tube_Layout;
            cbFluidInTubes.SelectedIndex = st.Tube_Fluid;

            tbBaffleCut.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_BaffleCut = ((TextBox)sender).Text.ParseExpressionToDouble(); };
            tbBaffleSpacing.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_BaffleSpacing = cv.ConvertToSI(su.thickness, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbNumberOfShellPasses.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_NumberOfPasses = (int)((TextBox)sender).Text.ParseExpressionToDouble(); };
            tbNumberOfTubesPerShell.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_NumberPerShell = (int)((TextBox)sender).Text.ParseExpressionToDouble(); };
            tbShellDi.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_Di = cv.ConvertToSI(su.diameter, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbShellFoulingFactor.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_Fouling = cv.ConvertToSI(su.foulingfactor, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbShellsInSeries.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Shell_NumberOfShellsInSeries = (int)((TextBox)sender).Text.ParseExpressionToDouble(); };
            tbTubeDe.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_De = cv.ConvertToSI(su.diameter, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubeDi.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_Di = cv.ConvertToSI(su.diameter, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubeFoulingFactor.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_Fouling = cv.ConvertToSI(su.foulingfactor, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubeLength.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_Length = cv.ConvertToSI(su.distance, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubePassesPerShell.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_PassesPerShell = (int)((TextBox)sender).Text.ParseExpressionToDouble(); };
            tbTubePitch.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_Pitch = cv.ConvertToSI(su.thickness, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubeRoughness.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_Roughness = cv.ConvertToSI(su.diameter, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            tbTubeThermalCond.TextChanged += (sender, e) => { if (((TextBox)sender).Text.IsValidDoubleExpression()) st.Tube_ThermalConductivity = cv.ConvertToSI(su.thermalConductivity, ((TextBox)sender).Text.ParseExpressionToDouble()); };
            cbBaffleOrientation.SelectedIndexChanged += (sender, e) => st.Shell_BaffleOrientation = cbBaffleOrientation.SelectedIndex;
            cbBaffleType.SelectedIndexChanged += (sender, e) => st.Shell_BaffleType = cbBaffleType.SelectedIndex;
            cbTubeLayout.SelectedIndexChanged += (sender, e) => st.Tube_Layout = cbTubeLayout.SelectedIndex;
            cbFluidInTubes.SelectedIndexChanged += (sender, e) => st.Tube_Fluid = cbFluidInTubes.SelectedIndex;

        }
    }
}
