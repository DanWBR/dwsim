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

using System.Reflection;
using System.IO;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class PipeSectionEditor
    {

        public DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe.PipeSection section;
        public IFlowsheet flowsheet;

        public DynamicLayout container;

        private List<string[]> ACD = new List<string[]>();
        private List<string> materials, sectypes;

        public PipeSectionEditor(IFlowsheet fs, DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe.PipeSection sec, DynamicLayout layout)
        {
            flowsheet = fs;
            section = sec;
            container = layout;

            materials = new List<string>();

            materials.Add(fs.GetTranslatedString("AoComum"));
            materials.Add(fs.GetTranslatedString("AoCarbono"));
            materials.Add(fs.GetTranslatedString("FerroBottomido"));
            materials.Add(fs.GetTranslatedString("AoInoxidvel"));
            materials.Add("PVC");
            materials.Add("PVC+PFRV");
            materials.Add(fs.GetTranslatedString("CommercialCopper"));
            materials.Add(fs.GetTranslatedString("UserDefined"));

            sectypes = new List<string>();
            sectypes.Add(fs.GetTranslatedString("Tubulaosimples"));

            var assembly = Assembly.GetAssembly(new PipeSection().GetType());

            var resourceName = "DWSIM.UnitOperations.fittings.dat";

            using (Stream stream = assembly.GetManifestResourceStream(resourceName))
            using (StreamReader reader = new StreamReader(stream))
            {
                while (!reader.EndOfStream)
                {
                    ACD.Add(reader.ReadLine().Split(';'));
                    sectypes.Add(ACD[ACD.Count - 1][0]);
                }
            }
            
            Initialize();

        }

        void Initialize()
        {

            double dummy = 0.0f;

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            if (section.TipoSegmento == "Tubulaosimples") section.TipoSegmento = "Straight Tube Section";

            var lblseg = container.CreateAndAddTwoLabelsRow("Segment", section.Indice.ToString());

            container.CreateAndAddDropDownRow("Type", sectypes, Array.IndexOf(sectypes.ToArray(), section.TipoSegmento), (sender, e) => section.TipoSegmento = sectypes[sender.SelectedIndex]);
            var cbm = container.CreateAndAddDropDownRow("Material", materials, Array.IndexOf(materials.ToArray(), section.Material), (sender, e) => section.Material = materials[sender.SelectedIndex]);
            var tbr = container.CreateAndAddTextBoxRow("G8", "Rugosity " + " (" + su.distance + ") *", cv.ConvertFromSI(su.distance, section.PipeWallRugosity), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.PipeWallRugosity = cv.ConvertToSI(su.distance, sender.Text.ParseExpressionToDouble()); });
            var tbtc = container.CreateAndAddStringEditorRow("Thermal Conductivity " + " (" + su.thermalConductivity + ") *", section.PipeWallThermalConductivityExpression, (sender, e) => { section.PipeWallThermalConductivityExpression = sender.Text.ToString(); });
            container.CreateAndAddTextBoxRow("N0", "Increments", section.Incrementos, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.Incrementos = int.Parse(sender.Text.ToString()); });
            container.CreateAndAddTextBoxRow("N0", "Quantity", section.Quantidade, (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.Quantidade = int.Parse(sender.Text.ToString());});
            container.CreateAndAddTextBoxRow(nf, "Length" + " (" + su.distance + ")", cv.ConvertFromSI(su.distance, section.Comprimento), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.Comprimento = cv.ConvertToSI(su.distance, sender.Text.ParseExpressionToDouble()); });
            container.CreateAndAddTextBoxRow(nf, "Elevation" + " (" + su.distance + ")", cv.ConvertFromSI(su.distance, section.Elevacao), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.Elevacao = cv.ConvertToSI(su.distance, sender.Text.ParseExpressionToDouble()); });
            container.CreateAndAddTextBoxRow(nf, "External Diameter" + " (" + su.diameter + ")", cv.Convert("in", su.diameter, section.DE), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.DE = cv.Convert(su.diameter, "in", sender.Text.ParseExpressionToDouble()); });
            container.CreateAndAddTextBoxRow(nf, "Internal Diameter" + " (" + su.diameter + ")", cv.Convert("in", su.diameter, section.DI), (sender, e) => { if (sender.Text.IsValidDoubleExpression()) section.DI = cv.Convert(su.diameter, "in", sender.Text.ParseExpressionToDouble()); });
            container.CreateAndAddDescriptionRow("* Fields required/used only for User-Defined materials");
            tbr.ReadOnly = section.Material != flowsheet.GetTranslatedString("UserDefined");
            tbtc.ReadOnly = tbr.ReadOnly;
            if (tbr.ReadOnly)
            {
                tbr.BackgroundColor = Eto.Drawing.Colors.LightGrey;
                tbtc.BackgroundColor = Eto.Drawing.Colors.LightGrey;
            }
            else
            {
                tbr.BackgroundColor = Eto.Drawing.SystemColors.ControlBackground;
                tbtc.BackgroundColor = Eto.Drawing.SystemColors.ControlBackground;
            }
            cbm.SelectedValueChanged += (sender, e) => {
                if (cbm.SelectedValue.ToString() == flowsheet.GetTranslatedString("UserDefined"))
                {
                    tbr.ReadOnly = false;
                    tbtc.ReadOnly = false;
                    tbr.BackgroundColor = Eto.Drawing.SystemColors.ControlBackground;
                    tbtc.BackgroundColor = Eto.Drawing.SystemColors.ControlBackground;
                }
                else {
                    tbr.ReadOnly = true;
                    tbtc.ReadOnly = true;
                    tbr.BackgroundColor = Eto.Drawing.Colors.LightGrey;
                    tbtc.BackgroundColor = Eto.Drawing.Colors.LightGrey;
                }
            };
        }
    }
}
