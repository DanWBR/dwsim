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

            var spinnerType = container.CreateAndAddDropDownRow("Type", sectypes, Array.IndexOf(sectypes.ToArray(), section.TipoSegmento), (sender, e) => section.TipoSegmento = sectypes[sender.SelectedIndex]);
            var spinnerMat = container.CreateAndAddDropDownRow("Material", materials, Array.IndexOf(materials.ToArray(), section.Material), (sender, e) => section.Material = materials[sender.SelectedIndex]);

            var txtIncrements = container.CreateAndAddTextBoxRow("N0", "Increments", section.Incrementos, (sender, e) => { if (double.TryParse(sender.Text.ToString(), out dummy)) section.Incrementos = int.Parse(sender.Text.ToString());});
            var txtLength = container.CreateAndAddTextBoxRow(nf, "Length" + " (" + su.distance + ")", cv.ConvertFromSI(su.distance, section.Comprimento), (sender, e) => { if (double.TryParse(sender.Text.ToString(), out dummy)) section.Comprimento = cv.ConvertToSI(su.distance, double.Parse(sender.Text.ToString())); });
            var txtElevation = container.CreateAndAddTextBoxRow(nf, "Elevation" + " (" + su.distance + ")", cv.ConvertFromSI(su.distance, section.Elevacao), (sender, e) => { if (double.TryParse(sender.Text.ToString(), out dummy)) section.Elevacao = cv.ConvertToSI(su.distance, double.Parse(sender.Text.ToString())); });
            var txtED = container.CreateAndAddTextBoxRow(nf, "External Diameter" + " (" + su.diameter + ")", cv.ConvertFromSI(su.diameter, section.DE), (sender, e) => { if (double.TryParse(sender.Text.ToString(), out dummy)) section.DE = cv.ConvertToSI(su.diameter, double.Parse(sender.Text.ToString())); });
            var txtID = container.CreateAndAddTextBoxRow(nf, "Internal Diameter" + " (" + su.diameter + ")", cv.ConvertFromSI(su.diameter, section.DI), (sender, e) => { if (double.TryParse(sender.Text.ToString(), out dummy)) section.DI = cv.ConvertToSI(su.diameter, double.Parse(sender.Text.ToString())); });
     


        }
    }
}
