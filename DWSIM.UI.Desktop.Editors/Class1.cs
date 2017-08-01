using System;
using System.Collections.Generic;
using Eto.Drawing;
using Eto.Forms;
using Editors;

namespace Editors
{
    public class Class1
    {

        public void Create()
        {

            var container = new DynamicLayout();

            container.BeginVertical();
            
            container.Padding = 10;

            CreateAndAddLabelRow(container, "Property Packages");

            CreateAndAddDescriptionRow(container, "A Property Package is a set of " +
                "models and methods/equations which are responsible for the calculation of compound and phase properties and for providing " +
                "thermodynamic properties for Unit Operation calculations, like enthalpy and entropy.\n\n" +
                "You need to add at least one Property Package to your simulation.");

            CreateAndAddDropDownRow(container, "Add New Property Package", new List<string>(), 0, (sender, e) => { });

            CreateAndAddLabelRow(container, "Added Property Packages");

            CreateAndAddLabelRow(container, "Flash Algorithms");

            CreateAndAddDescriptionRow(container, "The Flash Algorithms in DWSIM are the components responsible for determining a particular set " +
            "of phases at thermodynamic equilibrium, their amounts (and the amounts of the compounds on each phase) at the specified conditions like " +
            "Temperature, Pressure, Total Enthalpy and Total Entropy. Some Flash Algorithms are capable of predicting equilibrium between one vapor " +
            "and one liquid phase, while others support another co-existing liquid and/or solid phase. As the amount of phases considered in " +
            "equilibrium increases, the calculation time/complexity also increases while the results' reliability decreases.\n\n" +
            "Some flash algorithms are more capable/reliable than others, depending on the mixture for which the flash calculation request is being " +
            "requested. DWSIM features a selection of flash algorithms that are capable of calculating VLE, VLLE and SLE.\n\n" +
            "The 'Nested Loops (VLE)' algorithm satisfies the requirements of most Vapor-Liquid Equilibria systems.");

            CreateAndAddDropDownRow(container, "Add New Flash Algorithm", new List<string>(), 0, (sender, e) => { });

            CreateAndAddLabelRow(container, "Added Flash Algorithms");

            CreateAndAddEmptySpace(container);

            container.EndVertical();

            container.Width = 500 - container.Padding.Value.Left * 2 - container.Padding.Value.Right * 2;

            List<TabPage> tabs = new List<TabPage>();

            tabs.Add(new TabPage(new Scrollable { Content = container, Border = BorderType.None }) { Text = "Thermo" });
            tabs.Add(new TabPage(new Panel()) { Text = "Reactions" });

            var tabctrl = new TabControl();
            foreach (var tab in tabs)
            {
                tabctrl.Pages.Add(tab);
            }

            var form = new Form() { Content = new Scrollable { Content = tabctrl, Border = BorderType.None }, ClientSize = new Size(500, 400) };
            form.Show();

        }

        public static void CreateAndAddEmptySpace(DynamicLayout container)
        {
            container.AddRow(new TableRow(new Label { Text = "", Height = 10 }));
        }

        public Label CreateAndAddLabelRow(DynamicLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            container.AddRow(new TableRow(label));
            CreateAndAddEmptySpace(container);
            return label;
        }

        public static void CreateAndAddDescriptionRow(DynamicLayout container, String text)
        {
            container.AddRow(new TableRow(new Label { Text = text, Wrap = WrapMode.Word, Font = SystemFonts.Label(SystemFonts.Default().Size - 2.0f) }));
            CreateAndAddEmptySpace(container);
        }
        
        public static DropDown CreateAndAddDropDownRow(DynamicLayout container, String text, List<String> options, int position, Action<DropDown, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var drop = new DropDown { Width = 200 };

            foreach (var item in options)
            {
                drop.Items.Add(item);
            }

            drop.SelectedIndex = position;

            if (command != null) drop.SelectedIndexChanged += (sender, e) => command.Invoke((DropDown)sender, e);

            var tr = new TableRow(txt, null, drop);

            container.AddRow(tr);
            CreateAndAddEmptySpace(container);

            return drop;

        }

    }
}
