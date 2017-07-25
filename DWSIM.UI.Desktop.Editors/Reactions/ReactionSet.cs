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
using DWSIM.UI.Shared;
using Eto.Drawing;

namespace DWSIM.UI.Desktop.Editors
{
    public class ReactionSetEditor
    {

        public IFlowsheet flowsheet;
        public IReactionSet rset, rset0;
        public DynamicLayout container;

        public ReactionSetEditor(IFlowsheet fs, IReactionSet reactionset, DynamicLayout layout)
        {
            container = layout;
            flowsheet = fs;
            rset0 = reactionset;
            rset = (IReactionSet)((ICloneable)reactionset).Clone();
            rset.ID = reactionset.ID;
            Initialize();
        }

        void Initialize()
        {

            container.CreateAndAddLabelRow("Reaction Set Details");

            var txt1 = container.CreateAndAddStringEditorRow2("Name", "", rset.Name, (sender, e) => { rset.Name = sender.Text; });

            txt1.Enabled = (rset.ID != "DefaultSet");

            var txt2 = container.CreateAndAddStringEditorRow2("Description", "", rset.Description, (sender, e) => { rset.Description = sender.Text; });

            txt2.Enabled = (rset.ID != "DefaultSet");

            container.CreateAndAddLabelRow("Reaction Active in Set / Reaction Name / Sequence #)");

            var compcontainer = new DynamicLayout();
            compcontainer.BackgroundColor = Colors.White;

            Double val;

            foreach (IReaction item in flowsheet.Reactions.Values)
            {

                var chk = new CheckBox() { Text = item.Name, Checked = (rset.Reactions.ContainsKey(item.ID) ? rset.Reactions[item.ID].IsActive : false) };
                chk.CheckedChanged += (sender, e) =>
                {
                    if (!rset.Reactions.ContainsKey(item.ID))
                    {
                        rset.Reactions.Add(item.ID, new DWSIM.Thermodynamics.BaseClasses.ReactionSetBase(item.ID, 0, chk.Checked.GetValueOrDefault()));
                    }
                    else
                    {
                        rset.Reactions[item.ID].IsActive = chk.Checked.GetValueOrDefault();
                    }
                };

                var sc = new TextBox() { Width = 50, Text = (rset.Reactions.ContainsKey(item.ID) ? (rset.Reactions[item.ID].Rank.ToString()) : 0.0f.ToString()) };

                sc.TextChanged += (sender, e) =>
                {
                    if (Double.TryParse(sc.Text.ToString(), out val))
                    {
                        sc.TextColor = SystemColors.ControlText;
                        if (!rset.Reactions.ContainsKey(item.ID))
                        {
                            rset.Reactions.Add(item.ID, new DWSIM.Thermodynamics.BaseClasses.ReactionSetBase(item.ID, int.Parse(sc.Text), true));
                        }
                        else
                        {
                            rset.Reactions[item.ID].Rank = int.Parse(sc.Text);
                        }
                    }
                    else
                    {
                        sc.TextColor = Colors.Red;
                    }
                };
                compcontainer.Add(new TableRow(chk, null, sc));
            }

            container.CreateAndAddControlRow(compcontainer);
            container.CreateAndAddEmptySpace();

        }

    }
}