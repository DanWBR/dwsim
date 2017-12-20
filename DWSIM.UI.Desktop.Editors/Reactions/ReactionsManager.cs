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
    public class ReactionsManager
    {

        public IFlowsheet flowsheet;
        public DynamicLayout container;

        private DynamicLayout rxcontainer, rscontainer;

        public ReactionsManager(IFlowsheet fs, DynamicLayout layout)
        {
            flowsheet = fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            rxcontainer = new DynamicLayout();
            rscontainer = new DynamicLayout();

            rxcontainer.BackgroundColor = Colors.White;
            rscontainer.BackgroundColor = Colors.White;

            if (flowsheet.ReactionSets.Count == 0) { flowsheet.ReactionSets.Add("DefaultSet", new ReactionSet("DefaultSet", "Default Set", "")); }

            container.CreateAndAddLabelRow("Reactions");

            CreateReactionsList();

            container.CreateAndAddControlRow(rxcontainer);

            var btnAddConv = container.CreateAndAddLabelAndButtonRow("Add New Conversion Reaction", "New Conversion Reaction", null, (sender, e) =>
            {
                var _rx = new Reaction("NewConvReac", Guid.NewGuid().ToString(), "") { ReactionType = Interfaces.Enums.ReactionType.Conversion };
                var myview = s.GetDefaultContainer();
                var cre = new ConversionReaction(flowsheet, _rx, myview);
                var alert = s.GetDefaultEditorForm("Add Conversion Reaction", 500, 400, myview);
                myview.CreateAndAddTwoButtonsRow("Cancel", null, "Add", null, (sender2, e2) => alert.Close(),
                (sender2, e2) =>
                {
                    _rx = (Reaction)cre.rx;
                    flowsheet.Reactions.Add(_rx.ID, _rx);
                    flowsheet.ReactionSets["DefaultSet"].Reactions.Add(_rx.ID, new ReactionSetBase(_rx.ID, 0, true));
                    CreateReactionsList();
                    alert.Close();
                });
                alert.Topmost = true;
                alert.Show();
            });

            var btnAddEq = container.CreateAndAddLabelAndButtonRow("Add New Equilibrium Reaction", "New Equilibrium Reaction", null, (sender, e) =>
            {
                var _rx = new Reaction("NewEqReac", Guid.NewGuid().ToString(), "") { ReactionType = Interfaces.Enums.ReactionType.Equilibrium };
                var myview = s.GetDefaultContainer();
                var cre = new EquilibriumReaction(flowsheet, _rx, myview);
                var alert = s.GetDefaultEditorForm("Add Equilibrium Reaction", 500, 400, myview);
                myview.CreateAndAddTwoButtonsRow("Cancel", null, "Add", null, (sender2, e2) => alert.Close(),
                (sender2, e2) =>
                {
                    _rx = (Reaction)cre.rx;
                    flowsheet.Reactions.Add(_rx.ID, _rx);
                    flowsheet.ReactionSets["DefaultSet"].Reactions.Add(_rx.ID, new ReactionSetBase(_rx.ID, 0, true));
                    CreateReactionsList();
                    alert.Close();
                });
                alert.Topmost = true;
                alert.Show();
            });

            var btnAddKin = container.CreateAndAddLabelAndButtonRow("Add New Kinetic Reaction", "New Kinetic Reaction", null, (sender, e) =>
            {
                var _rx = new Reaction("NewKinReac", Guid.NewGuid().ToString(), "") { ReactionType = Interfaces.Enums.ReactionType.Kinetic };
                var myview = s.GetDefaultContainer();
                var cre = new KineticReaction(flowsheet, _rx, myview);
                var alert = s.GetDefaultEditorForm("Add Kinetic Reaction", 500, 400, myview);
                myview.CreateAndAddTwoButtonsRow("Cancel", null, "Add", null, (sender2, e2) => alert.Close(),
                (sender2, e2) =>
                {
                    _rx = (Reaction)cre.rx;
                    flowsheet.Reactions.Add(_rx.ID, _rx);
                    flowsheet.ReactionSets["DefaultSet"].Reactions.Add(_rx.ID, new ReactionSetBase(_rx.ID, 0, true));
                    CreateReactionsList();
                    alert.Close();
                });
                alert.Topmost = true;
                alert.Show();
            });

            var btnAddHC = container.CreateAndAddLabelAndButtonRow("Add New Heterogeneous Catalytic Reaction", "New HetCat Reaction", null, (sender, e) =>
            {
                var _rx = new Reaction("NewHetCatReac", Guid.NewGuid().ToString(), "") { ReactionType = Interfaces.Enums.ReactionType.Heterogeneous_Catalytic };
                var myview = s.GetDefaultContainer();
                var cre = new HetCatReaction(flowsheet, _rx, myview);
                var alert = s.GetDefaultEditorForm("Add Heterogeneous Catalytic Reaction", 500, 400, myview);
                myview.CreateAndAddTwoButtonsRow("Cancel", null, "Add", null, (sender2, e2) => alert.Close(),
                (sender2, e2) =>
                {
                    _rx = (Reaction)cre.rx;
                    flowsheet.Reactions.Add(_rx.ID, _rx);
                    flowsheet.ReactionSets["DefaultSet"].Reactions.Add(_rx.ID, new ReactionSetBase(_rx.ID, 0, true));
                    CreateReactionsList();
                    alert.Close();
                });
                alert.Topmost = true;
                alert.Show();
            });

            container.CreateAndAddEmptySpace();

            container.CreateAndAddLabelRow("Reaction Sets");

            container.CreateAndAddControlRow(rscontainer);

            container.CreateAndAddLabelAndButtonRow("Add New Reaction Set", "New Reaction Set", null, (sender, e) =>
            {
                var rsid = Guid.NewGuid().ToString();
                flowsheet.ReactionSets.Add(rsid, new ReactionSet(rsid, "NewReactionSet", ""));
                CreateReactionSetsList();
            });

            CreateReactionSetsList();

        }

        private void CreateReactionsList()
        {

            rxcontainer.RemoveAll();
            rxcontainer.Clear();

            foreach (var rx in flowsheet.Reactions.Values)
            {

                string rname = "";
                switch (rx.ReactionType)
                {
                    case Interfaces.Enums.ReactionType.Conversion:
                        rname = "[CON] " + rx.Name;
                        break;
                    case Interfaces.Enums.ReactionType.Equilibrium:
                        rname = "[EQL] " + rx.Name;
                        break;
                    case Interfaces.Enums.ReactionType.Kinetic:
                        rname = "[KIN] " + rx.Name;
                        break;
                    case Interfaces.Enums.ReactionType.Heterogeneous_Catalytic:
                        rname = "[HET] " + rx.Name;
                        break;
                }

                rxcontainer.CreateAndAddLabelAndTwoButtonsRow(rname, "Edit", null, "Remove", null,
                    (sender, e) =>
                    {
                        switch (rx.ReactionType)
                        {
                            case Interfaces.Enums.ReactionType.Conversion:
                                var myview = s.GetDefaultContainer();
                                var cre = new ConversionReaction(flowsheet, rx, myview);
                                var alert = s.GetDefaultEditorForm("Edit Conversion Reaction", 500, 400, myview);
                                myview.CreateAndAddTwoButtonsRow("Cancel", null, "Update", null, (sender2, e2) => alert.Close(),
                                (sender2, e2) =>
                                {
                                    flowsheet.Reactions[cre.rx.ID] = (IReaction)cre.rx;
                                    CreateReactionsList();
                                    alert.Close();
                                });
                                myview.CreateAndAddEmptySpace();
                                myview.CreateAndAddEmptySpace();
                                alert.Topmost = true;
                                alert.Show();
                                break;
                            case Interfaces.Enums.ReactionType.Equilibrium:
                                var myview2 = s.GetDefaultContainer();
                                var cre2 = new EquilibriumReaction(flowsheet, rx, myview2);
                                var alert2 = s.GetDefaultEditorForm("Edit Equilibrium Reaction", 500, 400, myview2);
                                myview2.CreateAndAddTwoButtonsRow("Cancel", null, "Update", null, (sender2, e2) => alert2.Close(),
                                (sender2, e2) =>
                                {
                                    flowsheet.Reactions[cre2.rx.ID] = (IReaction)cre2.rx;
                                    CreateReactionsList();
                                    alert2.Close();
                                });
                                myview2.CreateAndAddEmptySpace();
                                myview2.CreateAndAddEmptySpace();
                                alert2.Topmost = true;
                                alert2.Show();
                                break;
                            case Interfaces.Enums.ReactionType.Kinetic:
                                var myview3 = s.GetDefaultContainer();
                                var cre3 = new KineticReaction(flowsheet, rx, myview3);
                                var alert3 = s.GetDefaultEditorForm("Edit Kinetic Reaction", 500, 400, myview3);
                                myview3.CreateAndAddTwoButtonsRow("Cancel", null, "Update", null, (sender2, e2) => alert3.Close(),
                                (sender2, e2) =>
                                {
                                    flowsheet.Reactions[cre3.rx.ID] = (IReaction)cre3.rx;
                                    CreateReactionsList();
                                    alert3.Close();
                                });
                                myview3.CreateAndAddEmptySpace();
                                myview3.CreateAndAddEmptySpace();
                                alert3.Topmost = true;
                                alert3.Show();
                                break;
                            case Interfaces.Enums.ReactionType.Heterogeneous_Catalytic:
                                var myview4 = s.GetDefaultContainer();
                                var cre4 = new HetCatReaction(flowsheet, rx, myview4);
                                var alert4 = s.GetDefaultEditorForm("Edit Heterogeneous Reaction", 500, 400, myview4);
                                myview4.CreateAndAddTwoButtonsRow("Cancel", null, "Update", null, (sender2, e2) => alert4.Close(),
                                (sender2, e2) =>
                                {
                                    flowsheet.Reactions[cre4.rx.ID] = (IReaction)cre4.rx;
                                    CreateReactionsList();
                                    alert4.Close();
                                });
                                myview4.CreateAndAddEmptySpace();
                                myview4.CreateAndAddEmptySpace();
                                alert4.Topmost = true;
                                alert4.Show();
                                break;
                        }

                    },
                    (sender, e) =>
                    {

                        if (MessageBox.Show("Are you sure you want to remove this reaction from the flowsheet?", "Remove selected reaction", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                        {
                            flowsheet.Reactions.Remove(rx.ID);
                            flowsheet.ReactionSets["DefaultSet"].Reactions.Remove(rx.ID);
                            CreateReactionsList();
                        }

                    });

            }

            rxcontainer.Create();

        }

        private void CreateReactionSetsList()
        {

            rscontainer.RemoveAll();
            rscontainer.Clear();

            foreach (var rset in flowsheet.ReactionSets.Values)
            {
                rscontainer.CreateAndAddLabelAndTwoButtonsRow(rset.Name, "Edit", null, "Remove", null,
                    (sender, e) =>
                    {

                        var myview = s.GetDefaultContainer();
                        var cre = new ReactionSetEditor(flowsheet, rset, myview);
                        var alert = s.GetDefaultEditorForm("Edit Reaction Set", 500, 400, myview);
                        myview.CreateAndAddTwoButtonsRow("Cancel", null, "Update", null, (sender2, e2) => alert.Close(),
                        (sender2, e2) =>
                        {
                            flowsheet.ReactionSets[cre.rset.ID] = (IReactionSet)cre.rset;
                            CreateReactionSetsList();
                            alert.Close();
                        });
                        alert.Topmost = true;
                        alert.Show();

                    },
                    (sender, e) =>
                    {
                        if (rset.ID == "DefaultSet") { MessageBox.Show("The Default Reaction Set cannot be removed from the simulation.", MessageBoxType.Information); return; }
                        if (MessageBox.Show("Are you sure you want to remove this reaction set from the flowsheet?", "Remove selected reaction set", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                        {
                            flowsheet.ReactionSets.Remove(rset.ID);
                            CreateReactionSetsList();
                        }

                    }
                    );
            }

            rscontainer.Create();

        }


    }
}
