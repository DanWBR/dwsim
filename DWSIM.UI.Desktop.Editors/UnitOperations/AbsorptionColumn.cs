using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using s = DWSIM.UI.Shared.Common;
using Eto.Drawing;

using StringResources = DWSIM.UI.Desktop.Shared.StringArrays;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Interfaces.Enums;
using DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps;

using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class AbsorptionColumnEditor
    {

        public AbsorptionColumn column;

        public DynamicLayout container;

        public AbsorptionColumnEditor(ISimulationObject selectedobject, DynamicLayout layout)
        {
            column = (AbsorptionColumn)selectedobject;
            container = layout;
            Initialize();
        }
        void CallSolverIfNeeded()
        {
            if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)column.GetFlowsheet()).HighLevelSolve.Invoke();
        }

        void Initialize()
        {

            var su = column.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = column.GetFlowsheet().FlowsheetOptions.NumberFormat;
            var nff = column.GetFlowsheet().FlowsheetOptions.FractionNumberFormat;

            s.CreateAndAddLabelRow(container, "Absorption Column Editor");

            s.CreateAndAddDescriptionRow(container, "Property values are updated/stored as they are changed/edited. There's no need to press ENTER to commit the changes.");

            if ((Inspector.Host.Items.Where(x => x.Name.Contains(column.GraphicObject.Tag)).Count() > 0))
            {
                var ctn = new DynamicLayout();
                ctn.BackgroundColor = Colors.LightGrey;
                s.CreateAndAddLabelRow(ctn, "Inspector Reports");
                s.CreateAndAddLabelAndButtonRow(ctn, "An Inspector Report is ready for viewing.", "View Report", null, (btn, e) => {
                    var f = s.GetDefaultEditorForm("Inspector Report for '" + column.GraphicObject.Tag + "'", 1024, 768, Inspector.Window2_Eto.GetInspectorWindow(column), false);
                    f.Show();
                });
                container.Add(ctn);
            }

            s.CreateAndAddLabelRow(container, "Column Details");

            s.CreateAndAddTwoLabelsRow(container, "Type", column.GetDisplayName());

            s.CreateAndAddTwoLabelsRow(container, "Status", column.GraphicObject.Active ? "Active" : "Inactive");

            s.CreateAndAddStringEditorRow(container, "Name", column.GraphicObject.Tag, (TextBox arg3, EventArgs ev) =>
            {
                column.GraphicObject.Tag = arg3.Text;
                column.GetFlowsheet().UpdateInterface();
            }, () => {
                column.GetFlowsheet().UpdateOpenEditForms();
            });

            s.CreateAndAddLabelRow(container, "Property Package");

            var proppacks = column.GetFlowsheet().PropertyPackages.Values.Select((x) => x.Tag).ToList();

            if (proppacks.Count == 0)
            {
                column.GetFlowsheet().ShowMessage("Error: please add at least one Property Package before continuing.", IFlowsheet.MessageType.GeneralError);
            }
            else
            {
                var pp = column.PropertyPackage;
                string selectedpp = "";
                if (pp != null) selectedpp = pp.Tag;
                s.CreateAndAddDropDownRow(container, "Property Package", proppacks, proppacks.IndexOf(selectedpp), (DropDown arg1, EventArgs ev) =>
                {
                    if (proppacks.Count > 0) column.PropertyPackage = (IPropertyPackage)column.GetFlowsheet().PropertyPackages.Values.Where((x) => x.Tag == proppacks[arg1.SelectedIndex]).FirstOrDefault();
                }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)column.GetFlowsheet()).HighLevelSolve.Invoke(); });
            }

            s.CreateAndAddLabelRow(container, "Object Properties");

            s.CreateAndAddDropDownRow(container, "Operating Mode", new List<string> {"Gas-Liquid Absorption", "Liquid-Liquid Extraction"}, (int)column.OperationMode, (arg1, e) => {
                switch (arg1.SelectedIndex)
                {
                    case 0:
                        column.OperationMode = AbsorptionColumn.OpMode.Absorber;
                        break;
                    case 1:
                        column.OperationMode = AbsorptionColumn.OpMode.Extractor;
                        break;
                }

            });

            s.CreateAndAddButtonRow(container, "Define Number of Stages", null, (arg1, e) =>
            {

                var np = new Eto.Forms.NumericStepper();
                np.MinValue = 3;
                np.MaxValue = 100;
                np.MaximumDecimalPlaces = 0;
                np.Value = column.NumberOfStages;
                np.ValueChanged += (sender, e2) =>
                {
                    var refval = (int)np.Value;
                    column.NumberOfStages = refval;
                    int ne, nep, dif, i;
                    ne = refval;
                    nep = column.Stages.Count;
                    dif = ne - nep;
                    if (dif != 0)
                    {
                        if (dif < 0)
                        {
                            column.Stages.RemoveRange(ne - 1, -dif);
                        }
                        else if (dif > 0)
                        {
                            for (i = 0; i <= dif; i++)
                            {
                                column.Stages.Insert(column.Stages.Count - 1, new Stage(Guid.NewGuid().ToString()) { P = 101325, Efficiency = 1.0f });
                                column.Stages[column.Stages.Count - 2].Name = "Stage " + (column.Stages.Count - 2).ToString();
                            }
                        }
                    }
                };

                s.CreateDialog(np, "Set Number of Stages").ShowModal(container);

            });

            s.CreateAndAddButtonRow(container, "Edit Stages", null, (arg1, e) =>
            {

                var sview = DWSIM.UI.Shared.Common.GetDefaultContainer();

                s.CreateAndAddLabelRow(sview, "Edit Stages");
                s.CreateAndAddLabelRow(sview, "Number / Name / Pressure");
                var tlist = new List<TextBox>();
                foreach (var stage in column.Stages)
                {
                    tlist.Add(s.CreateAndAddDoubleTextBoxRow(sview, nf, (column.Stages.IndexOf(stage) + 1).ToString(), stage.Name, cv.ConvertFromSI(su.pressure, stage.P),
                                                   (arg10, arg20) =>
                                                   {
                                                       stage.Name = arg10.Text;
                                                   }, (arg11, arg22) =>
                                                   {
                                                       if (s.IsValidDouble(arg11.Text))
                                                       {
                                                           stage.P = cv.ConvertToSI(su.pressure, Double.Parse(arg11.Text));
                                                       }
                                                   }));
                }
                s.CreateAndAddLabelAndButtonRow(sview, "Interpolate Pressures", "Interpolate", null, (sender2, e2) =>
                {
                    var first = tlist[0].Text.ToDoubleFromCurrent();
                    var last = tlist[tlist.Count - 1].Text.ToDoubleFromCurrent();
                    var n = tlist.Count;
                    int i = 1;
                    for (i = 1; i < n - 1; i++)
                    {
                        tlist[i].Text = (first + (last - first) * i / (n - 1)).ToString(nf);
                    }
                });
                s.CreateAndAddDescriptionRow(sview, "Calculate inner pressures using end stage defined values.");

                var scroll = new Eto.Forms.Scrollable();
                scroll.Content = sview;

                s.CreateDialog(scroll, "Edit Stages", 600, 600).ShowModal(container);

            });

            var istrs = column.GraphicObject.InputConnectors.Where((x) => x.IsAttached && x.ConnectorName.Contains("Feed")).Select((x2) => x2.AttachedConnector.AttachedFrom.Name).ToList();
            var ostrs = column.GraphicObject.OutputConnectors.Where((x) => x.IsAttached && x.ConnectorName.Contains("Side")).Select((x2) => x2.AttachedConnector.AttachedTo.Name).ToList();
            var dist = column.GraphicObject.OutputConnectors.Where((x) => x.IsAttached && x.ConnectorName.Contains("Top Product")).Select((x2) => x2.AttachedConnector.AttachedTo.Name).ToList();
            var bottoms = column.GraphicObject.OutputConnectors.Where((x) => x.IsAttached && x.ConnectorName.Contains("Bottoms Product")).Select((x2) => x2.AttachedConnector.AttachedTo.Name).ToList();
         
            foreach (var id in istrs)
            {
                if (column.MaterialStreams.Values.Where(x => x.StreamID == id).Count() == 0)
                {
                    column.MaterialStreams.Add(id, new StreamInformation()
                    {
                        StreamID = id,
                        ID = id,
                        StreamType = StreamInformation.Type.Material,
                        StreamBehavior = StreamInformation.Behavior.Feed
                    });
                }
            }
            foreach (var id in ostrs)
            {
                if (column.MaterialStreams.Values.Where(x => x.StreamID == id).Count() == 0)
                {
                    column.MaterialStreams.Add(id, new StreamInformation()
                    {
                        StreamID = id,
                        ID = id,
                        StreamType = StreamInformation.Type.Material,
                        StreamBehavior = StreamInformation.Behavior.Sidedraw
                    });
                }
            }
            foreach (var id in dist)
            {
                if (column.MaterialStreams.Values.Where(x => x.StreamID == id).Count() == 0)
                {
                    column.MaterialStreams.Add(id, new StreamInformation()
                    {
                        StreamID = id,
                        ID = id,
                        StreamType = StreamInformation.Type.Material,
                        StreamBehavior = StreamInformation.Behavior.OverheadVapor
                    });
                }
            }
            foreach (var id in bottoms)
            {
                if (column.MaterialStreams.Values.Where(x => x.StreamID == id).Count() == 0)
                {
                    column.MaterialStreams.Add(id, new StreamInformation()
                    {
                        StreamID = id,
                        ID = id,
                        StreamType = StreamInformation.Type.Material,
                        StreamBehavior = StreamInformation.Behavior.BottomsLiquid
                    });
                }
            }
            List<string> remove = new List<string>();
            foreach (var si in column.MaterialStreams.Values)
            {
                if (!istrs.Contains(si.StreamID) && !ostrs.Contains(si.StreamID) && !dist.Contains(si.StreamID) && !bottoms.Contains(si.StreamID)) { remove.Add(si.ID); }
                if (!column.GetFlowsheet().SimulationObjects.ContainsKey(si.StreamID)) { remove.Add(si.ID); }
            }
            foreach (var id in remove)
            {
                if (column.MaterialStreams.ContainsKey(id)) { column.MaterialStreams.Remove(id); }
            }

            var stageNames = column.Stages.Select((x) => x.Name).ToList();
            stageNames.Insert(0, "");
            var stageIDs = column.Stages.Select((x) => x.ID).ToList();
            stageIDs.Insert(0, "");

            s.CreateAndAddLabelRow(container, "Streams");

            foreach (var si in column.MaterialStreams.Values)
            {
                if (si.StreamBehavior == StreamInformation.Behavior.Feed)
                {
                    s.CreateAndAddDropDownRow(container, "[FEED] " + column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag,
                                         stageNames, stageIDs.IndexOf(si.AssociatedStage), (arg1, arg2) =>
                                         {
                                             si.AssociatedStage = stageIDs[arg1.SelectedIndex];
                                         });
                }
                else if (si.StreamBehavior == StreamInformation.Behavior.Sidedraw)
                {
                    s.CreateAndAddDropDownRow(container, "[SIDEDRAW] " + column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag,
                                         stageNames, stageIDs.IndexOf(si.AssociatedStage), (arg1, arg2) =>
                                         {
                                             si.AssociatedStage = stageIDs[arg1.SelectedIndex];
                                         });
                }
                else if (si.StreamBehavior == StreamInformation.Behavior.Distillate || si.StreamBehavior == StreamInformation.Behavior.OverheadVapor)
                {
                    s.CreateAndAddDropDownRow(container, "[TOP PRODUCT] " + column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag,
                                         stageNames, stageIDs.IndexOf(si.AssociatedStage), (arg1, arg2) =>
                                         {
                                             si.AssociatedStage = stageIDs[arg1.SelectedIndex];
                                         });
                }
                else if (si.StreamBehavior == StreamInformation.Behavior.BottomsLiquid)
                {
                    s.CreateAndAddDropDownRow(container, "[BOTTOMS PRODUCT] " + column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag,
                                         stageNames, stageIDs.IndexOf(si.AssociatedStage), (arg1, arg2) =>
                                         {
                                             si.AssociatedStage = stageIDs[arg1.SelectedIndex];
                                         });
                }
            }
           
            s.CreateAndAddLabelRow(container, "Side Draw Specs");
            var sdphases = new List<string>() { "L", "V" };
            foreach (var si in column.MaterialStreams.Values)
            {
                string sp = "L";
                switch (si.StreamPhase)
                {
                    case StreamInformation.Phase.L:
                        sp = "L";
                        break;
                    case StreamInformation.Phase.V:
                        sp = "V";
                        break;
                }
                if (si.StreamBehavior == StreamInformation.Behavior.Sidedraw)
                {
                    s.CreateAndAddDropDownRow(container, column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag + " / Draw Phase",
                                             sdphases, sdphases.IndexOf(sp), (arg1, arg2) =>
                                             {
                                                 switch (arg1.SelectedIndex)
                                                 {
                                                     case 0:
                                                         si.StreamPhase = StreamInformation.Phase.L;
                                                         break;
                                                     case 1:
                                                         si.StreamPhase = StreamInformation.Phase.V;
                                                         break;
                                                 }
                                             });
                    s.CreateAndAddTextBoxRow(container, nf, column.GetFlowsheet().SimulationObjects[si.StreamID].GraphicObject.Tag + " / Molar Flow (" + su.molarflow + ")",
                                             cv.ConvertFromSI(su.molarflow, si.FlowRate.Value), (arg1, arg2) =>
                                             {
                                                 if (s.IsValidDouble(arg1.Text))
                                                 {
                                                     si.FlowRate.Value = cv.ConvertToSI(su.molarflow, Double.Parse(arg1.Text));
                                                 }
                                             });
                }
            }

            s.CreateAndAddLabelRow(container, "Solver Settings");

            var methods = new string[] { "Burningham-Otto (Sum Rates)", "Napthali-Sandholm (Simultaneous Correction)" };

            if (column.SolvingMethodName.Contains("Wang")) column.SolvingMethodName = "Burningham-Otto (Sum Rates)";

            s.CreateAndAddDropDownRow(container, "Solving Method", methods.ToList(), methods.ToList().IndexOf(column.SolvingMethodName), (sender, e) =>
            {
                column.SolvingMethodName = sender.SelectedValue.ToString();
            });

            s.CreateAndAddTextBoxRow(container, "N0", "Maximum Iterations", column.MaxIterations,
            (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) column.MaxIterations = (int)sender.Text.ToDoubleFromCurrent();
            }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)column.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddTextBoxRow(container, nf, "Convergence Tolerance", column.ExternalLoopTolerance,
            (sender, e) =>
            {
                if (sender.Text.IsValidDouble())
                {
                    column.ExternalLoopTolerance = sender.Text.ToDoubleFromCurrent();
                    column.InternalLoopTolerance = sender.Text.ToDoubleFromCurrent();
                }
            }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)column.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);

        }


    }

}

