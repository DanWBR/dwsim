using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Eto.Forms;
using Eto.Drawing;

using System.IO;

using DWSIM.Interfaces;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using System.Globalization;
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors
{
    public class ScriptManager : ScriptManagerBase
    {

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        private DocumentControl tabScripts;

        public ScriptManager(DWSIM.UI.Desktop.Shared.Flowsheet fs): base()
        {
            Flowsheet = fs;
            Init();
        }

        void Init()
        {

            var topcontainer = new TableLayout { Spacing = new Size(5, 5), Padding = new Padding(5) };

            tabScripts = new DocumentControl() { AllowReordering = true  };

            var btnNew = new Button { Text = "New Script" };
            btnNew.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnNew.Click += (sender, e) =>
            {
                try {
                    var script = new DWSIM.FlowsheetSolver.Script { ID = Guid.NewGuid().ToString(), Title = "Script" + (Flowsheet.Scripts.Count + 1).ToString() };
                    Flowsheet.Scripts.Add(script.ID, script);
                    AddScriptEditor(script);
                }
                catch (Exception ex) {
                    MessageBox.Show(ex.ToString());
                }
            };

            topcontainer.Rows.Add(new TableRow(btnNew, null));

            Rows.Add(topcontainer);
            Rows.Add(tabScripts);

        }

        void AddScriptEditor(DWSIM.FlowsheetSolver.Script script)
        {
            if (Application.Instance.Platform.IsWpf)
            {
                var si = new ScriptItem_WPF(Flowsheet, script.ID);
                var tabc = new DocumentPage { Tag = script.ID, Text = script.Title, Closable = true };
                si.ChangeNameCallback += (s) => tabc.Text = s;
                tabc.Closed += (sender, e) => {
                    if (MessageBox.Show(this, "Confirm removal?", "Delete Script", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                    {
                        Flowsheet.Scripts.Remove((string)tabScripts.SelectedPage.Tag);
                        tabScripts.Pages.Remove(tabScripts.SelectedPage);
                    }
                    else {
                        tabScripts.Pages.Insert(0, tabc);
                    }
                };
                tabc.Content = si;
                tabScripts.Pages.Add(tabc);
            }
            else
            {
                var si = new ScriptItem_GTK(Flowsheet, script.ID);
                var tabc = new DocumentPage { Tag = script.ID, Text = script.Title, Closable = true };
                si.ChangeNameCallback += (s) => tabc.Text = s;
                tabc.Closed += (sender, e) =>
                {
                    if (MessageBox.Show(this, "Confirm removal?", "Delete Script", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                    {
                        Flowsheet.Scripts.Remove((string)tabScripts.SelectedPage.Tag);
                        tabScripts.Pages.Remove(tabScripts.SelectedPage);
                    }
                    else
                    {
                        tabScripts.Pages.Insert(0, tabc);
                    }
                };
                tabc.Content = si;
                tabScripts.Pages.Add(tabc);
            }
        }

        public override void UpdateList()
        {
            tabScripts.Pages.Clear();
            foreach (var script in Flowsheet.Scripts.Values)
            {
                AddScriptEditor((DWSIM.FlowsheetSolver.Script)script);
            }
        }

        public override void UpdateScripts()
        {
            if (tabScripts.Pages.Count > 0)
            {
                Flowsheet.ShowMessage("Storing updated scripts for saving...", IFlowsheet.MessageType.Information);
            }
            foreach (var tabpage in tabScripts.Pages)
            {
                var script = Flowsheet.Scripts[(string)tabpage.Tag];
                if (Application.Instance.Platform.IsWpf)
                {
                    script.ScriptText = ((ScriptItem_WPF)(tabpage.Content)).txtScript.Text;
                }
                else
                {
                    script.ScriptText = ((ScriptItem_GTK)(tabpage.Content)).txtScript.ScriptText;
                }
            }
        }

    }
}
