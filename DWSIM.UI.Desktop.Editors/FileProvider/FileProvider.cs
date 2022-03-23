using Eto.Drawing;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Editors
{
    public class FileExplorerControl : TableLayout
    {
        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        private WebView Viewer;
        private ListBox ListBox1;
        private Label LabelSize;
        private Button btnImport, btnExport, btnDelete;

        private Shared.Flowsheet Flowsheet;
        private string TempDir;
        private bool IsLoaded = false;

        public FileExplorerControl(Shared.Flowsheet fs) : base()
        {
            Flowsheet = fs;
            Padding = new Padding(5);
            Spacing = new Size(5, 5);
        }

        public void Init()
        {

            TempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
            Directory.CreateDirectory(TempDir);

            UnLoad += (s, e) =>
            {
                try
                {
                    Directory.Delete(TempDir, true);
                }
                catch
                {
                }
            };

            var tl2 = new TableLayout { Spacing = new Size(10, 10) };

            btnImport = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Import Files", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-import.png", this.GetType().Assembly)).WithSize(16, 16) };
            btnExport = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Export Selected File", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-export.png", this.GetType().Assembly)).WithSize(16, 16) };
            btnDelete = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Delete Selected File", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-delete2.png", this.GetType().Assembly)).WithSize(16, 16) };

            ListBox1 = new ListBox();

            if (Application.Instance.Platform.IsGtk)
            {
                btnImport.Size = new Size(30, 30);
                btnExport.Size = new Size(30, 30);
                btnDelete.Size = new Size(30, 30);
            }

            var menu1 = new StackLayout
            {
                Items = { btnImport, btnExport, btnDelete },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Bottom,
                Padding = 5,
                Height = 34
            };
            tl2.Rows.Add(new TableRow(menu1));

            tl2.Rows.Add(new TableRow(ListBox1) { ScaleHeight = true });

            LabelSize = new Label();

            tl2.Rows.Add(new TableRow(LabelSize));

            tl2.Padding = new Padding(5, 5, 5, 5);
            tl2.Spacing = new Size(0, 0);

            var splitpanel = new Splitter();

            splitpanel.Panel1 = tl2;

            if (!Application.Instance.Platform.IsGtk)
            {
                Viewer = new WebView();
                splitpanel.Panel2 = Viewer;
            }

            splitpanel.FixedPanel = SplitterFixedPanel.Panel1;
            splitpanel.Panel1.Width = 300;

            Rows.Add(new TableRow(splitpanel));

            ListBox1.SelectedIndexChanged += (s, e) =>
            {
                if (ListBox1.SelectedIndex < 0) return;
                Flowsheet.RunCodeOnUIThread(() =>
                {
                    if (IsLoaded)
                    {
                        var provider = Flowsheet.FileDatabaseProvider;
                        if (provider.CheckIfExists(ListBox1.SelectedValue.ToString()))
                        {
                            try
                            {
                                string TempFilePath = Path.Combine(TempDir, ListBox1.SelectedValue.ToString());
                                provider.ExportFile(ListBox1.SelectedValue.ToString(), TempFilePath);
                                if (Viewer != null) Viewer.Url = new Uri(TempFilePath);
                            }
                            catch (Exception ex)
                            {
                                MessageBox.Show(ListBox1.SelectedValue.ToString() + ": " + ex.Message, "Error",
                                    MessageBoxButtons.OK, MessageBoxType.Error);
                            }
                        }
                    }
                });
            };

            btnExport.Click += (s, e) =>
            {
                if (ListBox1.SelectedIndex >= 0)
                {
                    var sfd1 = new SaveFileDialog();
                    sfd1.FileName = ListBox1.SelectedValue.ToString();
                    sfd1.Title = "Export File";
                    sfd1.Filters.Add(new FileFilter("All Files", new[] { "*.*" }));
                    if (sfd1.ShowDialog(this) == DialogResult.Ok)
                    {
                        var provider = Flowsheet.FileDatabaseProvider;
                        try
                        {
                            provider.ExportFile(ListBox1.SelectedValue.ToString(), sfd1.FileName);
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(ListBox1.SelectedValue.ToString() + ": " + ex.Message, "Error",
                                MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                    }
                }
            };

            btnImport.Click += (s, e) =>
            {
                var ofd1 = new OpenFileDialog();
                ofd1.Title = "Import Files";
                ofd1.Filters.Add(new FileFilter("All Supported Files", new[] { "*.pdf", "*.jpg", "*.png", "*.txt", "*.py", "*.html",
                    "*.dwxmz", "*.dwxml", "*.xml", "*.json","*.dwcsd2","*.dwrsd2","*.xlsx","*.xls","*.pxml" }));
                if (ofd1.ShowDialog(this) == DialogResult.Ok)
                {
                    var provider = Flowsheet.FileDatabaseProvider;
                    foreach (var file in ofd1.Filenames)
                    {
                        try
                        {
                            provider.PutFile(file);
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(file + ": " + ex.Message, "Error",
                                MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                    }
                    Flowsheet.RunCodeOnUIThread(() =>
                    {
                        ListFiles();
                        UpdateSize();
                    });
                }
            };

            btnDelete.Click += (s, e) =>
            {
                if (ListBox1.SelectedIndex >= 0)
                {
                    if ((MessageBox.Show("Confirm Operation?", "Remove File", MessageBoxButtons.YesNo, MessageBoxType.Question) == DialogResult.Yes))
                    {
                        var provider = Flowsheet.FileDatabaseProvider;
                        try
                        {
                            provider.DeleteFile(ListBox1.SelectedValue.ToString());
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(ListBox1.SelectedValue.ToString() + ": " + ex.Message, "Error",
                                MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                        Flowsheet.RunCodeOnUIThread(() =>
                        {
                            ListFiles();
                            UpdateSize();
                        });
                    }

                }

            };

            IsLoaded = true;

            UpdateSize();
            ListFiles();

        }

        public void UpdateSize()
        {
            Flowsheet.RunCodeOnUIThread(() =>
            {
                LabelSize.Text = string.Format("File Database Size: {0} KB", Flowsheet.FileDatabaseProvider.GetSizeinKB());
            });
        }

        public void ListFiles()
        {
            var provider = Flowsheet.FileDatabaseProvider;
            var files = provider.GetFiles();
            ListBox1.Items.Clear();
            foreach (var item in files)
            {
                ListBox1.Items.Add(item);
            }

        }

    }
}
