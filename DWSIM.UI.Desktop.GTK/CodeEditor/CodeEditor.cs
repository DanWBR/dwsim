using DWSIM.UI.Controls;

namespace DWSIM.UI.Desktop.GTK
{
    public class CodeEditorControlHandler : Eto.GtkSharp.Forms.GtkControl<Gtk.Widget, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        Mono.TextEditor.TextEditor te;

        public CodeEditorControlHandler()
        {
            te = new Mono.TextEditor.TextEditor();
            te.Document.SyntaxMode = Mono.TextEditor.Highlighting.SyntaxModeService.GetSyntaxMode(te.Document, "text/x-csharp");
            //te.ColorStyle = Mono.TextEditor.Highlighting.SyntaxModeService.GetColorStyle("Light");
            var scroll = new Gtk.ScrolledWindow();
            scroll.BorderWidth = 1;
            scroll.Add(te);
            this.Control = scroll;
        }

        public override string Text
        {
            get
            {
                return te.Text;
            }

            set
            {
                te.Text = value;
            }
        }

    }

}
