using DWSIM.UI.Controls;

namespace DWSIM.UI.Desktop.GTK
{
    public class CodeEditorControlHandler : Eto.GtkSharp.Forms.GtkControl<Gtk.Widget, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        Mono.TextEditor.TextEditor te;

        public CodeEditorControlHandler()
        {
            te = new Mono.TextEditor.TextEditor();
            this.Control = te;
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
