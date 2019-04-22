using System;
using System.Collections.Generic;
using Eto.Forms;
namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(ICodeEditor))]
    public class CodeEditorControl : Eto.Forms.Control
    {
        public new ICodeEditor Handler { get { return (ICodeEditor)base.Handler; } }

        public string Text
        {
            get { return Handler.Text; }
            set { Handler.Text = value; }
        }

        // interface to the platform implementations
        public interface ICodeEditor : Eto.Forms.Control.IHandler
        {

            string Text { get; set; }

            List<int> GetBookmarks();

        }

    }
}
