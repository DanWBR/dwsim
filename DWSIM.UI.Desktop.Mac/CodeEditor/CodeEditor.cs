using DWSIM.Drawing.SkiaSharp;
using AppKit;
using CoreGraphics;
using SkiaSharp;
using System;
using DWSIM.UI.Controls;
using System.Collections.Generic;

namespace DWSIM.UI.Desktop.Mac
{
    public class CodeEditorControlHandler : Eto.Mac.Forms.MacView<NSView, CodeEditorControl, CodeEditorControl.ICallback>, CodeEditorControl.ICodeEditor
    {

        //DWSIM.UI.Controls.Mac.TextEditor te;
        NSTextView te;
        NSScrollView sv;

        public CodeEditorControlHandler()
        {
                //te = new Controls.Mac.TextEditor();
                te = new NSTextView();
                te.Font = NSFont.FromFontName("Menlo", 11.0f);
                te.Editable = true;
                te.Selectable = true;
                te.AutoresizingMask = NSViewResizingMask.WidthSizable;
                te.MaxSize = new CGSize(1000, 10000000);
                //te.Formatter = new LanguageFormatter(te, new PythonDescriptor());
                sv = new NSScrollView { AutoresizesSubviews = true, BorderType = NSBorderType.NoBorder, HasVerticalScroller = true, HasHorizontalScroller = true, AutoresizingMask = NSViewResizingMask.WidthSizable };
                var cv = new NSClipView { AutoresizesSubviews = true };
                cv.DocumentView = te;
                sv.ContentView = cv;
                this.Control = sv;
                te.BecomeFirstResponder();
        }

        public override Eto.Drawing.Color BackgroundColor
        {
            get
            {
                return Eto.Drawing.Colors.White;
            }
            set
            {
                return;
            }
        }

        public override NSView ContainerControl
        {
            get
            {
                return Control;
            }
        }

        public override bool Enabled { get; set; }

        public List<int> GetBookmarks()
        {
            List<int> bookmarks = new List<int>();
            return bookmarks;
        }

        public string Text
        {
            get
            {
                if (te != null) { return te.Value; } else { return ""; }
            }
            set
            {
                if (te != null) {
                    te.Value = "";
                    te.Value = value;
                }
            }
        }
    }

}
