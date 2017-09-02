using System;
using MonoMac.AppKit;
using MonoMac.Foundation;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using MonoMac.AppKit.TextKit.Formatter;

namespace DWSIM.UI.Controls.Mac
{
	/// <summary>
	/// Defines the View Controller for a syntax highlighting text editor view.
	/// </summary>
    public class TextEditor : SourceTextView
	{
        		
		/// <summary>
		/// Gets or sets the default language that this <see cref="SourceWriter.ViewController"/> will
		/// be editing.
		/// </summary>
		/// <value>An integer representing the default language as: 0 - C#,
		/// 1 - HTML, 2 - MarkDown, 3 - XML.</value>
		public int DefaultLanguage = 0;

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="SourceWriter.ViewController"/> document 
		/// has been edited.
		/// </summary>
		/// <value><c>true</c> if document has been edited; otherwise, <c>false</c>.</value>
		public bool DocumentEdited {
			get { return Window.DocumentEdited; }
			set { Window.DocumentEdited = value; }
		}

        /// <summary>
        /// Gets or sets the text for the <c>NSTextView</c> being used as a text editor
        /// </summary>
        /// <value>The string content of the <c>NSTextView</c>.</value>
        public string Text {
			get { return TextStorage.Value; }
			set {
                Value = value;
				if (Formatter != null) Formatter.Reformat ();
				DocumentEdited = false;
			}
		}

		/// <summary>
		/// Gets or sets the full file path where this document was last loaded from
		/// or saved to.
		/// </summary>
		/// <value>The file path.</value>
		/// <remarks>>The path will be the empty string ("") if the document has never
		/// been saved to a file.</remarks>
		public string FilePath = "";

		/// <summary>
		/// Gets or sets the keyword that is currently selected.
		/// </summary>
		/// <value>The keyword.</value>
		public string Keyword = "";
        
        public TextEditor(): base()
        {
            ConfigureEditor();
            Initialize();
        }

        public TextEditor(MonoMac.CoreGraphics.CGRect frameRect) : base(frameRect)
        {
            ConfigureEditor();
            Initialize();
        }

        /// <summary>
        /// This method is called once the view controller has been inflated from the 
        /// Storyboard file. 
        /// </summary>
        public void Initialize ()
		{

            Formatter = new LanguageFormatter(this, new CSharpDescriptor());

            // Configure editor from user preferences
            //ConfigureEditor ();

            // Highligh the syntax of the text after an edit has been made
            TextStorage.TextStorageDidProcessEditing += (sender, e) => {
				DocumentEdited = true;
                Formatter.HighlightSyntaxRegion(TextStorage.Value, TextStorage.EditedRange);
			};

            // If the text selection or cursor location changes, attempt to display the Tool Tip
            // for any keyword defined in the current language being syntax highlighted
            //SourceSelectionChanged += (sender, e) => {
            //    var range = Formatter.FindWordBoundries(TextStorage.Value, SelectedRange);
            //    var word = TextStorage.Value.Substring((int)range.Location, (int)range.Length);

            //    // Found a keyword?
            //    KeywordDescriptor info;
            //    if (Formatter.Language.Keywords.TryGetValue(word, out info))
            //    {

            //        // Display the tool tip
            //        //StatusText.StringValue = string.Format("{0}: {1}", info.Type, word);
            //        //StatusText.TextColor = info.Color;
            //        //StatusDesc.StringValue = info.Tooltip;
            //        Keyword = word;
            //        KeywordInfo = info;
            //    }
            //    else {
            //        // Display the currently selected text
            //        //StatusText.StringValue = "Selection:";
            //        //StatusText.TextColor = NSColor.Black;
            //        //StatusDesc.StringValue = word;
            //        Keyword = "";
            //        KeywordInfo = null;
            //    }
            //};
		}
        
		/// <summary>
		/// Re-run syntax highlighting for the entire text of the document.
		/// </summary>
		/// <param name="updateLanguage">If set to <c>true</c>, the language descriptor will be reloaded as well.</param>
		public void ReformatText(bool updateLanguage) {

			// Redefine language to get any preference changes?
			if (updateLanguage) {
				Formatter.Language.Define ();
				//ConfigureEditor ();
			}

			// Re-highlight all text.
			Formatter.Reformat ();
		}

		/// <summary>
		/// Configures the editor with the current user preferences.
		/// </summary>
		public void ConfigureEditor() {

            // General Preferences
            AutomaticLinkDetectionEnabled = true;
            AutomaticQuoteSubstitutionEnabled = false;
            AutomaticDashSubstitutionEnabled = false;
            AutomaticDataDetectionEnabled = false;
            AutomaticTextReplacementEnabled = false;
            SmartInsertDeleteEnabled = false;
            ContinuousSpellCheckingEnabled = false;
            AutomaticSpellingCorrectionEnabled = false;
            GrammarCheckingEnabled = false;

            // Editor Preferences
            RichText = false;
            ImportsGraphics = false;
            //AllowsImageEditing = App.Preferences.AllowImageEditing;
            //AllowsDocumentBackgroundColorChange = App.Preferences.AllowBackgroundColor;
            //BackgroundColor = App.Preferences.EditorBackgroundColor;
            UsesFontPanel = true;
            UsesRuler = false;
            CompleteClosures = true;
            //WrapClosures = App.Preferences.WrapClosures;
            //SelectAfterWrap = App.Preferences.SelectAfterWrap;

            HorizontallyResizable = true;
            VerticallyResizable = true;
            MinSize = MonoMac.CoreGraphics.CGSize.Empty;
            MaxSize = new MonoMac.CoreGraphics.CGSize(float.MaxValue, float.MaxValue);
            TextContainer.WidthTracksTextView = true;

            // Auto Complete Preferences
            //AllowAutoComplete = true;
            //AutoCompleteKeywords = true;
            //AutoCompleteKeywords = true;
            DefaultWordsOnlyIfKeywordsEmpty = true;

        }

	}
}
