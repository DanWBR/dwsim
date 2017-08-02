using System;
using MonoMac.AppKit;
using MonoMac.Foundation;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using MonoMac.AppKit.TextKit.Formatter;

namespace SourceWriter
{
	/// <summary>
	/// Defines the View Controller for a syntax highlighting text editor view.
	/// </summary>
	public class TextEditor : NSView
	{
        		
		/// <summary>
		/// The information on the currently highlighted keyword.
		/// </summary>
		private KeywordDescriptor _keywordInfo = null;


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
		/// Gets the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> attached to this view.
		/// </summary>
		/// <value>The <see cref="AppKit.TextKit.Formatter.SourceTextView"/> used to edit source.</value>
		public SourceTextView Editor {
			get { return TextEditorView; }
		}

        MonoMac.AppKit.TextKit.Formatter.SourceTextView TextEditorView = new SourceTextView();

        /// <summary>
        /// Gets or sets the text for the <c>NSTextView</c> being used as a text editor
        /// </summary>
        /// <value>The string content of the <c>NSTextView</c>.</value>
        public string Text {
			get { return TextEditorView.TextStorage.Value; }
			set {
                TextEditorView.Value = value;
				Formatter.Reformat ();
				DocumentEdited = false;
			}
		}

		/// <summary>
		/// Gets or sets the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> used to perform
		/// syntax highlighting on the <c>NSTextView</c> containing the contents of the document being
		/// edited.
		/// </summary>
		/// <value>The <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> for the selected language.</value>
		public LanguageFormatter Formatter {
			get { return TextEditorView.Formatter; }
			set { TextEditorView.Formatter = value; }
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
		/// Gets or sets the info about the currently selected keyword.
		/// </summary>
		/// <value>The keyword info.</value>
		public KeywordDescriptor KeywordInfo { 
			get { return _keywordInfo; }
			set {
				_keywordInfo = value;
			}
		}

		/// <summary>
		/// Gets or sets the keyword that is currently selected.
		/// </summary>
		/// <value>The keyword.</value>
		public string Keyword = "";
        
        public TextEditor()
        {
            SetLanguageToCSharp();
            ConfigureEditor();
            Initialize();
        }

        public TextEditor(MonoMac.CoreGraphics.CGRect frameRect) : base(frameRect)
        {
            TextEditorView = new SourceTextView(frameRect);
            SetLanguageToCSharp();
            ConfigureEditor();
            Initialize();
        }

        /// <summary>
        /// This method is called once the view controller has been inflated from the 
        /// Storyboard file. 
        /// </summary>
        public void Initialize ()
		{
            // Configure editor from user preferences
            //ConfigureEditor ();

            TextEditorView.Bounds = Bounds;

            // Highligh the syntax of the text after an edit has been made
            TextEditorView.TextStorage.TextStorageDidProcessEditing += (sender, e) => {
				DocumentEdited = true;
				Formatter.HighlightSyntaxRegion(TextEditorView.TextStorage.Value, TextEditorView.TextStorage.EditedRange);
			};

            // If the text selection or cursor location changes, attempt to display the Tool Tip
            // for any keyword defined in the current language being syntax highlighted
            TextEditorView.SourceSelectionChanged += (sender, e) => {
				var range = Formatter.FindWordBoundries(TextEditorView.TextStorage.Value, TextEditorView.SelectedRange);
				var word = TextEditorView.TextStorage.Value.Substring((int)range.Location, (int)range.Length);

                // Found a keyword?
                KeywordDescriptor info;
                if (Formatter.Language.Keywords.TryGetValue(word, out info))
                {

                    // Display the tool tip
                    //StatusText.StringValue = string.Format("{0}: {1}", info.Type, word);
                    //StatusText.TextColor = info.Color;
                    //StatusDesc.StringValue = info.Tooltip;
                    Keyword = word;
                    KeywordInfo = info;
                }
                else {
                    // Display the currently selected text
                    //StatusText.StringValue = "Selection:";
                    //StatusText.TextColor = NSColor.Black;
                    //StatusDesc.StringValue = word;
                    Keyword = "";
                    KeywordInfo = null;
                }
            };
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
            TextEditorView.AutomaticLinkDetectionEnabled = true;
            TextEditorView.AutomaticQuoteSubstitutionEnabled = false;
            TextEditorView.AutomaticDashSubstitutionEnabled = false;
            TextEditorView.AutomaticDataDetectionEnabled = false;
            TextEditorView.AutomaticTextReplacementEnabled = false;
            TextEditorView.SmartInsertDeleteEnabled = false;
            TextEditorView.ContinuousSpellCheckingEnabled = false;
            TextEditorView.AutomaticSpellingCorrectionEnabled = false;
            TextEditorView.GrammarCheckingEnabled = false;

            // Editor Preferences
            TextEditorView.RichText = true;
            //TextEditorView.ImportsGraphics = App.Preferences.AllowGraphics;
            //TextEditorView.AllowsImageEditing = App.Preferences.AllowImageEditing;
            //TextEditorView.AllowsDocumentBackgroundColorChange = App.Preferences.AllowBackgroundColor;
            //TextEditorView.BackgroundColor = App.Preferences.EditorBackgroundColor;
            TextEditorView.UsesFontPanel = true;
            TextEditorView.UsesRuler = true;
            TextEditorView.CompleteClosures = true;
            //TextEditorView.WrapClosures = App.Preferences.WrapClosures;
            //TextEditorView.SelectAfterWrap = App.Preferences.SelectAfterWrap;



            // Auto Complete Preferences
            TextEditorView.AllowAutoComplete = true;
            TextEditorView.AutoCompleteKeywords = true;
            TextEditorView.AutoCompleteKeywords = true;
            TextEditorView.DefaultWordsOnlyIfKeywordsEmpty = true;

        }

        /// <summary>
        /// Sets the language to C sharp.
        /// </summary>
        public void SetLanguageToCSharp() {
			//UnpopulateFormattingMenu ();
			Formatter.Language = new CSharpDescriptor ();
			//StatusLanguage.StringValue = "C# Code";
			//PopulateFormattingMenu ();
		}

		/// <summary>
		/// Sets the language to HTML.
		/// </summary>
		//public void SetLanguageToHTML() {
		//	UnpopulateFormattingMenu ();
		//	Formatter.Language = new HTMLDescriptor ();
		//	StatusLanguage.StringValue = "HTML";
		//	PopulateFormattingMenu ();
		//}

		/// <summary>
		/// Sets the language to MarkDown.
		/// </summary>
		public void SetLanguageToMarkDown() {
			//UnpopulateFormattingMenu ();
			Formatter.Language = new MarkDownDescriptor ();
			//StatusLanguage.StringValue = "MarkDown";
			//PopulateFormattingMenu ();
		}

		/// <summary>
		/// Sets the language to XML.
		/// </summary>
		//public void SetLanguageToXML() {
		//	UnpopulateFormattingMenu ();
		//	Formatter.Language = new XMLDescriptor ();
		//	StatusLanguage.StringValue = "XML";
		//	PopulateFormattingMenu ();
		//}

		/// <summary>
		/// Attempts to set the syntax highlighting language based on
		/// the extension of the file being opened.
		/// </summary>
		/// <param name="path">Path.</param>
		public void SetLanguageFromPath(string path) {

			// Save path
			FilePath = path;

			// Attempt to set the language based on the file
			// extension
			if (path.EndsWith (".cs")) {
				SetLanguageToCSharp ();
			} else if (path.EndsWith (".htm")) {
				//SetLanguageToHTML ();
			} else if (path.EndsWith (".md")) {
				SetLanguageToMarkDown ();
			} else if (path.EndsWith (".xml")) {
				//SetLanguageToXML ();
			}

		}

		/// <summary>
		/// Prints the document that is currently being edited.
		/// </summary>
		/// <param name="info">A <c>NSPrintInfo</c> object defining the page layout to use
		/// while printing.</param>
		public void PrintDocument(NSPrintInfo info) {

            // Configure print job
            TextEditorView.Print (this);
		}
        
	}
}
