using System;
using MonoMac.Foundation;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
using System.IO;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// This is a customized <c>NSTextView</c> that provides specialized input handling for
	/// the editing of source code.
	/// </summary>
	/// <remarks>
	/// See Apple's documentation of the Cocoa Text Architecture: 
	/// https://developer.apple.com/library/mac/documentation/TextFonts/Conceptual/CocoaTextArchitecture/TextEditing/TextEditing.html#//apple_ref/doc/uid/TP40009459-CH3-SW16
	/// https://developer.apple.com/library/mac/documentation/Cocoa/Reference/ApplicationKit/Classes/NSTextView_Class/
	/// </remarks>
	[Register("SourceTextView")]
	public class SourceTextView : NSTextView
	{
		
		#region Static Constants
		/// <summary>
		/// Defines the constant Unicode value of the enter key.
		/// </summary>
		public const int EnterKey = 13;

		/// <summary>
		/// Defines the constant Unicode value of the tab key.
		/// </summary>
		public const int TabKey = 9;

		/// <summary>
		/// Defines the constant Unicode value of the shift-tab key.
		/// </summary>
		public const int ShiftTabKey = 25;
		#endregion

		#region Private Variables
		/// <summary>
		/// The current language formatter used to highlight syntax.
		/// </summary>
		private LanguageFormatter _formatter;

		/// <summary>
		/// Should the editor auto complete closures.
		/// </summary>
		private bool _completeClosures = true;

		/// <summary>
		/// Should the editor auto wrap selected text in. 
		/// </summary>
		private bool _wrapClosures = true;

		/// <summary>
		/// Should the edit select the section of text that has just been wrapped in a closure.
		/// </summary>
		private bool _selectAfterWrap = true;

		/// <summary>
		/// Should the editor provide auto completion of partial words.
		/// </summary>
		private bool _allowAutoComplete = true;

		/// <summary>
		/// Should the editor auto complete keywords as defined in the current langauge.
		/// </summary>
		private bool _autoCompleteKeywords = true;

		/// <summary>
		/// Should the editor use the default words list for auto complete.
		/// </summary>
		private bool _autoCompleteDefaultWords = true;

		/// <summary>
		/// Should the editor only use default words if the keyword list is empty.
		/// </summary>
		private bool _defaultWordsOnlyIfKeywordsEmpty = true;
		#endregion

		#region Computed Properties
		/// <summary>
		/// Gets or sets the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> used to perform
		/// syntax highlighting on this <c>NSTextView</c> containing the contents of the document being
		/// edited.
		/// </summary>
		/// <value>The <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> for the selected language.</value>
		[Export("Formatter")]
		public LanguageFormatter Formatter {
			get { return _formatter; }
			set { 
				WillChangeValue ("Formatter");
				_formatter = value; 
				DidChangeValue ("Formatter");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> allows auto complete
		/// of partial words.
		/// </summary>
		/// <value><c>true</c> if allows auto complete; otherwise, <c>false</c>.</value>
		[Export("AllowAutoComplete")]
		public bool AllowAutoComplete {
			get { return _allowAutoComplete; }
			set {
				WillChangeValue ("AllowAutoComplete");
				_allowAutoComplete = value;
				DidChangeValue ("AllowAutoComplete");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> auto completes keywords.
		/// </summary>
		/// <value><c>true</c> if auto completes keywords; otherwise, <c>false</c>.</value>
		[Export("AutoCompleteKeywords")]
		public bool AutoCompleteKeywords {
			get { return _autoCompleteKeywords; }
			set {
				WillChangeValue ("AutoCompleteKeywords");
				_autoCompleteKeywords = value;
				DidChangeValue ("AutoCompleteKeywords");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> auto completes
		/// default words.
		/// </summary>
		/// <value><c>true</c> if auto complete default words; otherwise, <c>false</c>.</value>
		[Export("AutoCompleteDefaultWords")]
		public bool AutoCompleteDefaultWords {
			get { return _autoCompleteDefaultWords; }
			set {
				WillChangeValue ("AutoCompleteDefaultWords");
				_autoCompleteDefaultWords = value;
				DidChangeValue ("AutoCompleteDefaultWords");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> 
		/// uses the default words (provided by OS X) only if keywords empty.
		/// </summary>
		/// <value><c>true</c> if use the default words only if keywords empty; otherwise, <c>false</c>.</value>
		[Export("DefaultWordsOnlyIfKeywordsEmpty")]
		public bool DefaultWordsOnlyIfKeywordsEmpty {
			get { return _defaultWordsOnlyIfKeywordsEmpty; }
			set {
				WillChangeValue ("DefaultWordsOnlyIfKeywordsEmpty");
				_defaultWordsOnlyIfKeywordsEmpty = value;
				DidChangeValue ("DefaultWordsOnlyIfKeywordsEmpty");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> complete closures.
		/// </summary>
		/// <value><c>true</c> if complete closures; otherwise, <c>false</c>.</value>
		[Export("CompleteClosures")]
		public bool CompleteClosures {
			get { return _completeClosures; }
			set {
				WillChangeValue ("CompleteClosures");
				_completeClosures = value;
				DidChangeValue ("CompleteClosures");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> wrap closures.
		/// </summary>
		/// <value><c>true</c> if wrap closures; otherwise, <c>false</c>.</value>
		[Export("WrapClosures")]
		public bool WrapClosures {
			get { return _wrapClosures; }
			set {
				WillChangeValue ("WrapClosures");
				_wrapClosures = true;
				DidChangeValue ("WrapClosures");
			}
		}

		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.TextKit.Formatter.SourceTextView"/> selects
		/// the text that has just been wrapped in a closure.
		/// </summary>
		/// <value><c>true</c> if select after wrap; otherwise, <c>false</c>.</value>
		[Export("SelectAfterWrap")]
		public bool SelectAfterWrap {
			get { return _selectAfterWrap; }
			set {
				WillChangeValue ("SelectAfterWrap");
				_selectAfterWrap = value;
				DidChangeValue ("SelectAfterWrap");
			}
		}
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> class.
		/// </summary>
		public SourceTextView ()
		{
			// Init
			Initialize();
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> class.
		/// </summary>
		/// <param name="frameRect">Frame rect.</param>
		public SourceTextView (CGRect frameRect) : base(frameRect)
		{
			// Init
			Initialize();
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> class.
		/// </summary>
		/// <param name="frameRect">Frame rect.</param>
		/// <param name="container">Container.</param>
		public SourceTextView (CGRect frameRect, NSTextContainer container) : base (frameRect, container)
		{
			// Init
			Initialize();
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> class.
		/// </summary>
		/// <param name="coder">Coder.</param>
		public SourceTextView (NSCoder coder) : base (coder)
		{
			// Init
			Initialize();
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextView"/> class.
		/// </summary>
		/// <param name="handle">Handle.</param>
		public SourceTextView (IntPtr handle) : base (handle)
		{
			// Init
			Initialize();
		}

		/// <summary>
		/// Initialize this instance.
		/// </summary>
		private void Initialize() {

			// Init
			this.Delegate = new SourceTextViewDelegate(this);

		}
		#endregion

		#region Private Methods
		/// <summary>
		/// Calculates the indent level by counting the number of tab characters
		/// at the start of the current line.
		/// </summary>
		/// <returns>The indent level as the number of tabs.</returns>
		/// <param name="line">The line of text being processed.</param>
		private int CalculateIndentLevel(string line) {
			var indent = 0;

			// Process all characters in the line
			for (int n = 0; n < line.Length; ++n) {
				var code = (int)line [n];

				// Are we on a tab character?
				if (code == TabKey) {
					++indent;
				} else {
					break;
				}
			}

			// Return result
			return indent;
		}

		/// <summary>
		/// Creates a string of n number of tab characters that will be used to keep
		/// the tab level of the current line of text.
		/// </summary>
		/// <returns>A string of n tab characters.</returns>
		/// <param name="indentLevel">The number of tab characters to insert in the string.</param>
		private string TabIndent(int indentLevel) {
			var indent = "";

			// Assemble string
			for (int n = 0; n < indentLevel; ++n) {
				indent += (char)TabKey;
			}

			// Return indention
			return indent;
		}

		/// <summary>
		/// Increases the tab indent on the given section of text.
		/// </summary>
		/// <returns>The text with the tab indent increased by one.</returns>
		/// <param name="text">The text to indent.</param>
		private string IncreaseTabIndent(string text) {
			var output = "";
			var found = false;

			// Add first intent
			output += (char)TabKey;
			for (int n = 0; n < text.Length; ++n) {
				var c = text [n];
				found = (c == Formatter.Newline || c == Formatter.LineSeparator || c == Formatter.ParagraphSeparator); 

				// Include char in output
				output += c;

				// Increase tab level?
				if (found) {
					// Yes
					output += (char)TabKey;
				}
			}

			// Return results
			return output;
		}

		/// <summary>
		/// Decreases the tab indent for the given text
		/// </summary>
		/// <returns>The text with the tab indent decreased by one.</returns>
		/// <param name="text">The text to outdent.</param>
		private string DecreaseTabIndent(string text) {
			var output = "";
			var found = false;
			var consume = true;

			// Add first intent
			for (int n = 0; n < text.Length; ++n) {
				var c = text [n];
				found = (c == Formatter.Newline || c == Formatter.LineSeparator || c == Formatter.ParagraphSeparator); 

				// Include char in output?
				if ((int)c == TabKey && consume) {
					consume = false;
				} else {
					output += c;
				}

				// Decrease tab level?
				if (found) {
					// Yes
					consume = true;
				}
			}

			// Return results
			return output;
		}
		#endregion

		#region Public Methods
		/// <summary>
		/// Indents the currently selected text.
		/// </summary>
		public void IndentText() {

			// Grab range
			var range = Formatter.FindLineBoundries(TextStorage.Value, SelectedRange);
			var line = TextStorage.Value.Substring((int)range.Location, (int)range.Length);

			// Increase tab indent
			var output = IncreaseTabIndent(line);

			// Reformat section
			TextStorage.BeginEditing();
			Replace(range, output);
			TextStorage.EndEditing ();
			SelectedRange = new NSRange(range.Location, (uint)output.Length);
			Formatter.HighlightSyntaxRegion(TextStorage.Value, SelectedRange);
		}

		/// <summary>
		/// Outdents the currently selected text.
		/// </summary>
		public void OutdentText() {

			// Grab range
			var range = Formatter.FindLineBoundries(TextStorage.Value, SelectedRange);
			var line = TextStorage.Value.Substring((int)range.Location, (int)range.Length);

			// Decrease tab indent
			var output = DecreaseTabIndent(line);

			// reformat section
			TextStorage.BeginEditing();
			Replace(range, output);
			TextStorage.EndEditing ();
			SelectedRange = new NSRange(range.Location, (uint)output.Length);
			Formatter.HighlightSyntaxRegion(TextStorage.Value, SelectedRange);
		}

		/// <summary>
		/// Performs the formatting command on the currectly selected range of text.
		/// </summary>
		/// <param name="command">The <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> to apply.</param>
		public void PerformFormattingCommand(LanguageFormatCommand command) {
			NSRange range = SelectedRange;
            // Apply to start of line?
			if (command.Postfix == "") {
				// Yes, find start
				range = Formatter.FindLineBoundries(TextStorage.Value, SelectedRange);
			}

			// Yes, get selected text
			var location = range.Location;
			var line = TextStorage.Value.Substring((int)range.Location, (int)range.Length);

			// Apply command
			var output = command.Prefix;
			output += line;
			output += command.Postfix;
			TextStorage.BeginEditing ();
			Replace(range, output);
			TextStorage.EndEditing ();
			Formatter.HighlightSyntaxRegion(TextStorage.Value, range);
		}
		#endregion

		#region Events
		/// <summary>
		/// Occurs when source cell clicked. 
		/// </summary>
		/// <remarks>NOTE: This replaces the built-in <c>CellClicked</c> event because we
		/// are providing a custom <c>NSTextViewDelegate</c> and it is unavialable.</remarks>
		public event EventHandler<NSTextViewClickedEventArgs> SourceCellClicked;

		/// <summary>
		/// Raises the source cell clicked event.
		/// </summary>
		/// <param name="sender">The controller raising the event.</param>
		/// <param name="e">Arguments defining the event.</param>
		internal void RaiseSourceCellClicked(object sender, NSTextViewClickedEventArgs e) {
			if (this.SourceCellClicked != null)
				this.SourceCellClicked (sender, e);
		}

		/// <summary>
		/// Occurs when source cell double clicked.
		/// </summary>
		/// <remarks>NOTE: This replaces the built-in <c>CellDoubleClicked</c> event because we
		/// are providing a custom <c>NSTextViewDelegate</c> and it is unavialable.</remarks>
		public event EventHandler<NSTextViewDoubleClickEventArgs> SourceCellDoubleClicked;

		/// <summary>
		/// Raises the source cell double clicked event.
		/// </summary>
		/// <param name="sender">The controller raising the event.</param>
		/// <param name="e">Arguments defining the event.</param>
		internal void RaiseSourceCellDoubleClicked(object sender, NSTextViewDoubleClickEventArgs e) {
			if (this.SourceCellDoubleClicked != null)
				this.SourceCellDoubleClicked (sender, e);
		}

		/// <summary>
		/// Occurs when source cell dragged.
		/// </summary>
		/// <remarks>NOTE: This replaces the built-in <c>DragCell</c> event because we
		/// are providing a custom <c>NSTextViewDelegate</c> and it is unavialable.</remarks>
		public event EventHandler<NSTextViewDraggedCellEventArgs> SourceCellDragged;

		/// <summary>
		/// Raises the source cell dragged event.
		/// </summary>
		/// <param name="sender">The controller raising the event.</param>
		/// <param name="e">Arguments defining the event.</param>
		internal void RaiseSourceCellDragged(object sender, NSTextViewDraggedCellEventArgs e) {
			if (this.SourceCellDragged != null)
				this.SourceCellDragged (sender, e);
		}

		/// <summary>
		/// Occurs when source selection changed.
		/// </summary>
		/// <remarks>NOTE: This replaces the built-in <c>DidChangeSelection</c> event because we
		/// are providing a custom <c>NSTextViewDelegate</c> and it is unavialable.</remarks>
		public event EventHandler SourceSelectionChanged;

		/// <summary>
		/// Raises the source selection changed event.
		/// </summary>
		/// <param name="sender">The controller raising the event.</param>
		/// <param name="e">Arguments defining the event.</param>
		internal void RaiseSourceSelectionChanged(object sender, EventArgs e) {
			if (this.SourceSelectionChanged != null)
				this.SourceSelectionChanged (sender, e);
		}

		/// <summary>
		/// Occurs when source typing attributes changed.
		/// </summary>
		/// <remarks>NOTE: This replaces the built-in <c>DidChangeTypingAttributes</c> event because we
		/// are providing a custom <c>NSTextViewDelegate</c> and it is unavialable.</remarks>
		public event EventHandler SourceTypingAttributesChanged;

		/// <summary>
		/// Raises the source typing attributes changed event.
		/// </summary>
		/// <param name="sender">The controller raising the event.</param>
		/// <param name="e">Arguments defining the event.</param>
		internal void RaiseSourceTypingAttributesChanged(object sender, EventArgs e) {
			if (this.SourceTypingAttributesChanged != null)
				this.SourceTypingAttributesChanged (sender, e);
		}
		#endregion
	}
}

