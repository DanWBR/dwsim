using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;

using nfloat = System.Double;
using nint = System.Int64;
using nuint = System.UInt64;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// The <see cref="AppKit.TextKit.Formatter.SourceTextViewDelegate"/> is used to respond to events that
	/// occur on a <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.
	/// </summary>
	public class SourceTextViewDelegate : NSTextViewDelegate
	{
		#region Computed Properties
		/// <summary>
		/// Gets or sets the text editor.
		/// </summary>
		/// <value>The <see cref="AppKit.TextKit.Formatter.SourceTextView"/> this delegate is attached to.</value>
		public SourceTextView TextEditor { get; set;}
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.SourceTextViewDelegate"/> class.
		/// </summary>
		/// <param name="textEditor">Text editor.</param>
		public SourceTextViewDelegate (SourceTextView textEditor)
		{
			// Initialize
			this.TextEditor = textEditor;
		}
        #endregion

        #region Override Methods
        /// <summary>
        /// Based on the user preferences set on the parent <see cref="AppKit.TextKit.Formatter.SourceTextView"/>, this
        /// method returns the available list of partial word completions.
        /// </summary>
        /// <returns>The list of word completions that will be presented to the user.</returns>
        /// <param name="textView">The source <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.</param>
        /// <param name="words">A list of default words automatically provided by OS X in the user's language.</param>
        /// <param name="charRange">The cursor location where the partial word exists.</param>
        /// <param name="index">The word that should be selected when the list is displayed (usually 0 meaning
        /// the first item in the list). Pass -1 for no selected items.</param>
        public override string[] GetCompletions(NSTextView textView, string[] words, NSRange charRange, int index)
		{
			List<string> completions = new List<string> ();

			// Is auto complete enabled?
			if (TextEditor.AllowAutoComplete) {
				// Use keywords in auto complete?
				if (TextEditor.AutoCompleteKeywords) {
					// Yes, grab word being expanded
					var range = TextEditor.Formatter.FindWordBoundries (TextEditor.TextStorage.Value, charRange);
					var word = TextEditor.TextStorage.Value.Substring ((int)range.Location, (int)range.Length);

					// Scan the keywords for the a possible match
					foreach (string keyword in TextEditor.Formatter.Language.Keywords.Keys) {
						// Found?
						if (keyword.Contains (word)) {
							completions.Add (keyword);
						}
					}
				}

				// Use default words?
				if (TextEditor.AutoCompleteDefaultWords) {
					// Only if keywords list is empty?
					if (TextEditor.DefaultWordsOnlyIfKeywordsEmpty) {
						if (completions.Count == 0) {
							// No keywords, add defaults
							completions.AddRange (words);
						}
					} else {
						// No, always include default words
						completions.AddRange (words);
					}
				}
			}

			// Return results
			return completions.ToArray();
		}

		/// <summary>
		/// Called when the cell is clicked.
		/// </summary>
		/// <param name="textView">The <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.</param>
		/// <param name="cell">The cell being acted upon.</param>
		/// <param name="cellFrame">The onscreen frame of the cell.</param>
		/// <param name="charIndex">The index of the character clicked.</param>
		/// <remarks>
		/// Because a custom <c>Delegate</c> has been attached to the <c>NSTextView</c>, the normal events
		/// will not work so we are using this method to call custom <see cref="AppKit.TextKit.Formatter.SourceTextView"/>
		/// events instead.
		/// </remarks>
		public override void CellClicked (NSTextView textView, NSTextAttachmentCell cell, CGRect cellFrame, uint charIndex)
		{
			// Pass through to Text Editor event
			TextEditor.RaiseSourceCellClicked(TextEditor, new NSTextViewClickedEventArgs(cell, cellFrame, charIndex));
		}

        /// <summary>
        /// Called when the cell is double-clicked.
        /// </summary>
        /// <param name="textView">The <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.</param>
        /// <param name="cell">The cell being acted upon.</param>
        /// <param name="cellFrame">The onscreen frame of the cell.</param>
        /// <param name="charIndex">The index of the character clicked.</param>
        /// <remarks>
        /// Because a custom <c>Delegate</c> has been attached to the <c>NSTextView</c>, the normal events
        /// will not work so we are using this method to call custom <see cref="AppKit.TextKit.Formatter.SourceTextView"/>
        /// events instead.
        /// </remarks>
        public override void CellDoubleClicked(NSTextView textView, NSTextAttachmentCell cell, CGRect cellFrame, uint charIndex)
        {
			// Pass through to Text Editor event
			TextEditor.RaiseSourceCellDoubleClicked(TextEditor, new NSTextViewDoubleClickEventArgs(cell, cellFrame, charIndex));
		}

		/// <summary>
		/// Called when the cell is dragged.
		/// </summary>
		/// <param name="textView">The <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.</param>
		/// <param name="cell">The cell being acted upon.</param>
		/// <param name="cellFrame">The onscreen frame of the cell.</param>
		/// <param name="theevent">An event defining the drag operation.</param>
		/// <remarks>
		/// Because a custom <c>Delegate</c> has been attached to the <c>NSTextView</c>, the normal events
		/// will not work so we are using this method to call custom <see cref="AppKit.TextKit.Formatter.SourceTextView"/>
		/// events instead.
		/// </remarks>
		public override void DraggedCell (NSTextView view, NSTextAttachmentCell cell, CGRect rect, NSEvent theevent)
		{
			// Pass through to Text Editor event
			TextEditor.RaiseSourceCellDragged(TextEditor, new NSTextViewDraggedCellEventArgs(cell, rect, theevent));
		}

		/// <summary>
		/// Called when the text selection has changed.
		/// </summary>
		/// <param name="notification">A notification defining the change.</param>
		/// <remarks>
		/// Because a custom <c>Delegate</c> has been attached to the <c>NSTextView</c>, the normal events
		/// will not work so we are using this method to call custom <see cref="AppKit.TextKit.Formatter.SourceTextView"/>
		/// events instead.
		/// </remarks>
		public override void DidChangeSelection (NSNotification notification)
		{
			// Pass through to Text Editor event
			TextEditor.RaiseSourceSelectionChanged(TextEditor, EventArgs.Empty);
		}

		/// <summary>
		/// Called when the typing attributes has changed.
		/// </summary>
		/// <param name="notification">A notification defining the change.</param>
		/// <remarks>
		/// Because a custom <c>Delegate</c> has been attached to the <c>NSTextView</c>, the normal events
		/// will not work so we are using this method to call custom <see cref="AppKit.TextKit.Formatter.SourceTextView"/>
		/// events instead.
		/// </remarks>
		public override void DidChangeTypingAttributes (NSNotification notification)
		{
			// Pass through to Text Editor event
			TextEditor.RaiseSourceTypingAttributesChanged(TextEditor, EventArgs.Empty);
		}
		#endregion
	}
}

