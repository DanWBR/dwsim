using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// The <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> defines a formatting command that can
	/// be added to a <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/>. When a document editor is using
	/// the given <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/>, the 
	/// <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/>s it defines will be added to the Formatting Menu.
	/// </summary>
	public class LanguageFormatCommand : NSObject
	{
		#region Computed Properties
		/// <summary>
		/// Gets or sets the title that will appear in the Formatting Menu.
		/// </summary>
		/// <value>The title.</value>
		public string Title { get; set; } = "";

		/// <summary>
		/// Gets or sets the prefix that will be added to the start of the line (if no <c>Postfix</c> has been defines),
		/// or that will be inserted to the start of the current selected text in the document editor.
		/// </summary>
		/// <value>The prefix.</value>
		public string Prefix { get; set; } = "";

		/// <summary>
		/// Gets or sets the postfix that will added to the end of the selected text in the document editor. If empty
		/// (""), the <c>Prefix</c> will be inserted at the start of the line that the cursor is on.
		/// </summary>
		/// <value>The postfix.</value>
		public string Postfix { get; set; } = "";

		/// <summary>
		/// Gets or sets the sub <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> commands that will be
		/// displayed under this item in the Formatting Menu.
		/// </summary>
		/// <value>The sub commands.</value>
		public List<LanguageFormatCommand> SubCommands { get; set; } = new List<LanguageFormatCommand>();
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> class.
		/// </summary>
		public LanguageFormatCommand () {

		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> class.
		/// </summary>
		/// <param name="title">The title for the menu item.</param>
		public LanguageFormatCommand (string title)
		{
			// Initialize
			this.Title = title;
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> class.
		/// </summary>
		/// <param name="title">The title for the menu item.</param>
		/// <param name="prefix">The prefix to insert.</param>
		public LanguageFormatCommand (string title, string prefix)
		{
			// Initialize
			this.Title = title;
			this.Prefix = prefix;
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> class.
		/// </summary>
		/// <param name="title">The title for the menu item.</param>
		/// <param name="prefix">The prefix to insert.</param>
		/// <param name="postfix">The postfix to insert.</param>
		public LanguageFormatCommand (string title, string prefix, string postfix)
		{
			// Initialize
			this.Title = title;
			this.Prefix = prefix;
			this.Postfix = postfix;
		}
		#endregion
	}
}

