using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;
using MarkdownSharp;

namespace MonoMac.AppKit.TextKit.Formatter
{
    /// <summary>
    /// Defines how MarkDown should be colorized using a <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.
    /// </summary>
    /// <remarks>
    /// This is a simplified, sample format provided as an example only. The <c>HighlightSyntaxRegion</c> method
    /// of the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> would need to be modified to properly format
    /// MarkDown Keywords as the current version was designed for languages like C#, JavaScript or Visual Basic.
    /// </remarks>
    public class MarkDownDescriptor : LanguageDescriptor
    {
        #region Private Variables
        /// <summary>
        /// A private instance of the MarkDown Processor.
        /// </summary>
        private Markdown md = new Markdown();
        #endregion

        #region Computed Properties
        /// <summary>
        /// Gets the language identifier.
        /// </summary>
        /// <value>The language identifier.</value>
        public override string LanguageIdentifier
        {
            get { return "MarkDown"; }
        }

        /// <summary>
        /// Gets or sets the language separators for MarkDown.
        /// </summary>
        /// <value>The language separators.</value>
        public override char[] LanguageSeparators { get; set; }

        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.MarkDownDescriptor"/> class.
        /// </summary>
        public MarkDownDescriptor()
        {
            LanguageSeparators = new char[] { '=', '+', '-', '*', '/', '%', '&', '<', '>', ';', ':', '^', '!', '~', '?', '|', ',', '"', '\'', '(', ')', '[', ']', '{', '}' };
        }
        #endregion

        #region Override Methods
        /// <summary>
        /// Define this instance.
        /// </summary>
        public override void Define()
        {
            base.Define();

            // Keywords
            // Keywords.Add("", new KeywordDescriptor(KeywordType.Keyword, KeywordColor, ""));

            // Parameters
            // Keywords.Add("", new KeywordDescriptor(KeywordType.MethodParameters, MethodParameterColor, ""));

            // Define HTML formats
            Formats.Add(new FormatDescriptor("<!--", "-->", CommentColor));
            Formats.Add(new FormatDescriptor("\"", "\"", StringLiteralColor));
            Formats.Add(new FormatDescriptor("<code>", "</code>", StatementColor));
            Formats.Add(new FormatDescriptor("`", "`", StatementColor));
            Formats.Add(new FormatDescriptor("&", ";", PreprocessorDirectiveColor));
            Formats.Add(new FormatDescriptor("<", ">", KeywordColor));

            // Define MarkDown formats
            Formats.Add(new FormatDescriptor("#", TypeColor));
            Formats.Add(new FormatDescriptor(">", ValueTypeColor));
            Formats.Add(new FormatDescriptor("[", "]", ReferenceTypeColor));
            Formats.Add(new FormatDescriptor("(", ")", AccessModifierColor));
            Formats.Add(new FormatDescriptor("**", "**", ModifierColor));
            Formats.Add(new FormatDescriptor("_", "_", SelectionStatementColor));

            // Define additional closures
            Closures.Add(new LanguageClosure('*'));
            Closures.Add(new LanguageClosure('_'));
            Closures.Add(new LanguageClosure('`'));

            // Define formatting commands
            FormattingCommands.Add(new LanguageFormatCommand("Stong", "**", "**"));
            FormattingCommands.Add(new LanguageFormatCommand("Emphasize", "_", "_"));
            FormattingCommands.Add(new LanguageFormatCommand("Inline Code", "`", "`"));
            FormattingCommands.Add(new LanguageFormatCommand("Code Block", "```\n", "\n```"));
            FormattingCommands.Add(new LanguageFormatCommand("Comment", "<!--", "-->"));
            FormattingCommands.Add(new LanguageFormatCommand());
            FormattingCommands.Add(new LanguageFormatCommand("Unordered List", "* "));
            FormattingCommands.Add(new LanguageFormatCommand("Ordered List", "1. "));
            FormattingCommands.Add(new LanguageFormatCommand("Block Quote", "> "));
            FormattingCommands.Add(new LanguageFormatCommand());

            var Headings = new LanguageFormatCommand("Headings");
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 1", "# "));
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 2", "## "));
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 3", "### "));
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 4", "#### "));
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 5", "##### "));
            Headings.SubCommands.Add(new LanguageFormatCommand("Heading 6", "###### "));
            FormattingCommands.Add(Headings);

            FormattingCommands.Add(new LanguageFormatCommand());
            FormattingCommands.Add(new LanguageFormatCommand("Link", "[", "]()"));
            FormattingCommands.Add(new LanguageFormatCommand("Image", "![](", ")"));
            FormattingCommands.Add(new LanguageFormatCommand("Image Link", "[ ![](", ")](LinkImageHere)"));
        }

        /// <summary>
        /// Formats the passed in string of text for previewing.
        /// </summary>
        /// <returns>The string formatted for preview.</returns>
        /// <param name="text">Text.</param>
        public override string FormatForPreview(string text)
        {
            return md.Transform(text);
        }
        #endregion
    }
}

