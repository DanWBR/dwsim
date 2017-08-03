using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;

namespace MonoMac.AppKit.TextKit.Formatter
{
    /// <summary>
    /// The <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> class uses a 
    /// <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> to provide syntax highlighting of a 
    /// <c>NSTextField</c> based on a set of <see cref="AppKit.TextKit.Formatter.KeywordDescriptor"/> and
    /// <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> objects..
    /// </summary>
    /// <remarks>
    /// Please see our Working with Text Controls Docs for more details:
    /// https://developer.xamarin.com/guides/mac/user-interface/standard-controls/#Working_with_Text_Controls
    /// </remarks>
    [Register("LanguageFormatter")]
    public class LanguageFormatter : NSObject
    {
        #region Private Variables
        /// <summary>
        /// The current language syntax highlighting descriptor.
        /// </summary>
        private LanguageDescriptor _language = new LanguageDescriptor();
        #endregion

        #region Computed Properties
        /// <summary>
        /// Gets or sets the text view that this language formatter will be performing
        /// syntax highlighting on.
        /// </summary>
        /// <value>The <c>NSTextView</c> to syntax highlight.</value>
        public NSTextView TextEditor { get; set; }

        /// <summary>
        /// Gets or sets the newline character used to define a given line of text.
        /// </summary>
        /// <value>The newline character.</value>
        public char Newline { get; set; }

        /// <summary>
        /// Gets or sets the Unitext line separator used to define a given line of text.
        /// </summary>
        /// <value>The line separator.</value>
        public char LineSeparator { get; set; }

        /// <summary>
        /// Gets or sets the Unitext paragraph separator used to define a given paragraph of text.
        /// </summary>
        /// <value>The paragraph separator.</value>
        public char ParagraphSeparator { get; set; }

        /// <summary>
        /// Gets or sets the descriptor used to define the syntax highlighting rules for a given
        /// language.
        /// </summary>
        /// <value>The <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> to syntax highlight.</value>
        public LanguageDescriptor Language
        {
            get { return _language; }
            set
            {
                _language = value;
                _language.Define();
                Reformat();
            }
        }
        #endregion

        #region Constructor
        /// <summary>
        /// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> class.
        /// </summary>
        /// <param name="textEditor">The <c>NSTextView</c> that this language formatter will syntax highlight.</param>
        /// <param name="language">The <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> defining the 
        /// language syntax highlighting rules.</param>
        public LanguageFormatter(NSTextView textEditor, LanguageDescriptor language)
        {
            Newline = '\n';
            LineSeparator = '\u2028';
            ParagraphSeparator = '\u2029';
            // initialize
            this.TextEditor = textEditor;
            this.Language = language;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Forces all of the text in the attached <c>NSTextView</c> (the <c>TextEditor</c> property) to
        /// have its syntax rehighlighted by re-running the formatter.
        /// </summary>
        public virtual void Reformat()
        {
            // Reformat all text in the view control
            var range = new NSRange(0, TextEditor.Value.Length);
            TextEditor.LayoutManager.RemoveTemporaryAttribute(NSAttributedString.ForegroundColorAttributeName, range);
            HighlightSyntaxRegion(TextEditor.Value, range);
            TextEditor.SetNeedsDisplay(TextEditor.Frame, false);
        }

        /// <summary>
        /// Determines whether the passed in character is a language separator.
        /// </summary>
        /// <returns><c>true</c> if the character is a language separator; otherwise, <c>false</c>.</returns>
        /// <param name="c">The character being tested.</param>
        public virtual bool IsLanguageSeparator(char c)
        {

            // Found separator?
            for (var n = 0; n < Language.LanguageSeparators.Length; ++n)
            {
                if (Language.LanguageSeparators[n] == c)
                    return true;
            }

            // Not found
            return false;
        }

        /// <summary>
        /// Finds the word boundries as defined by the <c>LanguageSeparators</c> in the
        /// <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> that is currently
        /// being syntax highlighted.
        /// </summary>
        /// <returns>An <c>NSRange</c> containing the starting and ending character locations
        /// of the current word.</returns>
        /// <param name="text">The string to be searched.</param>
        /// <param name="position">The <c>NSRange</c> specifying the starting location of a possible word.</param>
        public virtual NSRange FindWordBoundries(string text, NSRange position)
        {
            NSRange results = new NSRange(position.Location, 0);
            var found = false;

            // Find starting "word" boundry
            while (results.Location > 0 && !found)
            {
                var c = text[(int)results.Location - 1];
                found = char.IsWhiteSpace(c) || IsLanguageSeparator(c);
                if (!found) results.Location -= 1;
            };

            // Find ending "word" boundry
            found = false;
            while ((int)(results.Location + results.Length) < text.Length && !found)
            {
                var c = text[(int)(results.Location + results.Length)];
                found = char.IsWhiteSpace(c) || IsLanguageSeparator(c);
                if (!found) results.Length += 1;
            };

            return results;
        }

        /// <summary>
        /// Finds the line boundries as defined by the <c>NewLine</c>, <c>LineSeparator</c>
        /// and <c>ParagraphSeparator</c> characters.
        /// </summary>
        /// <returns>An <c>NSRange</c> containing the starting and ending character locations
        /// of the current line of text.</returns>
        /// <param name="text">The string to be searched.</param>
        /// <param name="position">The <c>NSRange</c> specifying the starting location of a possible 
        /// line of text.</param>
        public virtual NSRange FindLineBoundries(string text, NSRange position)
        {
            NSRange results = position;
            var found = false;

            // Find starting line boundry
            while (results.Location > 0 && !found)
            {
                var c = text[(int)results.Location - 1];
                found = (c == Newline || c == LineSeparator || c == ParagraphSeparator);
                if (!found) results.Location -= 1;
            };

            // Find ending line boundry
            found = false;
            while ((int)(results.Location + results.Length) < text.Length && !found)
            {
                var c = text[(int)(results.Location + results.Length)];
                found = (c == Newline || c == LineSeparator || c == ParagraphSeparator);
                if (!found) results.Length += 1;
            };

            return results;
        }

        /// <summary>
        /// Finds the start of line for the given location in the text as defined by the <c>NewLine</c>, <c>LineSeparator</c>
        /// and <c>ParagraphSeparator</c> characters.
        /// </summary>
        /// <returns>A <c>NSRange</c> containing the start of the line to the current cursor position.</returns>
        /// <param name="text">The text to find the start of the line in.</param>
        /// <param name="position">The current location of the cursor in the text and possible selection.</param>
        public virtual NSRange FindStartOfLine(string text, NSRange position)
        {
            NSRange results = new NSRange(position.Location, position.Length);
            var found = false;

            // Find starting line boundry
            while (results.Location > 0 && !found)
            {
                var c = text[(int)results.Location - 1];
                found = (c == Newline || c == LineSeparator || c == ParagraphSeparator);
                if (!found) results.Location -= 1;
            };

            // Calculate length
            results.Length = position.Location - results.Location;

            return results;
        }

        /// <summary>
        /// Finds the start of end for the given location in the text as defined by the <c>NewLine</c>, <c>LineSeparator</c>
        /// and <c>ParagraphSeparator</c> characters.
        /// </summary>
        /// <returns>A <c>NSRange</c> containing the end of the line from the current cursor position.</returns>
        /// <param name="text">The text to find the end of the line in.</param>
        /// <param name="position">The current location of the cursor in the text and possible selection.</param>
        public virtual NSRange FindEndOfLine(string text, NSRange position)
        {
            NSRange results = position;
            var found = false;

            // Find ending line boundry
            found = false;
            while ((int)(results.Location + results.Length) < text.Length && !found)
            {
                var c = text[(int)(results.Location + results.Length)];
                found = (c == Newline || c == LineSeparator || c == ParagraphSeparator);
                if (!found) results.Length += 1;
            };

            return results;
        }

        /// <summary>
        /// Tests to see if the preceeding character is whitespace or terminator.
        /// </summary>
        /// <returns><c>true</c>, if character is whitespace or terminator, <c>false</c> otherwise.</returns>
        /// <param name="text">The text to test.</param>
        /// <param name="position">The current cursor position inside the text.</param>
        /// <remarks>Returns <c>true</c> if at start of line.</remarks>
        public virtual bool PreceedingCharacterIsWhitespaceOrTerminator(string text, NSRange position)
        {
            var found = false;

            // At start of line?
            if (position.Location == 0)
            {
                // Yes, always true
                return true;
            }

            // Found?
            var c = text[(int)(position.Location - 1)];
            found = (c == ' ' | c == Newline || c == LineSeparator || c == ParagraphSeparator);

            // Return result
            return found;
        }

        /// <summary>
        /// Tests to see if the trailing character is whitespace or terminator.
        /// </summary>
        /// <returns><c>true</c>, if character is whitespace or terminator, <c>false</c> otherwise.</returns>
        /// <param name="text">The text to test.</param>
        /// <param name="position">The current cursor position inside the text.</param>
        /// <remarks>Returns <c>true</c> if at end of line.</remarks>
        public virtual bool TrailingCharacterIsWhitespaceOrTerminator(string text, NSRange position)
        {
            var found = false;

            // At end of line?
            if (position.Location >= text.Length - 1)
            {
                // Yes, always true
                return true;
            }

            // Found?
            var c = text[(int)(position.Location + 1)];
            found = (c == ' ' | c == Newline || c == LineSeparator || c == ParagraphSeparator);

            // Return result
            return found;
        }

        /// <summary>
        /// Uses the current <c>Language</c> (<see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/>) to
        /// syntax highlight the given word in the attached <c>TextEditor</c> (<c>NSTextView</c>) at the given
        /// character locations.
        /// </summary>
        /// <param name="word">The possible keyword to highlight.</param>
        /// <param name="range">An <c>NSRange</c> specifying the starting and ending character locations
        /// for the word to highlight.</param>
        /// <remarks>TODO: The Text Kit <c>SetTemporaryAttributes</c> routines are handiling the format of
        /// character strings such as HTML or XML tag incorrectly.</remarks>
        public virtual void HighlightSyntax(string word, NSRange range)
        {

            try
            {
                // Found a keyword?
                KeywordDescriptor info;
                if (Language.Keywords.TryGetValue(word, out info))
                {
                    // Yes, adjust attributes
                    TextEditor.LayoutManager.SetTemporaryAttributes(new NSDictionary(NSAttributedString.ForegroundColorAttributeName, info.Color), range);
                }
                else
                {
                    TextEditor.LayoutManager.RemoveTemporaryAttribute(NSAttributedString.ForegroundColorAttributeName, range);
                }
            }
            catch
            {
                // Ignore any exceptions at this point
            }

        }

        /// <summary>
        /// Based on the current <c>Language</c> (<see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/>),
        /// highlight the syntax of the given character region.
        /// </summary>
        /// <param name="text">The string value to be syntax highlighted.</param>
        /// <param name="position">The starting location of the text to be highlighted.</param>
        public virtual void HighlightSyntaxRegion(string text, NSRange position)
        {
            var range = FindLineBoundries(text, position);
            var word = "";
            var location = range.Location;
            var c = ' ';
            var l = ' ';
            var segment = new NSRange(range.Location, 0);
            var fragment = new NSRange(range.Location, 0);
            var handled = false;
            FormatDescriptor inFormat = null;

            // Initialize
            Language.ClearFormats();

            // Process all characters in range
            for (var n = 0; n < range.Length; ++n)
            {
                // Get next character
                c = text[(int)(range.Location + n)];
                //Console.Write ("[{0}]={1}", n, c);

                // Excape character?
                if (c == Language.EscapeCharacter || l == Language.EscapeCharacter)
                {
                    // Was the last chanacter an escape?
                    if (l == Language.EscapeCharacter)
                    {
                        // Handling outlying format exception
                        c = ' ';
                    }
                    handled = true;

                    // Are we inside a format?
                    if (inFormat != null)
                    {
                        // Yes, increase segment count
                        ++segment.Length;
                    }
                }
                else
                {
                    // Are we inside of a formatter?
                    if (inFormat == null)
                    {
                        // No, see if this character is recognized by a formatter
                        foreach (FormatDescriptor format in Language.Formats)
                        {
                            if (format.MatchesCharacter(c))
                            {
                                if (format.Triggered)
                                {
                                    Language.ClearFormats();
                                    inFormat = format;
                                    inFormat.Active = true;
                                    segment = new NSRange((int)(range.Location + n) - (inFormat.StartsWith.Length - 1), inFormat.StartsWith.Length);
                                    //Console.WriteLine ("Found Format [{0}] = {1}", inFormat.StartsWith, segment);
                                }
                                handled = true;
                            }
                        }
                    }
                    else
                    {
                        // Prefix or enclosure?
                        if (inFormat.Type == FormatDescriptorType.Prefix)
                        {
                            // At end of line?
                            if (c == Newline || c == LineSeparator || c == ParagraphSeparator)
                            {
                                ++segment.Length;
                                TextEditor.LayoutManager.SetTemporaryAttributes(new NSDictionary(NSAttributedString.ForegroundColorAttributeName, inFormat.Color), segment);
                                //Console.WriteLine ("Complete Prefix [{0}] = {1}", inFormat.StartsWith, segment);
                                location = (uint)(range.Location + n + 1);
                                word = "";
                                inFormat = null;
                                Language.ClearFormats();
                            }
                            else
                            {
                                ++segment.Length;
                            }
                            handled = true;
                        }
                        else
                        {
                            if (inFormat.MatchesCharacter(c))
                            {
                                if (inFormat.Triggered)
                                {
                                    ++segment.Length;
                                    TextEditor.LayoutManager.SetTemporaryAttributes(new NSDictionary(NSAttributedString.ForegroundColorAttributeName, inFormat.Color), segment);
                                    //Console.WriteLine ("Complete Enclosure [{0}] = {1}", inFormat.EndsWith, segment);
                                    inFormat = null;
                                    Language.ClearFormats();
                                }
                            }
                            ++segment.Length;
                            handled = true;
                        }
                    }
                }

                // Has this character already been handled?
                if (!handled)
                {
                    // No, handle normal characters
                    var found = char.IsWhiteSpace(c) || IsLanguageSeparator(c);
                    if (found)
                    {
                        segment = new NSRange(location, (uint)word.Length);
                        if (segment.Length > 0)
                        {
                            HighlightSyntax(word, segment);
                        }
                        location = (uint)(range.Location + n + 1);
                        word = "";
                    }
                    else
                    {
                        word += c;
                    }

                    // Clear any fully unmatched formats
                    if (inFormat == null)
                    {
                        Language.ClearFormats();
                    }
                }

                // Save last character
                l = c;
                handled = false;
            }

            // Finalize
            if (inFormat != null)
            {
                if (inFormat.Type == FormatDescriptorType.Prefix)
                {
                    TextEditor.LayoutManager.SetTemporaryAttributes(new NSDictionary(NSAttributedString.ForegroundColorAttributeName, inFormat.Color), segment);
                    //Console.WriteLine ("Finalize Prefix [{0}] = {1}", inFormat.StartsWith, segment);
                }
                inFormat = null;
                Language.ClearFormats();
            }
            else if (word != "")
            {
                segment = new NSRange(location, (uint)word.Length);
                if (segment.Length > 0)
                {
                    HighlightSyntax(word, segment);
                }
            }
            //Console.WriteLine (";");
        }
        #endregion
    }
}

