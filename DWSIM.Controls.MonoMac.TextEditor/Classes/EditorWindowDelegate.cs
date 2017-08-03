using System;
using MonoMac.AppKit;
using MonoMac.AppKit.TextKit.Formatter;
using System.IO;
using MonoMac.Foundation;

namespace SourceWriter
{
	/// <summary>
	/// Defines a delegate for handling events on our text editor window such as asking the
	/// user to save changes to a document before closing the window.
	/// </summary>
	/// <remarks>
	/// Please see our Modified Windows Content Docs for more info:
	/// https://developer.xamarin.com/guides/mac/user-interface/working-with-windows/#Modified_Windows_Content
	/// </remarks>
    //public class EditorWidowDelegate : NSWindowDelegate
    //{
    //    #region Application Access
    //    /// <summary>
    //    /// A helper shortcut to the app delegate.
    //    /// </summary>
    //    /// <value>The app.</value>
    //    public static AppDelegate App {
    //        get { return (AppDelegate)NSApplication.SharedApplication.Delegate; }
    //    }
    //    #endregion

    //    #region Computed Properties
    //    /// <summary>
    //    /// Gets or sets the window being managed.
    //    /// </summary>
    //    /// <value>The <c>NSWindow</c> being managed by the <c>NSWindowController</c> this delegate
    //    /// is attached to.</value>
    //    public NSWindow Window { get; set;}

    //    #endregion

    //    #region constructors
    //    /// <summary>
    //    /// Initializes a new instance of the <see cref="SourceWriter.EditorWidowDelegate"/> class.
    //    /// </summary>
    //    /// <param name="window">The <c>NSWindow</c> being managed by the <c>NSWindowController</c> this delegate
    //    /// is attached to.</param>
    //    public EditorWidowDelegate (NSWindow window)
    //    {
    //        // Initialize
    //        this.Window = window;

    //    }
    //    #endregion

    //    #region Override Methods

    //    /// <summary>
    //    /// Called when the window gains focus and becomes the active window.
    //    /// </summary>
    //    /// <param name="notification">Notification.</param>
    //    /// <remarks>We are using this method to update the preview of the document and
    //    /// to populate the Formatting Menu with any extra commands from the document's
    //    /// <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.</remarks>
    //    public override void DidBecomeKey (NSNotification notification)
    //    {
    //        // Valid
    //        //if (EditorController == null) return;

    //        // Populate Formatting Command menu
    //        //EditorController.ContentController.PopulateFormattingMenu();

    //        // Update preview
    //        //EditorController.ContentController.PreviewContents();
    //    }

    //    /// <summary>
    //    /// Called when the window loses focus and falls into the background.
    //    /// </summary>
    //    /// <param name="notification">Notification.</param>
    //    /// <remarks>We are using this method to remove any custom commands added
    //    /// to the Formatting Menu by the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.</remarks>
    //    public override void DidResignKey (NSNotification notification)
    //    {
    //        // Valid
    //        //if (EditorController == null) return;

    //        // Remove this window's extra formatting commands from the menu
    //        //EditorController.ContentController.UnpopulateFormattingMenu ();
    //    }
    //    #endregion
    //}
}

