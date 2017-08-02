using System;
using MonoMac.Foundation;
using MonoMac.AppKit;

namespace MonoMac.AppKit
{
	/// <summary>
	/// Manual toolbar item allows the developer to take control of the enabled/disabled
	/// state of a toolbar item.
	/// </summary>
	/// <remarks>See our Toolbar documentation for more infomation:
	/// http://developer.xamarin.com/guides/mac/user-interface/working-with-toolbars/#Disabling_Toolbar_Items</remarks>
	[Register("ManualToolbarItem")]
	public class ManualToolbarItem : NSToolbarItem
	{
		#region Computed Properties
		/// <summary>
		/// Gets or sets a value indicating whether this <see cref="AppKit.ManualToolbarItem"/> is disabled.
		/// </summary>
		/// <value><c>true</c> if disabled; otherwise, <c>false</c>.</value>
		public bool Disabled = false;
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.ManualToolbarItem"/> class.
		/// </summary>
		public ManualToolbarItem ()
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.ManualToolbarItem"/> class.
		/// </summary>
		/// <param name="handle">An <c>IntPtr</c>.</param>
		public ManualToolbarItem (IntPtr handle) : base (handle)
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.ManualToolbarItem"/> class.
		/// </summary>
		/// <param name="t">A <c>NSObjectFlag</c>.</param>
		public ManualToolbarItem (NSObjectFlag  t) : base (t)
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.ManualToolbarItem"/> class.
		/// </summary>
		/// <param name="title">The new title of the toolbar item.</param>
		public ManualToolbarItem (string title) : base (title)
		{
		}
		#endregion

		#region Override Methods
		/// <summary>
		/// This method overrides the automatic enabling/disabling of
		/// a toolbar item.
		/// </summary>
		public override void Validate ()
		{
			base.Validate ();

			// Set the interactive state based on the
			// Disabled property
			Enabled = (!Disabled);
		}
		#endregion
	}
}

