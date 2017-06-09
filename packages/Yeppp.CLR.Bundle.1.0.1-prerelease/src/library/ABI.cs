/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>Application binary interface.</summary>
	/// <seealso cref="Library.GetProcessABI" />
	public struct ABI
	{

		/// <summary>Application binary interface not known to the library.</summary>
		/// <remarks>This value is never returned on supported platforms.</remarks>
		public static readonly ABI Unknown        = new ABI(0);
		/// <summary>Windows x86 ABI.</summary>
		public static readonly ABI Windows_X86    = new ABI(10);
		/// <summary>Windows x86-64 ABI.</summary>
		public static readonly ABI Windows_X86_64 = new ABI(11);
		/// <summary>Windows IA64 ABI.</summary>
		public static readonly ABI Windows_IA64   = new ABI(12);
		/// <summary>Windows ARM ABI.</summary>
		public static readonly ABI Windows_ARM    = new ABI(13);
		/// <summary>OS X x86 ABI.</summary>
		public static readonly ABI OSX_X86     = new ABI(20);
		/// <summary>OS X x86-64 ABI.</summary>
		public static readonly ABI OSX_X86_64  = new ABI(21);
		/// <summary>OS X PowerPC ABI.</summary>
		public static readonly ABI OSX_PPC     = new ABI(22);
		/// <summary>OS X PowerPC 64 ABI.</summary>
		public static readonly ABI OSX_PPC64   = new ABI(23);
		/// <summary>Linux x86 ABI.</summary>
		public static readonly ABI Linux_X86      = new ABI(30);
		/// <summary>Linux x86-64 ABI.</summary>
		public static readonly ABI Linux_X86_64   = new ABI(31);
		/// <summary>Linux x32 ABI.</summary>
		public static readonly ABI Linux_X32      = new ABI(32);
		/// <summary>Linux IA64 ABI.</summary>
		public static readonly ABI Linux_IA64     = new ABI(33);
		/// <summary>Linux K1OM (Xeon Phi) ABI.</summary>
		public static readonly ABI Linux_K1OM     = new ABI(34);
		/// <summary>Linux Legacy ARM ABI (OABI).</summary>
		public static readonly ABI Linux_ARM      = new ABI(40);
		/// <summary>Linux ARM EABI with soft-float calling convention (armel, gnueabi).</summary>
		public static readonly ABI Linux_ARMEL    = new ABI(41);
		/// <summary>Linux ARM EABI with hard-float calling convention (armhf, gnueabihf).</summary>
		public static readonly ABI Linux_ARMHF    = new ABI(42);
		/// <summary>Linux ARM64 (AArch64) ABI.</summary>
		public static readonly ABI Linux_ARM64    = new ABI(43);
		/// <summary>Linux PowerPC ABI.</summary>
		public static readonly ABI Linux_PPC      = new ABI(50);
		/// <summary>Linux PowerPC 64 ABI.</summary>
		public static readonly ABI Linux_PPC64    = new ABI(51);

		private readonly uint id;

		internal ABI(uint id)
		{
			this.id = id;
		}

		internal uint Id
		{
			get
			{
				return this.id;
			}
		}

		/// <summary>Compares for equality with another <see cref="ABI" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(ABI other)
		{
			return this.id == other.id;
		}

		/// <summary>Compares for equality with another object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public override bool Equals(System.Object other)
		{
			if (other == null || GetType() != other.GetType())
				return false;

			return this.Equals((ABI)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="ABI" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)this.id);
		}

		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			switch (this.id)
			{
				case 10:
					return "Windows_X86";
				case 11:
					return "Windows_X86_64";
				case 12:
					return "Windows_IA64";
				case 13:
					return "Windows_ARM";
				case 20:
					return "OSX_X86";
				case 21:
					return "OSX_X86_64";
				case 22:
					return "OSX_PPC";
				case 23:
					return "OSX_PPC64";
				case 30:
					return "Linux_X86";
				case 31:
					return "Linux_X86_64";
				case 32:
					return "Linux_X32";
				case 33:
					return "Linux_IA64";
				case 34:
					return "Linux_K1OM";
				case 40:
					return "Linux_ARM";
				case 41:
					return "Linux_ARMEL";
				case 42:
					return "Linux_ARMHF";
				case 43:
					return "Linux_ARM64";
				case 50:
					return "Linux_PPC";
				case 51:
					return "Linux_PPC64";
				default:
					return "Unknown";
			}
		}

		/// <summary>Checks if the object represents one of Windows-specific ABIs.</summary>
		/// <returns>true if this is a Windows ABI and false otherwise.</returns>
		/// <seealso cref="ABI.IsUnix" />
		/// <seealso cref="ABI.IsLinux" />
		/// <seealso cref="ABI.IsOSX" />
		public bool IsWindows()
		{
			return ((this.id >= 10) && (this.id < 20));
		}

		/// <summary>Checks if the object represents one of Unix-specific ABIs.</summary>
		/// <returns>true if this is a Unix ABI and false otherwise.</returns>
		/// <seealso cref="ABI.IsOSX" />
		/// <seealso cref="ABI.IsLinux" />
		/// <seealso cref="ABI.IsWindows" />
		public bool IsUnix()
		{
			return this.id >= 20;
		}

		/// <summary>Checks if the object represents one of Linux-specific ABIs.</summary>
		/// <returns>true if this is a Linux ABI and false otherwise.</returns>
		/// <seealso cref="ABI.IsUnix" />
		/// <seealso cref="ABI.IsOSX" />
		/// <seealso cref="ABI.IsWindows" />
		public bool IsLinux()
		{
			return this.id >= 30;
		}

		/// <summary>Checks if the object represents one of OSX-specific ABIs.</summary>
		/// <returns>true if this is an OSX ABI and false otherwise.</returns>
		/// <seealso cref="ABI.IsUnix" />
		/// <seealso cref="ABI.IsLinux" />
		/// <seealso cref="ABI.IsWindows" />
		public bool IsOSX()
		{
			return ((this.id >= 20) && (this.id < 30));
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				switch (this.id)
				{
					case 10:
						return "Windows x86 ABI";
					case 11:
						return "Windows x86-64 ABI";
					case 12:
						return "Windows IA64 ABI";
					case 13:
						return "Windows ARM ABI";
					case 20:
						return "OS X x86 ABI";
					case 21:
						return "OS X x86-64 ABI";
					case 22:
						return "OS X PowerPC ABI";
					case 23:
						return "OS X PowerPC 64 ABI";
					case 30:
						return "Linux x86 ABI";
					case 31:
						return "Linux x86-64 ABI";
					case 32:
						return "Linux x32 ABI";
					case 33:
						return "Linux IA64 ABI";
					case 34:
						return "Linux K1OM (Xeon Phi) ABI";
					case 40:
						return "Linux Legacy ARM ABI (OABI)";
					case 41:
						return "Linux ARM EABI with soft-float calling convention (armel, gnueabi)";
					case 42:
						return "Linux ARM EABI with hard-float calling convention (armhf, gnueabihf)";
					case 43:
						return "Linux ARM64 (AArch64) ABI";
					case 50:
						return "Linux PowerPC ABI";
					case 51:
						return "Linux PowerPC 64 ABI";
					default:
						return "Unknown ABI";
				}
			}
		}

	}

}
