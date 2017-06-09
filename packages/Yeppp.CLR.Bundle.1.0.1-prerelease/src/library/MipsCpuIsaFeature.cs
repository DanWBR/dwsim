/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>MIPS-specific ISA extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	public sealed class MipsCpuIsaFeature : CpuIsaFeature
	{

		/// <summary>MIPS I instructions.</summary>
		public static readonly MipsCpuIsaFeature MIPS_I    = new MipsCpuIsaFeature(0);
		/// <summary>MIPS II instructions.</summary>
		public static readonly MipsCpuIsaFeature MIPS_II   = new MipsCpuIsaFeature(1);
		/// <summary>MIPS III instructions.</summary>
		public static readonly MipsCpuIsaFeature MIPS_III  = new MipsCpuIsaFeature(2);
		/// <summary>MIPS IV instructions.</summary>
		public static readonly MipsCpuIsaFeature MIPS_IV   = new MipsCpuIsaFeature(3);
		/// <summary>MIPS V instructions.</summary>
		public static readonly MipsCpuIsaFeature MIPS_V    = new MipsCpuIsaFeature(4);
		/// <summary>MIPS32/MIPS64 Release 1 instructions.</summary>
		public static readonly MipsCpuIsaFeature R1        = new MipsCpuIsaFeature(5);
		/// <summary>MIPS32/MIPS64 Release 2 instructions.</summary>
		public static readonly MipsCpuIsaFeature R2        = new MipsCpuIsaFeature(6);
		/// <summary>FPU with S, D, and W formats and instructions.</summary>
		public static readonly MipsCpuIsaFeature FPU       = new MipsCpuIsaFeature(24);
		/// <summary>MIPS16 extension.</summary>
		public static readonly MipsCpuIsaFeature MIPS16    = new MipsCpuIsaFeature(25);
		/// <summary>SmartMIPS extension.</summary>
		public static readonly MipsCpuIsaFeature SmartMIPS = new MipsCpuIsaFeature(26);
		/// <summary>Multi-threading extension.</summary>
		public static readonly MipsCpuIsaFeature MT        = new MipsCpuIsaFeature(27);
		/// <summary>MicroMIPS extension.</summary>
		public static readonly MipsCpuIsaFeature MicroMIPS = new MipsCpuIsaFeature(28);
		/// <summary>MIPS virtualization extension.</summary>
		public static readonly MipsCpuIsaFeature VZ        = new MipsCpuIsaFeature(29);

		internal MipsCpuIsaFeature(uint id) : base(id, CpuArchitecture.MIPS.Id)
		{
		}

	}

}
