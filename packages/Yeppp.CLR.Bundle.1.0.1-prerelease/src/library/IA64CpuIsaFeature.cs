/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>IA64-specific ISA extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	public sealed class IA64CpuIsaFeature : CpuIsaFeature
	{

		/// <summary>Long branch instruction.</summary>
		public static readonly IA64CpuIsaFeature Brl       = new IA64CpuIsaFeature(0);
		/// <summary>Atomic 128-bit (16-byte) loads, stores, and CAS.</summary>
		public static readonly IA64CpuIsaFeature Atomic128 = new IA64CpuIsaFeature(1);
		/// <summary>CLZ (count leading zeros) instruction.</summary>
		public static readonly IA64CpuIsaFeature Clz       = new IA64CpuIsaFeature(2);
		/// <summary>MPY4 and MPYSHL4 (Truncated 32-bit multiplication) instructions.</summary>
		public static readonly IA64CpuIsaFeature Mpy4      = new IA64CpuIsaFeature(3);

		internal IA64CpuIsaFeature(uint id) : base(id, CpuArchitecture.IA64.Id)
		{
		}

	}

}
