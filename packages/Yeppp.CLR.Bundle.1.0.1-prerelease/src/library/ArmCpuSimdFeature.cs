/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>ARM-specific SIMD extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuSimdFeature)" />
	public sealed class ArmCpuSimdFeature : CpuSimdFeature
	{

		/// <summary>XScale instructions.</summary>
		public static readonly ArmCpuSimdFeature XScale     = new ArmCpuSimdFeature(0);
		/// <summary>Wireless MMX instruction set.</summary>
		public static readonly ArmCpuSimdFeature WMMX       = new ArmCpuSimdFeature(1);
		/// <summary>Wireless MMX 2 instruction set.</summary>
		public static readonly ArmCpuSimdFeature WMMX2      = new ArmCpuSimdFeature(2);
		/// <summary>NEON (Advanced SIMD) instruction set.</summary>
		public static readonly ArmCpuSimdFeature NEON       = new ArmCpuSimdFeature(3);
		/// <summary>NEON (Advanced SIMD) half-precision extension.</summary>
		public static readonly ArmCpuSimdFeature NEONHP     = new ArmCpuSimdFeature(4);
		/// <summary>NEON (Advanced SIMD) v2 instruction set.</summary>
		public static readonly ArmCpuSimdFeature NEON2      = new ArmCpuSimdFeature(5);

		internal ArmCpuSimdFeature(uint id) : base(id, CpuArchitecture.ARM.Id)
		{
		}

	}

}
