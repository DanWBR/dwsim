/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>x86-specific SIMD extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuSimdFeature)" />
	public sealed class X86CpuSimdFeature : CpuSimdFeature
	{

		/// <summary>MMX instruction set.</summary>
		public static readonly X86CpuSimdFeature MMX               = new X86CpuSimdFeature(0);
		/// <summary>MMX+ instruction set.</summary>
		public static readonly X86CpuSimdFeature MMXPlus           = new X86CpuSimdFeature(1);
		/// <summary>EMMX instruction set.</summary>
		public static readonly X86CpuSimdFeature EMMX              = new X86CpuSimdFeature(2);
		/// <summary>3dnow! instruction set.</summary>
		public static readonly X86CpuSimdFeature ThreeDNow         = new X86CpuSimdFeature(3);
		/// <summary>3dnow!+ instruction set.</summary>
		public static readonly X86CpuSimdFeature ThreeDNowPlus     = new X86CpuSimdFeature(4);
		/// <summary>3dnow! prefetch instructions.</summary>
		public static readonly X86CpuSimdFeature ThreeDNowPrefetch = new X86CpuSimdFeature(5);
		/// <summary>Geode 3dnow! instructions.</summary>
		public static readonly X86CpuSimdFeature ThreeDNowGeode    = new X86CpuSimdFeature(6);
		/// <summary>SSE instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE               = new X86CpuSimdFeature(7);
		/// <summary>SSE 2 instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE2              = new X86CpuSimdFeature(8);
		/// <summary>SSE 3 instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE3              = new X86CpuSimdFeature(9);
		/// <summary>SSSE 3 instruction set.</summary>
		public static readonly X86CpuSimdFeature SSSE3             = new X86CpuSimdFeature(10);
		/// <summary>SSE 4.1 instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE4_1            = new X86CpuSimdFeature(11);
		/// <summary>SSE 4.2 instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE4_2            = new X86CpuSimdFeature(12);
		/// <summary>SSE 4A instruction set.</summary>
		public static readonly X86CpuSimdFeature SSE4A             = new X86CpuSimdFeature(13);
		/// <summary>AVX instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX               = new X86CpuSimdFeature(14);
		/// <summary>AVX 2 instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX2              = new X86CpuSimdFeature(15);
		/// <summary>XOP instruction set.</summary>
		public static readonly X86CpuSimdFeature XOP               = new X86CpuSimdFeature(16);
		/// <summary>F16C instruction set.</summary>
		public static readonly X86CpuSimdFeature F16C              = new X86CpuSimdFeature(17);
		/// <summary>FMA3 instruction set.</summary>
		public static readonly X86CpuSimdFeature FMA3              = new X86CpuSimdFeature(18);
		/// <summary>FMA4 instruction set.</summary>
		public static readonly X86CpuSimdFeature FMA4              = new X86CpuSimdFeature(19);
		/// <summary>Knights Ferry (aka Larrabee) instruction set.</summary>
		public static readonly X86CpuSimdFeature KNF               = new X86CpuSimdFeature(20);
		/// <summary>Knights Corner (aka Xeon Phi) instruction set.</summary>
		public static readonly X86CpuSimdFeature KNC               = new X86CpuSimdFeature(21);
		/// <summary>AVX-512 Foundation instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX512F           = new X86CpuSimdFeature(22);
		/// <summary>AVX-512 Conflict Detection instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX512CD          = new X86CpuSimdFeature(23);
		/// <summary>AVX-512 Exponential and Reciprocal instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX512ER          = new X86CpuSimdFeature(24);
		/// <summary>AVX-512 Prefetch instruction set.</summary>
		public static readonly X86CpuSimdFeature AVX512PF          = new X86CpuSimdFeature(25);

		internal X86CpuSimdFeature(uint id) : base(id, CpuArchitecture.X86.Id)
		{
		}

	}

}
