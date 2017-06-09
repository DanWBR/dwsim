/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>PowerPC-specific SIMD extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuSimdFeature)" />
	public sealed class PowerPCCpuSimdFeature : CpuSimdFeature
	{

		/// <summary>Vector Media eXtension (aka AltiVec and Velocity Engine).</summary>
		public static readonly PowerPCCpuSimdFeature VMX               = new PowerPCCpuSimdFeature(0);
		/// <summary>VMX VPERMXOR instruction from Power ISA 2.07.</summary>
		public static readonly PowerPCCpuSimdFeature VMXRAID           = new PowerPCCpuSimdFeature(1);
		/// <summary>Additional VMX instructions from Power ISA 2.07.</summary>
		public static readonly PowerPCCpuSimdFeature VMX207            = new PowerPCCpuSimdFeature(2);
		/// <summary>VSX instructions (Vector-Scalar eXtensions).</summary>
		public static readonly PowerPCCpuSimdFeature VSX               = new PowerPCCpuSimdFeature(8);
		/// <summary>Additional VSX instructions from Power ISA 2.07.</summary>
		public static readonly PowerPCCpuSimdFeature VSX207            = new PowerPCCpuSimdFeature(9);
		/// <summary>SPE (Signal Processing Engine).</summary>
		public static readonly PowerPCCpuSimdFeature SPE               = new PowerPCCpuSimdFeature(24);
		/// <summary>Embedded Floating-Point Vector instructions.</summary>
		public static readonly PowerPCCpuSimdFeature EFPV              = new PowerPCCpuSimdFeature(25);
		/// <summary>Double Hummer instruction set.</summary>
		/// <remarks>2-wide double precision floating-point SIMD for Blue Gene/L and Blue Gene/P supercomputers.</remarks>
		public static readonly PowerPCCpuSimdFeature DoubleHummer      = new PowerPCCpuSimdFeature(48);
		/// <summary>Quad Processing eXtension.</summary>
		/// <remarks>4-wide double precision floating-point SIMD for Blue Gene/Q supercomputers.</remarks>
		public static readonly PowerPCCpuSimdFeature QPX               = new PowerPCCpuSimdFeature(49);

		internal PowerPCCpuSimdFeature(uint id) : base(id, CpuArchitecture.PowerPC.Id)
		{
		}

	}

}
