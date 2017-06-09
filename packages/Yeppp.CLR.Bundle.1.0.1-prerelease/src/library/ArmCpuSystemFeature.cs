/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>ARM-specific non-ISA processor or system features.</summary>
	/// <seealso cref="Library.IsSupported(CpuSystemFeature)" />
	public sealed class ArmCpuSystemFeature : CpuSystemFeature {

		/// <summary>VFP vector mode is supported in hardware.</summary>
		public static readonly ArmCpuSystemFeature VFPVectorMode = new ArmCpuSystemFeature(32);
		/// <summary>The CPU has FPA registers (f0-f7), and the operating system preserves them during context switch.</summary>
		public static readonly ArmCpuSystemFeature FPA           = new ArmCpuSystemFeature(56);
		/// <summary>The CPU has WMMX registers (wr0-wr15), and the operating system preserves them during context switch.</summary>
		public static readonly ArmCpuSystemFeature WMMX          = new ArmCpuSystemFeature(57);
		/// <summary>The CPU has s0-s31 VFP registers, and the operating system preserves them during context switch.</summary>
		public static readonly ArmCpuSystemFeature S32           = new ArmCpuSystemFeature(58);
		/// <summary>The CPU has d0-d31 VFP registers, and the operating system preserves them during context switch.</summary>
		public static readonly ArmCpuSystemFeature D32           = new ArmCpuSystemFeature(59);

		internal ArmCpuSystemFeature(uint id) : base(id, CpuArchitecture.ARM.Id)
		{
		}

	}

}
