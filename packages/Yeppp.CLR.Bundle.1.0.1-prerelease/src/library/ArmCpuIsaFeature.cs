/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>ARM-specific ISA extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	public sealed class ArmCpuIsaFeature : CpuIsaFeature
	{

		/// <summary>ARMv4 instruction set.</summary>
		public static readonly ArmCpuIsaFeature V4         = new ArmCpuIsaFeature(0);
		/// <summary>ARMv5 instruciton set.</summary>
		public static readonly ArmCpuIsaFeature V5         = new ArmCpuIsaFeature(1);
		/// <summary>ARMv5 DSP instructions.</summary>
		public static readonly ArmCpuIsaFeature V5E        = new ArmCpuIsaFeature(2);
		/// <summary>ARMv6 instruction set.</summary>
		public static readonly ArmCpuIsaFeature V6         = new ArmCpuIsaFeature(3);
		/// <summary>ARMv6 Multiprocessing extensions.</summary>
		public static readonly ArmCpuIsaFeature V6K        = new ArmCpuIsaFeature(4);
		/// <summary>ARMv7 instruction set.</summary>
		public static readonly ArmCpuIsaFeature V7         = new ArmCpuIsaFeature(5);
		/// <summary>ARMv7 Multiprocessing extensions.</summary>
		public static readonly ArmCpuIsaFeature V7MP       = new ArmCpuIsaFeature(6);
		/// <summary>Thumb mode.</summary>
		public static readonly ArmCpuIsaFeature Thumb      = new ArmCpuIsaFeature(7);
		/// <summary>Thumb 2 mode.</summary>
		public static readonly ArmCpuIsaFeature Thumb2     = new ArmCpuIsaFeature(8);
		/// <summary>Thumb EE mode.</summary>
		public static readonly ArmCpuIsaFeature ThumbEE    = new ArmCpuIsaFeature(9);
		/// <summary>Jazelle extensions.</summary>
		public static readonly ArmCpuIsaFeature Jazelle    = new ArmCpuIsaFeature(10);
		/// <summary>FPA instructions.</summary>
		public static readonly ArmCpuIsaFeature FPA        = new ArmCpuIsaFeature(11);
		/// <summary>VFP instruction set.</summary>
		public static readonly ArmCpuIsaFeature VFP        = new ArmCpuIsaFeature(12);
		/// <summary>VFPv2 instruction set.</summary>
		public static readonly ArmCpuIsaFeature VFP2       = new ArmCpuIsaFeature(13);
		/// <summary>VFPv3 instruction set.</summary>
		public static readonly ArmCpuIsaFeature VFP3       = new ArmCpuIsaFeature(14);
		/// <summary>VFP implementation with 32 double-precision registers.</summary>
		public static readonly ArmCpuIsaFeature VFPd32     = new ArmCpuIsaFeature(15);
		/// <summary>VFPv3 half precision extension.</summary>
		public static readonly ArmCpuIsaFeature VFP3HP     = new ArmCpuIsaFeature(16);
		/// <summary>VFPv4 instruction set.</summary>
		public static readonly ArmCpuIsaFeature VFP4       = new ArmCpuIsaFeature(17);
		/// <summary>SDIV and UDIV instructions.</summary>
		public static readonly ArmCpuIsaFeature Div        = new ArmCpuIsaFeature(18);
		/// <summary>Marvell Armada instruction extensions.</summary>
		public static readonly ArmCpuIsaFeature Armada     = new ArmCpuIsaFeature(19);

		internal ArmCpuIsaFeature(uint id) : base(id, CpuArchitecture.ARM.Id)
		{
		}

	}

}
