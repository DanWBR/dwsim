/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>PowerPC-specific ISA extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	public sealed class PowerPCCpuIsaFeature : CpuIsaFeature
	{

		/// <summary>FPU instructions.</summary>
		public static readonly PowerPCCpuIsaFeature FPU        = new PowerPCCpuIsaFeature(0);
		/// <summary>MTOCRF and MFOCRF instructions.</summary>
		/// <remarks>MTOCRF and MFOCRF are new forms of MTCRF and MFCRF instructions. They were introduced in PowerPC ISA 2.00.</remarks>
		public static readonly PowerPCCpuIsaFeature MCRF       = new PowerPCCpuIsaFeature(1);
		/// <summary>FSQRT and FSQRTS instructions.</summary>
		/// <remarks>These optional general-purpose instructions are defined in PowerPC ISA 2.01.</remarks>
		public static readonly PowerPCCpuIsaFeature GPOpt      = new PowerPCCpuIsaFeature(2);
		/// <summary>FRES, FRSQRTE, and FSEL instructions.</summary>
		/// <remarks>These optional graphics instructions are defined in PowerPC ISA 2.01.</remarks>
		public static readonly PowerPCCpuIsaFeature GfxOpt     = new PowerPCCpuIsaFeature(3);
		/// <summary>FRE and FRSQRTES instructions.</summary>
		/// <remarks>These optional graphics instructions are defined in PowerPC ISA 2.02.</remarks>
		public static readonly PowerPCCpuIsaFeature GfxOpt202  = new PowerPCCpuIsaFeature(4);
		/// <summary>Legacy integer multiply-accumulate instructions.</summary>
		/// <remarks>These multiply-accumulate instructions were implemented in some 400 series processors.</remarks>
		public static readonly PowerPCCpuIsaFeature MAC        = new PowerPCCpuIsaFeature(5);
		/// <summary>Embedded Floating-Point Single Precision instructions.</summary>
		/// <remarks>These instructions are defined in POWER ISA 2.03 in SPE.Embedded Float Scalar Single category.</remarks>
		public static readonly PowerPCCpuIsaFeature EFPS       = new PowerPCCpuIsaFeature(6);
		/// <summary>Embedded Floating-Point Double Precision instructions.</summary>
		/// <remarks>These instructions are defined in POWER ISA 2.03 in SPE.Embedded Float Scalar Double category.</remarks>
		public static readonly PowerPCCpuIsaFeature EFPD       = new PowerPCCpuIsaFeature(7);
		/// <summary>ISEL instruction.</summary>
		/// <remarks>ISEL instruction is defined as optional (Phased-In) in Power ISA 2.03.</remarks>
		public static readonly PowerPCCpuIsaFeature ISEL       = new PowerPCCpuIsaFeature(8);
		/// <summary>POPCNTB instruction.</summary>
		/// <remarks>POPCNTB instruction is defined as optional (Phased-In) in Power ISA 2.03.</remarks>
		public static readonly PowerPCCpuIsaFeature POPCNTB    = new PowerPCCpuIsaFeature(9);
		/// <summary>FRIN, FRIZ, FRIP, and FRIM instructions.</summary>
		/// <remarks>Floating-point round-to-integer instructions are defined as optional (Phased-In) in Power ISA 2.03 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature FRI        = new PowerPCCpuIsaFeature(10);
		/// <summary>FCPSGN and LFIWAX instructions.</summary>
		/// <remarks>These instruction is defined in Power ISA 2.05 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature FPU205     = new PowerPCCpuIsaFeature(11);
		/// <summary>LFDP, STFDP, LFDPX, and STFDPX instructions.</summary>
		/// <remarks>Floating-point load/store double pair instructions are defined as optional (Phased-Out) in Power ISA 2.05 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature LFDP       = new PowerPCCpuIsaFeature(12);
		/// <summary>Decimal Floating-Point instructions.</summary>
		/// <remarks>These instructions are defined in POWER ISA 2.05 in Decimal Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature DFP        = new PowerPCCpuIsaFeature(13);
		/// <summary>CMPB, PRTYW, and PRTYD instructions.</summary>
		/// <remarks>These instructions are defined in Power ISA 2.05.</remarks>
		public static readonly PowerPCCpuIsaFeature ISA205     = new PowerPCCpuIsaFeature(14);
		/// <summary>BPERMD instruction.</summary>
		/// <remarks>This instruction is defined as optional (Embedded.Phased-In, Server) in Power ISA 2.06.</remarks>
		public static readonly PowerPCCpuIsaFeature BPERMD     = new PowerPCCpuIsaFeature(15);
		/// <summary>Extended division instructions (DIVWE, DIVWEO, DIVWEU, DIVWEUO, DIVDE, DIVDEO, DIVDEU, and DIVDEUO).</summary>
		/// <remarks>These instructions are defined as optional (Embedded.Phased-In, Server) in Power ISA 2.06.</remarks>
		public static readonly PowerPCCpuIsaFeature DIVWE      = new PowerPCCpuIsaFeature(16);
		/// <summary>POPCNTW and POPCNTD instructions.</summary>
		/// <remarks>These instructions are defined as optional (Embedded.Phased-In, Server) in Power ISA 2.06.</remarks>
		public static readonly PowerPCCpuIsaFeature POPCNTW    = new PowerPCCpuIsaFeature(17);
		/// <summary>LDBRX and STDBRX instructions.</summary>
		/// <remarks>These instructions are defined in Power ISA 2.06.</remarks>
		public static readonly PowerPCCpuIsaFeature ISA206     = new PowerPCCpuIsaFeature(18);
		/// <summary>LFIWZX instruction.</summary>
		/// <remarks>This instruction is defined as optional (Phased-In) in Power ISA 2.06 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature LFIWZX     = new PowerPCCpuIsaFeature(19);
		/// <summary>FCTIDU, FCTIDUZ, FCTIWU, FCTIWUZ, FCFIDU, FCFIDS, and FCFIDUS instructions.</summary>
		/// <remarks>These instructions are defined as optional (Phased-In) in Power ISA 2.06 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature FCTIWU     = new PowerPCCpuIsaFeature(20);
		/// <summary>FTDIV and FTSQRT instructions.</summary>
		/// <remarks>These instructions are defined as optional (Phased-In) in Power ISA 2.06 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature FTDIV      = new PowerPCCpuIsaFeature(21);
		/// <summary>LBARX, LHARX, STBCX, and STHCX instructions.</summary>
		/// <remarks>These instructions are defined as optional (Phased-In) in Power ISA 2.06 in Floating-Point category.</remarks>
		public static readonly PowerPCCpuIsaFeature LBARX      = new PowerPCCpuIsaFeature(22);
		/// <summary>LQARX and STQCX instructions.</summary>
		/// <remarks>These instructions are defined in Power ISA 2.07 in Load/Store Quadword category.</remarks>
		public static readonly PowerPCCpuIsaFeature LQARX      = new PowerPCCpuIsaFeature(23);
		/// <summary>LQ and STQ instructions (accessible in problem state).</summary>
		/// <remarks>LQ and STQ instructions are redefined as accessible in problem state in Power ISA 2.07 in Load/Store Quadword category.</remarks>
		public static readonly PowerPCCpuIsaFeature LQ         = new PowerPCCpuIsaFeature(24);
		/// <summary>VCIPHER, VCIPHERLAST, VNCIPHER, VNCIPHERLAST, VSBOX, VSHASIGMAW, and VSHASIGMAD instructions.</summary>
		/// <remarks>These instructions are defined in Power ISA 2.07 in VMX.Crypto category.</remarks>
		public static readonly PowerPCCpuIsaFeature VMXCrypto  = new PowerPCCpuIsaFeature(25);
		/// <summary>Transactional Memory instructions.</summary>
		/// <remarks>These instructions are defined in Power ISA 2.07 in Transactional Memory category.</remarks>
		public static readonly PowerPCCpuIsaFeature TM         = new PowerPCCpuIsaFeature(26);

		internal PowerPCCpuIsaFeature(uint id) : base(id, CpuArchitecture.PowerPC.Id)
		{
		}

	}

}
