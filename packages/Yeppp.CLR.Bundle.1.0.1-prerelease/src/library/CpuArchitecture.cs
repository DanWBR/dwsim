/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>The basic instruction set architecture of the processor.</summary>
	/// <seealso cref="Library.GetCpuArchitecture" />
	public struct CpuArchitecture
	{

		/// <summary>Instruction set architecture is not known to the library.</summary>
		/// <remarks>This value is never returned on supported architectures.</remarks>
		public static readonly CpuArchitecture Unknown = new CpuArchitecture(0);
		/// <summary>x86 or x86-64 ISA.</summary>
		public static readonly CpuArchitecture X86     = new CpuArchitecture(1);
		/// <summary>ARM ISA.</summary>
		public static readonly CpuArchitecture ARM     = new CpuArchitecture(2);
		/// <summary>MIPS ISA.</summary>
		public static readonly CpuArchitecture MIPS    = new CpuArchitecture(3);
		/// <summary>PowerPC ISA.</summary>
		public static readonly CpuArchitecture PowerPC = new CpuArchitecture(4);
		/// <summary>IA64 ISA.</summary>
		public static readonly CpuArchitecture IA64    = new CpuArchitecture(5);
		/// <summary>SPARC ISA.</summary>
		public static readonly CpuArchitecture SPARC   = new CpuArchitecture(6);

		private readonly uint id;

		internal CpuArchitecture(uint id)
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

		sealed class CpuIsaFeaturesIterator : System.Collections.Generic.IEnumerator<CpuIsaFeature> {

			public CpuIsaFeaturesIterator(uint architectureId)
			{
				this.architectureId = architectureId;
				this.Reset();
			}

			public bool MoveNext()
			{
				do
				{
					this.position++;
					if (CpuIsaFeature.IsDefined(unchecked((uint)this.position), architectureId))
						return true;
				} while (this.position < 64);
				this.position = 64;
				return false;
			}

			public void Reset()
			{
				this.position = -1;
			}

			CpuIsaFeature System.Collections.Generic.IEnumerator<CpuIsaFeature>.Current
			{
				get
				{
					if (this.position < 64)
					{
						if (architectureId == CpuArchitecture.X86.Id)
						{
							return new X86CpuIsaFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.ARM.Id)
						{
							return new ArmCpuIsaFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.MIPS.Id)
						{
							return new MipsCpuIsaFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.IA64.Id)
						{
							return new IA64CpuIsaFeature(unchecked((uint)position));
						}
						else
						{
							return new CpuIsaFeature(unchecked((uint)position), architectureId);
						}
					}
					else
					{
						throw new System.InvalidOperationException("No more CPU ISA Extensions for architecture " + Library.GetString(Enumeration.CpuArchitecture, architectureId, StringType.Description));
					}
				}
			}

			object System.Collections.IEnumerator.Current
			{
				get
				{
					return (unchecked((System.Collections.Generic.IEnumerator<CpuIsaFeature>)this)).Current;
				}
			}

			public void Dispose()
			{
			}

			private readonly uint architectureId;
			private int position;
		}

		sealed class CpuSimdFeaturesIterator : System.Collections.Generic.IEnumerator<CpuSimdFeature> {

			public CpuSimdFeaturesIterator(uint architectureId)
			{
				this.architectureId = architectureId;
				this.Reset();
			}

			public bool MoveNext()
			{
				do
				{
					this.position++;
					if (CpuSimdFeature.IsDefined(unchecked((uint)this.position), architectureId))
						return true;
				} while (this.position < 64);
				this.position = 64;
				return false;
			}

			public void Reset()
			{
				this.position = -1;
			}

			CpuSimdFeature System.Collections.Generic.IEnumerator<CpuSimdFeature>.Current
			{
				get
				{
					if (this.position < 64)
					{
						if (architectureId == CpuArchitecture.X86.Id)
						{
							return new X86CpuSimdFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.ARM.Id)
						{
							return new ArmCpuSimdFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.MIPS.Id)
						{
							return new MipsCpuSimdFeature(unchecked((uint)position));
						}
						else
						{
							return new CpuSimdFeature(unchecked((uint)position), architectureId);
						}
					}
					else
					{
						throw new System.InvalidOperationException("No more CPU SIMD Extensions for architecture " + Library.GetString(Enumeration.CpuArchitecture, architectureId, StringType.Description));
					}
				}
			}

			object System.Collections.IEnumerator.Current
			{
				get
				{
					return (unchecked((System.Collections.Generic.IEnumerator<CpuSimdFeature>)this)).Current;
				}
			}

			public void Dispose()
			{
			}

			private readonly uint architectureId;
			private int position;
		}

		sealed class CpuSystemFeaturesIterator : System.Collections.Generic.IEnumerator<CpuSystemFeature> {

			public CpuSystemFeaturesIterator(uint architectureId)
			{
				this.architectureId = architectureId;
				this.Reset();
			}

			public bool MoveNext()
			{
				do
				{
					this.position++;
					if (CpuSystemFeature.IsDefined(unchecked((uint)this.position), architectureId))
						return true;
				} while (this.position < 64);
				this.position = 64;
				return false;
			}

			public void Reset()
			{
				this.position = -1;
			}

			CpuSystemFeature System.Collections.Generic.IEnumerator<CpuSystemFeature>.Current
			{
				get
				{
					if (this.position < 64)
					{
						if (architectureId == CpuArchitecture.X86.Id)
						{
							return new X86CpuSystemFeature(unchecked((uint)position));
						}
						else if (architectureId == CpuArchitecture.ARM.Id)
						{
							return new ArmCpuSystemFeature(unchecked((uint)position));
						}
						else
						{
							return new CpuSystemFeature(unchecked((uint)position), architectureId);
						}
					}
					else
					{
						throw new System.InvalidOperationException("No more non-ISA CPU and System Features for architecture " + Library.GetString(Enumeration.CpuArchitecture, architectureId, StringType.Description));
					}
				}
			}

			object System.Collections.IEnumerator.Current
			{
				get
				{
					return (unchecked((System.Collections.Generic.IEnumerator<CpuSystemFeature>)this)).Current;
				}
			}

			public void Dispose()
			{
			}

			private readonly uint architectureId;
			private int position;
		}

		sealed class CpuIsaFeaturesEnumerable : System.Collections.Generic.IEnumerable<CpuIsaFeature>
		{
			private readonly uint architectureId;

			public CpuIsaFeaturesEnumerable(uint architectureId)
			{
				this.architectureId = architectureId;
			}

			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
			{
				return this.GetEnumerator();
			}

			public System.Collections.Generic.IEnumerator<CpuIsaFeature> GetEnumerator()
			{
				return new CpuIsaFeaturesIterator(architectureId);
			}
		}

		sealed class CpuSimdFeaturesEnumerable : System.Collections.Generic.IEnumerable<CpuSimdFeature>
		{
			private readonly uint architectureId;

			public CpuSimdFeaturesEnumerable(uint architectureId)
			{
				this.architectureId = architectureId;
			}

			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
			{
				return this.GetEnumerator();
			}

			public System.Collections.Generic.IEnumerator<CpuSimdFeature> GetEnumerator()
			{
				return new CpuSimdFeaturesIterator(architectureId);
			}
		}

		sealed class CpuSystemFeaturesEnumerable : System.Collections.Generic.IEnumerable<CpuSystemFeature>
		{
			private readonly uint architectureId;

			public CpuSystemFeaturesEnumerable(uint architectureId)
			{
				this.architectureId = architectureId;
			}

			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
			{
				return this.GetEnumerator();
			}

			public System.Collections.Generic.IEnumerator<CpuSystemFeature> GetEnumerator()
			{
				return new CpuSystemFeaturesIterator(architectureId);
			}
		}

		/// <summary>An iterable list of potentially available on this architecture ISA features.</summary>
		/// <remarks>For #Unknown architecture provides an iterator over generic ISA features.</remarks>
		public System.Collections.Generic.IEnumerable<CpuIsaFeature> CpuIsaFeatures
		{
			get
			{
				return new CpuIsaFeaturesEnumerable(this.id);
			}
		}

		/// <summary>An iterable list of potentially available on this architecture SIMD features.</summary>
		/// <remarks>For #Unknown architecture provides an iterator over generic SIMD features.</remarks>
		public System.Collections.Generic.IEnumerable<CpuSimdFeature> CpuSimdFeatures
		{
			get
			{
				return new CpuSimdFeaturesEnumerable(this.id);
			}
		}

		/// <summary>An iterable list of potentially available on this architecture non-ISA CPU and system features.</summary>
		/// <remarks>For #Unknown architecture provides an iterator over generic non-ISA CPU and system features.</remarks>
		public System.Collections.Generic.IEnumerable<CpuSystemFeature> CpuSystemFeatures
		{
			get
			{
				return new CpuSystemFeaturesEnumerable(this.id);
			}
		}

		/// <summary>Compares for equality with another <see cref="CpuArchitecture" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(CpuArchitecture other)
		{
			return this.id == other.id;
		}

		/// <summary>Compares for equality with another object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public override bool Equals(System.Object other)
		{
			if (other == null || GetType() != other.GetType())
				return false;

			return this.Equals((CpuArchitecture)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="CpuArchitecture" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)this.id);
		}

		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			return Library.GetString(Enumeration.CpuArchitecture, this.id, StringType.ID);
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				return Library.GetString(Enumeration.CpuArchitecture, this.id, StringType.Description);
			}
		}

	}

}
