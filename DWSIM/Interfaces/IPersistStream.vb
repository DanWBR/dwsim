Imports System.Runtime.InteropServices
Imports System.Runtime.InteropServices.ComTypes

Namespace Interfaces

    '<ComImport(), Guid("00000109-0000-0000-C000-000000000046"), ComVisible(False), InterfaceType(CShort(1))> _
    'Public Interface IPersistStream
    '    Sub GetClassID(<Out()> ByRef pClassID As Guid)
    '    <PreserveSig()> _
    '    Function IsDirty() As Integer
    '    Sub Load(ByVal pStm As IStream)
    '    Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
    '    Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
    'End Interface

    '<ComImport(), Guid("7FD52380-4E07-101B-AE2D-08002B2EC713"), ComVisible(False), InterfaceType(CShort(1))> _
    'Public Interface IPersistStreamInit
    '    Sub GetClassID(<Out()> ByRef pClassID As Guid)
    '    <PreserveSig()> _
    '    Function IsDirty() As Integer
    '    Sub Load(ByVal pStm As IStream)
    '    Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
    '    Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
    '    Sub InitNew()
    'End Interface

    <ComImport(), ComVisible(False), InterfaceType(CShort(1)), Guid("0000010c-0000-0000-C000-000000000046")> _
    Public Interface IPersist
        Sub GetClassID(<Out()> ByRef pClassID As Guid)
    End Interface

    <ComImport(), Guid("00000109-0000-0000-C000-000000000046"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStream
        Inherits IPersist
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
    End Interface

    <ComImport(), Guid("7FD52380-4E07-101B-AE2D-08002B2EC713"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStreamInit
        Inherits IPersist
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
        Sub InitNew()
    End Interface

End Namespace

Namespace Interfaces2

    <ComImport(), Guid("00000109-0000-0000-C000-000000000046"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStream
        Sub GetClassID(<Out()> ByRef pClassID As Guid)
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
    End Interface

    <ComImport(), Guid("7FD52380-4E07-101B-AE2D-08002B2EC713"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStreamInit
        Sub GetClassID(<Out()> ByRef pClassID As Guid)
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
        Sub InitNew()
    End Interface

End Namespace
