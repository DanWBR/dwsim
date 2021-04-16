Imports System.Runtime.InteropServices

Friend Module NativeMethods

    <DllImport("user32.dll", SetLastError:=True)> Friend Function SetProcessDpiAwarenessContext(ByVal dpiFlag As Integer) As Boolean

    End Function

    <DllImport("SHCore.dll", SetLastError:=True)>
    Friend Function SetProcessDpiAwareness(ByVal awareness As PROCESS_DPI_AWARENESS) As Boolean

    End Function

    <DllImport("user32.dll")>
    Friend Function SetProcessDPIAware() As Boolean

    End Function

    Friend Enum PROCESS_DPI_AWARENESS
        Process_DPI_Unaware = 0
        Process_System_DPI_Aware = 1
        Process_Per_Monitor_DPI_Aware = 2
    End Enum

    Friend Enum DPI_AWARENESS_CONTEXT
        DPI_AWARENESS_CONTEXT_UNAWARE = 16
        DPI_AWARENESS_CONTEXT_SYSTEM_AWARE = 17
        DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE = 18
        DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 = 34
    End Enum
End Module
