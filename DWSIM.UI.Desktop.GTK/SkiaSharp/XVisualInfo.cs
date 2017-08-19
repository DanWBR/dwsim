using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace OpenTK
{

    public enum GLXAttribute : int
    {
        TRANSPARENT_BLUE_VALUE_EXT = 0x27,
        GRAY_SCALE = 0x8006,
        RGBA_TYPE = 0x8014,
        TRANSPARENT_RGB_EXT = 0x8008,
        ACCUM_BLUE_SIZE = 16,
        SHARE_CONTEXT_EXT = 0x800A,
        STEREO = 6,
        ALPHA_SIZE = 11,
        FLOAT_COMPONENTS_NV = 0x20B0,
        NONE = 0x8000,
        DEPTH_SIZE = 12,
        TRANSPARENT_INDEX_VALUE_EXT = 0x24,
        MAX_PBUFFER_WIDTH_SGIX = 0x8016,
        GREEN_SIZE = 9,
        X_RENDERABLE_SGIX = 0x8012,
        LARGEST_PBUFFER = 0x801C,
        DONT_CARE = unchecked((int)0xFFFFFFFF),
        TRANSPARENT_ALPHA_VALUE_EXT = 0x28,
        PSEUDO_COLOR_EXT = 0x8004,
        USE_GL = 1,
        SAMPLE_BUFFERS_SGIS = 100000,
        TRANSPARENT_GREEN_VALUE_EXT = 0x26,
        HYPERPIPE_ID_SGIX = 0x8030,
        COLOR_INDEX_TYPE_SGIX = 0x8015,
        SLOW_CONFIG = 0x8001,
        PRESERVED_CONTENTS = 0x801B,
        ACCUM_RED_SIZE = 14,
        EVENT_MASK = 0x801F,
        VISUAL_ID_EXT = 0x800B,
        EVENT_MASK_SGIX = 0x801F,
        SLOW_VISUAL_EXT = 0x8001,
        TRANSPARENT_GREEN_VALUE = 0x26,
        MAX_PBUFFER_WIDTH = 0x8016,
        DIRECT_COLOR_EXT = 0x8003,
        VISUAL_ID = 0x800B,
        ACCUM_GREEN_SIZE = 15,
        DRAWABLE_TYPE_SGIX = 0x8010,
        SCREEN_EXT = 0x800C,
        SAMPLES = 100001,
        HEIGHT = 0x801E,
        TRANSPARENT_INDEX_VALUE = 0x24,
        SAMPLE_BUFFERS_ARB = 100000,
        PBUFFER = 0x8023,
        RGBA_TYPE_SGIX = 0x8014,
        MAX_PBUFFER_HEIGHT = 0x8017,
        FBCONFIG_ID_SGIX = 0x8013,
        DRAWABLE_TYPE = 0x8010,
        SCREEN = 0x800C,
        RED_SIZE = 8,
        VISUAL_SELECT_GROUP_SGIX = 0x8028,
        VISUAL_CAVEAT_EXT = 0x20,
        PSEUDO_COLOR = 0x8004,
        PBUFFER_HEIGHT = 0x8040,
        STATIC_GRAY = 0x8007,
        PRESERVED_CONTENTS_SGIX = 0x801B,
        RGBA_FLOAT_TYPE_ARB = 0x20B9,
        TRANSPARENT_RED_VALUE = 0x25,
        TRANSPARENT_ALPHA_VALUE = 0x28,
        WINDOW = 0x8022,
        X_RENDERABLE = 0x8012,
        STENCIL_SIZE = 13,
        TRANSPARENT_RGB = 0x8008,
        LARGEST_PBUFFER_SGIX = 0x801C,
        STATIC_GRAY_EXT = 0x8007,
        TRANSPARENT_BLUE_VALUE = 0x27,
        DIGITAL_MEDIA_PBUFFER_SGIX = 0x8024,
        BLENDED_RGBA_SGIS = 0x8025,
        NON_CONFORMANT_VISUAL_EXT = 0x800D,
        COLOR_INDEX_TYPE = 0x8015,
        TRANSPARENT_RED_VALUE_EXT = 0x25,
        GRAY_SCALE_EXT = 0x8006,
        WINDOW_SGIX = 0x8022,
        X_VISUAL_TYPE = 0x22,
        MAX_PBUFFER_HEIGHT_SGIX = 0x8017,
        DOUBLEBUFFER = 5,
        OPTIMAL_PBUFFER_WIDTH_SGIX = 0x8019,
        X_VISUAL_TYPE_EXT = 0x22,
        WIDTH_SGIX = 0x801D,
        STATIC_COLOR_EXT = 0x8005,
        BUFFER_SIZE = 2,
        DIRECT_COLOR = 0x8003,
        MAX_PBUFFER_PIXELS = 0x8018,
        NONE_EXT = 0x8000,
        HEIGHT_SGIX = 0x801E,
        RENDER_TYPE = 0x8011,
        FBCONFIG_ID = 0x8013,
        TRANSPARENT_INDEX_EXT = 0x8009,
        TRANSPARENT_INDEX = 0x8009,
        TRANSPARENT_TYPE_EXT = 0x23,
        ACCUM_ALPHA_SIZE = 17,
        PBUFFER_SGIX = 0x8023,
        MAX_PBUFFER_PIXELS_SGIX = 0x8018,
        OPTIMAL_PBUFFER_HEIGHT_SGIX = 0x801A,
        DAMAGED = 0x8020,
        SAVED_SGIX = 0x8021,
        TRANSPARENT_TYPE = 0x23,
        MULTISAMPLE_SUB_RECT_WIDTH_SGIS = 0x8026,
        NON_CONFORMANT_CONFIG = 0x800D,
        BLUE_SIZE = 10,
        TRUE_COLOR_EXT = 0x8002,
        SAMPLES_SGIS = 100001,
        SAMPLES_ARB = 100001,
        TRUE_COLOR = 0x8002,
        RGBA = 4,
        AUX_BUFFERS = 7,
        SAMPLE_BUFFERS = 100000,
        SAVED = 0x8021,
        MULTISAMPLE_SUB_RECT_HEIGHT_SGIS = 0x8027,
        DAMAGED_SGIX = 0x8020,
        STATIC_COLOR = 0x8005,
        PBUFFER_WIDTH = 0x8041,
        WIDTH = 0x801D,
        LEVEL = 3,
        CONFIG_CAVEAT = 0x20,
        RENDER_TYPE_SGIX = 0x8011,
        SWAP_INTERVAL_EXT = 0x20F1,
        MAX_SWAP_INTERVAL_EXT = 0x20F2,
    }

    [Flags]
    internal enum XVisualInfoMask
    {
        No = 0x0,
        ID = 0x1,
        Screen = 0x2,
        Depth = 0x4,
        Class = 0x8,
        Red = 0x10,
        Green = 0x20,
        Blue = 0x40,
        ColormapSize = 0x80,
        BitsPerRGB = 0x100,
        All = 0x1FF,
    }

    [StructLayout(LayoutKind.Sequential)]
    struct XVisualInfo
    {
        public IntPtr Visual;
        public IntPtr VisualID;
        public int Screen;
        public int Depth;
        public int Class;
        public long RedMask;
        public long GreenMask;
        public long blueMask;
        public int ColormapSize;
        public int BitsPerRgb;

        public override string ToString()
        {
            return String.Format("id ({0}), screen ({1}), depth ({2}), class ({3})",
                VisualID, Screen, Depth, Class);
        }
    }

}
