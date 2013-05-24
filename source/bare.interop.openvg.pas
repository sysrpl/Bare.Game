(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.interop.openvg.txt> }
unit Bare.Interop.OpenVG;

{$i bare.inc}

interface

type
  VGubyte = Byte;
  PVGubyte = PByte;
  VGbyte = Shortint;
  PVGbyte = PShortint;
  VGfloat = Single;
  PVGfloat = PSingle;
  VGshort = Smallint;
  PVGshort = PSmallint;
  VGbitfield = LongInt;
  PVGbitfield = PLongInt;
  VGint = LongInt;
  PVGint = ^VGint;
  VGuint = LongWord;
  PVGuint = ^VGuint;
  VGPointer = Pointer;
  VGHandle = UintPtr;

  VGPath = VGHandle;
  VGImage = VGHandle;
  VGMaskLayer = VGHandle;
  VGFont = VGHandle;
  VGPaint = VGHandle;
  VGBoolean = VGint;
  VGErrorCode = VGint;
  VGParamType = VGint;
  VGRenderingQuality = VGint;
  VGPixelLayout = VGint;
  VGMatrixMode = VGint;
  VGMaskOperation = VGint;
  VGPathDatatype = VGint;
  VGPathAbsRel = VGint;
  VGPathSegment = VGint;
  VGPathCommand = VGint;
  VGPathCapabilities = VGint;
  VGPathParamType = VGint;
  VGCapStyle = VGint;
  VGJoinStyle = VGint;
  VGFillRule = VGint;
  VGPaintMode = VGint;
  VGPaintParamType = VGint;
  VGPaintType = VGint;
  VGColorRampSpreadMode = VGint;
  VGColorRampInterpolationType = VGint;
  VGTilingMode = VGint;
  VGImageFormat = VGint;
  VGImageQuality = VGint;
  VGImageParamType = VGint;
  VGImageMode = VGint;
  VGImageChannel = VGint;
  VGBlendMode = VGint;
  VGFontParamType = VGint;
  VGHardwareQueryType = VGint;
  VGHardwareQueryResult = VGint;
  VGStringID = VGint;
  VGUErrorCode = VGint;
  VGUArcType = VGint;
  
  VGfloats = array[0..1] of VGfloat;

const
  VG_INVALID_HANDLE = 0;
  VG_PATH_FORMAT_STANDARD = 0;

{ VGBoolean }

  VG_FALSE = $0000;
  VG_TRUE = $0001;

{ VGErrorCode }

  VG_NO_ERROR = $0000;
  VG_BAD_HANDLE_ERROR = $1000;
  VG_ILLEGAL_ARGUMENT_ERROR = $1001;
  VG_OUT_OF_MEMORY_ERROR = $1002;
  VG_PATH_CAPABILITY_ERROR = $1003;
  VG_UNSUPPORTED_IMAGE_FORMAT_ERROR = $1004;
  VG_UNSUPPORTED_PATH_FORMAT_ERROR = $1005;
  VG_IMAGE_IN_USE_ERROR = $1006;
  VG_NO_CONTEXT_ERROR = $1007;

{ VGParamType }

  VG_MATRIX_MODE = $1100;
  VG_FILL_RULE = $1101;
  VG_IMAGE_QUALITY = $1102;
  VG_RENDERING_QUALITY = $1103;
  VG_BLEND_MODE = $1104;
  VG_STROKE_BLEND_MODE = $1190;
  VG_FILL_BLEND_MODE = $1191;
  VG_IMAGE_MODE = $1105;
  VG_SCISSOR_RECTS = $1106;
  VG_COLOR_TRANSFORM = $1170;
  VG_COLOR_TRANSFORM_VALUES = $1171;
  VG_STROKE_LINE_WIDTH = $1110;
  VG_STROKE_CAP_STYLE = $1111;
  VG_STROKE_JOIN_STYLE = $1112;
  VG_STROKE_MITER_LIMIT = $1113;
  VG_STROKE_DASH_PATTERN = $1114;
  VG_STROKE_DASH_PHASE = $1115;
  VG_STROKE_DASH_PHASE_RESET = $1116;
  VG_STROKE_START_CAP_STYLE = $1192;
  VG_STROKE_END_CAP_STYLE = $1193;
  VG_TILE_FILL_COLOR = $1120;
  VG_CLEAR_COLOR = $1121;
  VG_GLYPH_ORIGIN = $1122;
  VG_MASKING = $1130;
  VG_SCISSORING = $1131;
  VG_PIXEL_LAYOUT = $1140;
  VG_SCREEN_LAYOUT = $1141;
  VG_FILTER_FORMAT_LINEAR = $1150;
  VG_FILTER_FORMAT_PREMULTIPLIED = $1151;
  VG_FILTER_CHANNEL_MASK = $1152;
  VG_MAX_SCISSOR_RECTS = $1160;
  VG_MAX_DASH_COUNT = $1161;
  VG_MAX_KERNEL_SIZE = $1162;
  VG_MAX_SEPARABLE_KERNEL_SIZE = $1163;
  VG_MAX_COLOR_RAMP_STOPS = $1164;
  VG_MAX_IMAGE_WIDTH = $1165;
  VG_MAX_IMAGE_HEIGHT = $1166;
  VG_MAX_IMAGE_PIXELS = $1167;
  VG_MAX_IMAGE_BYTES = $1168;
  VG_MAX_FLOAT = $1169;
  VG_MAX_GAUSSIAN_STD_DEVIATION = $116A;

{ VGRenderingQuality }

  VG_RENDERING_QUALITY_NONANTIALIASED = $1200;
  VG_RENDERING_QUALITY_FASTER = $1201;
  VG_RENDERING_QUALITY_BETTER = $1202;

{ VGPixelLayout }

  VG_PIXEL_LAYOUT_UNKNOWN = $1300;
  VG_PIXEL_LAYOUT_RGB_VERTICAL = $1301;
  VG_PIXEL_LAYOUT_BGR_VERTICAL = $1302;
  VG_PIXEL_LAYOUT_RGB_HORIZONTAL = $1303;
  VG_PIXEL_LAYOUT_BGR_HORIZONTAL = $1304;

{ VGMatrixMode }

  VG_MATRIX_PATH_USER_TO_SURFACE = $1400;
  VG_MATRIX_IMAGE_USER_TO_SURFACE = $1401;
  VG_MATRIX_FILL_PAINT_TO_USER = $1402;
  VG_MATRIX_STROKE_PAINT_TO_USER = $1403;
  VG_MATRIX_GLYPH_USER_TO_SURFACE = $1404;

{ VGMaskOperation }

  VG_CLEAR_MASK = $1500;
  VG_FILL_MASK = $1501;
  VG_SET_MASK = $1502;
  VG_UNION_MASK = $1503;
  VG_INTERSECT_MASK = $1504;
  VG_SUBTRACT_MASK = $1505;

{ VGPathDatatype }

  VG_PATH_DATATYPE_S_8 = 0;
  VG_PATH_DATATYPE_S_16 = 1;
  VG_PATH_DATATYPE_S_32 = 2;
  VG_PATH_DATATYPE_F = 3;

{ VGPathAbsRel }

  VG_ABSOLUTE = $0000;
  VG_RELATIVE = $0001;

{ VGPathSegment }

  VG_CLOSE_PATH = 0 shl 1;
  VG_MOVE_TO = 1 shl 1;
  VG_LINE_TO = 2 shl 1;
  VG_HLINE_TO = 3 shl 1;
  VG_VLINE_TO = 4 shl 1;
  VG_QUAD_TO = 5 shl 1;
  VG_CUBIC_TO = 6 shl 1;
  VG_SQUAD_TO = 7 shl 1;
  VG_SCUBIC_TO = 8 shl 1;
  VG_SCCWARC_TO = 9 shl 1;
  VG_SCWARC_TO = 10 shl 1;
  VG_LCCWARC_TO = 11 shl 1;
  VG_LCWARC_TO = 12 shl 1;

{ VGPathCommand }

  VG_MOVE_TO_ABS = VG_MOVE_TO or VG_ABSOLUTE;
  VG_MOVE_TO_REL = VG_MOVE_TO or VG_RELATIVE;
  VG_LINE_TO_ABS = VG_LINE_TO or VG_ABSOLUTE;
  VG_LINE_TO_REL = VG_LINE_TO or VG_RELATIVE;
  VG_HLINE_TO_ABS = VG_HLINE_TO or VG_ABSOLUTE;
  VG_HLINE_TO_REL = VG_HLINE_TO or VG_RELATIVE;
  VG_VLINE_TO_ABS = VG_VLINE_TO or VG_ABSOLUTE;
  VG_VLINE_TO_REL = VG_VLINE_TO or VG_RELATIVE;
  VG_QUAD_TO_ABS = VG_QUAD_TO or VG_ABSOLUTE;
  VG_QUAD_TO_REL = VG_QUAD_TO or VG_RELATIVE;
  VG_CUBIC_TO_ABS = VG_CUBIC_TO or VG_ABSOLUTE;
  VG_CUBIC_TO_REL = VG_CUBIC_TO or VG_RELATIVE;
  VG_SQUAD_TO_ABS = VG_SQUAD_TO or VG_ABSOLUTE;
  VG_SQUAD_TO_REL = VG_SQUAD_TO or VG_RELATIVE;
  VG_SCUBIC_TO_ABS = VG_SCUBIC_TO or VG_ABSOLUTE;
  VG_SCUBIC_TO_REL = VG_SCUBIC_TO or VG_RELATIVE;
  VG_SCCWARC_TO_ABS = VG_SCCWARC_TO or VG_ABSOLUTE;
  VG_SCCWARC_TO_REL = VG_SCCWARC_TO or VG_RELATIVE;
  VG_SCWARC_TO_ABS = VG_SCWARC_TO or VG_ABSOLUTE;
  VG_SCWARC_TO_REL = VG_SCWARC_TO or VG_RELATIVE;
  VG_LCCWARC_TO_ABS = VG_LCCWARC_TO or VG_ABSOLUTE;
  VG_LCCWARC_TO_REL = VG_LCCWARC_TO or VG_RELATIVE;
  VG_LCWARC_TO_ABS = VG_LCWARC_TO or VG_ABSOLUTE;
  VG_LCWARC_TO_REL = VG_LCWARC_TO or VG_RELATIVE;

{ VGPathCapabilities }

  VG_PATH_CAPABILITY_APPEND_FROM = $0001;                                
  VG_PATH_CAPABILITY_APPEND_TO = $0002;
  VG_PATH_CAPABILITY_MODIFY = $0004;                          
  VG_PATH_CAPABILITY_TRANSFORM_FROM = $0008;                  
  VG_PATH_CAPABILITY_TRANSFORM_TO = $0010;                    
  VG_PATH_CAPABILITY_INTERPOLATE_FROM = $0020;                
  VG_PATH_CAPABILITY_INTERPOLATE_TO = $0040;                  
  VG_PATH_CAPABILITY_PATH_LENGTH = $0080;                     
  VG_PATH_CAPABILITY_POINT_ALONG_PATH = $0100;                
  VG_PATH_CAPABILITY_TANGENT_ALONG_PATH = $0200;
  VG_PATH_CAPABILITY_PATH_BOUNDS = $0400;
  VG_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS = $0800;         
  VG_PATH_CAPABILITY_ALL = $FFFF;

{ VGPathParamType }

  VG_PATH_FORMAT = $1600;
  VG_PATH_DATATYPE = $1601;
  VG_PATH_SCALE = $1602;
  VG_PATH_BIAS = $1603;
  VG_PATH_NUM_SEGMENTS = $1604;
  VG_PATH_NUM_COORDS = $1605;

{ VGCapStyle }

  VG_CAP_BUTT = $1700;
  VG_CAP_ROUND = $1701;
  VG_CAP_SQUARE = $1702;

{ VGJoinStyle }

  VG_JOIN_MITER = $1800;
  VG_JOIN_ROUND = $1801;
  VG_JOIN_BEVEL = $1802;

{ VGFillRule }

  VG_EVEN_ODD = $1900;
  VG_NON_ZERO = $1901;

{ VGPaintMode }

  VG_STROKE_PATH = $0001;
  VG_FILL_PATH = $0002;

{ VGPaintParamType }

  VG_PAINT_TYPE = $1A00;
  VG_PAINT_COLOR = $1A01;
  VG_PAINT_COLOR_RAMP_SPREAD_MODE = $1A02;
  VG_PAINT_COLOR_RAMP_PREMULTIPLIED = $1A07;
  VG_PAINT_COLOR_RAMP_STOPS = $1A03;
  VG_PAINT_LINEAR_GRADIENT = $1A04;
  VG_PAINT_RADIAL_GRADIENT = $1A05;
  VG_PAINT_PATTERN_TILING_MODE = $1A06;
  VG_PAINT_CONICAL_GRADIENT = $1A90;
  VG_PAINT_COLOR_RAMP_INTERPOLATION_TYPE = $1A91;
  VG_PAINT_OPACITY = $1A92;

{ VGPaintType }

  VG_PAINT_TYPE_COLOR = $1B00;
  VG_PAINT_TYPE_LINEAR_GRADIENT = $1B01;
  VG_PAINT_TYPE_RADIAL_GRADIENT = $1B02;
  VG_PAINT_TYPE_PATTERN = $1B03;
  VG_PAINT_TYPE_CONICAL_GRADIENT = $1B90;

{ VGColorRampSpreadMode }

  VG_COLOR_RAMP_SPREAD_PAD = $1C00;
  VG_COLOR_RAMP_SPREAD_REPEAT = $1C01;
  VG_COLOR_RAMP_SPREAD_REFLECT = $1C02;

{ VGColorRampInterpolationType }

  VG_COLOR_RAMP_INTERPOLATION_LINEAR = $1C90;
  VG_COLOR_RAMP_INTERPOLATION_SMOOTH = $1C91;

{ VGTilingMode }

  VG_TILE_FILL = $1D00;
  VG_TILE_PAD = $1D01;
  VG_TILE_REPEAT = $1D02;
  VG_TILE_REFLECT = $1D03;

{ VGImageFormat }

  VG_sRGBX_8888 = 0;
  VG_sRGBA_8888 = 1;
  VG_sRGBA_8888_PRE = 2;
  VG_sRGB_565 = 3;
  VG_sRGBA_5551 = 4;
  VG_sRGBA_4444 = 5;
  VG_sL_8 = 6;
  VG_lRGBX_8888 = 7;
  VG_lRGBA_8888 = 8;
  VG_lRGBA_8888_PRE = 9;
  VG_lL_8 = 10;
  VG_A_8 = 11;
  VG_BW_1 = 12;
  VG_A_1 = 13;
  VG_A_4 = 14;
  VG_sXRGB_8888 = 0 or (1 shl 6);
  VG_sARGB_8888 = 1 or (1 shl 6);
  VG_sARGB_8888_PRE = 2 or (1 shl 6);
  VG_sARGB_1555 = 4 or (1 shl 6);
  VG_sARGB_4444 = 5 or (1 shl 6);
  VG_lXRGB_8888 = 7 or (1 shl 6);
  VG_lARGB_8888 = 8 or (1 shl 6);
  VG_lARGB_8888_PRE = 9 or (1 shl 6);
  VG_sBGRX_8888 = 0 or (1 shl 7);
  VG_sBGRA_8888 = 1 or (1 shl 7);
  VG_sBGRA_8888_PRE = 2 or (1 shl 7);
  VG_sBGR_565 = 3 or (1 shl 7);
  VG_sBGRA_5551 = 4 or (1 shl 7);
  VG_sBGRA_4444 = 5 or (1 shl 7);
  VG_lBGRX_8888 = 7 or (1 shl 7);
  VG_lBGRA_8888 = 8 or (1 shl 7);
  VG_lBGRA_8888_PRE = 9 or (1 shl 7);
  VG_sXBGR_8888 = 0 or (1 shl 6) or (1 shl 7);
  VG_sABGR_8888 = 1 or (1 shl 6) or (1 shl 7);
  VG_sABGR_8888_PRE = 2 or (1 shl 6) or (1 shl 7);
  VG_sABGR_1555 = 4 or (1 shl 6) or (1 shl 7);
  VG_sABGR_4444 = 5 or (1 shl 6) or (1 shl 7);
  VG_lXBGR_8888 = 7 or (1 shl 6) or (1 shl 7);
  VG_lABGR_8888 = 8 or (1 shl 6) or (1 shl 7);
  VG_lABGR_8888_PRE = 9 or (1 shl 6) or (1 shl 7);

{ VGImageQuality }

  VG_IMAGE_QUALITY_NONANTIALIASED = $0001;
  VG_IMAGE_QUALITY_FASTER = $0002;
  VG_IMAGE_QUALITY_BETTER = $0004;

{ VGImageParamType }

  VG_IMAGE_FORMAT = $1E00;
  VG_IMAGE_WIDTH = $1E01;
  VG_IMAGE_HEIGHT = $1E02;

{ VGImageMode }

  VG_DRAW_IMAGE_NORMAL = $1F00;
  VG_DRAW_IMAGE_MULTIPLY = $1F01;
  VG_DRAW_IMAGE_STENCIL = $1F02;

{ VGImageChannel }

  VG_RED = $0008;
  VG_GREEN = $0004;
  VG_BLUE = $0002;
  VG_ALPHA = $0001;

{ VGBlendMode }

  VG_BLEND_SRC = $2000;
  VG_BLEND_SRC_OVER = $2001;
  VG_BLEND_DST_OVER = $2002;
  VG_BLEND_SRC_IN = $2003;
  VG_BLEND_DST_IN = $2004;
  VG_BLEND_MULTIPLY = $2005;
  VG_BLEND_SCREEN = $2006;
  VG_BLEND_DARKEN = $2007;
  VG_BLEND_LIGHTEN = $2008;
  VG_BLEND_ADDITIVE = $2009;
  VG_BLEND_CLEAR = $2090;
  VG_BLEND_DST = $2091;
  VG_BLEND_SRC_OUT = $2092;
  VG_BLEND_DST_OUT = $2093;
  VG_BLEND_SRC_ATOP = $2094;
  VG_BLEND_DST_ATOP = $2095;
  VG_BLEND_XOR = $2096;
  VG_BLEND_OVERLAY = $2097;
  VG_BLEND_COLOR_DODGE = $2098;
  VG_BLEND_COLOR_BURN = $2099;
  VG_BLEND_HARD_LIGHT = $209A;
  VG_BLEND_SOFT_LIGHT = $209B;
  VG_BLEND_DIFFERENCE = $209C;
  VG_BLEND_EXCLUSION = $209D;

{ VGFontParamType }

  VG_FONT_NUM_GLYPHS = $2F00;

{ VGHardwareQueryType }

  VG_IMAGE_FORMAT_QUERY = $2100;
  VG_PATH_DATATYPE_QUERY = $2101;

{ VGHardwareQueryResult }

  VG_HARDWARE_ACCELERATED = $2200;
  VG_HARDWARE_UNACCELERATED = $2201;

{ VGStringID }

  VG_VENDOR = $2300;
  VG_RENDERER = $2301;
  VG_VERSION = $2302;
  VG_EXTENSIONS = $2303;

{ VGUErrorCode }

  VGU_NO_ERROR = 0;
  VGU_BAD_HANDLE_ERROR = $F000;
  VGU_ILLEGAL_ARGUMENT_ERROR = $F001;
  VGU_OUT_OF_MEMORY_ERROR = $F002;
  VGU_PATH_CAPABILITY_ERROR = $F003;
  VGU_BAD_WARP_ERROR = $F004;

{ VGUArcType }

  VGU_ARC_OPEN = $F100;
  VGU_ARC_CHORD = $F101;
  VGU_ARC_PIE = $F102;

var
  vgGetError: function: VGErrorCode; cdecl;
  vgFlush: procedure; cdecl;
  vgFinish: procedure; cdecl;
  vgSetf: procedure(kind: VGParamType; value: VGfloat); cdecl;
  vgSeti: procedure(kind: VGParamType; value: VGint); cdecl;
  vgSetfv: procedure(kind: VGParamType; count: VGint; values: PVGfloat); cdecl;
  vgSetiv: procedure(kind: VGParamType; count: VGint; values: PVGint); cdecl;
  vgGetf: function(kind: VGParamType): VGfloat; cdecl;
  vgGeti: function(kind: VGParamType): VGint; cdecl;
  vgGetVectorSize: function(kind: VGParamType): VGint; cdecl;
  vgGetfv: procedure(kind: VGParamType; count: VGint; values: PVGfloat); cdecl;
  vgGetiv: procedure(kind: VGParamType; count: VGint; values: PVGint); cdecl;
  vgSetParameterf: procedure(obj: VGHandle; paramType: VGint; value: VGfloat); cdecl;
  vgSetParameteri: procedure(obj: VGHandle; paramType: VGint; value: VGint); cdecl;
  vgSetParameterfv: procedure(obj: VGHandle; paramType, count: VGint; values: PVGfloat); cdecl;
  vgSetParameteriv: procedure(obj: VGHandle; paramType, count: VGint; values: PVGint); cdecl;
  vgGetParameterf: function(obj: VGHandle; paramType: VGint): VGfloat; cdecl;
  vgGetParameteri: function(obj: VGHandle; paramType: VGint): VGint; cdecl;
  vgGetParameterVectorSize: function(obj: VGHandle; paramType: VGint): VGint; cdecl;
  vgGetParameterfv: procedure(obj: VGHandle; paramType: VGint; count: VGint; values: PVGfloat); cdecl;
  vgGetParameteriv: procedure(obj: VGHandle; paramType, count: VGint; values: PVGint); cdecl;
  vgLoadIdentity: procedure; cdecl;
  vgLoadMatrix: procedure(m: PVGfloat); cdecl;
  vgGetMatrix: procedure(m: PVGfloat); cdecl;
  vgMultMatrix: procedure(m: PVGfloat); cdecl;
  vgTranslate: procedure(tx: VGfloat; ty: VGfloat); cdecl;
  vgScale: procedure(sx: VGfloat; sy: VGfloat); cdecl;
  vgShear: procedure(shx: VGfloat; shy: VGfloat); cdecl;
  vgRotate: procedure(angle: VGfloat); cdecl;
  vgMask: procedure(mask: VGHandle; operation: VGMaskOperation; x, y, width, height: VGint); cdecl;
  vgRenderToMask: procedure(path: VGPath; paintModes: VGbitfield; operation: VGMaskOperation); cdecl;
  vgCreateMaskLayer: function(width: VGint; height: VGint): VGMaskLayer; cdecl;
  vgDestroyMaskLayer: procedure(maskLayer: VGMaskLayer); cdecl;
  vgFillMaskLayer: procedure(maskLayer: VGMaskLayer; x: VGint; y: VGint; width: VGint; height: VGint; value: VGfloat); cdecl;
  vgCopyMask: procedure(maskLayer: VGMaskLayer; dx, dy, sx, sy, width, height: VGint); cdecl;
  vgClear: procedure(x, y, width, height: VGint); cdecl;
  vgCreatePath: function(pathFormat: VGint; datatype: VGPathDataType; scale: VGfloat; bias: VGfloat; segmentCapacityHint: VGint; coordCapacityHint: VGint; capabilities: VGPathCapabilities): VGPath; cdecl;
  vgClearPath: procedure(path: VGPath; capabilities: VGbitfield); cdecl;
  vgDestroyPath: procedure(path: VGPath); cdecl;
  vgRemovePathCapabilities: procedure(path: VGPath; capabilities: VGbitfield); cdecl;
  vgGetPathCapabilities: function(path: VGPath): VGbitfield; cdecl;
  vgAppendPath: procedure(dstPath: VGPath; srcPath: VGPath); cdecl;
  vgAppendPathData: procedure(dstPath: VGPath; numSegments: VGint; pathSegments: PVGubyte; pathData: PVGfloat); cdecl;
  vgModifyPathCoords: procedure(dstPath: VGPath; startIndex, numSegments: VGint; pathData: PVGfloat); cdecl;
  vgTransformPath: procedure(dstPath: VGPath; srcPath: VGPath); cdecl;
  vgInterpolatePath: function(dstPath: VGPath; startPath: VGPath; endPath: VGPath; amount: VGfloat): VGBoolean; cdecl;
  vgPathLength: function(path: VGPath; startSegment: VGint; numSegments: VGint): VGfloat; cdecl;
  vgPointAlongPath: procedure(path: VGPath; startSegment: VGint; numSegments: VGint; distance: VGfloat; var x, y, tangentX, tangentY: VGfloat); cdecl;
  vgPathBounds: procedure(path: VGPath; var minX, minY, width, height: VGfloat); cdecl;
  vgPathTransformedBounds: procedure(path: VGPath; var minX, minY, width, height: VGfloat); cdecl;
  vgDrawPath: procedure(path: VGPath; paintModes: VGbitfield); cdecl;
  vgCreatePaint: function: VGPaint; cdecl;
  vgDestroyPaint: procedure(paint: VGPaint); cdecl;
  vgSetPaint: procedure(paint: VGPaint; paintModes: VGbitfield); cdecl;
  vgGetPaint: function(paintMode: VGPaintMode): VGPaint; cdecl;
  vgSetColor: procedure(paint: VGPaint; rgba: VGuint); cdecl;
  vgGetColor: function(paint: VGPaint): VGuint; cdecl;
  vgPaintPattern: procedure(paint: VGPaint; pattern: VGImage); cdecl;
  vgCreateImage: function(format: VGImageFormat; width: VGint; height: VGint; allowedQuality: VGbitfield): VGImage; cdecl;
  vgDestroyImage: procedure(image: VGImage); cdecl;
  vgClearImage: procedure(image: VGImage; x, y, width, height: VGint); cdecl;
  vgImageSubData: procedure(image: VGImage; data: Pointer; dataStride: VGint; dataFormat: VGImageFormat; x, y, width, height: VGint); cdecl;
  vgGetImageSubData: procedure(image: VGImage; data: Pointer; dataStride: VGint; dataFormat: VGImageFormat; x, y, width, height: VGint); cdecl;
  vgChildImage: function(parent: VGImage; x, y, width, height: VGint): VGImage; cdecl;
  vgGetParent: function(image: VGImage): VGImage; cdecl;
  vgCopyImage: procedure(dst: VGImage; dx, dy: VGint; src: VGImage; sx, sy, width, height: VGint; dither: VGBoolean); cdecl;
  vgDrawImage: procedure(image: VGImage); cdecl;
  vgSetPixels: procedure(dx, dy: VGint; src: VGImage; sx, sy, width, height: VGint); cdecl;
  vgWritePixels: procedure(data: Pointer; dataStride: VGint; dataFormat: VGImageFormat; dx, dy, width, height: VGint); cdecl;
  vgGetPixels: procedure(dst: VGImage; dx, dy, sx, sy, width, height: VGint); cdecl;
  vgReadPixels: procedure(data: Pointer; dataStride: VGint; dataFormat: VGImageFormat; sx, sy, width, height: VGint); cdecl;
  vgCopyPixels: procedure(dx, dy, sx, sy, width, height: VGint); cdecl;
  vgCreateFont: function(glyphCapacityHint: VGint): VGFont; cdecl;
  vgDestroyFont: procedure(font: VGFont); cdecl;
  vgSetGlyphToPath: procedure(font: VGFont; glyphIndex: VGuint; path: VGPath; isHinted: VGBoolean; const glyphOrigin, escapement: VGfloats); cdecl;
  vgSetGlyphToImage: procedure(font: VGFont; glyphIndex: VGuint; image: VGImage; const glyphOrigin, escapement: VGfloats); cdecl;
  vgClearGlyph: procedure(font: VGFont; glyphIndex: VGuint); cdecl;
  vgDrawGlyph: procedure(font: VGFont; glyphIndex: VGuint; paintModes: VGbitfield; allowAutoHinting: VGBoolean); cdecl;
  vgDrawGlyphs: procedure(font: VGFont; glyphCount: VGint; glyphIndices: PVGuint; adjustmentsX, adjustmentsY: PVGfloat; paintModes: VGbitfield; allowAutoHinting: VGBoolean); cdecl;
  vgColorMatrix: procedure(dst, src: VGImage; matrix: PVGfloat); cdecl;
  vgConvolve: procedure(dst, src: VGImage; kernelWidth, kernelHeight, shiftX, shiftY: VGint; kernel: PVGShort; scale: VGfloat; bias: VGfloat; tilingMode: VGTilingMode); cdecl;
  vgSeparableConvolve: procedure(dst, src: VGImage; kernelWidth, kernelHeight, shiftX, shiftY: VGint; kernelX, kernelY: PVGShort; scale: VGfloat; bias: VGfloat; tilingMode: VGTilingMode); cdecl;
  vgGaussianBlur: procedure(dst, src: VGImage; stdDeviationX, stdDeviationY: VGfloat; tilingMode: VGTilingMode); cdecl;
  vgLookup: procedure(dst, src: VGImage; redLUT, greenLUT, blueLUT, alphaLUT: PVGubyte; outputLinear: VGBoolean; outputPremultiplied: VGBoolean); cdecl;
  vgLookupSingle: procedure(dst, src: VGImage; lookupTable: PVGuint; sourceChannel: VGImageChannel; outputLinear: VGBoolean; outputPremultiplied: VGBoolean); cdecl;
  vgHardwareQuery: function(key: VGHardwareQueryType; setting: VGint): VGHardwareQueryResult; cdecl;
  vgGetString: function(name: VGStringId): PVGubyte; cdecl;
  vguLine: function(path: VGPath; x0, y0, x1, y1: VGfloat): VGUErrorCode; cdecl;
  vguPolygon: function(path: VGPath; points: PVGfloat; count: VGint; closed: VGBoolean): VGUErrorCode; cdecl;
  vguRect: function(path: VGPath; x, y, width, height: VGfloat): VGUErrorCode; cdecl;
  vguRoundRect: function(path: VGPath; x, y, width, height, arcWidth, arcHeight: VGfloat): VGUErrorCode; cdecl;
  vguEllipse: function(path: VGPath; cx, cy, width, height: VGfloat): VGUErrorCode; cdecl;
  vguArc: function(path: VGPath; x, y, width, height, startAngle, angleExtent: VGfloat; arcType: VGUArcType): VGUErrorCode; cdecl;
  vguComputeWarpQuadToSquare: function(sx0, sy0, sx1, sy1, sx2, sy2, sx3, sy3: VGfloat; matrix: PVGfloat): VGUErrorCode; cdecl;
  vguComputeWarpSquareToQuad: function(dx0, dy0, dx1, dy1, dx2, dy2, dx3, dy3: VGfloat; matrix: PVGfloat): VGUErrorCode; cdecl;
  vguComputeWarpQuadToQuad: function(dx0, dy0, dx1, dy1, dx2, dy2, dx3, dy3, sx0, sy0, sx1, sy1, sx2, sy2, sx3, sy3: VGFloat; matrix: PVGFloat): VGUErrorCode; cdecl;

const
  OVG_AM_SEPARABLE_CAP_STYLE = 1;
  OVG_AM_COLOR_RAMP_INTERPOLATION = 1;
  OVG_AM_CONICAL_GRADIENT = 1;
  OVG_AM_ADVANCED_BLEND_MODES = 1;
  OVG_AM_SEPARABLE_BLEND_MODES = 1;
  OVG_AM_PAINT_OPACITY = 1;

var
  vgInitContextAM: function(surfaceWidth, surfaceHeight: VGint; surfaceLinearColorSpace: VGBoolean): VGBoolean; cdecl;
  vgDestroyContextAM: procedure; cdecl;
  vgResizeSurfaceAM: procedure(surfaceWidth, surfaceHeight: VGint); cdecl;
  vgGetSurfaceWidthAM: function: VGint; cdecl;
  vgGetSurfaceHeightAM: function: VGint; cdecl;
  vgGetSurfaceFormatAM: function: VGImageFormat; cdecl;
  vgGetSurfacePixelsAM: function: VGPointer; cdecl;

var
  OpenVGManager: record
    Load: function: Boolean;
    GetProcAddress: function(ProcName: PChar): Pointer;
  end;

function OpenVGInit: Boolean;

implementation

var
  Initialized: Boolean;

function OpenVGInit: Boolean;

  procedure GetProc(var Proc: Pointer; Name: PChar);
  begin
    Proc := OpenVGManager.GetProcAddress(Name);
    Initialized := Initialized and (Proc <> nil);
  end;

begin
  if Initialized then
    Exit(True);
  if @OpenVGManager.Load = nil then
    Exit(False);
  if @OpenVGManager.GetProcAddress = nil then
    Exit(False);
  if not OpenVGManager.Load then
    Exit(False);
  Initialized := True;
  GetProc(@vgGetError, 'vgGetError');
  GetProc(@vgFlush, 'vgFlush');
  GetProc(@vgFinish, 'vgFinish');
  GetProc(@vgSetf, 'vgSetf');
  GetProc(@vgSeti, 'vgSeti');
  GetProc(@vgSetfv, 'vgSetfv');
  GetProc(@vgSetiv, 'vgSetiv');
  GetProc(@vgGetf, 'vgGetf');
  GetProc(@vgGeti, 'vgGeti');
  GetProc(@vgGetVectorSize, 'vgGetVectorSize');
  GetProc(@vgGetfv, 'vgGetfv');
  GetProc(@vgGetiv, 'vgGetiv');
  GetProc(@vgSetParameterf, 'vgSetParameterf');
  GetProc(@vgSetParameteri, 'vgSetParameteri');
  GetProc(@vgSetParameterfv, 'vgSetParameterfv');
  GetProc(@vgSetParameteriv, 'vgSetParameteriv');
  GetProc(@vgGetParameterf, 'vgGetParameterf');
  GetProc(@vgGetParameteri, 'vgGetParameteri');
  GetProc(@vgGetParameterVectorSize, 'vgGetParameterVectorSize');
  GetProc(@vgGetParameterfv, 'vgGetParameterfv');
  GetProc(@vgGetParameteriv, 'vgGetParameteriv');
  GetProc(@vgLoadIdentity, 'vgLoadIdentity');
  GetProc(@vgLoadMatrix, 'vgLoadMatrix');
  GetProc(@vgGetMatrix, 'vgGetMatrix');
  GetProc(@vgMultMatrix, 'vgMultMatrix');
  GetProc(@vgTranslate, 'vgTranslate');
  GetProc(@vgScale, 'vgScale');
  GetProc(@vgShear, 'vgShear');
  GetProc(@vgRotate, 'vgRotate');
  GetProc(@vgMask, 'vgMask');
  GetProc(@vgRenderToMask, 'vgRenderToMask');
  GetProc(@vgCreateMaskLayer, 'vgCreateMaskLayer');
  GetProc(@vgDestroyMaskLayer, 'vgDestroyMaskLayer');
  GetProc(@vgFillMaskLayer, 'vgFillMaskLayer');
  GetProc(@vgCopyMask, 'vgCopyMask');
  GetProc(@vgClear, 'vgClear');
  GetProc(@vgCreatePath, 'vgCreatePath');
  GetProc(@vgClearPath, 'vgClearPath');
  GetProc(@vgDestroyPath, 'vgDestroyPath');
  GetProc(@vgRemovePathCapabilities, 'vgRemovePathCapabilities');
  GetProc(@vgGetPathCapabilities, 'vgGetPathCapabilities');
  GetProc(@vgAppendPath, 'vgAppendPath');
  GetProc(@vgAppendPathData, 'vgAppendPathData');
  GetProc(@vgModifyPathCoords, 'vgModifyPathCoords');
  GetProc(@vgTransformPath, 'vgTransformPath');
  GetProc(@vgInterpolatePath, 'vginterpolatePath');
  GetProc(@VGPathLength, 'vgPathLength');
  GetProc(@vgPointAlongPath, 'vgPointAlongPath');
  GetProc(@VGPathBounds, 'vgPathBounds');
  GetProc(@VGPathTransformedBounds, 'vgPathTransformedBounds');
  GetProc(@vgDrawPath, 'vgDrawPath');
  GetProc(@vgCreatePaint, 'vgCreatePaint');
  GetProc(@vgDestroyPaint, 'vgDestroyPaint');
  GetProc(@vgSetPaint, 'vgSetPaint');
  GetProc(@vgGetPaint, 'vgGetPaint');
  GetProc(@vgSetColor, 'vgSetColor');
  GetProc(@vgGetColor, 'vgGetColor');
  GetProc(@vgPaintPattern, 'vgPaintPattern');
  GetProc(@vgCreateImage, 'vgCreateImage');
  GetProc(@vgDestroyImage, 'vgDestroyImage');
  GetProc(@vgClearImage, 'vgClearImage');
  GetProc(@vgImageSubData, 'vgImageSubData');
  GetProc(@vgGetImageSubData, 'vgGetImageSubData');
  GetProc(@vgChildImage, 'vgChildImage');
  GetProc(@vgGetParent, 'vgGetParent');
  GetProc(@vgCopyImage, 'vgCopyImage');
  GetProc(@vgDrawImage, 'vgDrawImage');
  GetProc(@vgSetPixels, 'vgSetPixels');
  GetProc(@vgWritePixels, 'vgWritePixels');
  GetProc(@vgGetPixels, 'vgGetPixels');
  GetProc(@vgReadPixels, 'vgReadPixels');
  GetProc(@vgCopyPixels, 'vgCopyPixels');
  GetProc(@vgCreateFont, 'vgCreateFont');
  GetProc(@vgDestroyFont, 'vgDestroyFont');
  GetProc(@vgSetGlyphToPath, 'vgSetGlyphToPath');
  GetProc(@vgSetGlyphToImage, 'vgSetGlyphToImage');
  GetProc(@vgClearGlyph, 'vgClearGlyph');
  GetProc(@vgDrawGlyph, 'vgDrawGlyph');
  GetProc(@vgDrawGlyphs, 'vgDrawGlyphs');
  GetProc(@vgColorMatrix, 'vgColorMatrix');
  GetProc(@vgConvolve, 'vgConvolve');
  GetProc(@vgSeparableConvolve, 'vgSeparableConvolve');
  GetProc(@vgGaussianBlur, 'vgGaussianBlur');
  GetProc(@vgLookup, 'vgLookup');
  GetProc(@vgLookupSingle, 'vgLookupSingle');
  GetProc(@vgHardwareQuery, 'vgHardwareQuery');
  GetProc(@vgGetString, 'vgGetString');
  GetProc(@vguLine, 'vguLine');
  GetProc(@vguPolygon, 'vguPolygon');
  GetProc(@vguRect, 'vguRect');
  GetProc(@vguRoundRect, 'vguRoundRect');
  GetProc(@vguEllipse, 'vguEllipse');
  GetProc(@vguArc, 'vguArc');
  GetProc(@vguComputeWarpQuadToSquare, 'vguComputeWarpQuadToSquare');
  GetProc(@vguComputeWarpSquareToQuad, 'vguComputeWarpSquareToQuad');
  GetProc(@vguComputeWarpQuadToQuad, 'vguComputeWarpQuadToQuad');
  GetProc(@vgInitContextAM, 'vgInitContextAM');
  GetProc(@vgDestroyContextAM, 'vgDestroyContextAM');
  GetProc(@vgResizeSurfaceAM, 'vgResizeSurfaceAM');
  GetProc(@vgGetSurfaceWidthAM, 'vgGetSurfaceWidthAM');
  GetProc(@vgGetSurfaceHeightAM, 'vgGetSurfaceHeightAM');
  GetProc(@vgGetSurfaceFormatAM, 'vgGetSurfaceFormatAM');
  GetProc(@vgGetSurfacePixelsAM, 'vgGetSurfacePixelsAM');
  Result := Initialized;
end;

end.
