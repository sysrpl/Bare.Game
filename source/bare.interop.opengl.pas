(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.interop.opengl.txt> }
unit Bare.Interop.OpenGL;

{$i bare.inc}

interface

{TODO: Add OpenGL ES 1.0 and 2.0, as well OpenGL versions > 3.0}

{ Choose one of the following OpenGL versions by removing the space before $ }

{ $define opengl_1_1 }
{ $define opengl_1_2 }
{ $define opengl_1_3 }
{ $define opengl_1_4 }
{ $define opengl_1_5 }
{ $define opengl_2_0 }
{ $define opengl_2_1 }
{$define opengl_3_0}

{$ifdef opengl_3_0}
  {$define opengl_1_1}
  {$define opengl_1_2}
  {$define opengl_1_3}
  {$define opengl_1_4}
  {$define opengl_1_5}
  {$define opengl_2_0}
  {$define opengl_2_1}
{$endif}
{$ifdef opengl_2_1}
  {$define opengl_1_1}
  {$define opengl_1_2}
  {$define opengl_1_3}
  {$define opengl_1_4}
  {$define opengl_1_5}
  {$define opengl_2_0}
{$endif}
{$ifdef opengl_2_0}
  {$define opengl_1_1}
  {$define opengl_1_2}
  {$define opengl_1_3}
  {$define opengl_1_4}
  {$define opengl_1_5}
{$endif}
{$ifdef opengl_1_5}
  {$define opengl_1_1}
  {$define opengl_1_2}
  {$define opengl_1_3}
  {$define opengl_1_4}
{$endif}
{$ifdef opengl_1_4}
  {$define opengl_1_1}
  {$define opengl_1_2}
  {$define opengl_1_3}
{$endif}
{$ifdef opengl_1_3}
  {$define opengl_1_1}
  {$define opengl_1_2}
{$endif}
{$ifdef opengl_1_2}
  {$define opengl_1_1}
{$endif}
{$ifndef opengl_1_1}
'You must choose a version of OpenGL (see comment above)'
{$endif}

type
  GLenum = LongWord;
  GLboolean = ByteBool;
  GLbitfield = Cardinal;
  GLbyte = Shortint;
  GLshort = SmallInt;
  GLint = LongInt;
  GLsizei = LongInt;
  GLubyte = Byte;
  GLushort = Word;
  GLuint = LongWord;
  GLfloat = Single;
  GLclampf = Single;
  GLdouble = Double;
  GLclampd = Double;
  PGLenum = ^GLenum;
  PGLboolean = ^GLboolean;
  PGLbyte = ^GLbyte;
  PGLshort = ^GLshort;
  PGLint = ^GLint;
  PGLsizei = ^GLsizei;
  PGLubyte = ^GLubyte;
  PGLushort = ^GLushort;
  PGLuint = ^GLuint;
  PGLclampf = ^GLclampf;
  PGLfloat =  ^GLfloat;
  PGLdouble = ^GLdouble;
  PGLclampd = ^GLclampd;
  GLchar = Char;
  PGLchar = PChar;
  PPGLchar = ^PGLChar;
  GLhandle = Cardinal;
  PGLhandle = ^GLhandle;
  PGLvoid = Pointer;
  PPGLvoid = ^PGLvoid;

{$ifdef opengl_1_1}
const
  GL_CURRENT_BIT                      = $00000001;
  GL_POINT_BIT                        = $00000002;
  GL_LINE_BIT                         = $00000004;
  GL_POLYGON_BIT                      = $00000008;
  GL_POLYGON_STIPPLE_BIT              = $00000010;
  GL_PIXEL_MODE_BIT                   = $00000020;
  GL_LIGHTING_BIT                     = $00000040;
  GL_FOG_BIT                          = $00000080;
  GL_DEPTH_BUFFER_BIT                 = $00000100;
  GL_ACCUM_BUFFER_BIT                 = $00000200;
  GL_STENCIL_BUFFER_BIT               = $00000400;
  GL_VIEWPORT_BIT                     = $00000800;
  GL_TRANSFORM_BIT                    = $00001000;
  GL_ENABLE_BIT                       = $00002000;
  GL_COLOR_BUFFER_BIT                 = $00004000;
  GL_HINT_BIT                         = $00008000;
  GL_EVAL_BIT                         = $00010000;
  GL_LIST_BIT                         = $00020000;
  GL_TEXTURE_BIT                      = $00040000;
  GL_SCISSOR_BIT                      = $00080000;
  GL_ALL_ATTRIB_BITS                  = $000FFFFF;

{ Boolean }

  GL_FALSE                            = 0;
  GL_TRUE                             = 1;

{ BeginMode }

  GL_POINTS                           = $0000;
  GL_LINES                            = $0001;
  GL_LINE_LOOP                        = $0002;
  GL_LINE_STRIP                       = $0003;
  GL_TRIANGLES                        = $0004;
  GL_TRIANGLE_STRIP                   = $0005;
  GL_TRIANGLE_FAN                     = $0006;
  GL_QUADS                            = $0007;
  GL_QUAD_STRIP                       = $0008;
  GL_POLYGON                          = $0009;

{ AccumOp }

  GL_ACCUM                            = $0100;
  GL_LOAD                             = $0101;
  GL_RETURN                           = $0102;
  GL_MULT                             = $0103;
  GL_ADD                              = $0104;

{ AlphaFunction }

  GL_NEVER                            = $0200;
  GL_LESS                             = $0201;
  GL_EQUAL                            = $0202;
  GL_LEQUAL                           = $0203;
  GL_GREATER                          = $0204;
  GL_NOTEQUAL                         = $0205;
  GL_GEQUAL                           = $0206;
  GL_ALWAYS                           = $0207;

{ BlendingFactorDest }

  GL_ZERO                             = 0;
  GL_ONE                              = 1;
  GL_SRC_COLOR                        = $0300;
  GL_ONE_MINUS_SRC_COLOR              = $0301;
  GL_SRC_ALPHA                        = $0302;
  GL_ONE_MINUS_SRC_ALPHA              = $0303;
  GL_DST_ALPHA                        = $0304;
  GL_ONE_MINUS_DST_ALPHA              = $0305;

{ BlendingFactorSrc }

  GL_DST_COLOR                        = $0306;
  GL_ONE_MINUS_DST_COLOR              = $0307;
  GL_SRC_ALPHA_SATURATE               = $0308;

{ DrawBufferMode }

  GL_NONE                             = 0;
  GL_FRONT_LEFT                       = $0400;
  GL_FRONT_RIGHT                      = $0401;
  GL_BACK_LEFT                        = $0402;
  GL_BACK_RIGHT                       = $0403;
  GL_FRONT                            = $0404;
  GL_BACK                             = $0405;
  GL_LEFT                             = $0406;
  GL_RIGHT                            = $0407;
  GL_FRONT_AND_BACK                   = $0408;
  GL_AUX0                             = $0409;
  GL_AUX1                             = $040A;
  GL_AUX2                             = $040B;
  GL_AUX3                             = $040C;

{ ErrorCode }

  GL_NO_ERROR                         = 0;
  GL_INVALID_ENUM                     = $0500;
  GL_INVALID_VALUE                    = $0501;
  GL_INVALID_OPERATION                = $0502;
  GL_STACK_OVERFLOW                   = $0503;
  GL_STACK_UNDERFLOW                  = $0504;
  GL_OUT_OF_MEMORY                    = $0505;

{ FeedBackMode }

  GL_2D                               = $0600;
  GL_3D                               = $0601;
  GL_3D_COLOR                         = $0602;
  GL_3D_COLOR_TEXTURE                 = $0603;
  GL_4D_COLOR_TEXTURE                 = $0604;

{ FeedBackToken }

  GL_PASS_THROUGH_TOKEN               = $0700;
  GL_POINT_TOKEN                      = $0701;
  GL_LINE_TOKEN                       = $0702;
  GL_POLYGON_TOKEN                    = $0703;
  GL_BITMAP_TOKEN                     = $0704;
  GL_DRAW_PIXEL_TOKEN                 = $0705;
  GL_COPY_PIXEL_TOKEN                 = $0706;
  GL_LINE_RESET_TOKEN                 = $0707;

{ FogMode }

  GL_EXP                              = $0800;
  GL_EXP2                             = $0801;

{ FrontFaceDirection }

  GL_CW                               = $0900;
  GL_CCW                              = $0901;

{ GetMapTarget }

  GL_COEFF                            = $0A00;
  GL_ORDER                            = $0A01;
  GL_DOMAIN                           = $0A02;

{ GetPixelMap }

  GL_PIXEL_MAP_I_TO_I                 = $0C70;
  GL_PIXEL_MAP_S_TO_S                 = $0C71;
  GL_PIXEL_MAP_I_TO_R                 = $0C72;
  GL_PIXEL_MAP_I_TO_G                 = $0C73;
  GL_PIXEL_MAP_I_TO_B                 = $0C74;
  GL_PIXEL_MAP_I_TO_A                 = $0C75;
  GL_PIXEL_MAP_R_TO_R                 = $0C76;
  GL_PIXEL_MAP_G_TO_G                 = $0C77;
  GL_PIXEL_MAP_B_TO_B                 = $0C78;
  GL_PIXEL_MAP_A_TO_A                 = $0C79;

{ GetTarget }

  GL_CURRENT_COLOR                    = $0B00;
  GL_CURRENT_INDEX                    = $0B01;
  GL_CURRENT_NORMAL                   = $0B02;
  GL_CURRENT_TEXTURE_COORDS           = $0B03;
  GL_CURRENT_RASTER_COLOR             = $0B04;
  GL_CURRENT_RASTER_INDEX             = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS    = $0B06;
  GL_CURRENT_RASTER_POSITION          = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID    = $0B08;
  GL_CURRENT_RASTER_DISTANCE          = $0B09;
  GL_POINT_SMOOTH                     = $0B10;
  GL_POINT_SIZE                       = $0B11;
  GL_POINT_SIZE_RANGE                 = $0B12;
  GL_POINT_SIZE_GRANULARITY           = $0B13;
  GL_LINE_SMOOTH                      = $0B20;
  GL_LINE_WIDTH                       = $0B21;
  GL_LINE_WIDTH_RANGE                 = $0B22;
  GL_LINE_WIDTH_GRANULARITY           = $0B23;
  GL_LINE_STIPPLE                     = $0B24;
  GL_LINE_STIPPLE_PATTERN             = $0B25;
  GL_LINE_STIPPLE_REPEAT              = $0B26;
  GL_LIST_MODE                        = $0B30;
  GL_MAX_LIST_NESTING                 = $0B31;
  GL_LIST_BASE                        = $0B32;
  GL_LIST_INDEX                       = $0B33;
  GL_POLYGON_MODE                     = $0B40;
  GL_POLYGON_SMOOTH                   = $0B41;
  GL_POLYGON_STIPPLE                  = $0B42;
  GL_EDGE_FLAG                        = $0B43;
  GL_CULL_FACE                        = $0B44;
  GL_CULL_FACE_MODE                   = $0B45;
  GL_FRONT_FACE                       = $0B46;
  GL_LIGHTING                         = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER         = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE             = $0B52;
  GL_LIGHT_MODEL_AMBIENT              = $0B53;
  GL_SHADE_MODEL                      = $0B54;
  GL_COLOR_MATERIAL_FACE              = $0B55;
  GL_COLOR_MATERIAL_PARAMETER         = $0B56;
  GL_COLOR_MATERIAL                   = $0B57;
  GL_FOG                              = $0B60;
  GL_FOG_INDEX                        = $0B61;
  GL_FOG_DENSITY                      = $0B62;
  GL_FOG_START                        = $0B63;
  GL_FOG_END                          = $0B64;
  GL_FOG_MODE                         = $0B65;
  GL_FOG_COLOR                        = $0B66;
  GL_DEPTH_RANGE                      = $0B70;
  GL_DEPTH_TEST                       = $0B71;
  GL_DEPTH_WRITEMASK                  = $0B72;
  GL_DEPTH_CLEAR_VALUE                = $0B73;
  GL_DEPTH_FUNC                       = $0B74;
  GL_ACCUM_CLEAR_VALUE                = $0B80;
  GL_STENCIL_TEST                     = $0B90;
  GL_STENCIL_CLEAR_VALUE              = $0B91;
  GL_STENCIL_FUNC                     = $0B92;
  GL_STENCIL_VALUE_MASK               = $0B93;
  GL_STENCIL_FAIL                     = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL          = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS          = $0B96;
  GL_STENCIL_REF                      = $0B97;
  GL_STENCIL_WRITEMASK                = $0B98;
  GL_MATRIX_MODE                      = $0BA0;
  GL_NORMALIZE                        = $0BA1;
  GL_VIEWPORT                         = $0BA2;
  GL_MODELVIEW_STACK_DEPTH            = $0BA3;
  GL_PROJECTION_STACK_DEPTH           = $0BA4;
  GL_TEXTURE_STACK_DEPTH              = $0BA5;
  GL_MODELVIEW_MATRIX                 = $0BA6;
  GL_PROJECTION_MATRIX                = $0BA7;
  GL_TEXTURE_MATRIX                   = $0BA8;
  GL_ATTRIB_STACK_DEPTH               = $0BB0;
  GL_ALPHA_TEST                       = $0BC0;
  GL_ALPHA_TEST_FUNC                  = $0BC1;
  GL_ALPHA_TEST_REF                   = $0BC2;
  GL_DITHER                           = $0BD0;
  GL_BLEND_DST                        = $0BE0;
  GL_BLEND_SRC                        = $0BE1;
  GL_BLEND                            = $0BE2;
  GL_LOGIC_OP_MODE                    = $0BF0;
  GL_LOGIC_OP                         = $0BF1;
  GL_AUX_BUFFERS                      = $0C00;
  GL_DRAW_BUFFER                      = $0C01;
  GL_READ_BUFFER                      = $0C02;
  GL_SCISSOR_BOX                      = $0C10;
  GL_SCISSOR_TEST                     = $0C11;
  GL_INDEX_CLEAR_VALUE                = $0C20;
  GL_INDEX_WRITEMASK                  = $0C21;
  GL_COLOR_CLEAR_VALUE                = $0C22;
  GL_COLOR_WRITEMASK                  = $0C23;
  GL_INDEX_MODE                       = $0C30;
  GL_RGBA_MODE                        = $0C31;
  GL_DOUBLEBUFFER                     = $0C32;
  GL_STEREO                           = $0C33;
  GL_RENDER_MODE                      = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT      = $0C50;
  GL_POINT_SMOOTH_HINT                = $0C51;
  GL_LINE_SMOOTH_HINT                 = $0C52;
  GL_POLYGON_SMOOTH_HINT              = $0C53;
  GL_FOG_HINT                         = $0C54;
  GL_TEXTURE_GEN_S                    = $0C60;
  GL_TEXTURE_GEN_T                    = $0C61;
  GL_TEXTURE_GEN_R                    = $0C62;
  GL_TEXTURE_GEN_Q                    = $0C63;
  GL_PIXEL_MAP_I_TO_I_SIZE            = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE            = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE            = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE            = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE            = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE            = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE            = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE            = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE            = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE            = $0CB9;
  GL_UNPACK_SWAP_BYTES                = $0CF0;
  GL_UNPACK_LSB_FIRST                 = $0CF1;
  GL_UNPACK_ROW_LENGTH                = $0CF2;
  GL_UNPACK_SKIP_ROWS                 = $0CF3;
  GL_UNPACK_SKIP_PIXELS               = $0CF4;
  GL_UNPACK_ALIGNMENT                 = $0CF5;
  GL_PACK_SWAP_BYTES                  = $0D00;
  GL_PACK_LSB_FIRST                   = $0D01;
  GL_PACK_ROW_LENGTH                  = $0D02;
  GL_PACK_SKIP_ROWS                   = $0D03;
  GL_PACK_SKIP_PIXELS                 = $0D04;
  GL_PACK_ALIGNMENT                   = $0D05;
  GL_MAP_COLOR                        = $0D10;
  GL_MAP_STENCIL                      = $0D11;
  GL_INDEX_SHIFT                      = $0D12;
  GL_INDEX_OFFSET                     = $0D13;
  GL_RED_SCALE                        = $0D14;
  GL_RED_BIAS                         = $0D15;
  GL_ZOOM_X                           = $0D16;
  GL_ZOOM_Y                           = $0D17;
  GL_GREEN_SCALE                      = $0D18;
  GL_GREEN_BIAS                       = $0D19;
  GL_BLUE_SCALE                       = $0D1A;
  GL_BLUE_BIAS                        = $0D1B;
  GL_ALPHA_SCALE                      = $0D1C;
  GL_ALPHA_BIAS                       = $0D1D;
  GL_DEPTH_SCALE                      = $0D1E;
  GL_DEPTH_BIAS                       = $0D1F;
  GL_MAX_EVAL_ORDER                   = $0D30;
  GL_MAX_LIGHTS                       = $0D31;
  GL_MAX_CLIP_PLANES                  = $0D32;
  GL_MAX_TEXTURE_SIZE                 = $0D33;
  GL_MAX_PIXEL_MAP_TABLE              = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH           = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH        = $0D36;
  GL_MAX_NAME_STACK_DEPTH             = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH       = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH          = $0D39;
  GL_MAX_VIEWPORT_DIMS                = $0D3A;
  GL_SUBPIXEL_BITS                    = $0D50;
  GL_INDEX_BITS                       = $0D51;
  GL_RED_BITS                         = $0D52;
  GL_GREEN_BITS                       = $0D53;
  GL_BLUE_BITS                        = $0D54;
  GL_ALPHA_BITS                       = $0D55;
  GL_DEPTH_BITS                       = $0D56;
  GL_STENCIL_BITS                     = $0D57;
  GL_ACCUM_RED_BITS                   = $0D58;
  GL_ACCUM_GREEN_BITS                 = $0D59;
  GL_ACCUM_BLUE_BITS                  = $0D5A;
  GL_ACCUM_ALPHA_BITS                 = $0D5B;
  GL_NAME_STACK_DEPTH                 = $0D70;
  GL_AUTO_NORMAL                      = $0D80;
  GL_MAP1_COLOR_4                     = $0D90;
  GL_MAP1_INDEX                       = $0D91;
  GL_MAP1_NORMAL                      = $0D92;
  GL_MAP1_TEXTURE_COORD_1             = $0D93;
  GL_MAP1_TEXTURE_COORD_2             = $0D94;
  GL_MAP1_TEXTURE_COORD_3             = $0D95;
  GL_MAP1_TEXTURE_COORD_4             = $0D96;
  GL_MAP1_VERTEX_3                    = $0D97;
  GL_MAP1_VERTEX_4                    = $0D98;
  GL_MAP2_COLOR_4                     = $0DB0;
  GL_MAP2_INDEX                       = $0DB1;
  GL_MAP2_NORMAL                      = $0DB2;
  GL_MAP2_TEXTURE_COORD_1             = $0DB3;
  GL_MAP2_TEXTURE_COORD_2             = $0DB4;
  GL_MAP2_TEXTURE_COORD_3             = $0DB5;
  GL_MAP2_TEXTURE_COORD_4             = $0DB6;
  GL_MAP2_VERTEX_3                    = $0DB7;
  GL_MAP2_VERTEX_4                    = $0DB8;
  GL_MAP1_GRID_DOMAIN                 = $0DD0;
  GL_MAP1_GRID_SEGMENTS               = $0DD1;
  GL_MAP2_GRID_DOMAIN                 = $0DD2;
  GL_MAP2_GRID_SEGMENTS               = $0DD3;
  GL_TEXTURE_1D                       = $0DE0;
  GL_TEXTURE_2D                       = $0DE1;

  GL_POLYGON_OFFSET_UNITS             = $2A00;
  GL_POLYGON_OFFSET_POINT             = $2A01;
  GL_POLYGON_OFFSET_LINE              = $2A02;
  GL_POLYGON_OFFSET_FILL              = $8037;
  GL_POLYGON_OFFSET_FACTOR            = $8038;

{ GetTextureParameter }

  GL_TEXTURE_WIDTH                    = $1000;
  GL_TEXTURE_HEIGHT                   = $1001;
  GL_TEXTURE_COMPONENTS               = $1003;
  GL_TEXTURE_BORDER_COLOR             = $1004;
  GL_TEXTURE_BORDER                   = $1005;

{ HintMode }

  GL_DONT_CARE                        = $1100;
  GL_FASTEST                          = $1101;
  GL_NICEST                           = $1102;

{ LightParameter }

  GL_AMBIENT                          = $1200;
  GL_DIFFUSE                          = $1201;
  GL_SPECULAR                         = $1202;
  GL_POSITION                         = $1203;
  GL_SPOT_DIRECTION                   = $1204;
  GL_SPOT_EXPONENT                    = $1205;
  GL_SPOT_CUTOFF                      = $1206;
  GL_CONSTANT_ATTENUATION             = $1207;
  GL_LINEAR_ATTENUATION               = $1208;
  GL_QUADRATIC_ATTENUATION            = $1209;

{ ListMode }

  GL_COMPILE                          = $1300;
  GL_COMPILE_AND_EXECUTE              = $1301;

{ ListNameType }

  GL_BYTE                             = $1400;
  GL_UNSIGNED_BYTE                    = $1401;
  GL_SHORT                            = $1402;
  GL_UNSIGNED_SHORT                   = $1403;
  GL_INT                              = $1404;
  GL_UNSIGNED_INT                     = $1405;
  GL_FLOAT                            = $1406;
  GL_2_BYTES                          = $1407;
  GL_3_BYTES                          = $1408;
  GL_4_BYTES                          = $1409;

{ LogicOp }

  GL_CLEAR                            = $1500;
  GL_AND                              = $1501;
  GL_AND_REVERSE                      = $1502;
  GL_COPY                             = $1503;
  GL_AND_INVERTED                     = $1504;
  GL_NOOP                             = $1505;
  GL_XOR                              = $1506;
  GL_OR                               = $1507;
  GL_NOR                              = $1508;
  GL_EQUIV                            = $1509;
  GL_INVERT                           = $150A;
  GL_OR_REVERSE                       = $150B;
  GL_COPY_INVERTED                    = $150C;
  GL_OR_INVERTED                      = $150D;
  GL_NAND                             = $150E;
  GL_SET                              = $150F;

{ MaterialParameter }

  GL_EMISSION                         = $1600;
  GL_SHININESS                        = $1601;
  GL_AMBIENT_AND_DIFFUSE              = $1602;
  GL_COLOR_INDEXES                    = $1603;

{ MatrixMode }

  GL_MODELVIEW                        = $1700;
  GL_PROJECTION                       = $1701;
  GL_TEXTURE                          = $1702;

{ PixelCopyType }

  GL_COLOR                            = $1800;
  GL_DEPTH                            = $1801;
  GL_STENCIL                          = $1802;

{ PixelFormat }

  GL_COLOR_INDEX                      = $1900;
  GL_STENCIL_INDEX                    = $1901;
  GL_DEPTH_COMPONENT                  = $1902;
  GL_RED                              = $1903;
  GL_GREEN                            = $1904;
  GL_BLUE                             = $1905;
  GL_ALPHA                            = $1906;
  GL_RGB                              = $1907;
  GL_RGBA                             = $1908;
  GL_LUMINANCE                        = $1909;
  GL_LUMINANCE_ALPHA                  = $190A;

{ PixelType }

  GL_BITMAP                           = $1A00;

{ PolygonMode }

  GL_POINT                            = $1B00;
  GL_LINE                             = $1B01;
  GL_FILL                             = $1B02;

{ RenderingMode }

  GL_RENDER                           = $1C00;
  GL_FEEDBACK                         = $1C01;
  GL_SELECT                           = $1C02;

{ ShadingModel }

  GL_FLAT                             = $1D00;
  GL_SMOOTH                           = $1D01;

{ StencilOp }

  GL_KEEP                             = $1E00;
  GL_REPLACE                          = $1E01;
  GL_INCR                             = $1E02;
  GL_DECR                             = $1E03;

{ StringName }

  GL_VENDOR                           = $1F00;
  GL_RENDERER                         = $1F01;
  GL_VERSION                          = $1F02;
  GL_EXTENSIONS                       = $1F03;
  GL_SHADING_LANGUAGE_VERSION         = $8B8C;

{ TextureCoordName }

  GL_S                                = $2000;
  GL_T                                = $2001;
  GL_R                                = $2002;
  GL_Q                                = $2003;

{ TextureEnvMode }

  GL_MODULATE                         = $2100;
  GL_DECAL                            = $2101;

{ TextureEnvParameter }

  GL_TEXTURE_ENV_MODE                 = $2200;
  GL_TEXTURE_ENV_COLOR                = $2201;

{ TextureEnvTarget }

  GL_TEXTURE_ENV                      = $2300;

{ TextureGenMode }

  GL_EYE_LINEAR                       = $2400;
  GL_OBJECT_LINEAR                    = $2401;
  GL_SPHERE_MAP                       = $2402;

{ TextureGenParameter }

  GL_TEXTURE_GEN_MODE                 = $2500;
  GL_OBJECT_PLANE                     = $2501;
  GL_EYE_PLANE                        = $2502;

{ TextureMagFilter }

  GL_NEAREST                          = $2600;
  GL_LINEAR                           = $2601;

{ TextureMinFilter }

  GL_NEAREST_MIPMAP_NEAREST           = $2700;
  GL_LINEAR_MIPMAP_NEAREST            = $2701;
  GL_NEAREST_MIPMAP_LINEAR            = $2702;
  GL_LINEAR_MIPMAP_LINEAR             = $2703;

{ TextureParameterName }

  GL_TEXTURE_MAG_FILTER               = $2800;
  GL_TEXTURE_MIN_FILTER               = $2801;
  GL_TEXTURE_WRAP_R                   = $8072;
  GL_TEXTURE_WRAP_S                   = $2802;
  GL_TEXTURE_WRAP_T                   = $2803;
  GL_GENERATE_MIPMAP                  = $8191;
  GL_GENERATE_MIPMAP_HINT             = $8192;
  GL_TEXTURE_RESIDENT                 = $8067;
  GL_TEXTURE_1D_BINDING               = $8068;
  GL_TEXTURE_2D_BINDING               = $8069;
  GL_TEXTURE_MIN_LOD                  = $813A;
  GL_TEXTURE_MAX_LOD                  = $813B;
  GL_TEXTURE_BASE_LEVEL               = $813C;
  GL_TEXTURE_MAX_LEVEL                = $813D;
  GL_TEXTURE_DEPTH                    = $8071;

{ TextureWrapMode }

  GL_CLAMP                            = $2900;
  GL_REPEAT                           = $2901;
  GL_CLAMP_TO_EDGE                    = $812F;

{ ClipPlaneName }

  GL_CLIP_PLANE0                      = $3000;
  GL_CLIP_PLANE1                      = $3001;
  GL_CLIP_PLANE2                      = $3002;
  GL_CLIP_PLANE3                      = $3003;
  GL_CLIP_PLANE4                      = $3004;
  GL_CLIP_PLANE5                      = $3005;

{ LightName }

  GL_LIGHT0                           = $4000;
  GL_LIGHT1                           = $4001;
  GL_LIGHT2                           = $4002;
  GL_LIGHT3                           = $4003;
  GL_LIGHT4                           = $4004;
  GL_LIGHT5                           = $4005;
  GL_LIGHT6                           = $4006;
  GL_LIGHT7                           = $4007;

  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;

{ Extensions }

  GL_EXT_vertex_array                 = 1;
  GL_WIN_swap_hint                    = 1;

{ vertex_array }

  GL_VERTEX_ARRAY               = $8074;
  GL_NORMAL_ARRAY               = $8075;
  GL_COLOR_ARRAY                = $8076;
  GL_INDEX_ARRAY                = $8077;
  GL_TEXTURE_COORD_ARRAY        = $8078;
  GL_EDGE_FLAG_ARRAY            = $8079;
  GL_VERTEX_ARRAY_SIZE          = $807A;
  GL_VERTEX_ARRAY_TYPE          = $807B;
  GL_VERTEX_ARRAY_STRIDE        = $807C;
  GL_VERTEX_ARRAY_COUNT         = $807D;
  GL_NORMAL_ARRAY_TYPE          = $807E;
  GL_NORMAL_ARRAY_STRIDE        = $807F;
  GL_NORMAL_ARRAY_COUNT         = $8080;
  GL_COLOR_ARRAY_SIZE           = $8081;
  GL_COLOR_ARRAY_TYPE           = $8082;
  GL_COLOR_ARRAY_STRIDE         = $8083;
  GL_COLOR_ARRAY_COUNT          = $8084;
  GL_INDEX_ARRAY_TYPE           = $8085;
  GL_INDEX_ARRAY_STRIDE         = $8086;
  GL_INDEX_ARRAY_COUNT          = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT      = $808D;
  GL_VERTEX_ARRAY_POINTER       = $808E;
  GL_NORMAL_ARRAY_POINTER       = $808F;
  GL_COLOR_ARRAY_POINTER        = $8090;
  GL_INDEX_ARRAY_POINTER        = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER    = $8093;

var
  glAccum: procedure(op: GLenum; value: GLfloat); apicall;
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); apicall;
  glBegin: procedure(mode: GLenum); apicall;
  glBitmap: procedure(width, height: GLsizei; xorig, yorig: GLfloat; xmove, ymove: GLfloat; bitmap: Pointer); apicall;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); apicall;
  glCallList: procedure(list: GLuint); apicall;
  glCallLists: procedure(n: GLsizei; cltype: GLenum; lists: Pointer); apicall;
  glClear: procedure(mask: GLbitfield); apicall;
  glClearAccum: procedure(red, green, blue, alpha: GLfloat); apicall;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); apicall;
  glClearDepth: procedure(depth: GLclampd); apicall;
  glClearIndex: procedure(c: GLfloat); apicall;
  glClearStencil: procedure(s: GLint); apicall;
  glClipPlane: procedure(plane: GLenum; equation: PGLDouble); apicall;
  glColor3b: procedure(red, green, blue: GLbyte); apicall;
  glColor3bv: procedure(var v: GLByte); apicall;
  glColor3d: procedure(red, green, blue: GLdouble); apicall;
  glColor3dv: procedure(var v: GLdouble); apicall;
  glColor3f: procedure(red, green, blue: GLfloat); apicall;
  glColor3fv: procedure(var v: GLfloat); apicall;
  glColor3i: procedure(red, green, blue: GLint); apicall;
  glColor3iv: procedure(var v: GLint); apicall;
  glColor3s: procedure(red, green, blue: GLshort); apicall;
  glColor3sv: procedure(var v: GLshort); apicall;
  glColor3ub: procedure(red, green, blue: GLubyte); apicall;
  glColor3ubv: procedure(var v: GLubyte); apicall;
  glColor3ui: procedure(red, green, blue: GLuint); apicall;
  glColor3uiv: procedure(var v: GLuint); apicall;
  glColor3us: procedure(red, green, blue: GLushort); apicall;
  glColor3usv: procedure(var v: GLushort); apicall;
  glColor4b: procedure(red, green, blue, alpha: GLbyte); apicall;
  glColor4bv: procedure(var v: GLbyte); apicall;
  glColor4d: procedure(red, green, blue, alpha: GLdouble); apicall;
  glColor4dv: procedure(var v: GLdouble); apicall;
  glColor4f: procedure(red, green, blue, alpha: GLfloat); apicall;
  glColor4fv: procedure(var v: GLfloat); apicall;
  glColor4i: procedure(red, green, blue, alpha: GLint); apicall;
  glColor4iv: procedure(var v: GLint); apicall;
  glColor4s: procedure(red, green, blue, alpha: GLshort); apicall;
  glColor4sv: procedure(var v: GLshort); apicall;
  glColor4ub: procedure(red, green, blue, alpha: GLubyte); apicall;
  glColor4ubv: procedure(var v: GLubyte); apicall;
  glColor4ui: procedure(red, green, blue, alpha: GLuint); apicall;
  glColor4uiv: procedure(var v: GLuint); apicall;
  glColor4us: procedure(red, green, blue, alpha: GLushort); apicall;
  glColor4usv: procedure(var v: GLushort); apicall;
  glColorMask: procedure(red, green, blue, alpha: GLboolean); apicall;
  glColorMaterial: procedure(face, mode: GLenum); apicall;
  glCopyPixels: procedure(x, y: GLint; width, height: GLsizei; pixeltype: GLenum); apicall;
  glCullFace: procedure(mode: GLenum); apicall;
  glDeleteLists: procedure(list: GLuint; range: GLsizei); apicall;
  glDepthFunc: procedure(func: GLenum); apicall;
  glDepthMask: procedure(flag: GLboolean); apicall;
  glDepthRange: procedure(zNear, zFar: GLclampd); apicall;
  glDisable: procedure(cap: GLenum); apicall;
  glDisableClientState: procedure(cap: GLenum); apicall;
  glEnableClientState: procedure(cap: GLenum); apicall;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); apicall;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); apicall;
  glDrawBuffer: procedure(mode: GLenum); apicall;
  glDrawPixels: procedure(width, height: GLsizei; format, pixeltype: GLenum; pixels: Pointer); apicall;
  glEdgeFlag: procedure(flag: GLboolean); apicall;
  glEdgeFlagv: procedure(flag: PGLboolean); apicall;
  glEnable: procedure(cap: GLenum); apicall;
  glEnd: procedure apicall;
  glEndList: procedure; apicall;
  glEvalCoord1d: procedure(u: GLdouble); apicall;
  glEvalCoord1dv: procedure(u: PGLdouble); apicall;
  glEvalCoord1f: procedure(u: GLfloat); apicall;
  glEvalCoord1fv: procedure(u: PGLfloat); apicall;
  glEvalCoord2d: procedure(u, v: GLdouble); apicall;
  glEvalCoord2dv: procedure(u: PGLdouble); apicall;
  glEvalCoord2f: procedure(u, v: GLfloat); apicall;
  glEvalCoord2fv: procedure(u: PGLfloat); apicall;
  glEvalMesh1: procedure(mode: GLenum; i1, i2: GLint); apicall;
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: GLint); apicall;
  glEvalPoint1: procedure(i: GLint); apicall;
  glEvalPoint2: procedure(i, j: GLint); apicall;
  glFeedbackBuffer: procedure(size: GLsizei; buftype: GLenum; buffer: PGLFloat); apicall;
  glFinish: procedure; apicall;
  glFlush: procedure; apicall;
  glFogf: procedure(pname: GLenum; param: GLfloat); apicall;
  glFogfv: procedure(pname: GLenum; params: PGLfloat); apicall;
  glFogi: procedure(pname: GLenum; param: GLint); apicall;
  glFogiv: procedure(pname: GLenum; params: PGLint); apicall;
  glFrontFace: procedure(mode: GLenum); apicall;
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: GLdouble); apicall;
  glGenLists: function(range: GLsizei): GLuint; apicall;
  glGetBooleanv: procedure(pname: GLenum; out params: GLboolean); apicall;
  glGetClipPlane: procedure(plane: GLenum; out equation: GLdouble); apicall;
  glGetDoublev: procedure(pname: GLenum; out params: GLdouble); apicall;
  glGetError: function: GLenum; apicall;
  glGetFloatv: procedure(pname: GLenum; out params: GLfloat); apicall;
  glGetIntegerv: procedure(pname: GLenum; out params: GLint); apicall;
  glGetLightfv: procedure(light: GLenum; pname: GLenum; out params:PGLfloat); apicall;
  glGetLightiv: procedure(light: GLenum; pname: GLenum; out params: GLint); apicall;
  glGetMapdv: procedure(target: GLenum; query: GLenum; out v: GLdouble); apicall;
  glGetMapfv: procedure(target: GLenum; query: GLenum; out v: GLfloat); apicall;
  glGetMapiv: procedure(target: GLenum; query: GLenum; out v: GLint); apicall;
  glGetMaterialfv: procedure(face: GLenum; pname: GLenum; out params: GLfloat); apicall;
  glGetMaterialiv: procedure(face: GLenum; pname: GLenum; out params: GLint); apicall;
  glGetPixelMapfv: procedure(map: GLenum; out values: GLfloat); apicall;
  glGetPixelMapuiv: procedure(map: GLenum; out values: GLuint); apicall;
  glGetPixelMapusv: procedure(map: GLenum; out values: GLushort); apicall;
  glGetPolygonStipple: procedure(out mask: GLubyte); apicall;
  glGetString: function(name: GLenum): PChar; apicall;
  glGetTexEnvfv: procedure(target: GLenum; pname: GLenum; out params: GLfloat); apicall;
  glGetTexEnviv: procedure(target: GLenum; pname: GLenum; out params: GLint); apicall;
  glGetTexGendv: procedure(coord: GLenum; pname: GLenum; out params: GLdouble); apicall;
  glGetTexGenfv: procedure(coord: GLenum; pname: GLenum; out params: GLfloat); apicall;
  glGetTexGeniv: procedure(coord: GLenum; pname: GLenum; out params: GLint); apicall;
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; _type: GLenum; pixels: pointer); apicall;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; out params: GLfloat); apicall;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; out params: GLint); apicall;
  glGetTexParameterfv: procedure(target, pname: GLenum; out params: GLfloat); apicall;
  glGetTexParameteriv: procedure(target, pname: GLenum; out params: GLint); apicall;
  glHint: procedure(target, mode: GLenum); apicall;
  glIndexMask: procedure(mask: GLuint); apicall;
  glIndexd: procedure(c: GLdouble); apicall;
  glIndexdv: procedure(c: PGLdouble); apicall;
  glIndexf: procedure(c: GLfloat); apicall;
  glIndexfv: procedure(c: PGLfloat); apicall;
  glIndexi: procedure(c: GLint); apicall;
  glIndexiv: procedure(c: PGLint); apicall;
  glIndexs: procedure(c: GLshort); apicall;
  glIndexsv: procedure(c: PGLshort); apicall;
  glInitNames: procedure; apicall;
  glIsEnabled: function(cap: GLenum): GLBoolean; apicall;
  glIsList: function(list: GLuint): GLBoolean; apicall;
  glLightModelf: procedure(pname: GLenum; param: GLfloat); apicall;
  glLightModelfv: procedure(pname: GLenum; params: PGLfloat); apicall;
  glLightModeli: procedure(pname: GLenum; param: GLint); apicall;
  glLightModeliv: procedure(pname: GLenum; params: PGLint); apicall;
  glLightf: procedure(light, pname: GLenum; param: GLfloat); apicall;
  glLightfv: procedure(light, pname: GLenum; params: PGLfloat); apicall;
  glLighti: procedure(light, pname: GLenum; param: GLint); apicall;
  glLightiv: procedure(light, pname: GLenum; params: PGLint); apicall;
  glLineStipple: procedure(factor: GLint; pattern: GLushort); apicall;
  glLineWidth: procedure(width: GLfloat); apicall;
  glListBase: procedure(base: GLuint); apicall;
  glLoadIdentity: procedure; apicall;
  glLoadMatrixd: procedure(m: PGLdouble); apicall;
  glLoadMatrixf: procedure(m: PGLfloat); apicall;
  glLoadName: procedure(name: GLuint); apicall;
  glLogicOp: procedure(opcode: GLenum); apicall;
  glMap1d: procedure(target: GLenum; u1, u2: GLdouble; stride, order: GLint; Points: PGLdouble); apicall;
  glMap1f: procedure(target: GLenum; u1, u2: GLfloat; stride, order: GLint; Points: PGLfloat); apicall;
  glMap2d: procedure(target: GLenum; u1, u2: GLdouble; ustride, uorder: GLint; v1, v2: GLdouble; vstride, vorder: GLint; Points: PGLdouble); apicall;
  glMap2f: procedure(target: GLenum; u1, u2: GLfloat; ustride, uorder: GLint; v1, v2: GLfloat; vstride, vorder: GLint; Points: PGLfloat); apicall;
  glMapGrid1d: procedure(un: GLint; u1, u2: GLdouble); apicall;
  glMapGrid1f: procedure(un: GLint; u1, u2: GLfloat); apicall;
  glMapGrid2d: procedure(un: GLint; u1, u2: GLdouble; vn: GLint; v1, v2: GLdouble); apicall;
  glMapGrid2f: procedure(un: GLint; u1, u2: GLfloat; vn: GLint; v1, v2: GLfloat); apicall;
  glMaterialf: procedure(face, pname: GLenum; param: GLfloat); apicall;
  glMaterialfv: procedure(face, pname: GLenum; params: PGLfloat); apicall;
  glMateriali: procedure(face, pname: GLenum; param: GLint); apicall;
  glMaterialiv: procedure(face, pname: GLenum; params: PGLint); apicall;
  glMatrixMode: procedure(mode: GLenum); apicall;
  glMultMatrixd: procedure(m: PGLdouble); apicall;
  glMultMatrixf: procedure(m: PGLfloat); apicall;
  glNewList: procedure(ListIndex: GLuint; mode: GLenum); apicall;
  glNormal3b: procedure(nx, ny, nz: GLbyte); apicall;
  glNormal3bv: procedure(v: PGLbyte); apicall;
  glNormal3d: procedure(nx, ny, nz: GLdouble); apicall;
  glNormal3dv: procedure(v: PGLdouble); apicall;
  glNormal3f: procedure(nx, ny, nz: GLFloat); apicall;
  glNormal3fv: procedure(v: PGLfloat); apicall;
  glNormal3i: procedure(nx, ny, nz: GLint); apicall;
  glNormal3iv: procedure(v: PGLint); apicall;
  glNormal3s: procedure(nx, ny, nz: GLshort); apicall;
  glNormal3sv: procedure(v: PGLshort); apicall;
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: GLdouble); apicall;
  glPassThrough: procedure(token: GLfloat); apicall;
  glPixelMapfv: procedure(map: GLenum; mapsize: GLint; values: PGLfloat); apicall;
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLint; values: PGLuint); apicall;
  glPixelMapusv: procedure(map: GLenum; mapsize: GLint; values: PGLushort); apicall;
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); apicall;
  glPixelStorei: procedure(pname: GLenum; param: GLint); apicall;
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); apicall;
  glPixelTransferi: procedure(pname: GLenum; param: GLint); apicall;
  glPixelZoom: procedure(xfactor, yfactor: GLfloat); apicall;
  glPointSize: procedure(size: GLfloat); apicall;
  glPolygonMode: procedure(face, mode: GLenum); apicall;
  glPolygonOffset: procedure(factor, units: GLfloat); apicall;
  glPolygonStipple: procedure(mask: PGLubyte); apicall;
  glPopAttrib: procedure; apicall;
  glPopMatrix: procedure; apicall;
  glPopName: procedure; apicall;
  glPushAttrib: procedure(mask: GLbitfield); apicall;
  glPushMatrix: procedure; apicall;
  glPushName: procedure(name: GLuint); apicall;
  glRasterPos2d: procedure(x, y: GLdouble); apicall;
  glRasterPos2dv: procedure(v: PGLdouble); apicall;
  glRasterPos2f: procedure(x, y: GLfloat); apicall;
  glRasterPos2fv: procedure(v: PGLfloat); apicall;
  glRasterPos2i: procedure(x, y: GLint); apicall;
  glRasterPos2iv: procedure(v: PGLint); apicall;
  glRasterPos2s: procedure(x, y: GLshort); apicall;
  glRasterPos2sv: procedure(v: PGLshort); apicall;
  glRasterPos3d: procedure(x, y, z: GLdouble); apicall;
  glRasterPos3dv: procedure(v: PGLdouble); apicall;
  glRasterPos3f: procedure(x, y, z: GLfloat); apicall;
  glRasterPos3fv: procedure(v: PGLfloat); apicall;
  glRasterPos3i: procedure(x, y, z: GLint); apicall;
  glRasterPos3iv: procedure(v: PGLint); apicall;
  glRasterPos3s: procedure(x, y, z: GLshort); apicall;
  glRasterPos3sv: procedure(v: PGLshort); apicall;
  glRasterPos4d: procedure(x, y, z, w: GLdouble); apicall;
  glRasterPos4dv: procedure(v: PGLdouble); apicall;
  glRasterPos4f: procedure(x, y, z, w: GLfloat); apicall;
  glRasterPos4fv: procedure(v: PGLfloat); apicall;
  glRasterPos4i: procedure(x, y, z, w: GLint); apicall;
  glRasterPos4iv: procedure(v: PGLint); apicall;
  glRasterPos4s: procedure(x, y, z, w: GLshort); apicall;
  glRasterPos4sv: procedure(v: PGLshort); apicall;
  glReadBuffer: procedure(mode: GLenum); apicall;
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, _type: GLenum; pixels: Pointer); apicall;
  glRectd: procedure(x1, y1, x2, y2: GLdouble); apicall;
  glRectdv: procedure(v1, v2: PGLdouble); apicall;
  glRectf: procedure(x1, y1, x2, y2: GLfloat); apicall;
  glRectfv: procedure(v1, v2: PGLfloat); apicall;
  glRecti: procedure(x1, y1, x2, y2: GLint); apicall;
  glRectiv: procedure(v1, v2: PGLint); apicall;
  glRects: procedure(x1, y1, x2, y2: GLshort); apicall;
  glRectsv: procedure(v1, v2: PGLshort); apicall;
  glRenderMode: function(mode: GLenum): GLint; apicall;
  glRotated: procedure(angle, x, y, z: GLdouble); apicall;
  glRotatef: procedure(angle, x, y, z: GLfloat); apicall;
  glScaled: procedure(x, y, z: GLdouble); apicall;
  glScalef: procedure(x, y, z: GLfloat); apicall;
  glScissor: procedure(x, y: GLint; width, height: GLsizei); apicall;
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); apicall;
  glShadeModel: procedure(mode: GLenum); apicall;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilMask: procedure(mask: GLuint); apicall;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); apicall;
  glTexCoord1d: procedure(s: GLdouble); apicall;
  glTexCoord1dv: procedure(v: PGLdouble); apicall;
  glTexCoord1f: procedure(s: GLfloat); apicall;
  glTexCoord1fv: procedure(v: PGLfloat); apicall;
  glTexCoord1i: procedure(s: GLint); apicall;
  glTexCoord1iv: procedure(v: PGLint); apicall;
  glTexCoord1s: procedure(s: GLshort); apicall;
  glTexCoord1sv: procedure(v: PGLshort); apicall;
  glTexCoord2d: procedure(s, t: GLdouble); apicall;
  glTexCoord2dv: procedure(v: PGLdouble); apicall;
  glTexCoord2f: procedure(s, t: GLfloat); apicall;
  glTexCoord2fv: procedure(v: PGLfloat); apicall;
  glTexCoord2i: procedure(s, t: GLint); apicall;
  glTexCoord2iv: procedure(v: PGLint); apicall;
  glTexCoord2s: procedure(s, t: GLshort); apicall;
  glTexCoord2sv: procedure(v: PGLshort); apicall;
  glTexCoord3d: procedure(s, t, r: GLdouble); apicall;
  glTexCoord3dv: procedure(v: PGLdouble); apicall;
  glTexCoord3f: procedure(s, t, r: GLfloat); apicall;
  glTexCoord3fv: procedure(v: PGLfloat); apicall;
  glTexCoord3i: procedure(s, t, r: GLint); apicall;
  glTexCoord3iv: procedure(v: PGLint); apicall;
  glTexCoord3s: procedure(s, t, r: GLshort); apicall;
  glTexCoord3sv: procedure(v: PGLshort); apicall;
  glTexCoord4d: procedure(s, t, r, q: GLdouble); apicall;
  glTexCoord4dv: procedure(v: PGLdouble); apicall;
  glTexCoord4f: procedure(s, t, r, q: GLfloat); apicall;
  glTexCoord4fv: procedure(v: PGLfloat); apicall;
  glTexCoord4i: procedure(s, t, r, q: GLint); apicall;
  glTexCoord4iv: procedure(v: PGLint); apicall;
  glTexCoord4s: procedure(s, t, r, q: GLshort); apicall;
  glTexCoord4sv: procedure(v: PGLshort); apicall;
  glTexEnvf: procedure(target, pname: GLenum; param: GLfloat); apicall;
  glTexEnvfv: procedure(target, pname: GLenum; params: PGLfloat); apicall;
  glTexEnvi: procedure(target, pname: GLenum; param: GLint); apicall;
  glTexEnviv: procedure(target, pname: GLenum; params: PGLint); apicall;
  glTexGend: procedure(coord, pname: GLenum; param: GLdouble); apicall;
  glTexGendv: procedure(coord, pname: GLenum; params: PGLdouble); apicall;
  glTexGenf: procedure(coord, pname: GLenum; param: GLfloat); apicall;
  glTexGenfv: procedure(coord, pname: GLenum; params: PGLfloat); apicall;
  glTexGeni: procedure(coord, pname: GLenum; param: GLint); apicall;
  glTexGeniv: procedure(coord, pname: GLenum; params: PGLint); apicall;
  glTexImage1D: procedure(target: GLenum; level, components: GLint; width: GLsizei; border: GLint; format, _type: GLenum; pixels: Pointer); apicall;
  glTexImage2D: procedure(target: GLenum; level, components: GLint; width, height: GLsizei; border: GLint; format, _type: GLenum; pixels: Pointer); apicall;
  glCopyTexImage1D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); apicall;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); apicall;
  glTexParameterf: procedure(target, pname: GLenum; param: GLfloat); apicall;
  glTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); apicall;
  glTexParameteri: procedure(target, pname: GLenum; param: GLint); apicall;
  glTexParameteriv: procedure(target, pname: GLenum; params: PGLint); apicall;
  glTranslated: procedure(x, y, z: GLdouble); apicall;
  glTranslatef: procedure(x, y, z: GLfloat); apicall;
  glVertex2d: procedure(x, y: GLdouble); apicall;
  glVertex2dv: procedure(v: PGLdouble); apicall;
  glVertex2f: procedure(x, y: GLfloat); apicall;
  glVertex2fv: procedure(v: PGLfloat); apicall;
  glVertex2i: procedure(x, y: GLint); apicall;
  glVertex2iv: procedure(v: PGLint); apicall;
  glVertex2s: procedure(x, y: GLshort); apicall;
  glVertex2sv: procedure(v: PGLshort); apicall;
  glVertex3d: procedure(x, y, z: GLdouble); apicall;
  glVertex3dv: procedure(v: PGLdouble); apicall;
  glVertex3f: procedure(x, y, z: GLfloat); apicall;
  glVertex3fv: procedure(v: PGLfloat); apicall;
  glVertex3i: procedure(x, y, z: GLint); apicall;
  glVertex3iv: procedure(v: PGLint); apicall;
  glVertex3s: procedure(x, y, z: GLshort); apicall;
  glVertex3sv: procedure(v: PGLshort); apicall;
  glVertex4d: procedure(x, y, z, w: GLdouble); apicall;
  glVertex4dv: procedure(v: PGLdouble); apicall;
  glVertex4f: procedure(x, y, z, w: GLfloat); apicall;
  glVertex4fv: procedure(v: PGLfloat); apicall;
  glVertex4i: procedure(x, y, z, w: GLint); apicall;
  glVertex4iv: procedure(v: PGLint); apicall;
  glVertex4s: procedure(x, y, z, w: GLshort); apicall;
  glVertex4sv: procedure(v: PGLshort); apicall;
  glViewport: procedure(x, y: GLint; width, height: GLsizei); apicall;
  glBindTexture: procedure(target: GLEnum; texture: GLuint); apicall;
  glGenTextures: procedure(n: GLsizei; var textures: GLuint); apicall;
  glDeleteTextures: procedure(n: GLsizei; var textures: GLuint); apicall;
  glVertexPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); apicall;
  glNormalPointer: procedure(atype: GLenum; stride: GLsizei; data: Pointer); apicall;
  glColorPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); apicall;
  glTexCoordPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; data: Pointer); apicall;
{$endif}

{$ifdef opengl_1_2}
const
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_RESCALE_NORMAL = $803A;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_SINGLE_COLOR = $81F9;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_CONVOLUTION_1D = $8010;
  GL_CONVOLUTION_2D = $8011;
  GL_SEPARABLE_2D = $8012;
  GL_CONVOLUTION_BORDER_MODE = $8013;
  GL_CONVOLUTION_FILTER_SCALE = $8014;
  GL_CONVOLUTION_FILTER_BIAS = $8015;
  GL_REDUCE = $8016;
  GL_CONVOLUTION_FORMAT = $8017;
  GL_CONVOLUTION_WIDTH = $8018;
  GL_CONVOLUTION_HEIGHT = $8019;
  GL_MAX_CONVOLUTION_WIDTH = $801A;
  GL_MAX_CONVOLUTION_HEIGHT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE = $801F;
  GL_POST_CONVOLUTION_RED_BIAS = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS = $8023;
  GL_HISTOGRAM = $8024;
  GL_PROXY_HISTOGRAM = $8025;
  GL_HISTOGRAM_WIDTH = $8026;
  GL_HISTOGRAM_FORMAT = $8027;
  GL_HISTOGRAM_RED_SIZE = $8028;
  GL_HISTOGRAM_GREEN_SIZE = $8029;
  GL_HISTOGRAM_BLUE_SIZE = $802A;
  GL_HISTOGRAM_ALPHA_SIZE = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE = $802C;
  GL_HISTOGRAM_SINK = $802D;
  GL_MINMAX = $802E;
  GL_MINMAX_FORMAT = $802F;
  GL_MINMAX_SINK = $8030;
  GL_TABLE_TOO_LARGE = $8031;
  GL_COLOR_MATRIX = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS = $80BA;
  GL_POST_COLOR_MATIX_ALPHA_BIAS = $80BB;
  GL_COLOR_TABLE = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2;
  GL_PROXY_COLOR_TABLE = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
  GL_COLOR_TABLE_SCALE = $80D6;
  GL_COLOR_TABLE_BIAS = $80D7;
  GL_COLOR_TABLE_FORMAT = $80D8;
  GL_COLOR_TABLE_WIDTH = $80D9;
  GL_COLOR_TABLE_RED_SIZE = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE = $80DF;
  GL_IGNORE_BORDER = $8150;
  GL_CONSTANT_BORDER = $8151;
  GL_WRAP_BORDER = $8152;
  GL_REPLICATE_BORDER = $8153;
  GL_CONVOLUTION_BORDER_COLOR = $8154;

var
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); apicall;
  glColorTable: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: PGLvoid); apicall;
  glColorTableParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); apicall;
  glColorTableParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); apicall;
  glCopyColorTable: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); apicall;
  glGetColorTable: procedure(target: GLenum; format: GLenum; _type: GLenum; table: PGLvoid); apicall;
  glGetColorTableParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetColorTableParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glColorSubTable: procedure(target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; _type: GLenum; const data: PGLvoid); apicall;
  glCopyColorSubTable: procedure(target: GLenum; start: GLsizei; x: GLint; y: GLint; width: GLsizei); apicall;
  glConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); apicall;
  glConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); apicall;
  glConvolutionParameterf: procedure(target: GLenum; pname: GLenum; params: GLfloat); apicall;
  glConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); apicall;
  glConvolutionParameteri: procedure(target: GLenum; pname: GLenum; params: GLint); apicall;
  glConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); apicall;
  glCopyConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); apicall;
  glCopyConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glGetConvolutionFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; image: PGLvoid); apicall;
  glGetConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetSeparableFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); apicall;
  glSeparableFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const row: PGLvoid; const column: PGLvoid); apicall;
  glGetHistogram: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); apicall;
  glGetHistogramParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetHistogramParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetMinmax: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); apicall;
  glGetMinmaxParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetMinmaxParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glHistogram: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); apicall;
  glMinmax: procedure(target: GLenum; internalformat: GLenum; sink: GLboolean); apicall;
  glResetHistogram: procedure(target: GLenum); apicall;
  glResetMinmax: procedure(target: GLenum); apicall;
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); apicall;
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); apicall;
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
{$endif}

{$ifdef opengl_1_3}
{ Should have the following extensions:

  GL_ARB_multisample
  GL_ARB_multitexture
  GL_ARB_texture_border_clamp
  GL_ARB_texture_compression
  GL_ARB_texture_cube_map
  GL_ARB_texture_env_add
  GL_ARB_texture_env_combine
  GL_ARB_texture_env_dot3
  GL_ARB_transpose_matrix }

const
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX = $84E6;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_MULTISAMPLE_BIT = $20000000;
  GL_NORMAL_MAP = $8511;
  GL_REFLECTION_MAP = $8512;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_ALPHA = $84E9;
  GL_COMPRESSED_LUMINANCE = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
  GL_COMPRESSED_INTENSITY = $84EC;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;
  GL_CLAMP_TO_BORDER_SGIS = $812D;
  GL_COMBINE = $8570;
  GL_COMBINE_RGB = $8571;
  GL_COMBINE_ALPHA = $8572;
  GL_SOURCE0_RGB = $8580;
  GL_SOURCE1_RGB = $8581;
  GL_SOURCE2_RGB = $8582;
  GL_SOURCE0_ALPHA = $8588;
  GL_SOURCE1_ALPHA = $8589;
  GL_SOURCE2_ALPHA = $858A;
  GL_OPERAND0_RGB = $8590;
  GL_OPERAND1_RGB = $8591;
  GL_OPERAND2_RGB = $8592;
  GL_OPERAND0_ALPHA = $8598;
  GL_OPERAND1_ALPHA = $8599;
  GL_OPERAND2_ALPHA = $859A;
  GL_RGB_SCALE = $8573;
  GL_ADD_SIGNED = $8574;
  GL_INTERPOLATE = $8575;
  GL_SUBTRACT = $84E7;
  GL_CONSTANT = $8576;
  GL_PRIMARY_COLOR = $8577;
  GL_PREVIOUS = $8578;
  GL_DOT3_RGB = $86AE;
  GL_DOT3_RGBA = $86AF;

var
  glActiveTexture: procedure(texture: GLenum); apicall;
  glClientActiveTexture: procedure(texture: GLenum); apicall;
  glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); apicall;
  glMultiTexCoord1dv: procedure(target: GLenum; const v: PGLdouble); apicall;
  glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); apicall;
  glMultiTexCoord1fv: procedure(target: GLenum; const v: PGLfloat); apicall;
  glMultiTexCoord1i: procedure(target: GLenum; s: GLint); apicall;
  glMultiTexCoord1iv: procedure(target: GLenum; const v: PGLint); apicall;
  glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); apicall;
  glMultiTexCoord1sv: procedure(target: GLenum; const v: PGLshort); apicall;
  glMultiTexCoord2d: procedure(target: GLenum; s: GLdouble; t: GLdouble); apicall;
  glMultiTexCoord2dv: procedure(target: GLenum; const v: PGLdouble); apicall;
  glMultiTexCoord2f: procedure(target: GLenum; s: GLfloat; t: GLfloat); apicall;
  glMultiTexCoord2fv: procedure(target: GLenum; const v: PGLfloat); apicall;
  glMultiTexCoord2i: procedure(target: GLenum; s: GLint; t: GLint); apicall;
  glMultiTexCoord2iv: procedure(target: GLenum; const v: PGLint); apicall;
  glMultiTexCoord2s: procedure(target: GLenum; s: GLshort; t: GLshort); apicall;
  glMultiTexCoord2sv: procedure(target: GLenum; const v: PGLshort); apicall;
  glMultiTexCoord3d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); apicall;
  glMultiTexCoord3dv: procedure(target: GLenum; const v: PGLdouble); apicall;
  glMultiTexCoord3f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); apicall;
  glMultiTexCoord3fv: procedure(target: GLenum; const v: PGLfloat); apicall;
  glMultiTexCoord3i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); apicall;
  glMultiTexCoord3iv: procedure(target: GLenum; const v: PGLint); apicall;
  glMultiTexCoord3s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); apicall;
  glMultiTexCoord3sv: procedure(target: GLenum; const v: PGLshort); apicall;
  glMultiTexCoord4d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); apicall;
  glMultiTexCoord4dv: procedure(target: GLenum; const v: PGLdouble); apicall;
  glMultiTexCoord4f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); apicall;
  glMultiTexCoord4fv: procedure(target: GLenum; const v: PGLfloat); apicall;
  glMultiTexCoord4i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); apicall;
  glMultiTexCoord4iv: procedure(target: GLenum; const v: PGLint); apicall;
  glMultiTexCoord4s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); apicall;
  glMultiTexCoord4sv: procedure(target: GLenum; const v: PGLshort); apicall;
  glLoadTransposeMatrixf: procedure(const m: PGLfloat); apicall;
  glLoadTransposeMatrixd: procedure(const m: PGLdouble); apicall;
  glMultTransposeMatrixf: procedure(const m: PGLfloat); apicall;
  glMultTransposeMatrixd: procedure(const m: PGLdouble); apicall;
  glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); apicall;
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); apicall;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); apicall;
  glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); apicall;
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); apicall;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); apicall;
  glCompressedTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); apicall;
  glGetCompressedTexImage: procedure(target: GLenum; level: GLint; img: PGLvoid); apicall;
{$endif}

{$ifdef opengl_1_4}
{ Should have the following extensions:

  GL_ARB_depth_texture
  GL_ARB_point_parameters
  GL_ARB_shadow
  GL_ARB_texture_env_crossbar
  GL_ARB_texture_mirrored_repeat
  GL_ARB_window_pos
  GL_EXT_blend_color
  GL_EXT_blend_func_separate
  GL_EXT_blend_minmax
  GL_EXT_blend_subtract
  GL_EXT_fog_coord
  GL_EXT_multi_draw_arrays
  GL_EXT_secondary_color
  GL_EXT_stencil_wrap
  GL_EXT_texture_lod_bias  }

{ GL_ARB_depth_texture
  http://www.opengl.org/registry/specs/ARB/depth_texture.txt }
const
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_DEPTH_TEXTURE_MODE = $884B;

{ GL_ARB_point_parameters
  http://www.opengl.org/registry/specs/ARB/point_parameters.txt }
const
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_POINT_DISTANCE_ATTENUATION = $8129;

var
  glPointParameterf: procedure(pname: GLenum; param: GLfloat); apicall;
  glPointParameterfv: procedure(pname: GLenum; params: PGLfloat); apicall;

{ GL_ARB_shadow
  http://www.opengl.org/registry/specs/ARB/shadow.txt }
const
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;

{ ARB_texture_env_crossbar
  http://www.opengl.org/registry/specs/ARB/texture_env_crossbar.txt }

{ GL_ARB_texture_mirrored_repeat
  http://www.opengl.org/registry/specs/ARB/texture_mirrored_repeat.txt }
const
  GL_MIRRORED_REPEAT = $8370;

{ GL_ARB_window_pos
  http://www.opengl.org/registry/specs/ARB/window_pos.txt }
var
  glWindowPos2d: procedure(x: GLdouble; y: GLdouble); apicall;
  glWindowPos2f: procedure(x: GLfloat; y: GLfloat); apicall;
  glWindowPos2i: procedure(x: GLint; y: GLint); apicall;
  glWindowPos2s: procedure(x: GLshort; y: GLshort); apicall;
  glWindowPos2dv: procedure(const p: PGLdouble); apicall;
  glWindowPos2fv: procedure(const p: PGLfloat); apicall;
  glWindowPos2iv: procedure(const p: PGLint); apicall;
  glWindowPos2sv: procedure(const p: PGLshort); apicall;
  glWindowPos3d: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glWindowPos3f: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glWindowPos3i: procedure(x: GLint; y: GLint; z: GLint); apicall;
  glWindowPos3s: procedure(x: GLshort; y: GLshort; z: GLshort); apicall;
  glWindowPos3dv: procedure(const p: PGLdouble); apicall;
  glWindowPos3fv: procedure(const p: PGLfloat); apicall;
  glWindowPos3iv: procedure(const p: PGLint); apicall;
  glWindowPos3sv: procedure(const p: PGLshort); apicall;

{ GL_EXT_blend_color
  http://www.opengl.org/registry/specs/EXT/blend_color.txt }
const
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;

var
  glBlendColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); apicall;

{ GL_EXT_blend_func_separate
  http://www.opengl.org/registry/specs/EXT/blend_func_separate.txt }
const
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;

var
  glBlendFuncSeparate: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); apicall;

{ GL_EXT_blend_minmax
  http://www.opengl.org/registry/specs/EXT/blend_minmax.txt }
const
  GL_FUNC_ADD = $8006;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_BLEND_EQUATION = $8009;

var
  glBlendEquation: procedure(mode: GLenum); apicall;

{ GL_EXT_blend_subtract
  http://www.opengl.org/registry/specs/EXT/blend_subtract.txt }
const
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;

{ GL_EXT_fog_coord
  http://www.opengl.org/registry/specs/EXT/fog_coord.txt }
const
  GL_FOG_COORDINATE_SOURCE = $8450;
  GL_FOG_COORDINATE = $8451;
  GL_FRAGMENT_DEPTH = $8452;
  GL_CURRENT_FOG_COORDINATE = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORDINATE_ARRAY = $8457;

var
  glFogCoordf: procedure(coord: GLfloat); apicall;
  glFogCoordd: procedure(coord: GLdouble); apicall;
  glFogCoordfv: procedure(coord: PGLfloat); apicall;
  glFogCoorddv: procedure(coord: PGLdouble); apicall;
  glFogCoordPointer: procedure(_type: GLenum; stride: GLsizei; pointer: PGLvoid); apicall;

{ GL_EXT_multi_draw_arrays
  http://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt }
var
  glMultiDrawArrays: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); apicall;
  glMultiDrawElements: procedure(mode: GLenum; count: PGLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); apicall;

{ GL_EXT_secondary_color
  http://www.opengl.org/registry/specs/EXT/secondary_color.txt }
const
  GL_COLOR_SUM = $8458;
  GL_CURRENT_SECONDARY_COLOR = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D;
  GL_SECONDARY_COLOR_ARRAY = $845E;

var
  glSecondaryColor3b: procedure(components: GLbyte); apicall;
  glSecondaryColor3s: procedure(components: GLshort); apicall;
  glSecondaryColor3i: procedure(components: GLint); apicall;
  glSecondaryColor3f: procedure(components: GLfloat); apicall;
  glSecondaryColor3d: procedure(components: GLdouble); apicall;
  glSecondaryColor3ub: procedure(components: GLubyte); apicall;
  glSecondaryColor3us: procedure(components: GLushort); apicall;
  glSecondaryColor3ui: procedure(components: GLuint); apicall;
  glSecondaryColor3bv: procedure(components: GLbyte); apicall;
  glSecondaryColor3sv: procedure(components: GLshort); apicall;
  glSecondaryColor3iv: procedure(components: GLint); apicall;
  glSecondaryColor3fv: procedure(components: GLfloat); apicall;
  glSecondaryColor3dv: procedure(components: GLdouble); apicall;
  glSecondaryColor3ubv: procedure(components: GLubyte); apicall;
  glSecondaryColor3usv: procedure(components: GLushort); apicall;
  glSecondaryColor3uiv: procedure(components: GLuint); apicall;
  glSecondaryColorPointer: procedure(size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); apicall;

{ GL_EXT_stencil_wrap
  http://www.opengl.org/registry/specs/EXT/stencil_wrap.txt }
const
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;

{ GL_EXT_texture_lod_bias
  http://www.opengl.org/registry/specs/EXT/texture_lod_bias.txt }
const
  GL_TEXTURE_FILTER_CONTROL = $8500;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
{$endif}

{$ifdef opengl_1_5}
{ Should have the following extensions:

  GL_ARB_occlusion_query
  GL_ARB_vertex_buffer_object
  GL_EXT_shadow_funcs }

{ GL_ARB_occlusion_query
  http://www.opengl.org/registry/specs/ARB/occlusion_query.txt }
const
  GL_SAMPLES_PASSED = $8914;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;

var
  glGenQueries: procedure(n: GLsizei; ids: PGLuint); apicall;
  glDeleteQueries: procedure(n: GLsizei; const ids: PGLuint); apicall;
  glIsQuery: function(id: GLuint): GLboolean; apicall;
  glBeginQuery: procedure(target: GLenum; id: GLuint); apicall;
  glEndQuery: procedure(target: GLenum); apicall;
  glGetQueryiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetQueryObjectiv: procedure(id: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetQueryObjectuiv: procedure(id: GLuint; pname: GLenum; params: PGLuint); apicall;

{ GL_ARB_vertex_buffer_object
  http://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt }
const
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;

var
  glBindBuffer: procedure(target : GLenum; buffer: GLuint); apicall;
  glDeleteBuffers: procedure(n : GLsizei; buffers : PGLuint); apicall;
  glGenBuffers: procedure(n : GLsizei; buffers : PGLuint); apicall;
  glIsBuffer: function (buffer : GLuint) :GLboolean; apicall;
  glBufferData: procedure(target : GLenum; size:GLsizei; data:PGLvoid;usage: GLenum); apicall;
  glBufferSubData: procedure(target : GLenum; offset :GLint; size : GLsizei; data: PGLvoid); apicall;
  glGetBufferSubData: procedure(target : GLenum; offset :GLint; size : GLsizei; data: PGLvoid); apicall;
  glMapBuffer: function (target :GLenum; access: GLenum) : PGLvoid; apicall;
  glUnmapBuffer: function (target :GLenum) :GLboolean; apicall;
  glGetBufferParameteriv: procedure(target:GLenum; pname:GLenum; params:PGLint); apicall;
  glGetBufferPointerv: procedure(target: GLenum; pname:GLenum; params: PPGLvoid); apicall;

{ GL_EXT_shadow_funcs
  http://www.opengl.org/registry/specs/EXT/shadow_funcs.txt }
{$endif}

{$ifdef opengl_2_0}
{ Should have the following extensions:

  GL_ARB_draw_buffers
  GL_ARB_fragment_shader
  GL_ARB_point_sprite
  GL_ARB_shader_objects
  GL_ARB_shading_language_100
  GL_ARB_texture_non_power_of_two
  GL_ARB_vertex_shader
  GL_EXT_blend_equation_separate
  GL_EXT_stencil_two_side }

{ GL_ARB_draw_buffers
  http://www.opengl.org/registry/specs/ARB/draw_buffers.txt }
const
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;

var
  glDrawBuffers: procedure(n: GLsizei; const bufs: PGLenum); apicall;

{ GL_ARB_fragment_shader
  http://www.opengl.org/registry/specs/ARB/fragment_shader.txt }
const
  GL_FRAGMENT_SHADER = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_TEXTURE_COORDS = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;

{ GL_ARB_point_sprite
  http://www.opengl.org/registry/specs/ARB/point_sprite.txt }
const
  GL_POINT_SPRITE = $8861;
  GL_COORD_REPLACE = $8862;

{ GL_ARB_shader_objects
  http://www.opengl.org/registry/specs/ARB/shader_objects.txt }
const
  GL_PROGRAM_OBJECT = $8B40;
  GL_OBJECT_TYPE = $8B4E;
  GL_OBJECT_SUBTYPE = $8B4F;
  GL_OBJECT_DELETE_STATUS = $8B80;
  GL_OBJECT_COMPILE_STATUS = $8B81;
  GL_OBJECT_LINK_STATUS = $8B82;
  GL_OBJECT_VALIDATE_STATUS = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH = $8B88;
  GL_SHADER_OBJECT = $8B48;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;

var
  glDeleteObject: procedure(obj: GLhandle); apicall;
  glGetHandle: function(pname: GLenum): GLhandle; apicall;
  glDetachObject: procedure(containerObj: GLhandle; attachedObj: GLhandle); apicall;
  glCreateShaderObject: function(shaderType: GLenum): GLhandle; apicall;
  glShaderSource: procedure(shaderObj: GLhandle; count: GLsizei; const _string: PGLvoid; const length: PGLint); apicall;
  glCompileShader: procedure(shaderObj: GLhandle); apicall;
  glCreateProgramObject: function(): GLhandle; apicall;
  glAttachObject: procedure(containerObj: GLhandle; obj: GLhandle); apicall;
  glLinkProgram: procedure(programObj: GLhandle); apicall;
  glUseProgramObject: procedure(programObj: GLhandle); apicall;
  glValidateProgram: procedure(programObj: GLhandle); apicall;
  glUniform1f: procedure(location: GLint; v0: GLfloat); apicall;
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glUniform1i: procedure(location: GLint; v0: GLint); apicall;
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); apicall;
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glUniform1fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform2fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform1iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform2iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform3iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform4iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glGetObjectParameterfv: procedure(obj: GLhandle; pname: GLenum; params: PGLfloat); apicall;
  glGetObjectParameteriv: procedure(obj: GLhandle; pname: GLenum; params: PGLint); apicall;
  glGetInfoLog: procedure(obj: GLhandle; maxLength: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetAttachedObjects: procedure(containerObj: GLhandle; maxCount: GLsizei; count: PGLsizei; obj: PGLhandle); apicall;
  glGetUniformLocation: function(programObj: GLhandle; const name: PGLchar): GLint; apicall;
  glGetActiveUniform: procedure(programObj: GLhandle; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); apicall;
  glGetUniformfv: procedure(programObj: GLhandle; location: GLint; params: PGLfloat); apicall;
  glGetUniformiv: procedure(programObj: GLhandle; location: GLint; params: PGLint); apicall;
  glGetShaderSource: procedure(obj: GLhandle; maxLength: GLsizei; length: PGLsizei; source: PGLchar); apicall;

{ GL_ARB_shading_language_100
  http://www.opengl.org/registry/specs/ARB/shading_language_100.txt }

{ GL_ARB_texture_non_power_of_two
  http://www.opengl.org/registry/specs/ARB/texture_non_power_of_two.txt }

{ GL_ARB_vertex_shader
  http://www.opengl.org/registry/specs/ARB/vertex_shader.txt }
const
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643;
  GL_OBJECT_ACTIVE_ATTRIBUTES = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_CURRENT_VERTEX_ATTRIB = $8626;

var
  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); apicall;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); apicall;
  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); apicall;
  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); apicall;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); apicall;
  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); apicall;
  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); apicall;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); apicall;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); apicall;
  glVertexAttrib1sv: procedure(index: GLuint; const v: PGLshort); apicall;
  glVertexAttrib1fv: procedure(index: GLuint; const v: PGLfloat); apicall;
  glVertexAttrib1dv: procedure(index: GLuint; const v: PGLdouble); apicall;
  glVertexAttrib2sv: procedure(index: GLuint; const v: PGLshort); apicall;
  glVertexAttrib2fv: procedure(index: GLuint; const v: PGLfloat); apicall;
  glVertexAttrib2dv: procedure(index: GLuint; const v: PGLdouble); apicall;
  glVertexAttrib3sv: procedure(index: GLuint; const v: PGLshort); apicall;
  glVertexAttrib3fv: procedure(index: GLuint; const v: PGLfloat); apicall;
  glVertexAttrib3dv: procedure(index: GLuint; const v: PGLdouble); apicall;
  glVertexAttrib4bv: procedure(index: GLuint; const v: PGLbyte); apicall;
  glVertexAttrib4sv: procedure(index: GLuint; const v: PGLshort); apicall;
  glVertexAttrib4iv: procedure(index: GLuint; const v: PGLint); apicall;
  glVertexAttrib4ubv: procedure(index: GLuint; const v: PGLubyte); apicall;
  glVertexAttrib4usv: procedure(index: GLuint; const v: PGLushort); apicall;
  glVertexAttrib4uiv: procedure(index: GLuint; const v: PGLuint); apicall;
  glVertexAttrib4fv: procedure(index: GLuint; const v: PGLfloat); apicall;
  glVertexAttrib4dv: procedure(index: GLuint; const v: PGLdouble); apicall;
  glVertexAttrib4Nbv: procedure(index: GLuint; const v: PGLbyte); apicall;
  glVertexAttrib4Nsv: procedure(index: GLuint; const v: PGLshort); apicall;
  glVertexAttrib4Niv: procedure(index: GLuint; const v: PGLint); apicall;
  glVertexAttrib4Nubv: procedure(index: GLuint; const v: PGLubyte); apicall;
  glVertexAttrib4Nusv: procedure(index: GLuint; const v: PGLushort); apicall;
  glVertexAttrib4Nuiv: procedure(index: GLuint; const v: PGLuint); apicall;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid); apicall;
  glEnableVertexAttribArray: procedure(index: GLuint); apicall;
  glDisableVertexAttribArray: procedure(index: GLuint); apicall;
  glBindAttribLocation: procedure(programObj: GLhandle; index: GLuint; const name: PGLchar); apicall;
  glGetActiveAttrib: procedure(programObj: GLhandle; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); apicall;
  glGetAttribLocation: function(programObj: GLhandle; const name: PGLchar): GLint; apicall;
  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); apicall;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; pointer: PGLvoid); apicall;

{ GL_EXT_blend_equation_separate
  http://www.opengl.org/registry/specs/EXT/blend_equation_separate.txt }
const
  GL_BLEND_EQUATION_RGB = $8009;
  GL_BLEND_EQUATION_ALPHA = $883D;

var
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); apicall;

{ GL_EXT_stencil_two_side
  http://www.opengl.org/registry/specs/EXT/stencil_two_side.txt }
const
  GL_STENCIL_TEST_TWO_SIDE = $8910;
  GL_ACTIVE_STENCIL_FACE = $8911;

var
  glActiveStencilFace: procedure(face: GLenum); apicall;
{$endif}

{$ifdef opengl_2_1}
{ Should have the following extensions:

  GL_ARB_pixel_buffer_object
  GL_EXT_texture_sRGB }

{ GL_ARB_pixel_buffer_object
  http://www.opengl.org/registry/specs/ARB/pixel_buffer_object.txt }
const
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;

{ GL_EXT_texture_sRGB
  http://www.opengl.org/registry/specs/EXT/texture_sRGB.txt }
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;

  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
{$endif}

{$ifdef opengl_3_0}
{ Should have the following extensions:

  GL_EXT_gpu_shader4
  GL_ARB_color_buffer_float
  GL_ARB_texture_float
  GL_EXT_packed_float
  GL_EXT_texture_shared_exponent
  GL_ARB_half_float_pixel
  GL_EXT_framebuffer_object
  GL_EXT_framebuffer_sRGB
  GL_EXT_framebuffer_blit
  GL_EXT_framebuffer_multisample
  GL_EXT_texture_integer
  GL_EXT_packed_depth_stencil
  GL_EXT_draw_buffers2
  GL_EXT_texture_integer
  GL_EXT_texture_array
  GL_EXT_texture_compression_rgtc
  GL_EXT_transform_feedback }

{ EXT_gpu_shader4
  http://www.opengl.org/registry/specs/EXT/gpu_shader4.txt }
const
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_2D_RECT = $8DCD;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;

var
  glVertexAttribI1i: procedure(index: GLuint; x: GLint); apicall;
  glVertexAttribI2i: procedure(index: GLuint; x: GLint; y: GLint); apicall;
  glVertexAttribI3i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint); apicall;
  glVertexAttribI4i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glVertexAttribI1ui: procedure(index: GLuint; x: GLuint); apicall;
  glVertexAttribI2ui: procedure(index: GLuint; x: GLuint; y: GLuint); apicall;
  glVertexAttribI3ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint); apicall;
  glVertexAttribI4ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); apicall;
  glVertexAttribI1iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI2iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI3iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI4iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI1uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI2uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI3uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI4uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI4bv: procedure(index: GLuint; v: PGLbyte); apicall;
  glVertexAttribI4sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttribI4ubv: procedure(index: GLuint; v: PGLubyte); apicall;
  glVertexAttribI4usv: procedure(index: GLuint; v: PGLushort); apicall;
  glGetVertexAttribIiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribIuiv: procedure(index: GLuint; pname: GLenum; params: PGLuint); apicall;
  glUniform1ui: procedure(location: GLint; v0: GLuint); apicall;
  glUniform2ui: procedure(location: GLint; v0: GLuint; v1: GLuint); apicall;
  glUniform3ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); apicall;
  glUniform4ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); apicall;
  glUniform1uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform2uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform3uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform4uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glGetUniformuiv: procedure(_program: GLuint; location: GLint; params: PGLuint); apicall;
  glBindFragDataLocation: procedure(_program: GLuint; colorNumber: GLuint; name: PGLchar); apicall;
  glGetFragDataLocation: function(_program: GLuint; name: PGLchar): GLint; apicall;

{ GL_ARB_color_buffer_float
  http://www.opengl.org/registry/specs/ARB/color_buffer_float.txt }
const
  GL_RGBA_FLOAT_MODE = $8820;
  GL_CLAMP_VERTEX_COLOR = $891A;
  GL_CLAMP_FRAGMENT_COLOR = $891B;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  WGL_TYPE_RGBA_FLOAT = $21A0;

var
  glClampColor: procedure(target: GLenum; clamp: GLenum); apicall;

{ GL_ARB_texture_float
  http://www.opengl.org/registry/specs/ARB/texture_float.txt }
const
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE = $8C14;
  GL_TEXTURE_INTENSITY_TYPE = $8C15;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_ALPHA32F = $8816;
  GL_INTENSITY32F = $8817;
  GL_LUMINANCE32F = $8818;
  GL_LUMINANCE_ALPHA32F = $8819;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_ALPHA16F = $881C;
  GL_INTENSITY16F = $881D;
  GL_LUMINANCE16F = $881E;
  GL_LUMINANCE_ALPHA16F = $881F;

{ GL_EXT_texture_shared_exponent
  http://www.opengl.org/registry/specs/EXT/texture_shared_exponent.txt }
const
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;

{ GL_ARB_half_float_pixel
  http://www.opengl.org/registry/specs }
const
  GL_HALF_FLOAT = $140B;

{ GL_EXT_framebuffer_object
http://www.opengl.org/registry/specs/EXT/framebuffer_object.txt }
const
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET = $8CD4;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_FRAMEBUFFER_STATUS_ERROR = $8CDE;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

var
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; apicall;
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); apicall;
  glDeleteRenderbuffers: procedure(n: GLsizei; const renderbuffers: PGLuint); apicall;
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; apicall;
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); apicall;
  glDeleteFramebuffers: procedure(n: GLsizei; const framebuffers: PGLuint); apicall;
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glCheckFramebufferStatus: function(target: GLenum): GLenum; apicall;
  glFramebufferTexture1D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFramebufferTexture3D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); apicall;
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); apicall;
  glGenerateMipmap: procedure(target: GLenum); apicall;

{ GL_EXT_framebuffer_sRGB
http://www.opengl.org/registry/specs/EXT/framebuffer_sRGB.txt }
const
  FRAMEBUFFER_SRGB = $8DB9;
  FRAMEBUFFER_SRGB_CAPABLE = $8DBA;

{ GL_EXT_framebuffer_blit
http://www.opengl.org/registry/specs/EXT/framebuffer_blit.txt }
const
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_DRAW_FRAMEBUFFER_BINDING = $8CA6;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;

var
  glBlitFramebuffer: procedure(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1: GLint; mask: GLbitfield; filter: GLenum); apicall;

{ GL_EXT_framebuffer_multisample
http://www.opengl.org/registry/specs/EXT/framebuffer_multisample.txt }
const
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;

var
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width, height: GLsizei); apicall;

{ GL_EXT_texture_integer
http://www.opengl.org/registry/specs/EXT/texture_integer.txt }
const
  GL_RGBA_INTEGER_MODE = $8D9E;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_ALPHA32UI = $8D72;
  GL_INTENSITY32UI = $8D73;
  GL_LUMINANCE32UI = $8D74;
  GL_LUMINANCE_ALPHA32UI = $8D75;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_ALPHA16UI = $8D78;
  GL_INTENSITY16UI = $8D79;
  GL_LUMINANCE16UI = $8D7A;
  GL_LUMINANCE_ALPHA16UI = $8D7B;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_ALPHA8UI = $8D7E;
  GL_INTENSITY8UI = $8D7F;
  GL_LUMINANCE8UI = $8D80;
  GL_LUMINANCE_ALPHA8UI = $8D81;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_ALPHA32I = $8D84;
  GL_INTENSITY32I = $8D85;
  GL_LUMINANCE32I = $8D86;
  GL_LUMINANCE_ALPHA32I = $8D87;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_ALPHA16I = $8D8A;
  GL_INTENSITY16I = $8D8B;
  GL_LUMINANCE16I = $8D8C;
  GL_LUMINANCE_ALPHA16I = $8D8D;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_ALPHA8I = $8D90;
  GL_INTENSITY8I = $8D91;
  GL_LUMINANCE8I = $8D92;
  GL_LUMINANCE_ALPHA8I = $8D93;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_ALPHA_INTEGER = $8D97;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  GL_LUMINANCE_INTEGER = $8D9C;
  GL_LUMINANCE_ALPHA_INTEGER = $8D9D;

var
  glClearColorIi: procedure(r: GLint; g: GLint; b: GLint; a: GLint); apicall;
  glClearColorIui: procedure(r: GLuint; g: GLuint; b: GLuint; a: GLuint); apicall;
  glTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;
  glGetTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;

{ GL_EXT_packed_depth_stencil
http://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt }
const
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;

{ GL_EXT_draw_buffers2
http://www.opengl.org/registry/specs/EXT/draw_buffers2.txt }
var
  glColorMaskIndexed: procedure(buf: GLuint; r: GLboolean; g: GLboolean; b, a: GLboolean); apicall;
  glGetBooleanIndexedv: procedure(value: GLenum; index: GLuint; data: PGLboolean); apicall;
  glGetIntegerIndexedv: procedure(value: GLenum; index: GLuint; data: PGLint); apicall;
  glEnableIndexed: procedure(target: GLenum; index: GLuint); apicall;
  glDisableIndexed: procedure(target: GLenum; index: GLuint); apicall;
  glIsEnabledIndexed: function(target: GLenum; index: GLuint): GLboolean; apicall;

{ GL_EXT_texture_array
http://www.opengl.org/registry/specs/EXT/texture_array.txt }
const
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_COMPARE_REF_DEPTH_TO_TEXTURE = $884E;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;

var
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); apicall;

{ GL_EXT_texture_compression_rgtc
http://www.opengl.org/registry/specs/EXT/texture_compression_rgtc.txt }
const
  GL_COMPRESSED_RED_RGTC1 = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1 = $8DBC;
  GL_COMPRESSED_RED_GREEN_RGTC2 = $8DBD;
  GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2 = $8DBE;

{ GL_EXT_transform_feedback
http://www.opengl.org/registry/specs/EXT/transform_feedback.txt }
const
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;

var
  glBindBufferRange: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: PGLint; size: PGLsizei); apicall;
  glBindBufferOffset: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: PGLint); apicall;
  glBindBufferBase: procedure(target: GLenum; index: GLuint; buffer: GLuint); apicall;
  glBeginTransformFeedback: procedure(primitiveMode: GLenum); apicall;
  glEndTransformFeedback: procedure; apicall;
  glTransformFeedbackVaryings: procedure(_program: GLuint; count: GLsizei; varyings: PPGLchar; bufferMode: GLenum); apicall;
  glGetTransformFeedbackVarying: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLchar); apicall;
{$endif}

{ glu replacements }

procedure gluPerspective(fovY, aspect, zNear, zFar: GLdouble);
procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: GLdouble);

{ OpenGL management }

var
  OpenGLManager: record
    MajorVersion: Word;
    MinorVersion: Word;
    Load: function: Boolean;
    GetProcAddress: function(ProcName: PChar): Pointer;
    ExtensionSupported: function(Extension: PChar): Boolean;
  end;

function OpenGLInit: Boolean;

implementation

function Tan(const X: Extended): Extended;
begin
  Result := Sin(X) / Cos(X);
end;

{ glu replacements }

procedure gluPerspective(fovY, aspect, zNear, zFar: GLdouble);
var
  fw, fh: GLdouble;
begin
  if @glFrustum = nil then
    Exit;
  fh := Tan(fovY / 360 * PI) * zNear;
  fW := fH * aspect;
  glFrustum(-fW, fW, -fH, fH, zNear, zFar);
end;

procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: GLdouble);
begin
  if @glTranslated = nil then
    Exit;
end;

(*
gluLookAt(GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx,
      GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy,
      GLdouble upz)
{
    float forward[3], side[3], up[3];
    GLfloat m[4][4];

    forward[0] = centerx - eyex;
    forward[1] = centery - eyey;
    forward[2] = centerz - eyez;

    up[0] = upx;
    up[1] = upy;
    up[2] = upz;

    normalize(forward);

    /* Side = forward x up */
    cross(forward, up, side);
    normalize(side);

    /* Recompute up as: up = side x forward */
    cross(side, forward, up);

    __gluMakeIdentityf(&m[0][0]);
    m[0][0] = side[0];
    m[1][0] = side[1];
    m[2][0] = side[2];

    m[0][1] = up[0];
    m[1][1] = up[1];
    m[2][1] = up[2];

    m[0][2] = -forward[0];
    m[1][2] = -forward[1];
    m[2][2] = -forward[2];

    glMultMatrixf(&m[0][0]);
    glTranslated(-eyex, -eyey, -eyez);
} *)
{ Library management }

var
  Initialized: Boolean;

function OpenGLInit: Boolean;

  procedure GetProc(var Proc: Pointer; Name: PChar);
  begin
    Proc := OpenGLManager.GetProcAddress(Name);
    Initialized := Initialized and (Proc <> nil);
  end;

  procedure CheckExt(Name: PChar);
  begin
    Initialized := Initialized and OpenGLManager.ExtensionSupported(Name);
  end;

begin
  if Initialized then
    Exit(True);
  if @OpenGLManager.Load = nil then
    Exit(False);
  if @OpenGLManager.GetProcAddress = nil then
    Exit(False);
  if @OpenGLManager.ExtensionSupported = nil then
    Exit(False);
  if not OpenGLManager.Load then
    Exit(False);
  OpenGLManager.MajorVersion := 0;
  OpenGLManager.MinorVersion := 0;
  Initialized := True;
  {$ifdef opengl_1_1}
  GetProc(@glAccum, 'glAccum');
  GetProc(@glAlphaFunc, 'glAlphaFunc');
  GetProc(@glBegin, 'glBegin');
  GetProc(@glBitmap, 'glBitmap');
  GetProc(@glBlendFunc, 'glBlendFunc');
  GetProc(@glCallList, 'glCallList');
  GetProc(@glCallLists, 'glCallLists');
  GetProc(@glClear, 'glClear');
  GetProc(@glClearAccum, 'glClearAccum');
  GetProc(@glClearColor, 'glClearColor');
  GetProc(@glClearDepth, 'glClearDepth');
  GetProc(@glClearIndex, 'glClearIndex');
  GetProc(@glClearStencil, 'glClearStencil');
  GetProc(@glClipPlane, 'glClipPlane');
  GetProc(@glColor3b, 'glColor3b');
  GetProc(@glColor3bv, 'glColor3bv');
  GetProc(@glColor3d, 'glColor3d');
  GetProc(@glColor3dv, 'glColor3dv');
  GetProc(@glColor3f, 'glColor3f');
  GetProc(@glColor3fv, 'glColor3fv');
  GetProc(@glColor3i, 'glColor3i');
  GetProc(@glColor3iv, 'glColor3iv');
  GetProc(@glColor3s, 'glColor3s');
  GetProc(@glColor3sv, 'glColor3sv');
  GetProc(@glColor3ub, 'glColor3ub');
  GetProc(@glColor3ubv, 'glColor3ubv');
  GetProc(@glColor3ui, 'glColor3ui');
  GetProc(@glColor3uiv, 'glColor3uiv');
  GetProc(@glColor3us, 'glColor3us');
  GetProc(@glColor3usv, 'glColor3usv');
  GetProc(@glColor4b, 'glColor4b');
  GetProc(@glColor4bv, 'glColor4bv');
  GetProc(@glColor4d, 'glColor4d');
  GetProc(@glColor4dv, 'glColor4dv');
  GetProc(@glColor4f, 'glColor4f');
  GetProc(@glColor4fv, 'glColor4fv');
  GetProc(@glColor4i, 'glColor4i');
  GetProc(@glColor4iv, 'glColor4iv');
  GetProc(@glColor4s, 'glColor4s');
  GetProc(@glColor4sv, 'glColor4sv');
  GetProc(@glColor4ub, 'glColor4ub');
  GetProc(@glColor4ubv, 'glColor4ubv');
  GetProc(@glColor4ui, 'glColor4ui');
  GetProc(@glColor4uiv, 'glColor4uiv');
  GetProc(@glColor4us, 'glColor4us');
  GetProc(@glColor4usv, 'glColor4usv');
  GetProc(@glColorMask, 'glColorMask');
  GetProc(@glColorMaterial, 'glColorMaterial');
  GetProc(@glCopyPixels, 'glCopyPixels');
  GetProc(@glCullFace, 'glCullFace');
  GetProc(@glDeleteLists, 'glDeleteLists');
  GetProc(@glDepthFunc, 'glDepthFunc');
  GetProc(@glDepthMask, 'glDepthMask');
  GetProc(@glDepthRange, 'glDepthRange');
  GetProc(@glDisable, 'glDisable');
  GetProc(@glDisableClientState, 'glDisableClientState');
  GetProc(@glEnableClientState, 'glEnableClientState');
  GetProc(@glDrawArrays, 'glDrawArrays');
  GetProc(@glDrawElements, 'glDrawElements');
  GetProc(@glDrawBuffer, 'glDrawBuffer');
  GetProc(@glDrawPixels, 'glDrawPixels');
  GetProc(@glEdgeFlag, 'glEdgeFlag');
  GetProc(@glEdgeFlagv, 'glEdgeFlagv');
  GetProc(@glEnable, 'glEnable');
  GetProc(@glEnd, 'glEnd');
  GetProc(@glEndList, 'glEndList');
  GetProc(@glEvalCoord1d, 'glEvalCoord1d');
  GetProc(@glEvalCoord1dv, 'glEvalCoord1dv');
  GetProc(@glEvalCoord1f, 'glEvalCoord1f');
  GetProc(@glEvalCoord1fv, 'glEvalCoord1fv');
  GetProc(@glEvalCoord2d, 'glEvalCoord2d');
  GetProc(@glEvalCoord2dv, 'glEvalCoord2dv');
  GetProc(@glEvalCoord2f, 'glEvalCoord2f');
  GetProc(@glEvalCoord2fv, 'glEvalCoord2fv');
  GetProc(@glEvalMesh1, 'glEvalMesh1');
  GetProc(@glEvalMesh2, 'glEvalMesh2');
  GetProc(@glEvalPoint1, 'glEvalPoint1');
  GetProc(@glEvalPoint2, 'glEvalPoint2');
  GetProc(@glFeedbackBuffer, 'glFeedbackBuffer');
  GetProc(@glFinish, 'glFinish');
  GetProc(@glFlush, 'glFlush');
  GetProc(@glFogf, 'glFogf');
  GetProc(@glFogfv, 'glFogfv');
  GetProc(@glFogi, 'glFogi');
  GetProc(@glFogiv, 'glFogiv');
  GetProc(@glFrontFace, 'glFrontFace');
  GetProc(@glFrustum, 'glFrustum');
  GetProc(@glGenLists, 'glGenLists');
  GetProc(@glGetBooleanv, 'glGetBooleanv');
  GetProc(@glGetClipPlane, 'glGetClipPlane');
  GetProc(@glGetDoublev, 'glGetDoublev');
  GetProc(@glGetError, 'glGetError');
  GetProc(@glGetFloatv, 'glGetFloatv');
  GetProc(@glGetIntegerv, 'glGetIntegerv');
  GetProc(@glGetLightfv, 'glGetLightfv');
  GetProc(@glGetLightiv, 'glGetLightiv');
  GetProc(@glGetMapdv, 'glGetMapdv');
  GetProc(@glGetMapfv, 'glGetMapfv');
  GetProc(@glGetMapiv, 'glGetMapiv');
  GetProc(@glGetMaterialfv, 'glGetMaterialfv');
  GetProc(@glGetMaterialiv, 'glGetMaterialiv');
  GetProc(@glGetPixelMapfv, 'glGetPixelMapfv');
  GetProc(@glGetPixelMapuiv, 'glGetPixelMapuiv');
  GetProc(@glGetPixelMapusv, 'glGetPixelMapusv');
  GetProc(@glGetPolygonStipple, 'glGetPolygonStipple');
  GetProc(@glGetString, 'glGetString');
  GetProc(@glGetTexEnvfv, 'glGetTexEnvfv');
  GetProc(@glGetTexEnviv, 'glGetTexEnviv');
  GetProc(@glGetTexGendv, 'glGetTexGendv');
  GetProc(@glGetTexGenfv, 'glGetTexGenfv');
  GetProc(@glGetTexGeniv, 'glGetTexGeniv');
  GetProc(@glGetTexImage, 'glGetTexImage');
  GetProc(@glGetTexLevelParameterfv, 'glGetTexLevelParameterfv');
  GetProc(@glGetTexLevelParameteriv, 'glGetTexLevelParameteriv');
  GetProc(@glGetTexParameterfv, 'glGetTexParameterfv');
  GetProc(@glGetTexParameteriv, 'glGetTexParameteriv');
  GetProc(@glHint, 'glHint');
  GetProc(@glIndexMask, 'glIndexMask');
  GetProc(@glIndexd, 'glIndexd');
  GetProc(@glIndexdv, 'glIndexdv');
  GetProc(@glIndexf, 'glIndexf');
  GetProc(@glIndexfv, 'glIndexfv');
  GetProc(@glIndexi, 'glIndexi');
  GetProc(@glIndexiv, 'glIndexiv');
  GetProc(@glIndexs, 'glIndexs');
  GetProc(@glIndexsv, 'glIndexsv');
  GetProc(@glInitNames, 'glInitNames');
  GetProc(@glIsEnabled, 'glIsEnabled');
  GetProc(@glIsList, 'glIsList');
  GetProc(@glLightModelf, 'glLightModelf');
  GetProc(@glLightModelfv, 'glLightModelfv');
  GetProc(@glLightModeli, 'glLightModeli');
  GetProc(@glLightModeliv, 'glLightModeliv');
  GetProc(@glLightf, 'glLightf');
  GetProc(@glLightfv, 'glLightfv');
  GetProc(@glLighti, 'glLighti');
  GetProc(@glLightiv, 'glLightiv');
  GetProc(@glLineStipple, 'glLineStipple');
  GetProc(@glLineWidth, 'glLineWidth');
  GetProc(@glListBase, 'glListBase');
  GetProc(@glLoadIdentity, 'glLoadIdentity');
  GetProc(@glLoadMatrixd, 'glLoadMatrixd');
  GetProc(@glLoadMatrixf, 'glLoadMatrixf');
  GetProc(@glLoadName, 'glLoadName');
  GetProc(@glLogicOp, 'glLogicOp');
  GetProc(@glMap1d, 'glMap1d');
  GetProc(@glMap1f, 'glMap1f');
  GetProc(@glMap2d, 'glMap2d');
  GetProc(@glMap2f, 'glMap2f');
  GetProc(@glMapGrid1d, 'glMapGrid1d');
  GetProc(@glMapGrid1f, 'glMapGrid1f');
  GetProc(@glMapGrid2d, 'glMapGrid2d');
  GetProc(@glMapGrid2f, 'glMapGrid2f');
  GetProc(@glMaterialf, 'glMaterialf');
  GetProc(@glMaterialfv, 'glMaterialfv');
  GetProc(@glMateriali, 'glMateriali');
  GetProc(@glMaterialiv, 'glMaterialiv');
  GetProc(@glMatrixMode, 'glMatrixMode');
  GetProc(@glMultMatrixd, 'glMultMatrixd');
  GetProc(@glMultMatrixf, 'glMultMatrixf');
  GetProc(@glNewList, 'glNewList');
  GetProc(@glNormal3b, 'glNormal3b');
  GetProc(@glNormal3bv, 'glNormal3bv');
  GetProc(@glNormal3d, 'glNormal3d');
  GetProc(@glNormal3dv, 'glNormal3dv');
  GetProc(@glNormal3f, 'glNormal3f');
  GetProc(@glNormal3fv, 'glNormal3fv');
  GetProc(@glNormal3i, 'glNormal3i');
  GetProc(@glNormal3iv, 'glNormal3iv');
  GetProc(@glNormal3s, 'glNormal3s');
  GetProc(@glNormal3sv, 'glNormal3sv');
  GetProc(@glOrtho, 'glOrtho');
  GetProc(@glPassThrough, 'glPassThrough');
  GetProc(@glPixelMapfv, 'glPixelMapfv');
  GetProc(@glPixelMapuiv, 'glPixelMapuiv');
  GetProc(@glPixelMapusv, 'glPixelMapusv');
  GetProc(@glPixelStoref, 'glPixelStoref');
  GetProc(@glPixelStorei, 'glPixelStorei');
  GetProc(@glPixelTransferf, 'glPixelTransferf');
  GetProc(@glPixelTransferi, 'glPixelTransferi');
  GetProc(@glPixelZoom, 'glPixelZoom');
  GetProc(@glPointSize, 'glPointSize');
  GetProc(@glPolygonMode, 'glPolygonMode');
  GetProc(@glPolygonOffset, 'glPolygonOffset');
  GetProc(@glPolygonStipple, 'glPolygonStipple');
  GetProc(@glPopAttrib, 'glPopAttrib');
  GetProc(@glPopMatrix, 'glPopMatrix');
  GetProc(@glPopName, 'glPopName');
  GetProc(@glPushAttrib, 'glPushAttrib');
  GetProc(@glPushMatrix, 'glPushMatrix');
  GetProc(@glPushName, 'glPushName');
  GetProc(@glRasterPos2d, 'glRasterPos2d');
  GetProc(@glRasterPos2dv, 'glRasterPos2dv');
  GetProc(@glRasterPos2f, 'glRasterPos2f');
  GetProc(@glRasterPos2fv, 'glRasterPos2fv');
  GetProc(@glRasterPos2i, 'glRasterPos2i');
  GetProc(@glRasterPos2iv, 'glRasterPos2iv');
  GetProc(@glRasterPos2s, 'glRasterPos2s');
  GetProc(@glRasterPos2sv, 'glRasterPos2sv');
  GetProc(@glRasterPos3d, 'glRasterPos3d');
  GetProc(@glRasterPos3dv, 'glRasterPos3dv');
  GetProc(@glRasterPos3f, 'glRasterPos3f');
  GetProc(@glRasterPos3fv, 'glRasterPos3fv');
  GetProc(@glRasterPos3i, 'glRasterPos3i');
  GetProc(@glRasterPos3iv, 'glRasterPos3iv');
  GetProc(@glRasterPos3s, 'glRasterPos3s');
  GetProc(@glRasterPos3sv, 'glRasterPos3sv');
  GetProc(@glRasterPos4d, 'glRasterPos4d');
  GetProc(@glRasterPos4dv, 'glRasterPos4dv');
  GetProc(@glRasterPos4f, 'glRasterPos4f');
  GetProc(@glRasterPos4fv, 'glRasterPos4fv');
  GetProc(@glRasterPos4i, 'glRasterPos4i');
  GetProc(@glRasterPos4iv, 'glRasterPos4iv');
  GetProc(@glRasterPos4s, 'glRasterPos4s');
  GetProc(@glRasterPos4sv, 'glRasterPos4sv');
  GetProc(@glReadBuffer, 'glReadBuffer');
  GetProc(@glReadPixels, 'glReadPixels');
  GetProc(@glRectd, 'glRectd');
  GetProc(@glRectdv, 'glRectdv');
  GetProc(@glRectf, 'glRectf');
  GetProc(@glRectfv, 'glRectfv');
  GetProc(@glRecti, 'glRecti');
  GetProc(@glRectiv, 'glRectiv');
  GetProc(@glRects, 'glRects');
  GetProc(@glRectsv, 'glRectsv');
  GetProc(@glRenderMode, 'glRenderMode');
  GetProc(@glRotated, 'glRotated');
  GetProc(@glRotatef, 'glRotatef');
  GetProc(@glScaled, 'glScaled');
  GetProc(@glScalef, 'glScalef');
  GetProc(@glScissor, 'glScissor');
  GetProc(@glSelectBuffer, 'glSelectBuffer');
  GetProc(@glShadeModel, 'glShadeModel');
  GetProc(@glStencilFunc, 'glStencilFunc');
  GetProc(@glStencilMask, 'glStencilMask');
  GetProc(@glStencilOp, 'glStencilOp');
  GetProc(@glTexCoord1d, 'glTexCoord1d');
  GetProc(@glTexCoord1dv, 'glTexCoord1dv');
  GetProc(@glTexCoord1f, 'glTexCoord1f');
  GetProc(@glTexCoord1fv, 'glTexCoord1fv');
  GetProc(@glTexCoord1i, 'glTexCoord1i');
  GetProc(@glTexCoord1iv, 'glTexCoord1iv');
  GetProc(@glTexCoord1s, 'glTexCoord1s');
  GetProc(@glTexCoord1sv, 'glTexCoord1sv');
  GetProc(@glTexCoord2d, 'glTexCoord2d');
  GetProc(@glTexCoord2dv, 'glTexCoord2dv');
  GetProc(@glTexCoord2f, 'glTexCoord2f');
  GetProc(@glTexCoord2fv, 'glTexCoord2fv');
  GetProc(@glTexCoord2i, 'glTexCoord2i');
  GetProc(@glTexCoord2iv, 'glTexCoord2iv');
  GetProc(@glTexCoord2s, 'glTexCoord2s');
  GetProc(@glTexCoord2sv, 'glTexCoord2sv');
  GetProc(@glTexCoord3d, 'glTexCoord3d');
  GetProc(@glTexCoord3dv, 'glTexCoord3dv');
  GetProc(@glTexCoord3f, 'glTexCoord3f');
  GetProc(@glTexCoord3fv, 'glTexCoord3fv');
  GetProc(@glTexCoord3i, 'glTexCoord3i');
  GetProc(@glTexCoord3iv, 'glTexCoord3iv');
  GetProc(@glTexCoord3s, 'glTexCoord3s');
  GetProc(@glTexCoord3sv, 'glTexCoord3sv');
  GetProc(@glTexCoord4d, 'glTexCoord4d');
  GetProc(@glTexCoord4dv, 'glTexCoord4dv');
  GetProc(@glTexCoord4f, 'glTexCoord4f');
  GetProc(@glTexCoord4fv, 'glTexCoord4fv');
  GetProc(@glTexCoord4i, 'glTexCoord4i');
  GetProc(@glTexCoord4iv, 'glTexCoord4iv');
  GetProc(@glTexCoord4s, 'glTexCoord4s');
  GetProc(@glTexCoord4sv, 'glTexCoord4sv');
  GetProc(@glTexEnvf, 'glTexEnvf');
  GetProc(@glTexEnvfv, 'glTexEnvfv');
  GetProc(@glTexEnvi, 'glTexEnvi');
  GetProc(@glTexEnviv, 'glTexEnviv');
  GetProc(@glTexGend, 'glTexGend');
  GetProc(@glTexGendv, 'glTexGendv');
  GetProc(@glTexGenf, 'glTexGenf');
  GetProc(@glTexGenfv, 'glTexGenfv');
  GetProc(@glTexGeni, 'glTexGeni');
  GetProc(@glTexGeniv, 'glTexGeniv');
  GetProc(@glTexImage1D, 'glTexImage1D');
  GetProc(@glTexImage2D, 'glTexImage2D');
  GetProc(@glCopyTexImage1D, 'glCopyTexImage1D');
  GetProc(@glCopyTexImage2D, 'glCopyTexImage2D');
  GetProc(@glTexParameterf, 'glTexParameterf');
  GetProc(@glTexParameterfv, 'glTexParameterfv');
  GetProc(@glTexParameteri, 'glTexParameteri');
  GetProc(@glTexParameteriv, 'glTexParameteriv');
  GetProc(@glTranslated, 'glTranslated');
  GetProc(@glTranslatef, 'glTranslatef');
  GetProc(@glVertex2d, 'glVertex2d');
  GetProc(@glVertex2dv, 'glVertex2dv');
  GetProc(@glVertex2f, 'glVertex2f');
  GetProc(@glVertex2fv, 'glVertex2fv');
  GetProc(@glVertex2i, 'glVertex2i');
  GetProc(@glVertex2iv, 'glVertex2iv');
  GetProc(@glVertex2s, 'glVertex2s');
  GetProc(@glVertex2sv, 'glVertex2sv');
  GetProc(@glVertex3d, 'glVertex3d');
  GetProc(@glVertex3dv, 'glVertex3dv');
  GetProc(@glVertex3f, 'glVertex3f');
  GetProc(@glVertex3fv, 'glVertex3fv');
  GetProc(@glVertex3i, 'glVertex3i');
  GetProc(@glVertex3iv, 'glVertex3iv');
  GetProc(@glVertex3s, 'glVertex3s');
  GetProc(@glVertex3sv, 'glVertex3sv');
  GetProc(@glVertex4d, 'glVertex4d');
  GetProc(@glVertex4dv, 'glVertex4dv');
  GetProc(@glVertex4f, 'glVertex4f');
  GetProc(@glVertex4fv, 'glVertex4fv');
  GetProc(@glVertex4i, 'glVertex4i');
  GetProc(@glVertex4iv, 'glVertex4iv');
  GetProc(@glVertex4s, 'glVertex4s');
  GetProc(@glVertex4sv, 'glVertex4sv');
  GetProc(@glViewport, 'glViewport');
  GetProc(@glBindTexture, 'glBindTexture');
  GetProc(@glGenTextures, 'glGenTextures');
  GetProc(@glDeleteTextures, 'glDeleteTextures');
  GetProc(@glVertexPointer, 'glVertexPointer');
  GetProc(@glNormalPointer, 'glNormalPointer');
  GetProc(@glColorPointer, 'glColorPointer');
  GetProc(@glTexCoordPointer, 'glTexCoordPointer');
  if Initialized then
  begin
    OpenGLManager.MajorVersion := 1;
    OpenGLManager.MinorVersion := 1;
  end;
  {$endif}
  {$ifdef opengl_1_2}
  GetProc(@glBlendColor, 'glBlendColor');
  GetProc(@glBlendEquation, 'glBlendEquation');
  GetProc(@glDrawRangeElements, 'glDrawRangeElements');
  GetProc(@glColorTable, 'glColorTable');
  GetProc(@glColorTableParameterfv, 'glColorTableParameterfv');
  GetProc(@glColorTableParameteriv, 'glColorTableParameteriv');
  GetProc(@glCopyColorTable, 'glCopyColorTable');
  GetProc(@glGetColorTable, 'glGetColorTable');
  GetProc(@glGetColorTableParameterfv, 'glGetColorTableParameterfv');
  GetProc(@glGetColorTableParameteriv, 'glGetColorTableParameteriv');
  GetProc(@glColorSubTable, 'glColorSubTable');
  GetProc(@glCopyColorSubTable, 'glCopyColorSubTable');
  GetProc(@glConvolutionFilter1D, 'glConvolutionFilter1D');
  GetProc(@glConvolutionFilter2D, 'glConvolutionFilter2D');
  GetProc(@glConvolutionParameterf, 'glConvolutionParameterf');
  GetProc(@glConvolutionParameterfv, 'glConvolutionParameterfv');
  GetProc(@glConvolutionParameteri, 'glConvolutionParameteri');
  GetProc(@glConvolutionParameteriv, 'glConvolutionParameteriv');
  GetProc(@glCopyConvolutionFilter1D, 'glCopyConvolutionFilter1D');
  GetProc(@glCopyConvolutionFilter2D, 'glCopyConvolutionFilter2D');
  GetProc(@glGetConvolutionFilter, 'glGetConvolutionFilter');
  GetProc(@glGetConvolutionParameterfv, 'glGetConvolutionParameterfv');
  GetProc(@glGetConvolutionParameteriv, 'glGetConvolutionParameteriv');
  GetProc(@glGetSeparableFilter, 'glGetSeparableFilter');
  GetProc(@glSeparableFilter2D, 'glSeparableFilter2D');
  GetProc(@glGetHistogram, 'glGetHistogram');
  GetProc(@glGetHistogramParameterfv, 'glGetHistogramParameterfv');
  GetProc(@glGetHistogramParameteriv, 'glGetHistogramParameteriv');
  GetProc(@glGetMinmax, 'glGetMinmax');
  GetProc(@glGetMinmaxParameterfv, 'glGetMinmaxParameterfv');
  GetProc(@glGetMinmaxParameteriv, 'glGetMinmaxParameteriv');
  GetProc(@glHistogram, 'glHistogram');
  GetProc(@glMinmax, 'glMinmax');
  GetProc(@glResetHistogram, 'glResetHistogram');
  GetProc(@glResetMinmax, 'glResetMinmax');
  GetProc(@glTexImage3D, 'glTexImage3D');
  GetProc(@glTexSubImage3D, 'glTexSubImage3D');
  GetProc(@glCopyTexSubImage3D, 'glCopyTexSubImage3D');
  if Initialized then
    OpenGLManager.MinorVersion := 2;
  {$endif}
  {$ifdef opengl_1_3}
  CheckExt('GL_ARB_multisample');
  CheckExt('GL_ARB_multitexture');
  CheckExt('GL_ARB_texture_border_clamp');
  CheckExt('GL_ARB_texture_compression');
  CheckExt('GL_ARB_texture_cube_map');
  CheckExt('GL_ARB_texture_env_add');
  CheckExt('GL_ARB_texture_env_combine');
  CheckExt('GL_ARB_texture_env_dot3');
  CheckExt('GL_ARB_transpose_matrix');
  GetProc(@glActiveTexture, 'glActiveTexture');
  GetProc(@glClientActiveTexture, 'glClientActiveTexture');
  GetProc(@glMultiTexCoord1d, 'glMultiTexCoord1d');
  GetProc(@glMultiTexCoord1dv, 'glMultiTexCoord1dv');
  GetProc(@glMultiTexCoord1f, 'glMultiTexCoord1f');
  GetProc(@glMultiTexCoord1fv, 'glMultiTexCoord1fv');
  GetProc(@glMultiTexCoord1i, 'glMultiTexCoord1i');
  GetProc(@glMultiTexCoord1iv, 'glMultiTexCoord1iv');
  GetProc(@glMultiTexCoord1s, 'glMultiTexCoord1s');
  GetProc(@glMultiTexCoord1sv, 'glMultiTexCoord1sv');
  GetProc(@glMultiTexCoord2d, 'glMultiTexCoord2d');
  GetProc(@glMultiTexCoord2dv, 'glMultiTexCoord2dv');
  GetProc(@glMultiTexCoord2f, 'glMultiTexCoord2f');
  GetProc(@glMultiTexCoord2fv, 'glMultiTexCoord2fv');
  GetProc(@glMultiTexCoord2i, 'glMultiTexCoord2i');
  GetProc(@glMultiTexCoord2iv, 'glMultiTexCoord2iv');
  GetProc(@glMultiTexCoord2s, 'glMultiTexCoord2s');
  GetProc(@glMultiTexCoord2sv, 'glMultiTexCoord2sv');
  GetProc(@glMultiTexCoord3d, 'glMultiTexCoord3d');
  GetProc(@glMultiTexCoord3dv, 'glMultiTexCoord3dv');
  GetProc(@glMultiTexCoord3f, 'glMultiTexCoord3f');
  GetProc(@glMultiTexCoord3fv, 'glMultiTexCoord3fv');
  GetProc(@glMultiTexCoord3i, 'glMultiTexCoord3i');
  GetProc(@glMultiTexCoord3iv, 'glMultiTexCoord3iv');
  GetProc(@glMultiTexCoord3s, 'glMultiTexCoord3s');
  GetProc(@glMultiTexCoord3sv, 'glMultiTexCoord3sv');
  GetProc(@glMultiTexCoord4d, 'glMultiTexCoord4d');
  GetProc(@glMultiTexCoord4dv, 'glMultiTexCoord4dv');
  GetProc(@glMultiTexCoord4f, 'glMultiTexCoord4f');
  GetProc(@glMultiTexCoord4fv, 'glMultiTexCoord4fv');
  GetProc(@glMultiTexCoord4i, 'glMultiTexCoord4i');
  GetProc(@glMultiTexCoord4iv, 'glMultiTexCoord4iv');
  GetProc(@glMultiTexCoord4s, 'glMultiTexCoord4s');
  GetProc(@glMultiTexCoord4sv, 'glMultiTexCoord4sv');
  GetProc(@glLoadTransposeMatrixf, 'glLoadTransposeMatrixf');
  GetProc(@glLoadTransposeMatrixd, 'glLoadTransposeMatrixd');
  GetProc(@glMultTransposeMatrixf, 'glMultTransposeMatrixf');
  GetProc(@glMultTransposeMatrixd, 'glMultTransposeMatrixd');
  GetProc(@glSampleCoverage, 'glSampleCoverage');
  GetProc(@glCompressedTexImage3D, 'glCompressedTexImage3D');
  GetProc(@glCompressedTexImage2D, 'glCompressedTexImage2D');
  GetProc(@glCompressedTexImage1D, 'glCompressedTexImage1D');
  GetProc(@glCompressedTexSubImage3D, 'glCompressedTexSubImage3D');
  GetProc(@glCompressedTexSubImage2D, 'glCompressedTexSubImage2D');
  GetProc(@glCompressedTexSubImage1D, 'glCompressedTexSubImage1D');
  GetProc(@glGetCompressedTexImage, 'glGetCompressedTexImage');
  if Initialized then
    OpenGLManager.MinorVersion := 3;
  {$endif}
  {$ifdef opengl_1_4}
  CheckExt('GL_ARB_depth_texture');
  CheckExt('GL_ARB_point_parameters');
  CheckExt('GL_ARB_shadow');
  CheckExt('GL_ARB_texture_env_crossbar');
  CheckExt('GL_ARB_texture_mirrored_repeat');
  CheckExt('GL_ARB_window_pos');
  CheckExt('GL_EXT_blend_color');
  CheckExt('GL_EXT_blend_func_separate');
  CheckExt('GL_EXT_blend_minmax');
  CheckExt('GL_EXT_blend_subtract');
  CheckExt('GL_EXT_fog_coord');
  CheckExt('GL_EXT_multi_draw_arrays');
  CheckExt('GL_EXT_secondary_color');
  CheckExt('GL_EXT_stencil_wrap');
  CheckExt('GL_EXT_texture_lod_bias');
  GetProc(@glPointParameterf, 'glPointParameterfARB');
  GetProc(@glPointParameterfv, 'glPointParameterfvARB');
  GetProc(@glWindowPos2d, 'glWindowPos2dARB');
  GetProc(@glWindowPos2f, 'glWindowPos2fARB');
  GetProc(@glWindowPos2i, 'glWindowPos2iARB');
  GetProc(@glWindowPos2s, 'glWindowPos2sARB');
  GetProc(@glWindowPos2dv, 'glWindowPos2dvARB');
  GetProc(@glWindowPos2fv, 'glWindowPos2fvARB');
  GetProc(@glWindowPos2iv, 'glWindowPos2ivARB');
  GetProc(@glWindowPos2sv, 'glWindowPos2svARB');
  GetProc(@glWindowPos3d, 'glWindowPos3dARB');
  GetProc(@glWindowPos3f, 'glWindowPos3fARB');
  GetProc(@glWindowPos3i, 'glWindowPos3iARB');
  GetProc(@glWindowPos3s, 'glWindowPos3sARB');
  GetProc(@glWindowPos3dv, 'glWindowPos3dvARB');
  GetProc(@glWindowPos3fv, 'glWindowPos3fvARB');
  GetProc(@glWindowPos3iv, 'glWindowPos3ivARB');
  GetProc(@glWindowPos3sv, 'glWindowPos3svARB');
  GetProc(@glBlendColor, 'glBlendColorEXT');
  GetProc(@glBlendFuncSeparate, 'glBlendFuncSeparateEXT');
  GetProc(@glBlendEquation, 'glBlendEquationEXT');
  GetProc(@glFogCoordf, 'glFogCoordfEXT');
  GetProc(@glFogCoordd, 'glFogCoorddEXT');
  GetProc(@glFogCoordfv, 'glFogCoordfvEXT');
  GetProc(@glFogCoorddv, 'glFogCoorddvEXT');
  GetProc(@glFogCoordPointer, 'glFogCoordPointerEXT');
  GetProc(@glMultiDrawArrays, 'glMultiDrawArraysEXT');
  GetProc(@glMultiDrawElements, 'glMultiDrawElementsEXT');
  GetProc(@glSecondaryColor3b, 'glSecondaryColor3bEXT');
  GetProc(@glSecondaryColor3s, 'glSecondaryColor3sEXT');
  GetProc(@glSecondaryColor3i, 'glSecondaryColor3iEXT');
  GetProc(@glSecondaryColor3f, 'glSecondaryColor3fEXT');
  GetProc(@glSecondaryColor3d, 'glSecondaryColor3dEXT');
  GetProc(@glSecondaryColor3ub, 'glSecondaryColor3ubEXT');
  GetProc(@glSecondaryColor3us, 'glSecondaryColor3usEXT');
  GetProc(@glSecondaryColor3ui, 'glSecondaryColor3uiEXT');
  GetProc(@glSecondaryColor3bv, 'glSecondaryColor3bvEXT');
  GetProc(@glSecondaryColor3sv, 'glSecondaryColor3svEXT');
  GetProc(@glSecondaryColor3iv, 'glSecondaryColor3ivEXT');
  GetProc(@glSecondaryColor3fv, 'glSecondaryColor3fvEXT');
  GetProc(@glSecondaryColor3dv, 'glSecondaryColor3dvEXT');
  GetProc(@glSecondaryColor3ubv, 'glSecondaryColor3ubvEXT');
  GetProc(@glSecondaryColor3usv, 'glSecondaryColor3usvEXT');
  GetProc(@glSecondaryColor3uiv, 'glSecondaryColor3uivEXT');
  GetProc(@glSecondaryColorPointer, 'glSecondaryColorPointerEXT');
  if Initialized then
    OpenGLManager.MinorVersion := 4;
  {$endif}
  {$ifdef opengl_1_5}
  CheckExt('GL_ARB_occlusion_query');
  CheckExt('GL_ARB_vertex_buffer_object');
  CheckExt('GL_EXT_shadow_funcs');
  GetProc(@glDeleteQueries, 'glDeleteQueriesARB');
  GetProc(@glIsQuery, 'glIsQueryARB');
  GetProc(@glBeginQuery, 'glBeginQueryARB');
  GetProc(@glEndQuery, 'glEndQueryARB');
  GetProc(@glGetQueryiv, 'glGetQueryivARB');
  GetProc(@glGetQueryObjectiv, 'glGetQueryObjectivARB');
  GetProc(@glGetQueryObjectuiv, 'glGetQueryObjectuivARB');
  GetProc(@glBindBuffer, 'glBindBufferARB');
  GetProc(@glDeleteBuffers, 'glDeleteBuffersARB');
  GetProc(@glGenBuffers, 'glGenBuffersARB');
  GetProc(@glIsBuffer, 'glIsBufferARB');
  GetProc(@glBufferData, 'glBufferDataARB');
  GetProc(@glBufferSubData, 'glBufferSubDataARB');
  GetProc(@glGetBufferSubData, 'glGetBufferSubDataARB');
  GetProc(@glMapBuffer, 'glMapBufferARB');
  GetProc(@glUnmapBuffer, 'glUnmapBufferARB');
  GetProc(@glGetBufferParameteriv, 'glGetBufferParameterivARB');
  GetProc(@glGetBufferPointerv, 'glGetBufferPointervARB');
  if Initialized then
    OpenGLManager.MinorVersion := 5;
  {$endif}
  {$ifdef opengl_2_0}
  CheckExt('GL_ARB_draw_buffers');
  CheckExt('GL_ARB_fragment_shader');
  CheckExt('GL_ARB_point_sprite');
  CheckExt('GL_ARB_shader_objects');
  CheckExt('GL_ARB_shading_language_100');
  CheckExt('GL_ARB_texture_non_power_of_two');
  CheckExt('GL_ARB_vertex_shader');
  CheckExt('GL_EXT_blend_equation_separate');
  CheckExt('GL_EXT_stencil_two_side');
  GetProc(@glDrawBuffers, 'glDrawBuffersARB');
  GetProc(@glDeleteObject, 'glDeleteObjectARB');
  GetProc(@glGetHandle, 'glGetHandleARB');
  GetProc(@glDetachObject, 'glDetachObjectARB');
  GetProc(@glCreateShaderObject, 'glCreateShaderObjectARB');
  GetProc(@glShaderSource, 'glShaderSourceARB');
  GetProc(@glCompileShader, 'glCompileShaderARB');
  GetProc(@glCreateProgramObject, 'glCreateProgramObjectARB');
  GetProc(@glAttachObject, 'glAttachObjectARB');
  GetProc(@glLinkProgram, 'glLinkProgramARB');
  GetProc(@glUseProgramObject, 'glUseProgramObjectARB');
  GetProc(@glValidateProgram, 'glValidateProgramARB');
  GetProc(@glUniform1f, 'glUniform1fARB');
  GetProc(@glUniform2f, 'glUniform2fARB');
  GetProc(@glUniform3f, 'glUniform3fARB');
  GetProc(@glUniform4f, 'glUniform4fARB');
  GetProc(@glUniform1i, 'glUniform1iARB');
  GetProc(@glUniform2i, 'glUniform2iARB');
  GetProc(@glUniform3i, 'glUniform3iARB');
  GetProc(@glUniform4i, 'glUniform4iARB');
  GetProc(@glUniform1fv, 'glUniform1fvARB');
  GetProc(@glUniform2fv, 'glUniform2fvARB');
  GetProc(@glUniform3fv, 'glUniform3fvARB');
  GetProc(@glUniform4fv, 'glUniform4fvARB');
  GetProc(@glUniform1iv, 'glUniform1ivARB');
  GetProc(@glUniform2iv, 'glUniform2ivARB');
  GetProc(@glUniform3iv, 'glUniform3ivARB');
  GetProc(@glUniform4iv, 'glUniform4ivARB');
  GetProc(@glUniformMatrix2fv, 'glUniformMatrix2fvARB');
  GetProc(@glUniformMatrix3fv, 'glUniformMatrix3fvARB');
  GetProc(@glUniformMatrix4fv, 'glUniformMatrix4fvARB');
  GetProc(@glGetObjectParameterfv, 'glGetObjectParameterfvARB');
  GetProc(@glGetObjectParameteriv, 'glGetObjectParameterivARB');
  GetProc(@glGetInfoLog, 'glGetInfoLogARB');
  GetProc(@glGetAttachedObjects, 'glGetAttachedObjectsARB');
  GetProc(@glGetUniformLocation, 'glGetUniformLocationARB');
  GetProc(@glGetActiveUniform, 'glGetActiveUniformARB');
  GetProc(@glGetUniformfv, 'glGetUniformfvARB');
  GetProc(@glGetUniformiv, 'glGetUniformivARB');
  GetProc(@glGetShaderSource, 'glGetShaderSourceARB');
  GetProc(@glVertexAttrib1s, 'glVertexAttrib1sARB');
  GetProc(@glVertexAttrib1f, 'glVertexAttrib1fARB');
  GetProc(@glVertexAttrib1d, 'glVertexAttrib1dARB');
  GetProc(@glVertexAttrib2s, 'glVertexAttrib2sARB');
  GetProc(@glVertexAttrib2f, 'glVertexAttrib2fARB');
  GetProc(@glVertexAttrib2d, 'glVertexAttrib2dARB');
  GetProc(@glVertexAttrib3s, 'glVertexAttrib3sARB');
  GetProc(@glVertexAttrib3f, 'glVertexAttrib3fARB');
  GetProc(@glVertexAttrib3d, 'glVertexAttrib3dARB');
  GetProc(@glVertexAttrib4s, 'glVertexAttrib4sARB');
  GetProc(@glVertexAttrib4f, 'glVertexAttrib4fARB');
  GetProc(@glVertexAttrib4d, 'glVertexAttrib4dARB');
  GetProc(@glVertexAttrib4Nub, 'glVertexAttrib4NubARB');
  GetProc(@glVertexAttrib1sv, 'glVertexAttrib1svARB');
  GetProc(@glVertexAttrib1fv, 'glVertexAttrib1fvARB');
  GetProc(@glVertexAttrib1dv, 'glVertexAttrib1dvARB');
  GetProc(@glVertexAttrib2sv, 'glVertexAttrib2svARB');
  GetProc(@glVertexAttrib2fv, 'glVertexAttrib2fvARB');
  GetProc(@glVertexAttrib2dv, 'glVertexAttrib2dvARB');
  GetProc(@glVertexAttrib3sv, 'glVertexAttrib3svARB');
  GetProc(@glVertexAttrib3fv, 'glVertexAttrib3fvARB');
  GetProc(@glVertexAttrib3dv, 'glVertexAttrib3dvARB');
  GetProc(@glVertexAttrib4bv, 'glVertexAttrib4bvARB');
  GetProc(@glVertexAttrib4sv, 'glVertexAttrib4svARB');
  GetProc(@glVertexAttrib4iv, 'glVertexAttrib4ivARB');
  GetProc(@glVertexAttrib4ubv, 'glVertexAttrib4ubvARB');
  GetProc(@glVertexAttrib4usv, 'glVertexAttrib4usvARB');
  GetProc(@glVertexAttrib4uiv, 'glVertexAttrib4uivARB');
  GetProc(@glVertexAttrib4fv, 'glVertexAttrib4fvARB');
  GetProc(@glVertexAttrib4dv, 'glVertexAttrib4dvARB');
  GetProc(@glVertexAttrib4Nbv, 'glVertexAttrib4NbvARB');
  GetProc(@glVertexAttrib4Nsv, 'glVertexAttrib4NsvARB');
  GetProc(@glVertexAttrib4Niv, 'glVertexAttrib4NivARB');
  GetProc(@glVertexAttrib4Nubv, 'glVertexAttrib4NubvARB');
  GetProc(@glVertexAttrib4Nusv, 'glVertexAttrib4NusvARB');
  GetProc(@glVertexAttrib4Nuiv, 'glVertexAttrib4NuivARB');
  GetProc(@glVertexAttribPointer, 'glVertexAttribPointerARB');
  GetProc(@glEnableVertexAttribArray, 'glEnableVertexAttribArrayARB');
  GetProc(@glDisableVertexAttribArray, 'glDisableVertexAttribArrayARB');
  GetProc(@glBindAttribLocation, 'glBindAttribLocationARB');
  GetProc(@glGetActiveAttrib, 'glGetActiveAttribARB');
  GetProc(@glGetAttribLocation, 'glGetAttribLocationARB');
  GetProc(@glGetVertexAttribdv, 'glGetVertexAttribdvARB');
  GetProc(@glGetVertexAttribfv, 'glGetVertexAttribfvARB');
  GetProc(@glGetVertexAttribiv, 'glGetVertexAttribivARB');
  GetProc(@glGetVertexAttribPointerv, 'glGetVertexAttribPointervARB');
  GetProc(@glBlendEquationSeparate, 'glBlendEquationSeparateEXT');
  GetProc(@glActiveStencilFace, 'glActiveStencilFaceEXT');
  if Initialized then
  begin
    OpenGLManager.MajorVersion := 2;
    OpenGLManager.MinorVersion := 0;
  end;
  {$endif}
  {$ifdef opengl_2_1}
  CheckExt('GL_ARB_pixel_buffer_object');
  CheckExt('GL_EXT_texture_sRGB');
  if Initialized then
    OpenGLManager.MinorVersion := 1;
  {$endif}
  {$ifdef opengl_3_0}
  CheckExt('GL_EXT_gpu_shader4');
  CheckExt('GL_ARB_color_buffer_float');
  CheckExt('GL_ARB_texture_float');
  CheckExt('GL_EXT_packed_float');
  CheckExt('GL_EXT_texture_shared_exponent');
  CheckExt('GL_ARB_half_float_pixel');
  CheckExt('GL_EXT_framebuffer_object');
  CheckExt('GL_EXT_framebuffer_sRGB');
  CheckExt('GL_EXT_framebuffer_blit');
  CheckExt('GL_EXT_framebuffer_multisample');
  CheckExt('GL_EXT_texture_integer');
  CheckExt('GL_EXT_packed_depth_stencil');
  CheckExt('GL_EXT_draw_buffers2');
  CheckExt('GL_EXT_texture_integer');
  CheckExt('GL_EXT_texture_array');
  CheckExt('GL_EXT_texture_compression_rgtc');
  CheckExt('GL_EXT_transform_feedback');
  GetProc(@glVertexAttribI1i, 'glVertexAttribI1iEXT');
  GetProc(@glVertexAttribI2i, 'glVertexAttribI2iEXT');
  GetProc(@glVertexAttribI3i, 'glVertexAttribI3iEXT');
  GetProc(@glVertexAttribI4i, 'glVertexAttribI4iEXT');
  GetProc(@glVertexAttribI1ui, 'glVertexAttribI1uiEXT');
  GetProc(@glVertexAttribI2ui, 'glVertexAttribI2uiEXT');
  GetProc(@glVertexAttribI3ui, 'glVertexAttribI3uiEXT');
  GetProc(@glVertexAttribI4ui, 'glVertexAttribI4uiEXT');
  GetProc(@glVertexAttribI1iv, 'glVertexAttribI1ivEXT');
  GetProc(@glVertexAttribI2iv, 'glVertexAttribI2ivEXT');
  GetProc(@glVertexAttribI3iv, 'glVertexAttribI3ivEXT');
  GetProc(@glVertexAttribI4iv, 'glVertexAttribI4ivEXT');
  GetProc(@glVertexAttribI1uiv, 'glVertexAttribI1uivEXT');
  GetProc(@glVertexAttribI2uiv, 'glVertexAttribI2uivEXT');
  GetProc(@glVertexAttribI3uiv, 'glVertexAttribI3uivEXT');
  GetProc(@glVertexAttribI4uiv, 'glVertexAttribI4uivEXT');
  GetProc(@glVertexAttribI4bv, 'glVertexAttribI4bvEXT');
  GetProc(@glVertexAttribI4sv, 'glVertexAttribI4svEXT');
  GetProc(@glVertexAttribI4ubv, 'glVertexAttribI4ubvEXT');
  GetProc(@glVertexAttribI4usv, 'glVertexAttribI4usvEXT');
  GetProc(@glGetVertexAttribIiv, 'glGetVertexAttribIivEXT');
  GetProc(@glGetVertexAttribIuiv, 'glGetVertexAttribIuivEXT');
  GetProc(@glUniform1ui, 'glUniform1uiEXT');
  GetProc(@glUniform2ui, 'glUniform2uiEXT');
  GetProc(@glUniform3ui, 'glUniform3uiEXT');
  GetProc(@glUniform4ui, 'glUniform4uiEXT');
  GetProc(@glUniform1uiv, 'glUniform1uivEXT');
  GetProc(@glUniform2uiv, 'glUniform2uivEXT');
  GetProc(@glUniform3uiv, 'glUniform3uivEXT');
  GetProc(@glUniform4uiv, 'glUniform4uivEXT');
  GetProc(@glGetUniformuiv, 'glGetUniformuivEXT');
  GetProc(@glBindFragDataLocation, 'glBindFragDataLocationEXT');
  GetProc(@glGetFragDataLocation, 'glGetFragDataLocationEXT');
  GetProc(@glClampColor, 'glClampColorARB');
  GetProc(@glIsRenderbuffer, 'glIsRenderbufferEXT');
  GetProc(@glBindRenderbuffer, 'glBindRenderbufferEXT');
  GetProc(@glDeleteRenderbuffers, 'glDeleteRenderbuffersEXT');
  GetProc(@glGenRenderbuffers, 'glGenRenderbuffersEXT');
  GetProc(@glRenderbufferStorage, 'glRenderbufferStorageEXT');
  GetProc(@glGetRenderbufferParameteriv, 'glGetRenderbufferParameterivEXT');
  GetProc(@glIsFramebuffer, 'glIsFramebufferEXT');
  GetProc(@glBindFramebuffer, 'glBindFramebufferEXT');
  GetProc(@glDeleteFramebuffers, 'glDeleteFramebuffersEXT');
  GetProc(@glGenFramebuffers, 'glGenFramebuffersEXT');
  GetProc(@glCheckFramebufferStatus, 'glCheckFramebufferStatusEXT');
  GetProc(@glFramebufferTexture1D, 'glFramebufferTexture1DEXT');
  GetProc(@glFramebufferTexture2D, 'glFramebufferTexture2DEXT');
  GetProc(@glFramebufferTexture3D, 'glFramebufferTexture3DEXT');
  GetProc(@glFramebufferRenderbuffer, 'glFramebufferRenderbufferEXT');
  GetProc(@glGetFramebufferAttachmentParameteriv, 'glGetFramebufferAttachmentParameterivEXT');
  GetProc(@glGenerateMipmap, 'glGenerateMipmapEXT');
  GetProc(@glBlitFramebuffer, 'glBlitFramebufferEXT');
  GetProc(@glRenderbufferStorageMultisample, 'glRenderbufferStorageMultisampleEXT');
  GetProc(@glClearColorIi, 'glClearColorIiEXT');
  GetProc(@glClearColorIui, 'glClearColorIuiEXT');
  GetProc(@glTexParameterIiv, 'glTexParameterIivEXT');
  GetProc(@glTexParameterIuiv, 'glTexParameterIuivEXT');
  GetProc(@glGetTexParameterIiv, 'glGetTexParameterIivEXT');
  GetProc(@glGetTexParameterIuiv, 'glGetTexParameterIuivEXT');
  GetProc(@glColorMaskIndexed, 'glColorMaskIndexedEXT');
  GetProc(@glGetBooleanIndexedv, 'glGetBooleanIndexedvEXT');
  GetProc(@glGetIntegerIndexedv, 'glGetIntegerIndexedvEXT');
  GetProc(@glEnableIndexed, 'glEnableIndexedEXT');
  GetProc(@glDisableIndexed, 'glDisableIndexedEXT');
  GetProc(@glIsEnabledIndexed, 'glIsEnabledIndexedEXT');
  GetProc(@glFramebufferTextureLayer, 'glFramebufferTextureLayerEXT');
  GetProc(@glBindBufferRange, 'glBindBufferRangeEXT');
  GetProc(@glBindBufferOffset, 'glBindBufferOffsetEXT');
  GetProc(@glBindBufferBase, 'glBindBufferBaseEXT');
  GetProc(@glBeginTransformFeedback, 'glBeginTransformFeedbackEXT');
  GetProc(@glEndTransformFeedback, 'glEndTransformFeedbackEXT');
  GetProc(@glTransformFeedbackVaryings, 'glTransformFeedbackVaryingsEXT');
  GetProc(@glGetTransformFeedbackVarying, 'glGetTransformFeedbackVaryingEXT');
  if Initialized then
  begin
    OpenGLManager.MajorVersion := 3;
    OpenGLManager.MinorVersion := 0;
  end;
  {$endif}
  Initialized := Initialized and (OpenGLManager.MajorVersion > 0);
  Result := Initialized;
end;

end.
